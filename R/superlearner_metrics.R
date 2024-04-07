#' superlearner_metrics
#'
#'
#' @name superlearner_metrics
#' @rdname superlearner_metrics
#' @keywords internal
NULL

#' @export

num_metrics <-
  function(object, test_data, outcome) {
    reqlibs()
    cat(".")

    outcome <- rlang::enquo(outcome)

    truth_estimate <-
      dplyr::tibble(
        truth = dplyr::pull(test_data, !!outcome),
        estimate = predict(object, test_data)$.pred
      )

    truth_u <- mean(truth_estimate$truth)
    truth_sd <- sd(truth_estimate$truth)

    samples <-
      kfolds(
        dataset = truth_estimate,
        times = 5,
        rep = 3,
        reverse = T
      )$splits

    samples <-
      lapply(samples, rsample::analysis)

    samples <-
      lapply(samples, function(x) {
        list(
          truth = (x$truth - truth_u) / truth_sd,
          estimate = (x$estimate - truth_u) / truth_sd
        )
      })

    metrics_res <-
      purrr::map_dfr(
        .x = samples,
        ~ do.call(num_metrics_impl, .x) %>%
          dplyr::mutate(
            rank = dplyr::case_when(
              direction == "max" ~ score,
              direction == "min" ~ -1 * score,
              direction == "zero" ~ -1 * abs(score)
            )
          )
      ) %>%
      group_summarise(
        group_vars = c("metrica"),
        score = mean(score),
        rank = mean(rank)
      )


    metrics_res
  }

#' @export

clasprob_metrics <-
  function(object, test_data, outcome) {
    reqlibs()
    cat(".")

    outcome <- rlang::enquo(outcome)

    truth_estimate <-
      dplyr::tibble(
        truth = dplyr::pull(test_data, !!outcome),
        pred_class = predict(object, test_data)$.pred_class
      ) %>%
      dplyr::bind_cols(standardized_predict(
        object = object,
        new_data =  test_data,
        lvls = levels(dplyr::pull(test_data, !!outcome))
      ))

    samples <-
      kfolds(
        dataset = truth_estimate,
        times = 5,
        rep = 3,
        reverse = T
      )$splits

    samples <-
      lapply(samples, rsample::analysis)

    samples <-
      lapply(samples, function(x) {
        list(
          truth = x$truth,
          estimate = dplyr::select(x, contains(".pred_")),
          x = mgalda::vec2table(x$truth, x$pred_class)
        )
      })

    metrics_res <-
      purrr::map_dfr(
        .x = samples,
        ~ do.call(clasprob_metrics_impl, .x) %>%
          dplyr::mutate(
            rank = dplyr::case_when(
              direction == "max" ~ score,
              direction == "min" ~ -1 * score,
              direction == "zero" ~ -1 * abs(score)
            )
          )
      ) %>%
      group_summarise(
        group_vars = c("metrica"),
        score = mean(score),
        rank = mean(rank)
      )


    metrics_res
  }

#' @export

evalnum <- function(truth, estimate) {
  u <- mean(truth)
  s <- sd(truth)
  truth <- (truth - u) / s
  estimate <- (estimate - u) / s
  num_metrics_impl(truth, estimate)
}

#' @export

clasprobeval <- function(truth, pred_class, estimate) {
  x <- vec2table(truth, pred_class)
  clasprob_metrics_impl(truth, estimate, x)
}

#' @keywords internal

num_metrics_impl <-
  function(truth, estimate) {
    purrr::map_dfr(mets$num, ~ .x(truth, estimate), .id = "metrica")
  }


#' @keywords internal

clasprob_metrics_impl <-
  function(truth, estimate, x) {
    agr <- list(
      accuracy = list(x = x),
      j_index = list(x = x),
      kap = list(x = x),
      mn_log_loss = list(truth = truth, estimate = estimate),
      npv = list(x = x),
      ppv = list(x = x),
      precision = list(x = x),
      recall = list(x = x),
      roc_auc = list(truth = truth, estimate = estimate),
      sens = list(x = x),
      spec = list(x = x),
      w_accuracy = list(x = x)
    )
    purrr::map2_dfr(
      .x = mets$classprob,
      .y = agr,
      .f = ~ do.call(what = .x, args = .y),
      .id = "metrica"
    )
  }

#' @export

mwflow_reg <-
  function(object, test_data, outcome) {
    outcome <- rlang::enquo(outcome)
    metric_object <-
      purrr::map2_dfr(
        .x = object$.extracts,
        .y = object$id,
        ~ dplyr::mutate(.x, id_splits = .y),
        .id = "row"
      ) %>%
      dplyr::mutate(
        metricas =
          purrr::map(
            .x = .extracts,
            .f = ~ num_metrics(object = .x, test_data = test_data, !!outcome)
          )
      )

    metric_object <- metric_object %>%
      dplyr::mutate(no_valid = purrr::map_dbl(.x = metricas, .f = ~ sum(is_numinvalid(.x$score)))) %>%
      dplyr::filter(no_valid == 0)
    rescalar <-
      function(x,
               to = c(0, 1),
               from = range(x, na.rm = TRUE, finite = TRUE)) {
        (x - from[1]) / diff(from) * diff(to) + to[1]
      }

    sm <-
      dplyr::bind_rows(metric_object$metricas, .id = "row") %>%
      group_mutate(
        group_vars = c("metrica"),
        rank = rescalar(rank)
      ) %>%
      dplyr::select(!score) %>%
      tidyr::pivot_wider(names_from = metrica, values_from = rank) %>%
      dplyr::select(where(~ sum(is_numinvalid(.x)) == 0))%>%
      tidyr::drop_na()

    sm <- sm[which.max(rowSums(sm[, -1])), 1]

    metric_object %>%
      as_tibble() %>%
      dplyr::slice(as.numeric(sm$row)) %>%
      dplyr::select(penalty, mixture, .extracts)
  }

#' @export

mwflow_clasprob <-
  function(object, test_data, outcome) {
    outcome <- rlang::enquo(outcome)
    metric_object <-
      purrr::map2_dfr(
        .x = object$.extracts,
        .y = object$id,
        ~ dplyr::mutate(.x, id_splits = .y),
        .id = "row"
      ) %>%
      dplyr::mutate(
        metricas =
          purrr::map(
            .x = .extracts,
            .f = ~ clasprob_metrics(object = .x, test_data = test_data, !!outcome)
          )
      )

    metric_object <- metric_object %>%
      dplyr::mutate(no_valid = purrr::map_dbl(
        .x = metricas,
        .f = ~ sum(is_numinvalid(.x$score))
      )) %>%
      dplyr::filter(no_valid == 0)

    rescalar <-
      function(x,
               to = c(0, 1),
               from = range(x, na.rm = TRUE, finite = TRUE)) {
        (x - from[1]) / diff(from) * diff(to) + to[1]
      }

    sm <-
      dplyr::bind_rows(metric_object$metricas, .id = "row") %>%
      group_mutate(
        group_vars = c("metrica"),
        rank = rescalar(rank)
      ) %>%
      dplyr::select(!score) %>%
      tidyr::pivot_wider(names_from = metrica, values_from = rank) %>%
      dplyr::select(where(~ sum(is_numinvalid(.x)) == 0))%>%
      tidyr::drop_na()

    sm <- sm[which.max(rowSums(sm[, -1])), 1]

    metric_object %>%
      dplyr::slice(as.numeric(sm$row)) %>%
      dplyr::select(penalty, mixture, .extracts)
  }
