#' base_wflow
#'
#' @return base_wflow
#' @export
#' @rdname base_wflow

base_wflow <-
  function(dataset,
           outcome,
           rep_specs = 3,
           times = 4,
           rango = c(.3, .6),
           engines = NULL,
           ns = TRUE,
           poly = TRUE,
           add_step = NULL,
           rango_cols = c(.25, .7)) {
    UseMethod("base_wflow")
  }


base_wflowsmall <-
  function(dataset,
           outcome,
           rep_specs = 3,
           times = 4,
           rango = c(.7, .8),
           engines = NULL,
           add_step = NULL) {
    UseMethod("base_wflowsmall")
  }


#' base de construccion de modelos
#'
#' @name base_wflow
#' @rdname base_wflow
#' @export

base_wflow.default <-
  function(dataset,
           outcome,
           rep_specs = 3,
           times = 4,
           rango = c(.3, .6),
           engines = NULL,
           ns = TRUE,
           poly = TRUE,
           add_step = NULL,
           rango_cols = c(.25, .7)) {
    cat("Preparar Argumentos: Workflow")

    outcome <- rlang::enquo(outcome)

    splits <- initial_split(
      data = dataset,
      strata = !!outcome,
      prop = .8
    )

    train_data <- rsample::training(splits)

    check_rango_muestras(rango)

    res_arg <-
      list(
        .data = train_data,
        times = times,
        outcome = rlang::quo_squash(outcome),
        rango = rango
      )

    formula_outcome <-
      stringr::str_c(rlang::as_label(outcome), " ~ .", collapse = "")

    formula_outcome <- formula(formula_outcome, rlang::env())
    cat("\r Seleccionar model_spec")

    if (is.factor(dataset[[rlang::as_label(outcome)]])) {
        data("base_class", envir = environment())
      specs_base <- base_class
    } else if (is_dblint(dataset[[rlang::as_label(outcome)]])) {
      data("base_reg", envir = environment())
      specs_base <- base_reg
    }

    specs_base <-
      specs_base %>%
      dplyr::mutate(spec = purrr::map(
        .x = spec,
        .f = ~ dplyr::sample_n(
          tbl = .x,
          size = rep_specs,
          replace = dplyr::if_else(nrow(.x) < rep_specs, true = TRUE, FALSE)
        )
      )) %>%
      tidyr::unnest(spec)

    if (length(engines) > 0) {
      specs_base <- filter(specs_base, engine %in% engines)
    }

    cat("\r Resamples")

    specs_base <-
      specs_base %>%
      dplyr::mutate(
        splits = list(do.call(manual_resamples, res_arg)),
        id_spec = dplyr::row_number()
      ) %>%
      tidyr::unnest(splits)

    cat("\r Preproceso")
    specs_base <-
      specs_base %>%
      dplyr::mutate(recs = purrr::map(
        .x = splits,
        .f = ~ build_preprocess(
          dataset = rsample::analysis(.x),
          outcome = !!outcome,
          ns = ns,
          poly = poly,
          add_step = add_step,
          rango_cols = rango_cols
        )
      ))

    specs_base <-
      specs_base[!purrr::map_lgl(specs_base$recs, is.null), ]

    cat("\r Workflow")
    specs_base <-
      specs_base %>%
      dplyr::mutate(
        wflow = purrr::map2(
          .x = spec,
          .y = recs,
          .f = ~ workflows::workflow() %>%
            workflows::add_model(spec = .x) %>%
            workflows::add_recipe(recipe = .y)
        ),
        recs = NULL,
        spec = NULL,
        wflow_id = stringr::str_c(id_spec, id, sep = "_"),
        .before = tidyr::everything(),
        .keep = "unused"
      )

    model_stack <-
      structure(
        list(
          outcome = as_label(outcome),
          dataset = dataset,
          splits = list(
            splits = splits,
            test_data = testing(splits),
            train_data = train_data
          ),
          members_candidates = specs_base,
          trained = FALSE
        ),
        class = c("base_wflow", "list")
      )

    if (is.factor(dataset[[rlang::as_label(outcome)]])) {

      model_stack[["lvls"]] <- levels(dataset[[rlang::as_label(outcome)]])
    }


    model_stack
  }


#' predict sl clas
#' @rdname base_wflow
#' @export

base_wflowsmall <-
  function(dataset,
           outcome,
           rep_specs = 3,
           times = 4,
           rango = c(.7, .8),
           engines = NULL,
           add_step = NULL) {
    cat("Preparar Argumentos: Workflow")

    outcome <- rlang::enquo(outcome)

    splits <- initial_split(
      data = dataset,
      strata = !!outcome,
      prop = .8
    )

    train_data <- rsample::training(splits)

    check_rango_muestras(rango)

    res_arg <-
      list(
        .data = train_data,
        times = times,
        outcome = rlang::quo_squash(outcome),
        rango = rango
      )

    formula_outcome <-
      stringr::str_c(rlang::as_label(outcome), " ~ .", collapse = "")

    formula_outcome <- formula(formula_outcome, rlang::env())
    cat("\r Seleccionar model_spec")

    if (is.factor(dataset[[rlang::as_label(outcome)]])) {
      data("base_class", envir = environment())
      specs_base <- base_class
    } else if (is_dblint(dataset[[rlang::as_label(outcome)]])) {
      data("base_reg", envir = environment())
      specs_base <- base_reg
    }

    specs_base <-
      specs_base %>%
      dplyr::mutate(spec = purrr::map(
        .x = spec,
        .f = ~ dplyr::sample_n(
          tbl = .x,
          size = rep_specs,
          replace = dplyr::if_else(nrow(.x) < rep_specs, true = TRUE, FALSE)
        )
      )) %>%
      tidyr::unnest(spec)

    if (length(engines) > 0) {
      specs_base <- filter(specs_base, engine %in% engines)
    }

    cat("\r Resamples")

    specs_base <-
      specs_base %>%
      dplyr::mutate(
        splits = list(do.call(manual_resamples, res_arg)),
        id_spec = dplyr::row_number()
      ) %>%
      tidyr::unnest(splits)

    cat("\r Preproceso")
    specs_base <-
      specs_base %>%
      dplyr::mutate(recs = purrr::map(
        .x = splits,
        .f = ~ build_preprocess_small(
          dataset = rsample::analysis(.x),
          outcome = !!outcome,
          add_step = add_step
        )
      ))

    specs_base <-
      specs_base[!purrr::map_lgl(specs_base$recs, is.null), ]

    cat("\r Workflow")
    specs_base <-
      specs_base %>%
      dplyr::mutate(
        wflow = purrr::map2(
          .x = spec,
          .y = recs,
          .f = ~ workflows::workflow() %>%
            workflows::add_model(spec = .x) %>%
            workflows::add_recipe(recipe = .y)
        ),
        recs = NULL,
        spec = NULL,
        wflow_id = stringr::str_c(id_spec, id, sep = "_"),
        .before = tidyr::everything(),
        .keep = "unused"
      )

    model_stack <-
      structure(
        list(
          outcome = as_label(outcome),
          dataset = dataset,
          splits = list(
            splits = splits,
            test_data = testing(splits),
            train_data = train_data
          ),
          members_candidates = specs_base,
          trained = FALSE
        ),
        class = c("base_wflow", "list")
      )
    if (is.factor(dataset[[rlang::as_label(outcome)]])) {

      model_stack[["lvls"]] <- levels(dataset[[rlang::as_label(outcome)]])
    }


    model_stack
  }


#' @name base_wflow
#' @aliases print.base_wflow
#' @rdname base_wflow
#' @method print base_wflow
#' @export

print.base_wflow <-
  function(x) {
    res <-
      x$members_candidates %>%
      count(engine, parsnip) %>%
      as_tibble() %>%
      arrange(engine, parsnip) %>%
      mutate(parsnip = paste(parsnip, " (", n, ")", sep = "")) %>%
      group_by(engine) %>%
      summarise(parsnip = paste(parsnip, collapse = ", ")) %>%
      mutate(engine = paste("\n\033[1m     ", engine, ":\033[22m \n         ", parsnip, sep = "")) %>%
      pull(engine)

    cat(paste(rep("-", max(
      20, options()$width - 38
    )), collapse = ""), "\n")

    if (x$trained) {
      cat("\033[1m base wflow\033[22m \033[3m(entrenado)\033[23m  \n")
      cat("\033[1m ", attr(x, "model_mode"), "\033[22m \n")
      cat("total modelos: ", nrow(x), "\n")
    } else {
      cat("\033[1m base wflow\033[22m \033[3m(no entrenado)\033[23m  \n")
      cat("total modelos: ", nrow(x$members_candidates), "\n")
    }

    cat(res)
  }


#' @name base_wflow
#' @aliases print.base_wflowsmall
#' @rdname base_wflow
#' @method print base_wflowsmall
#' @export

print.base_wflowsmall <-
  function(x) {
    res <-
      x$members_candidates %>%
      count(engine, parsnip) %>%
      as_tibble() %>%
      arrange(engine, parsnip) %>%
      mutate(parsnip = paste(parsnip, " (", n, ")", sep = "")) %>%
      group_by(engine) %>%
      summarise(parsnip = paste(parsnip, collapse = ", ")) %>%
      mutate(engine = paste("\n\033[1m     ", engine, ":\033[22m \n         ", parsnip, sep = "")) %>%
      pull(engine)

    cat(paste(rep("-", max(
      20, options()$width - 38
    )), collapse = ""), "\n")

    if (x$trained) {
      cat("\033[1m base wflow\033[22m \033[3m(entrenado)\033[23m  \n")
      cat("\033[1m ", attr(x, "model_mode"), "\033[22m \n")
      cat("total modelos: ", nrow(x), "\n")
    } else {
      cat("\033[1m base wflow\033[22m \033[3m(no entrenado)\033[23m  \n")
      cat("total modelos: ", nrow(x$members_candidates), "\n")
    }

    cat(res)
  }
