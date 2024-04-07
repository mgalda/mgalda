#' custom_metrics
#'
#'
#' @name custom_metrics
#' @rdname custom_metrics
#' @keywords internal
NULL

# nrmse -------------------------------------------------------------------

#' formula
#'
#' @name nrmse
#' @family numeric metrics
#' @family accuracy metrics
#' @param data un `data.frame` que contenga `truth` y `estimate`
#'
#' @param truth columna con valor observado. Habilitada para quasiquotation
#'
#' @param estimate  columna con valor estimado. Habilitada para quasiquotation
#'
#' @param na_rm valor `logical` indicando si `NA` deben ser omitidos
#'
#' @param ... Sin uso
#' @rdname custom_metrics
#' @return el rmse normalizado por la desviacion estandar
#' @export
#'
nrmse <- function(data, ...) {
  UseMethod("nrmse")
}

nrmse <-
  yardstick::new_numeric_metric(nrmse, direction = "minimize")

#' @rdname custom_metrics
#' @export


nrmse.data.frame <-
  function(data, truth, estimate, na_rm = TRUE, ...) {
    yardstick::metric_summarizer(
      metric_nm = "nrmse",
      metric_fn = nrmse_vec,
      data = data,
      truth = !!rlang::enquo(truth),
      estimate = !!rlang::enquo(estimate),
      na_rm = na_rm,
      ...
    )
  }

#'  nrmse vect
#' @rdname custom_metrics
#'
#' @export

nrmse_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  nrmse_impl <- function(truth, estimate) {
    .mse <- sum((truth - estimate)^2) / length(truth)
    .rmse <- sqrt(.mse)
    .nrmse <- .rmse / stats::sd(truth)
    return(.nrmse)
  }

  yardstick::metric_vec_template(
    metric_impl = nrmse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}



# rsq_sliding ------------------------------------------------------------------


#' R squared/traditional quantiles extremos
#'
#' @name rsq_sliding
#' @family numeric metrics
#' @family consistency metrics
#' @param data un `data.frame` que contenga `truth` y `estimate`
#'
#' @param truth columna con valor observado. Habilitada para quasiquotation
#'
#' @param estimate  columna con valor estimado. Habilitada para quasiquotation
#'
#' @param na_rm valor `logical` indicando si `NA` deben ser omitidos
#'
#' @param ... Sin uso
#' @rdname custom_metrics
#' @return el r2 para los percentiles extremos
#' @export
#'

rsq_sliding <- function(data, ...) {
  UseMethod("rsq_sliding")
}

rsq_sliding <-
  yardstick::new_numeric_metric(rsq_sliding, direction = "maximize")

#' @rdname custom_metrics
#' @export
rsq_sliding.data.frame <-
  function(data, truth, estimate, na_rm = TRUE, ...) {
    yardstick::metric_summarizer(
      metric_nm = "rsq_sliding",
      metric_fn = rsq_sliding_vec,
      data = data,
      truth = !!rlang::enquo(truth),
      estimate = !!rlang::enquo(estimate),
      na_rm = na_rm
    )
  }

#' @export
#' @rdname custom_metrics

rsq_sliding_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  rsq_sliding_impl <- function(truth, estimate) {

    tbl <- tibble(
      truth = truth,
      estimate = estimate
    ) %>%
      arrange(truth)

    rset_tbl <- sliding_resamples(dataset = tbl, times = 10)
    rset_tbl <- map(rset_tbl$splits, rsample::assessment)


    mean(map_dbl(
      .x = rset_tbl,
      .f = ~ yardstick::rsq_vec(
        truth = .x$truth,
        estimate = .x$estimate,
        na_rm = TRUE
      )
    ),na.rm = TRUE)
  }

  yardstick::metric_vec_template(
    metric_impl = rsq_sliding_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric"
  )
}




# w_accuracy --------------------------------------------------------------




#' w_accuracy
#'
#' @name w_accuracy
#' @rdname custom_metrics
#' @family class metrics
#' @export

w_accuracy <- function(data, ...) {
  UseMethod("w_accuracy")
}
w_accuracy <- yardstick::new_class_metric(w_accuracy,
                                          direction = "maximize"
)

#' @export
#' @rdname custom_metrics
w_accuracy.data.frame <- function(data,
                                  truth,
                                  estimate,
                                  na_rm = TRUE,
                                  ...) {
  yardstick::metric_summarizer(
    metric_nm = "w_accuracy",
    metric_fn = w_accuracy_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm
  )
}

#' @export
w_accuracy.table <- function(data, ...) {
  check_table <- function(x) {
    if (!identical(nrow(x), ncol(x))) {
      cat_stop("the table must have nrow = ncol")
    }
    if (!isTRUE(all.equal(rownames(x), colnames(x)))) {
      cat_stop("the table must the same groups in the same order")
    }
    invisible(NULL)
  }

  check_table(data)
  estimator <-
    yardstick::finalize_estimator(data, metric_class = "w_accuracy")

  metric_tibbler(
    .metric = "w_accuracy",
    .estimator = estimator,
    .estimate = w_accuracy_table_impl(data)
  )
}

#' @export
w_accuracy.matrix <- function(data, ...) {
  data <- as.table(data)
  w_accuracy.table(data)
}

#' @export
#' @rdname custom_metrics
w_accuracy_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  estimator <-
    yardstick::finalize_estimator(truth, metric_class = "w_accuracy")
  vec2table <- function(truth, estimate) {
    dnn <- c("Prediction", "Truth")
    if (!is.factor(truth)) {
      truth <- factor(truth)
    }

    if (!is.factor(estimate)) {
      if (!isTRUE(all.equal(sort(unique(estimate)), sort(levels(truth))))) {
        cat_stop("There are different possible values in `truth` and `estimate`")
      }
      estimate <- factor(estimate, levels = levels(truth))
    }

    est_lev <- levels(estimate)
    tru_lev <- levels(truth)

    if (length(est_lev) != length(tru_lev)) {
      cat_stop(
        "The columns containing the predicted and true ",
        "classes must have the same levels."
      )
    }

    if (!any(est_lev %in% tru_lev)) {
      cat_stop("`estimate` must contain levels that overlap `truth`.")
    }

    if (any(tru_lev != est_lev)) {
      cat_stop(
        "Levels are not in the same (or in the same order) for ",
        "true and predicted classes. ")
    }

    if (length(est_lev) < 2) {
      cat_stop(
        "There must be at least 2 factors levels in the ",
        "column containing the predicted class")
    }

    xtab <- table(estimate, truth, dnn = dnn)

    xtab
  }
  w_accuracy_impl <- function(truth, estimate) {
    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    w_accuracy_table_impl(xtab)
  }

  yardstick::metric_vec_template(
    metric_impl = w_accuracy_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor"
  )
}

w_accuracy_table_impl <- function(data) {
  w_accuracy_binary(data)
}

# binary and multiclass case are equivalent
w_accuracy_binary <- function(data) {
  mean(diag(data) / colSums(data))
}
