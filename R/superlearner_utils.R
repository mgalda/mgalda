# outcome_ ----------------------------------------------------------------
#' superlearner_utils
#'
#' @rdname superlearner_utils
#' @name superlearner_utils
#' @keywords internal
NULL

#' @rdname superlearner_utils
#' @export
outcome_expr <- function(x) {
  UseMethod("outcome_expr")
}
#' @rdname superlearner_utils
#' @export
outcome_quo <- function(x) {
  UseMethod("outcome_quo")
}
#' @rdname superlearner_utils
#' @export
outcome_chr <- function(x) {
  UseMethod("outcome_chr")
}


#' @export
outcome_expr.default <- function(x) {
  generic_default()
}
#' @export
outcome_quo.default <- function(x) {
  generic_default()
}
#' @export
outcome_chr.default <- function(x) {
  generic_default()
}


#' @export
outcome_chr.ensemble_models <- function(x) {
  lang2str(x$base_data$outcome)
}
#' @export
outcome_expr.ensemble_models <- function(x) {
  x$base_data$outcome$expr
}
#' @export
outcome_quo.ensemble_models <- function(x) {
  x$base_data$outcome
}

#' @export

outcome_chr.new_ensemble_models <- function(x) {
  lang2str(x$base_data$outcome)
}
#' @export
outcome_expr.new_ensemble_models <- function(x) {
  x$base_data$outcome$expr
}
#' @export
outcome_quo.new_ensemble_models <- function(x) {
  x$base_data$outcome
}


#' @export
outcome_chr.recipe <- function(x) {
  x$var_info$variable[x$var_info$role == "outcome"]
}
#' @export
outcome_expr.recipe <- function(x) {
  str2lang(x$var_info$variable[x$var_info$role == "outcome"])
}
#' @export
outcome_quo.recipe <- function(x) {
  xchr <- x$var_info$variable[x$var_info$role == "outcome"]
  out <- parse_quo(
    x = xchr,
    env = env(dataset = x$template, outcome_name = xchr)
  )
  out <- as_lazy(out)
  if (set_mode_model(x)[1] == "classification") {
    attr(out, "lvls") <- levels(x$template[[xchr]])
  }
  out
}


#' @export
outcome_chr.workflow <- function(x) {
  x <- workflows::extract_preprocessor(x)
  outcome_chr(x)
}
#' @export
outcome_expr.workflow <- function(x) {
  x <- workflows::extract_preprocessor(x)
  outcome_expr(x)
}
#' @export
outcome_quo.workflow <- function(x) {
  x <- workflows::extract_preprocessor(x)
  outcome_quo(x)
}

#' @export
outcome_quo._glm <- function(x) {
  out <- rlang::sym(x$preproc$y_var)
  env <- attr(x$fit$terms,".Environment")
  out <- rlang::new_quosure(out,env)
  out <- as_lazy(out)
  if (x$spec$mode == "classification") {
    attr(out, "lvls") <- x$lvl
  }
  out
}
#' @export
outcome_chr._glm <- function(x) {
  x$preproc$y_var
}
#' @export
outcome_expr._glm <- function(x) {
  rlang::sym(x$preproc$y_var)
}

#' @export
outcome_quo._lognet <- function(x) {
  out <- rlang::sym(x$preproc$y_var)
  out <- rlang::quo(!!out)
  out <- as_lazy(out)
  if (x$spec$mode == "classification") {
    attr(out, "lvls") <- x$lvl
  }
  out
}
#' @export
outcome_chr._lognet <- function(x) {
  x$preproc$y_var
}
#' @export
outcome_expr._lognet <- function(x) {
  rlang::sym(x$preproc$y_var)
}

# eval_model --------------------------------------------------------------

#' eval_models
#'
#' @rdname eval_models
#' @name eval_models
#' @export
eval_models <- function(object, ...) {
  UseMethod("eval_models")
}

#' @export
eval_models.default <- function(object, ...) {
  generic_default()
}

#' @export
eval_models.model_fit <-
  function(object, test_data, id = NULL) {
    eval_models_tidymodels(
      object = object,
      test_data = test_data,
      id = id
    )
  }

#' @export
eval_models.workflow <-
  function(object, test_data, id = NULL) {
    eval_models_tidymodels(
      object = object,
      test_data = test_data,
      id = id
    )
  }

#' @export
eval_models.stacking <-
  function(object, test_data, id = NULL) {
    eval_models_superlearner(
      object = object,
      test_data = test_data,
      id = id
    )
  }

#' @export
eval_models.softvoting <-
  function(object, test_data, id = NULL) {
    eval_models_superlearner(
      object = object,
      test_data = test_data,
      id = id
    )
  }

# set_mode_model ----------------------------------------------------------

#' set_mode_model
#'
#' @rdname set_mode_model
#' @name set_mode_model
#' @export
set_mode_model <- function(x, ...) {
  UseMethod("set_mode_model")
}
#' @export
set_mode_model.default <- function(x) {
  generic_default()
}
#' @export
set_mode_model.data.frame <- function(x, outcome) {
  y <- x[[outcome]]
  if (is_dblint(y)) {
    type <- "regression"
  } else if (is.factor(y)) {
    if (length(levels(y)) == 2) {
      type <- c("classification", "class")
    } else if (length(levels(y)) >= 3) {
      type <- c("classification", "multiclass")
    }
  }
  type
}
#' @export
set_mode_model.recipe <- function(x) {
  y <- x$var_info$variable[x$var_info$role == "outcome"]
  y <- x$template[[y]]
  if (is_dblint(y)) {
    type <- "regression"
  } else if (is.factor(y)) {
    if (length(levels(y)) == 2) {
      type <- c("classification", "class")
    } else if (length(levels(y)) >= 3) {
      type <- c("classification", "multiclass")
    }
  }
  type
}
#' @export
set_mode_model.workflow <- function(x) {
  x <- workflows::extract_preprocessor(x)
  y <- x$var_info$variable[x$var_info$role == "outcome"]
  y <- x$template[[y]]
  if (is_dblint(y)) {
    type <- "regression"
  } else if (is.factor(y)) {
    if (length(levels(y)) == 2) {
      type <- c("classification", "class")
    } else if (length(levels(y)) >= 3) {
      type <- c("classification", "multiclass")
    }
  }
  type
}
#' @export
set_mode_model.NULL <- function(x) {
  NULL
}
#' @export
set_mode_model.formula <- function(x) {
  outcome <- rlang::f_lhs(x)
  outcome <- lang2str(outcome)
  x <- do_try(model.frame(formula = x, data = environment(x)$object))
  if (is_empty(x)) {
    cat_stop("formula no tiene environment")
  }
  y <- x[[outcome]]
  if (is_dblint(y)) {
    type <- "regression"
  } else if (is.factor(y)) {
    if (length(levels(y)) == 2) {
      type <- c("classification", "class")
    } else if (length(levels(y)) >= 3) {
      type <- c("classification", "multiclass")
    }
  }
  type
}

# pull_importances --------------------------------------------------------

#' pull_importances
#'
#' @rdname pull_importances
#' @name pull_importances
#' @export
pull_importances <- function(object, scaled = TRUE, ...) {
  UseMethod("pull_importances", object)
}

#' @keywords internal
rescale_vip <- function(x) {
  (x - min(x)) / (max(x) - min(x)) * 100
}

#' @export
pull_importances.default <- function(object, scaled = TRUE, ...) {
 cat_message(paste(
    "No method for pulling feature importances is defined for",
    class(object)[1]
  ))
}


#' @export
pull_importances._xgb.Booster <-
  function(object,
           scaled = TRUE,
           type = "Gain",
           ...) {
    call <- rlang::call2(
      .fn = "xgb.importance",
      .ns = "xgboost",
      model = object$fit
    )
    scores <- rlang::eval_tidy(call)
    scores <-
      tibble(feature = scores$Feature, importance = scores[[type]])

    if (scaled) {
      scores$importance <- rescale_vip(scores$importance)
    }

    scores
  }

#' @export
pull_importances._C5.0 <- function(object, scaled = TRUE, ...) {
  others <- list(...)

  if (!length(others)) {
    others$metric <- "usage"
  }

  call <-
    rlang::call2(
      .fn = "C5imp",
      .ns = "C50",
      object = object$fit,
      !!!others
    )
  scores <- rlang::eval_tidy(call)

  scores <-
    tibble(feature = rownames(scores), importance = scores$Overall)

  if (scaled) {
    scores$importance <- rescale_vip(scores$importance)
  }

  scores
}

#' @export
pull_importances._H2OMultinomialModel <-
  function(object, scaled = TRUE, ...) {
    call <-
      rlang::call2(
        .fn = "h2o.varimp",
        .ns = "h2o",
        object = object$fit
      )
    scores <- rlang::eval_tidy(call)

    scores <-
      tibble(
        feature = scores$variable,
        importance = scores$relative_importance
      )

    if (scaled) {
      scores$importance <- rescale_vip(scores$importance)
    }

    scores
  }

#' @export
pull_importances._H2ORegressionModel <-
  function(object, scaled = TRUE, ...) {
    call <-
      rlang::call2(
        .fn = "h2o.varimp",
        .ns = "h2o",
        object = object$fit
      )
    scores <- rlang::eval_tidy(call)

    scores <-
      tibble(
        feature = scores$variable,
        importance = scores$relative_importance
      )

    if (scaled) {
      scores$importance <- rescale_vip(scores$importance)
    }

    scores
  }

#' @export
pull_importances._ranger <- function(object, scaled = TRUE, ...) {
  call <-
    rlang::call2(.fn = "importance", .ns = "ranger", x = object$fit)
  scores <- rlang::eval_tidy(call)

  scores <-
    tibble(feature = names(scores), importance = as_num(scores))

  if (scaled) {
    scores$importance <- rescale_vip(scores$importance)
  }

  scores
}

#' @export
pull_importances._cubist <- function(object, scaled = TRUE, ...) {
  scores <- object$fit$usage

  scores <-
    tibble(feature = scores$Variable, importance = scores$Model)

  if (scaled) {
    scores$importance <- rescale_vip(scores$importance)
  }

  scores
}

#' @export
pull_importances._earth <- function(object, scaled = TRUE, ...) {
  call <-
    rlang::call2(
      .fn = "evimp",
      .ns = "earth",
      object = object$fit
    )
  scores <- rlang::eval_tidy(call)

  scores <-
    tibble(feature = rownames(scores), importance = scores[, "rss"])

  if (scaled) {
    scores$importance <- rescale_vip(scores$importance)
  }

  scores
}

#' @export
pull_importances._lm <-
  function(object,
           scaled = FALSE,
           intercept = FALSE,
           ...) {
    scores <- tibble(
      feature = names(stats::coefficients(object$fit)),
      importance = stats::coefficients(object$fit)
    )

    if (!intercept) {
      scores <- scores[scores$feature != "(Intercept)", ]
    }

    if (scaled) {
      scores$importance <- rescale_vip(abs(scores$importance))
    }

    scores
  }

#' @export
pull_importances._glm <-
  function(object,
           scaled = FALSE,
           intercept = FALSE,
           ...) {
    scores <- tibble(
      feature = names(stats::coefficients(object$fit)),
      importance = stats::coefficients(object$fit)
    )

    if (!intercept) {
      scores <- scores[scores$feature != "(Intercept)", ]
    }

    if (scaled) {
      scores$importance <- rescale_vip(abs(scores$importance))
    }

    scores
  }

#' @export
pull_importances._elnet <-
  function(object,
           scaled = FALSE,
           intercept = FALSE,
           penalty = NULL,
           ...) {
    if (is.null(penalty)) {
      penalty <- object$spec$args$penalty
    }

    if (is.null(penalty)) {
      cat_stop(
        "model specification was not fitted using a `penalty` value. `penalty` should be supplied to the `pull_importances` method"
      )
    }

    scores <-
      tibble(
        feature = rownames(stats::coef(object$fit, s = penalty)),
        importance = stats::coef(object$fit, s = penalty)[, 1]
      )

    if (!intercept) {
      scores <- scores[scores$feature != "(Intercept)", ]
    }

    if (scaled) {
      scores$importance <- rescale_vip(abs(scores$importance))
    }

    scores
  }

#' @export
pull_importances._lognet <-
  function(object,
           scaled = FALSE,
           intercept = FALSE,
           penalty = NULL,
           ...) {
    if (!is.null(penalty)) {
      s <- penalty
    } else {
      s <- object$spec$args$penalty
    }

    if (is.null(s)) {
      cat_stop(
        "model specification was not fitted using a `penalty` value. `penalty` should be supplied to the `pull_importances` method"
      )
    }

    scores <- tibble(
      feature = rownames(stats::coef(object$fit, s = s)),
      importance = stats::coef(object$fit, s = s)[, 1]
    )

    if (!intercept) {
      scores <- scores[scores$feature != "(Intercept)", ]
    }

    if (scaled) {
      scores$importance <- rescale_vip(abs(scores$importance))
    }

    scores
  }

#' @export
pull_importances._randomForest <-
  function(object, scaled = TRUE, ...) {
    scores <- tibble(
      feature = rownames(object$fit$importance),
      importance = object$fit$importance
    )

    if (scaled) {
      scores$importance <- rescale_vip(scores$importance)
    }

    scores
  }

#' @export
pull_importances._rpart <- function(object, scaled = TRUE, ...) {
  scores <- tibble(
    feature = names(object$fit$variable.importance),
    importance = object$fit$variable.importance
  )

  if (scaled) {
    scores$importance <- rescale_vip(scores$importance)
  }

  scores
}


# get_mode ----------------------------------------------------------------

#' get_mode
#'
#' @rdname get_mode
#' @name get_mode
#' @export
get_mode <- function(x, ...) {
  UseMethod("get_mode")
}

#' @export
get_mode.default <- function(x, ...) {
  cat_message(
    "No method for { fn } is defined",
    "for { paste0(class(x), collapse = ',') }.",
    .sep = "",
    fn = stringr::str_remove(
      string = deparse(match.call()[[1]]),
      pattern = stringr::fixed(".default")
    )
  )
}

#' @export
get_mode.ensemble_models <- function(x) {
  x$model_mode
}

#' @export
get_mode.tunensemble_models <- function(x) {
  x$model_mode
}

#' @export
get_mode.new_ensemble_models <- function(x) {
  x$model_mode
}

#' @export
get_mode.workflow <- function(x) {
  mode <- try(workflows::extract_fit_parsnip(x)$spec$mode, silent = T)

  if (inherits(mode, "try-error")) {
    mode <- workflows::extract_spec_parsnip(x)$mode
  }

  if (mode == "classification") {
    lvls <-
      workflows::extract_recipe(x)$levels[[outcome_chr(x)]]$values

    mode <-
      c(mode, ifelse(length(lvls) == 2, "class", "multiclass"))
  }
  mode
}

#' @export
get_mode.model_fit <- function(x) {
  mode <- x$spec$mode

  if (mode == "classification") {
    lvls <- x$lvl

    mode <-
      c(mode, ifelse(length(lvls) == 2, "class", "multiclass"))
  }
  mode
}

# fit_safe ----------------------------------------------------------------

#' fit_safe
#'
#' @rdname fit_safe
#' @name fit_safe
#' @export
fit_safe <-
  function(object, ...) {
    UseMethod("fit_safe")
  }

#' @export
fit_safe.default <- function(object, ...) {
  generic_default()
}

#' @export
fit_safe.workflow <-
  function(object,
           data,
           verbosity = 1,
           catch = FALSE,
           ...) {
    reqlibs_impl()
    fit_workflow <-
      function(object, data, control = workflows::control_workflow()) {
        ellipsis::check_dots_empty()
        if (is_empty(data)) {
          cat_warn("`data` must be provided to fit a workflow.")
          return(NULL)
        }
        workflow <- object
        workflow <- workflows::.fit_pre(workflow, data)
        workflow <- workflows::.fit_model(workflow, control)
        workflow <- workflows::.fit_finalize(workflow)
        workflow
      }
    if (inherits(data, c("rset", "rsplit"))) {
      data <- rsample::analysis(data)
    }

    if (!inherits(data, c("data.frame"))) {
      cat_warn(
        "`data` must be 'data.frame' to",
        " fit a { paste0(class(object), collapse = ',') }."
      )
      return(NULL)
    }

    control <-
      parsnip::control_parsnip(verbosity = verbosity, catch = catch)

    control <-
      workflows::control_workflow(control)

    if (is.null(object$pre$mold)) {
      fitted <-
        do_try(fit_workflow(
          object = object,
          data = data,
          control = control
        ))

      if (is.null(fitted)) {
        cat_warn("error on 'fit_workflow'")
        return(NULL)
      }
    } else {
      fitted <-
        do_try(workflows::.fit_model(workflow = object, control = control))
      if (is.null(fitted)) {
        cat_warn("error on 'workflows::.fit_model'")
        return(NULL)
      }
      fitted <- do_try(workflows::.fit_finalize(fitted))
      if (is.null(fitted)) {
        cat_warn("error on 'workflows::.fit_finalize'")
        return(NULL)
      }
    }
    fitted
  }


#' @export
fit_safe.model_spec <-
  function(object,
           formula,
           data,
           verbosity = 1,
           catch = FALSE,
           ...) {
    reqlibs_impl()

    if (inherits(data, c("rset", "rsplit"))) {
      data <- rsample::analysis(data)
    }

    if (!inherits(data, c("data.frame"))) {
      cat_warn(
        "`data` must be 'data.frame' to",
        "fit a { paste0(class(object), collapse = ',') } ."
      )
      return(NULL)
    }

    control <-
      parsnip::control_parsnip(verbosity = verbosity, catch = catch, ...)

    fitted <-
      do_try(parsnip::fit.model_spec(
        object = object,
        formula = formula,
        data = data,
        control = control
      ))

    if (is.null(fitted)) {
      cat_warn("error on 'parsnip::fit.model_spec'")
      return(NULL)
    }
    fitted
  }
# misc --------------------------------------------------------------------

two_class <- function (...) {
  mets <- yardstick::metric_set(sens, spec, j_index)
  mets(...)
}
standardized_predict <- function(object, new_data, lvls) {
  new_pred <- predict(object, new_data, type = "prob")
  if (length(lvls) == 2 & ncol(new_pred) == 1) {
    new_pred$.pred_value2 <- 1 - new_pred$.pred_value
    names(new_pred) <- paste(".pred", lvls, sep = "_")
  }
  new_pred
}
expand_preds <- function (.data, threshold, inc = NULL) {
  threshold <- unique(threshold)
  nth <- length(threshold)
  n_data <- nrow(.data)
  if (!is.null(inc))
    .data <- dplyr::select(.data, all_of(inc))
  .data <- .data[rep(1:nrow(.data), times = nth), ]
  .data$.threshold <- rep(threshold, each = n_data)
  .data
}
recode_data <- function(obs, prob, threshold) {
  lvl <- levels(obs)
  pred <- ifelse(prob >= threshold, lvl[1], lvl[2])
  factor(pred, levels = lvl)
}
#' @export
threshold_perf <-
  function(.data,
           truth,
           estimate,
           thresholds = seq(.5, .9, .1),
           na_rm = TRUE,
           ...) {
    if (is.null(thresholds)) {
      thresholds <- seq(0.5, 1, length = 21)
    }
    nms <- names(.data)
    obs <- tidyselect::vars_select(nms, !!enquo(truth))
    probs <- tidyselect::vars_select(nms, !!enquo(estimate))
    rs_ch <- dplyr::group_vars(.data)
    rs_ch <- unname(rs_ch)
    obs_sym <- sym(obs)
    probs_sym <- sym(probs)
    if (length(rs_ch) == 0) {
      rs_ch <- NULL
      rs_id <- NULL
    } else {
      rs_id <- syms(rs_ch)
    }
    if (length(probs) > 1 | length(obs) > 1) {
      cat_stop("`truth` and `estimate` should only be single columns.")
    }
    if (!inherits(.data[[obs]], "factor")) {
      cat_stop("`truth` should be a factor")
    }
    if (length(levels(.data[[obs]])) != 2) {
      cat_stop("`truth` should be a 2 level factor")
    }
    if (!is.numeric(.data[[probs]])) {
      cat_stop("`estimate` should be numericr")
    }
    .data <-
      dplyr::rename(.data,
        truth = !!obs_sym,
        prob = !!probs_sym
      )
    if (!is.null(rs_id)) {
      .data <- dplyr::select(.data, truth, prob, !!!rs_id)
    } else {
      .data <- dplyr::select(.data, truth, prob)
    }
    if (na_rm) {
      .data <- na.omit(.data)
    }
    .data <-
      .data %>%
      expand_preds(
        threshold = thresholds,
        inc = c(
          "truth",
          "prob", rs_ch
        )
      ) %>%
      mutate(alt_pred = recode_data(
        truth,
        prob, .threshold
      ))
    if (!is.null(rs_id)) {
      .data <- .data %>% group_by(!!!rs_id, .threshold)
    } else {
      .data <- .data %>% group_by(.threshold)
    }
    .data_metrics <- .data %>% two_class(truth, estimate = alt_pred)
    sens_vec <-
      .data_metrics %>%
      dplyr::filter(.metric == "sens") %>%
      dplyr::pull(.estimate)
    dist <- .data_metrics %>%
      dplyr::filter(.metric == "spec") %>%
      dplyr::mutate(
        .metric = "distance",
        .estimate = (1 -
          sens_vec)^2 + (1 - .estimate)^2
      )
    .data_metrics <- dplyr::bind_rows(.data_metrics, dist)
    .data_metrics
  }
#' @export
probability_threshold <- function(object,data,times = 10,rep = 3) {
  create_fnthr <- function(object) {
    outchr <- outcome_chr(object)
    p_class <- attr(outcome_quo(object), "lvl")[1]
    p_class <- paste0(".pred_", p_class)
    thr_chr <- paste0(outchr, ", ", p_class)
    thr_fn <- function(x) {
      x
    }
    thr_body <-
      paste0(
        "probably::threshold_perf(.data = x, ",
        thr_chr,
        ", thresholds = seq(0.5, 1, by = 0.0025))"
      )
    body(thr_fn) <- str2lang(thr_body)

    thr_fn
  }

  if (!all(get_mode(object) == c("classification", "class"))) {
    cat_stop("Model mode no es 'classification, class'")

  }
  #

  if (inherits(object, c("ensemble_models", "bagging"))) {
    pred <- predict(object, new_data = data)
  } else {
    pred <-
      dplyr::bind_cols(
        predict(object, new_data = data),
        predict(object, new_data = data, type = "prob")
      )
  }

  pred <-
    dplyr::bind_cols(dplyr::select(data, !!outcome_expr(object)), pred)

  pred_folds <-
    eval_expr(
      kfolds(
        dataset = !!pred,
        times = !!times,
        outcome = !!outcome_expr(object),
        rep = !!rep,
        reverse = T
      )
    )

  pred_folds <- lapply(pred_folds$splits, rsample::analysis)

  get_threshold <- create_fnthr(object)

  threshold_data <-
    do.call(rbind, lapply(pred_folds, get_threshold)) %>%
    group_by(.threshold, .metric, .estimator) %>%
    summarise(.estimate = mean(.estimate), .groups = "drop") %>%
    ungroup()

  threshold_data

  threshold_data <- threshold_data %>%
    filter(.metric != "distance") %>%
    mutate(group = case_when(
      .metric == "sens" |
        .metric == "spec" ~ "1",
      TRUE ~ "2"
    ))

  threshold_data %>%
    filter(.metric == "j_index") %>%
    filter(.estimate == max(.estimate)) %>%
    pull(.threshold) %>% mean()
}

#' @keywords internal
create_specs <- function(x, rep, times) {
  .create_specs <- function(mode, n, rep) {
    models <- mgalda::base_superlearner
    models <- models[models$mode %in% mode, ]
    if (mode[1] == "classification") {
      models$tune_grid[models$engine == "rpart"][[1]]$min_n <-
        round(models$tune_grid[models$engine ==
                                 "rpart"][[1]]$min_n * n)
    }
    models <-
      slice_sample(
        .data = models,
        n = rep,
        replace = rep >
          nrow(models)
      )
    models$spec <- map(.x = 1:nrow(models), .f = ~ try({
      if (ncol(models[.x, ]$tune_grid[[1]]) == 0) {
        eval(parse(text = models[.x, ]$chr_call))
      } else {
        tune::finalize_model(
          x = eval(parse(text = models[.x, ]$chr_call)),
          parameters = slice_sample(models[.x, ]$tune_grid[[1]], n = 1)
        )
      }
    }))
    models <- dplyr::select(models,
                            parsnip = model, engine,
                            spec
    ) %>% filter(map_lgl(spec, ~ !inherits(
      .x, "try-error"
    )))
    if (nrow(models) < rep) {
      models <- slice_sample(
        .data = models,
        n = rep,
        replace = TRUE
      )
    }
    models
  }
  specs_base <-
    .create_specs(
      mode = get_mode(x),
      n = get_nobs(x),
      rep = rep * times
    )
  specs_base$spec <-
    map(specs_base$spec, ~ workflows::workflow() %>% workflows::add_model(.x))
  specs_base
}
#' @keywords internal
get_train <- function(x) {
  rsample::training(x$base_data$splits)
}
#' @keywords internal
models_frm <- function(x, envir = env_curr()) {
  df2vct <-
    function(.data,
             x,
             .names = NULL,
             aggregate = FALSE,
             fun = mean) {
      check_inherits(x, "character")

      if (aggregate) {
        x <- aggregate(
          .data[[x]],
          by = list(names = .data[[.names]]),
          FUN = fun,
          simplify = T,
          drop = T
        )

        x <- purrr::set_names(x = x$x, nm = x$names)
      } else {
        x <- .data[[x]]
      }
      x
    }
  data <- x$base_data$dataset
  outexpr <- outcome_expr(x)
  outchr <- outcome_chr(x)
  predictors <- setdiff(names(data), outchr)
  splits <-
    do_call(
      "kfolds",
      list(
        dataset = data,
        rep =  3,
        times =  3,
        outcome = outexpr,
        reverse = T
      )
    )$splits

  if (ncol(data) - 1 <= 5) {
    fnlits <- purrr::rerun(nrow(x$full_fits), identity(predictors))
  } else {
    vi <-
      purrr::map_dfr(
        .x = splits,
        .f = ~ do_call("select_vip", list(
          .data = rsample::analysis(.x), y = outexpr, quote(everything())
        ))$res
      )
    vi <- df2vct(
      .data = vi,
      x = "score",
      .names = "variable",
      aggregate = T,
      fun = mean
    )
    ncs <- ncase(vi)
    smin <- max(ncs, 4, ncol(data) - 2)
    smax <- max(smin, ncs * 1.5)
    smax <- min(ceiling(smax), 10, ncol(data) - 1)
    fnlits <- purrr::rerun(
      .n = nrow(x$full_fits),
      sample(smin:smax, 1)
    )
    vi <- log10(vi + 2) / sum(log10(vi + 2))
    fnlits <- lapply(fnlits, function(x) {
      sample(names(vi), size = x, prob = vi)
    })
  }

  fnlits <- lapply(fnlits, function(x) {
    str2lang(paste0(x, collapse = " + "))
  })
  envir <- new.env(parent = baseenv())
  assign("data", data, envir = envir)
  x$full_fits$formulas <- lapply(fnlits, function(x) {
    make_formula(
      lhs = outexpr,
      rhs = x,
      env = envir
    )
  })
  x
}
#' @keywords internal
repro_prepros <- function(x, x01, x02) {
  if (nrow(x02$full_fits) > 0) {
    x02$full_fits$wflow <-
      lapply(1:length(x02$full_fits$wflow), function(w) {
        z <- sample(seq_along(x$full_fits$wflow), 1)
        wfl <-
          try(workflows::extract_spec_parsnip(x$full_fits$wflow[[z]]))
        workflows::workflow() %>%
          try(workflows::add_model(wfl))
      })
    x02$full_fits <-
      x02$full_fits[map_lgl(x02$full_fits$wflow, ~ inherits(.x, "try-error")), ]


    x02 <- simple_rec(x02)
    x02$full_fits <-
      x02$full_fits[!map_lgl(x02$full_fits$wflow, ~ inherits(.x, "try-error")), ]
    if (nrow(x02$full_fits) > 0) {
      x$full_fits <- rbind(x02$full_fits, x$full_fits)
    }
  }

  if (nrow(x01$full_fits) > 0) {
    x01$full_fits$wflow <-
      lapply(1:length(x01$full_fits$wflow), function(w) {
        z <- sample(seq_along(x$full_fits$wflow), 1)

        wfl <-
          try(workflows::extract_spec_parsnip(x$full_fits$wflow[[z]]))
        workflows::workflow() %>%
          workflows::add_model(wfl)
      })

    x01$full_fits <-
      x01$full_fits[map_lgl(x01$full_fits$wflow, ~ inherits(.x, "try-error")), ]

    x01 <- simple_rec(x01)
    x01$full_fits <-
      x01$full_fits[!map_lgl(x01$full_fits$wflow, ~ inherits(.x, "try-error")), ]
    if (nrow(x01$full_fits) > 0) {
      x$full_fits <- rbind(x01$full_fits, x$full_fits)
    }
  }
  x
}
#' @keywords internal
reqlibs_impl <- function() {
  current_session <- sessionInfo()
  current_packs <- names(current_session$otherPkgs)
  p <- getOption("mgalda.supl_libs")
  p <- p[!p %in% current_packs]
  if (length(p) > 0) {
    invisible(require_library(p))
  }
}
#' @keywords internal
get_obj_metrics <- function(x) {
  switch(x,
         classification = yardstick::metric_set(
           yardstick::accuracy,
           yardstick::roc_auc,
           yardstick::sens,
           yardstick::spec,
           mgalda::w_accuracy
         ),
         regression = yardstick::metric_set(
           yardstick::rpiq,
           yardstick::smape,
           yardstick::rsq,
           yardstick::msd,
           mgalda::nrmse,
           mgalda::rsq_sliding
         )
  )
}
#' @keywords internal
standardized_predict <- function(object, new_data, lvls) {
  new_pred <- suppressall(predict(object, new_data,
                                  type = "prob"
  ))
  if (length(lvls) == 2 & ncol(new_pred) == 1) {
    new_pred$.pred_value2 <- 1 - new_pred$.pred_value
    names(new_pred) <- paste(".pred", lvls, sep = "_")
  }
  new_pred
}
#' @keywords internal
eval_models_tidymodels <- function(object, test_data, id = NULL) {
  reqlibs_impl()

  outcome <- outcome_quo(object)
  mode <- get_mode(object)
  met_obj <- get_obj_metrics(mode[1])
  lvls <- attr(outcome, "lvls")


  if (mode[1] == "classification") {
    preds <-
      bind_cols(
        truth = test_data[[lang2str(outcome)]],
        estimate = predict(object = object, test_data)$.pred_class,
        standardized_predict(
          object = object,
          new_data = test_data,
          lvls = lvls
        ) %>%
          rename_with(~ str_remove(
            string = .x, pattern = ".pred_"
          ))
      )

    if (mode[2] == "class") {
      met <-
        eval_expr(
          met_obj(
            !!preds,
            truth =  truth,
            estimate =  estimate,
            !!rlang::sym(names(preds)[3]),
            na_rm = F
          ),
          envir = env_curr()
        )
    } else {
      met <- paste0(names(preds)[c(3, ncol(preds))], collapse = ":")
      met <- str2lang(met)
      met <-
        eval_expr(met_obj(
          !!preds,
          truth =  truth,
          estimate =  estimate,
          !!met,
          na_rm = F
        ),
        envir = env_curr()
        )
    }
  } else if (mode[1] == "regression") {
    preds <-
      bind_cols(
        truth = test_data[[lang2str(outcome)]],
        estimate = predict(object = object, new_data = test_data)$.pred
      )
    met <-
      eval_expr(met_obj(
        !!preds,
        truth =  truth,
        estimate =  estimate,
        na_rm = F
      ),
      envir = env_curr()
      )
  }

  info_mets <- as_tbl(met_obj) %>%
    dplyr::select(metric, dir = direction)

  mat <- met %>%
    dplyr::select(metric = .metric, .estimate) %>%
    inner_join(info_mets, by = "metric") %>%
    dplyr::relocate(.estimate, .after = everything())

    if (!is_emptyna(id)) {
      names(mat)[3] <- id
    }


  attr(mat, "metrics") <- met_obj
  mat
}
#' @keywords internal
eval_models_superlearner <- function(object, test_data, id = NULL) {
  reqlibs_impl()

  outcome <- outcome_quo(object)
  mode <- get_mode(object)
  met_obj <- get_obj_metrics(mode[1])

  if (mode[1] == "classification") {
    preds <-
      eval_expr(predict(
        object = !!object,
        new_data = !!test_data
      ),
      envir = env_curr()
      )
    preds <-
      bind_cols(
        tibble(
          truth = test_data[[outcome_chr(object)]],
          estimate = preds$.pred_class
        ),
        dplyr::select(preds, !.pred_class)
      )

    if (mode[2] == "class") {
      met <-
        eval_expr(
          met_obj(
            !!preds,
            truth =  truth,
            estimate =  estimate,
            !!rlang::sym(names(preds)[3]),
            na_rm = T
          ),
          envir = env_curr()
        )
    } else if (mode[2] == "multiclass") {
      met <- ncol(preds)
      met <- paste0(names(preds)[c(3, met)], collapse = ":")
      met <- str2lang(met)
      met <-
        eval_expr(met_obj(
          !!preds,
          truth =  truth,
          estimate =  estimate,
          !!met,
          na_rm = F
        ),
        envir = env_curr()
        )
    }
  } else if (mode[1] == "regression") {
    preds <-
      bind_cols(
        truth = test_data[[lang2str(outcome)]],
        estimate = predict(object = object, new_data = test_data)$.pred
      )
    met <-
      eval_expr(met_obj(
        !!preds,
        truth =  truth,
        estimate =  estimate,
        na_rm = F
      ),
      envir = env_curr()
      )
  }
  info_mets <- as_tbl(met_obj)

  info_mets <-
    dplyr::select(info_mets, metric, dir = direction)

  mat <-
    suppressWarnings(eval(met)) %>%
    dplyr::select(metric = .metric, .estimate) %>%
    inner_join(info_mets, by = "metric") %>%
    dplyr::relocate(.estimate, .after = everything())

    if (!is_emptyna(id)) {
      names(mat)[3] <- id
    }


  attr(mat, "metrics") <- met_obj
  mat
}
#' @keywords internal
predict_misleading <- function(object, new_data) {
  predictbind_impl <- function(object, new_data) {
    predict(object = object, new_data = new_data)
  }
  r <- tryCatch(
    withCallingHandlers(
      {
        error_text <- "no_error"
        list(
          value = predictbind_impl(object, new_data),
          error_text = error_text
        )
      },
      warning = function(e) {
        error_text <<- trimws(paste0("WARNING: ", e))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      return(list(value = NA, error_text = trimws(paste0(
        "ERROR: ",
        e
      ))))
    },
    finally = {

    }
  )
  if (r$error_text != "no_error") {
    if (stringr::str_detect(string = r$error_text, pattern = "rank-deficient fit may be misleading")) {
      r$value <- NA
    }
  }
  r$value
}
#' @keywords internal
nested_preds_reg <- function(x) {
  full_fits <- x$full_fits
  outcome <- outcome_expr(x)

  full_fits %>%
    dplyr::select(wflow_id, preds) %>%
    unnest(preds) %>%
    pivot_wider(
      id_cols = c(obs, !!outcome),
      names_from = wflow_id,
      values_from = contains(".pred")
    ) %>%
    rename_with(~ stringr::str_remove_all(
      string = .,
      pattern = ".pred_"
    )) %>%
    dplyr::select(!obs)
}
#' @keywords internal
nested_preds_class <- function(x) {
  outcome <- outcome_expr(x)
  full_fits <- x$full_fits
  lvls <- attr(x$base_data$outcome, "lvls")

  new_preds <- full_fits %>%
    dplyr::select(wflow_id, preds) %>%
    unnest(preds)
  new_preds %>%
    pivot_wider(
      id_cols = c(obs, !!outcome),
      names_from = wflow_id,
      values_from = tidyselect::contains(".pred"),
      values_fill = 0
    ) %>%
    rename_with(~ stringr::str_remove_all(string = ., pattern = ".pred_")) %>%
    dplyr::select(!obs) %>%
    dplyr::select(!!outcome, !where(~ sum(is_nonnum(.x)) > length(.x) * 0.5)) %>%
    mutate_if(is.numeric, ~ tidyr::replace_na(
      .x,
      0
    ))
}
#' @keywords internal
clean_predictors <- function(data) {
  form <- formula(data)
  recipe(form, data = data) %>%
    step_corr(all_numeric_predictors()) %>%
    step_nzv(all_numeric_predictors()) %>%
    step_lincomb(all_numeric_predictors()) %>%
    prep() %>%
    recipes::juice()
}
#' @keywords internal
nested_preds <- function(object) {
  if (object$model_mode[1] == "classification") {
    nested_data <- nested_preds_class(object)
  } else if (object$model_mode[1] == "regression") {
    nested_data <- nested_preds_reg(object)
  }
  nested_data <- clean_predictors(nested_data)
  vip_selc <-
    do_call(
      "select_vip",
      list(
        .data = nested_data,
        y = outcome_expr(object),
        quote(everything()),
        threshold = NULL,
        top_p = floor(ncol(nested_data) -
                        1 / 2)
      )
    )
  predict(vip_selc, nested_data)
}
#' @keywords internal
meta_wflow <- function(x, train_data) {
  regmeta_wfolw <- function(formula_outcome, train_data) {
    meta_recipe <- recipe(formula = formula_outcome, data = train_data)
    xspec <- parsnip::linear_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    ) %>%
      parsnip::set_mode("regression") %>%
      parsnip::set_engine(
        engine = "glmnet",
        lower.limits = 0,
        lambda.min.ratio = 0,
        family = "gaussian"
      )
    workflows::workflow() %>%
      workflows::add_model(spec = xspec) %>%
      workflows::add_recipe(recipe = meta_recipe)
  }
  classmeta_wfolw <- function(formula_outcome, train_data,
                              lvls) {
    meta_recipe <- recipe(formula = formula_outcome, data = train_data)
    if (length(lvls) > 2) {
      xspec <- parsnip::multinom_reg(
        penalty = tune::tune(),
        mixture = tune::tune()
      ) %>%
        parsnip::set_mode("classification") %>%
        parsnip::set_engine(engine = "glmnet")
    } else {
      xspec <- parsnip::logistic_reg(
        penalty = tune::tune(),
        mixture = tune::tune()
      ) %>%
        parsnip::set_mode("classification") %>%
        parsnip::set_engine(
          engine = "glmnet",
          lower.limits = 0,
          lambda.min.ratio = 0,
          family = "binomial"
        )
    }
    workflows::workflow() %>%
      workflows::add_model(spec = xspec) %>%
      workflows::add_recipe(recipe = meta_recipe)
  }
  if (x$model_mode[1] == "classification") {
    wflow <- classmeta_wfolw(
      formula_outcome = x$base_data$formula,
      train_data = train_data,
      lvls = attr(
        x$base_data$outcome,
        "lvls"
      )
    )
  } else if (x$model_mode[1] == "regression") {
    wflow <- regmeta_wfolw(
      formula_outcome = x$base_data$formula,
      train_data = train_data
    )
  }
  wflow
}
#' @keywords internal
get_expressions <- function(x, model_mode, ...) {
  if (model_mode[1] == "classification") {
    if (model_mode[2] == "class") {
      expr_fn <- function(x, model_mode, ...) {
        list(
          class = prediction_eqn(x, model_mode, type = "class"),
          prob = prediction_eqn(x, model_mode, type = "prob")
        )
      }
    } else if (model_mode[2] == "multiclass") {
      expr_fn <- function(x, model_mode, ...) {
        list(
          class = prediction_eqn(x, model_mode, type = "class"),
          prob = prediction_eqn(x, model_mode, type = "prob")
        )
      }
    }
  } else if (model_mode[1] == "regression") {
    expr_fn <- function(x, model_mode, ...) {
      list(numeric = prediction_eqn(x, model_mode, type = "numeric"))
    }
  }
  expr_fn(x, model_mode, ...)
}
#' @keywords internal
prediction_eqn <- function(x, model_mode, ...) {
  eqn_constuctor <- function(x, model, type, lvls) {
    type <- match.arg(type, c("class", "prob", "numeric"))
    new_class <- paste(model, type, sep = "_")
    structure(x, class = new_class, levels = lvls)
  }
  eexp <- function(x) {
    rlang::expr(exp(!!x))
  }
  if (model_mode[1] == "classification") {
    if (model_mode[2] == "class") {
      pred_eqn <- function(x, type = "class", ...) {
        type <- match.arg(type, c("class", "prob"))
        model_class <- class(x$fit)[1]
        lp <- build_linear_predictor(x, model_mode)
        lvls <- x$lvl
        if (type == "prob") {
          elem_names <- paste0(".pred_", lvls)
          res <- vector(mode = "list", length = 2)
          names(res) <- elem_names
          res[[elem_names[2]]] <-
            rlang::expr(stats::binomial()$linkinv(!!lp))
          res[[elem_names[1]]] <-
            rlang::expr(stats::binomial()$linkinv(-(!!lp)))
        } else {
          res <- list(.pred_class = rlang::expr(factor(
            ifelse((!!lp) >
                     0, !!lvls[2], !!lvls[1]),
            levels = !!lvls
          )))
        }
        eqn_constuctor(res, model_class, type, lvls)
      }
    } else if (model_mode[2] == "multiclass") {
      pred_eqn <- function(x, type = "class", ...) {
        type <- match.arg(type, c("class", "prob"))
        model_class <- class(x$fit)[1]
        lvls <- x$lvl
        res <- build_linear_predictor(x, model_mode) %>%
          mutate(.pred = map(lp, eexp))
        names(res$.pred) <- paste0(".pred_", res$class)
        eqn_constuctor(res, model_class, type, lvls)
      }
    }
  } else if (model_mode[1] == "regression") {
    pred_eqn <- function(x, type = "numeric", ...) {
      type <- match.arg(type, "numeric")
      model_class <- class(x$fit)[1]
      res <-
        list(.pred = build_linear_predictor(x, model_mode))
      eqn_constuctor(res, model_class, type, NULL)
    }
  }
  pred_eqn(x, ...)
}
#' @keywords internal
build_linear_predictor <- function(x, model_mode, ...) {
  build_linear_predictor_eng <- function(x, ...) {
    slopes <- x %>% filter(.data$terms != "(Intercept)")
    lin_pred <-
      map2(slopes$terms, slopes$estimate, ~ rlang::expr((!!sym(.x) *
                                                           !!.y)))
    if (any(x$terms == "(Intercept)")) {
      beta_0 <- x$estimate[x$terms == "(Intercept)"]
      lin_pred <- c(beta_0, lin_pred)
    }
    lin_pred <- purrr::reduce(lin_pred, function(l, r) {
      rlang::expr(!!l + !!r)
    })
    lin_pred
  }
  build_linear_predictor_glmnet <- function(x, ...) {
    coefs <- get_glmn_coefs(x$fit, x$spec$args$penalty) %>%
      filter(.data$estimate != 0)
    lp <- build_linear_predictor_eng(coefs)
    lp
  }
  if (model_mode[1] == "classification") {
    if (model_mode[2] == "class") {
      blin <- function(x, ...) {
        build_linear_predictor_glmnet(x, ...)
      }
    } else if (model_mode[2] == "multiclass") {
      blin <- function(x, ...) {
        coefs <- get_glmn_coefs(x$fit, x$spec$args$penalty) %>%
          filter(.data$estimate != 0) %>%
          dplyr::group_nest(class,
                            .key = "coefs"
          ) %>%
          mutate(lp = map(
            coefs,
            build_linear_predictor_eng
          ))
        coefs
      }
    }
  } else if (model_mode[1] == "regression") {
    blin <- function(x, ...) {
      build_linear_predictor_glmnet(x, ...)
    }
  }
  blin(x, ...)
}
#' @keywords internal
get_glmn_coefs <- function(x, penalty = 0.01) {
  x <- glmnet::coef.glmnet(x, s = penalty)
  x <- as.matrix(x)
  colnames(x) <- "estimate"
  rn <- rownames(x)
  x <-
    as_tbl(x) %>% mutate(terms = rn, penalty = penalty)
  x <- dplyr::select(x, terms, estimate, penalty)
  if (is.list(x$estimate)) {
    x$estimate <-
      map(
        x$estimate,
        ~ as_tbl(as.matrix(.x), rownames = "terms")
      )
    x <-
      unnest(x,
             cols = c(estimate),
             names_repair = "minimal"
      )
    names(x) <- c("class", "terms", "estimate", "penalty")
  }
  x
}
#' @keywords internal
glmnet_metrics <- function(x) {
  num_members <- function(x, penalties) {
    glmn_coef <- coef(x, s = penalties)
    if (is.list(glmn_coef)) {
      glmn_coef <- do.call("rbind", glmn_coef)
    }
    glmn_coef <-
      glmn_coef[rownames(glmn_coef) != "(Intercept)", , drop = FALSE]
    mems <- apply(glmn_coef, 2, function(x) {
      sum(x != 0)
    })
    tibble(penalty = penalties, members = unname(mems))
  }


  res <- tune::collect_metrics(x)
  pens <- sort(unique(res$penalty))
  num_mem <-
    dplyr::select(x, id, .extracts) %>%
    unnest(.extracts) %>%
    dplyr::group_nest(id, penalty, mixture) %>%
    # There are redundant model objects over penalty values
    mutate(data = map(data, ~ .x$.extracts[[1]])) %>%
    mutate(members = map(data, ~ num_members(.x, pens))) %>%
    dplyr::select(mixture, members) %>%
    unnest(cols = members) %>%
    group_by(penalty, mixture) %>%
    dplyr::summarize(
      .metric = "num_members",
      .estimator = "Poisson",
      mean = mean(members, na.rm = TRUE),
      n = sum(!is.na(members)),
      std_err = sqrt(mean / n),
      .groups = "drop"
    ) %>%
    full_join(
      res %>% dplyr::select(penalty, mixture, .config) %>% distinct(),
      by = c("penalty", "mixture")
    )

  bind_rows(res, num_mem) %>%
    arrange(.config)
}
#' @keywords internal
get_nobs <- function(x) {
  nrow(x$base_data$dataset)
}
#' @keywords internal
get_train <- function(x) {
  if (inherits(
    x,
    c(
      "ensemble_models",
      "tunensemble_models",
      "new_ensemble_models"
    )
  )) {
    rsample::training(x$base_data$splits)
  } else {
    NULL
  }
}
#' @keywords internal
get_test <- function(x) {
  if (inherits(
    x,
    c(
      "ensemble_models",
      "tunensemble_models",
      "new_ensemble_models"
    )
  )) {
    rsample::testing(x$base_data$splits)
  } else {
    NULL
  }
}

# helpers bag -------------------------------------------------------------
#' @keywords internal
bagger_extractor <- function(x) {
  model_failure <- function(x) {
    if (inherits(x, "model_fit")) {
      res <- inherits(x$fit, "try-error")
    } else {
      res <- inherits(x, "try-error")
    }
    res
  }
  x <-
    mutate(x, passed = !map_lgl(model, model_failure))

  if (sum(x$passed) == 0) {
    if (inherits(x$model[[1]], "try-error")) {
      msg <- as_chr(x$model[[1]])
    } else {
      if (inherits(x$model[[1]], "model_fit")) {
        msg <- as_chr(x$model[[1]]$fit)
      } else {
        msg <- NA
      }
    }

    if (!is.na(msg)) {
      msg <- paste0("An example message was:\n  ", msg)
    } else {
      msg <- ""
    }


    cat_stop(paste0("All of the models failed. ", msg))
  }
  if (any(names(x) == "passed")) {
    x <- x %>% filter(.data$passed)
  }

  x
}
#' @keywords internal
cart_imp <- function(x) {
  if (!any(names(x$fit) == "variable.importance")) {
    x <-
      tibble(
        predictor = rlang::na_chr,
        importance = rlang::na_dbl
      )
  } else {
    x <-
      tibble(
        predictor = names(x$fit$variable.importance),
        importance = unname(x$fit$variable.importance)
      )
    x <- x[x$importance > 0, ]
  }
  x %>%
    mutate(importance = importance / max(importance, na.rm = TRUE) * 100)
}
#' @keywords internal
mars_imp <- function(x) {
  imps <- earth::evimp(x$fit)
  imps <- imps[, "gcv", drop = FALSE]
  x <-
    tibble(predictor = rownames(imps), importance = unname(imps[, "gcv"]))
  x <- x[x$importance > 0, ]
  x
}
#' @keywords internal
replace_parsnip_terms <- function(x) {
  replaced <- function(x, replacement) {
    x$preproc$terms <- replacement
    x
  }

  new_terms <- butcher::axe_env(x$model[[1]]$preproc$terms)
  x <- x %>%
    mutate(model = map(model, replaced, replacement = new_terms))
  x
}
#' @keywords internal
join_args <- function(default, others) {
  res <- default
  for (i in seq_along(others)) {
    res[[names(others)[i]]] <- others[[i]]
  }
  res
}
#' @keywords internal
make_cart_spec <- function(classif, opt = list()) {
  model_defaults <-
    list(
      cost_complexity = 0,
      xval = 0,
      min_n = 20,
      tree_depth = 30,
      model = FALSE
    )
  opts <- join_args(model_defaults, opt)

  if (classif) {
    cart_md <- "classification"
  } else {
    cart_md <- "regression"
  }

  cart_spec <-
    parsnip::decision_tree(
      mode = cart_md,
      cost_complexity = !!opts$cost_complexity,
      min_n = !!opts$min_n,
      tree_depth = !!opts$tree_depth
    )

  opts <-
    opts[!(names(opts) %in% c("cost_complexity", "tree_depth", "min_n"))]

  if (length(opts) > 0) {
    main_args <- list()

    if (any(names(opts) == "method")) {
      main_args$method <- opts$method
      opts$method <- NULL
    }
    if (any(names(opts) == "parms")) {
      main_args$parms <- opts$parms
      opts$parms <- NULL
    }
    if (any(names(opts) == "cost")) {
      main_args$cost <- opts$cost
      opts$cost <- NULL
    }
    if (length(opts) == 0) {
      opts <- NULL
    }
    if (length(main_args) == 0) {
      main_args <- NULL
    }

    cart_spec <-
      parsnip::set_engine(cart_spec, engine = "rpart", !!!main_args, !!!opts)
  } else {
    cart_spec <- parsnip::set_engine(cart_spec, engine = "rpart")
  }
  cart_spec
}
#' @keywords internal
make_mars_spec <- function(classif, opt = list()) {
  opts <- join_args(
    list(
      prune_method = "none",
      num_terms = NULL,
      prod_degree = 1
    ),
    opt
  )
  if (classif) {
    mars_md <- "classification"
  } else {
    mars_md <- "regression"
  }
  mars_spec <-
    parsnip::mars(
      mode = mars_md,
      num_terms = !!opts$num_terms,
      prod_degree = !!opts$prod_degree,
      prune_method = !!opts$prune_method
    )
  opts <-
    opts[!(names(opts) %in% c("prune_method", "num_terms", "prod_degree"))]


  opts$keepxy <- TRUE

  if (length(opts) > 0) {
    mars_spec <-
      parsnip::set_engine(mars_spec, engine = "earth", !!!opts)
  } else {
    mars_spec <- parsnip::set_engine(mars_spec, engine = "earth")
  }
  mars_spec
}
#' @keywords internal
cart_fit <- function(split, spec, control) {
  dat <- rsample::analysis(split)

  if (control$sampling == "down") {
    dat <- down_sampler(dat)
  }
  dat <-
    dat[, c(sample(nmdiff(dat, ".outcome"), round((ncol(
      dat
    ) - 1) * .8)), ".outcome")]
  ctrl <- parsnip::fit_control(catch = TRUE)
  mod <-
    parsnip::fit.model_spec(spec, .outcome ~ ., data = dat, control = ctrl)
  mod
}
#' @keywords internal
mars_fit <- function(split, spec, control) {
  ctrl <- parsnip::fit_control(catch = TRUE)

  dat <- rsample::analysis(split)
  # only na.fail is supported by earth::earth
  dat <- dat[complete.cases(dat), , drop = FALSE]

  if (control$sampling == "down") {
    dat <- down_sampler(dat)
  }
  dat <-
    dat[, c(sample(nmdiff(dat, ".outcome"), round((ncol(
      dat
    ) - 1) * .8)), ".outcome")]
  mod <-
    parsnip::fit.model_spec(spec, .outcome ~ ., data = dat, control = ctrl)
  mod
}
#' @keywords internal
compute_imp <- function(rs, .fn, compute) {
  if (compute) {
    num_mod <- nrow(rs)
    imps <-
      purrr::map_df(rs$model, .fn) %>%
      filter(!is.na(predictor) & !is.na(importance))
    if (nrow(imps) > 0) {
      imps <-
        imps %>%
        group_by(predictor) %>%
        dplyr::summarize(
          value = sum(importance, na.rm = TRUE) / num_mod,
          sds = sd(importance, na.rm = TRUE),
          sds = ifelse(length(predictor) == 1, 0, sds),
          std.error = sds / sqrt(num_mod),
          used = length(predictor)
        ) %>%
        dplyr::select(-sds) %>%
        arrange(desc(value)) %>%
        rename(term = predictor)
    } else {
      imps <- tibble(
        term = character(0),
        value = numeric(0),
        std.error = numeric(0),
        used = integer(0)
      )
    }
  } else {
    imps <- NULL
  }

  imps
}
#' @keywords internal
bagger_mars <- function(rs, control, processed, mod_spec) {
  rs <- rs %>%
    mutate(
      model = map2(fit_seed,
                   splits,
                   function(seed, split, .fn, ...) {
                     withr::with_seed(seed, .fn(split, ...))
                   },
                   .fn = mars_fit,
                   spec = mod_spec,
                   control = control
      )
    )

  rs <- bagger_extractor(rs)

  imps <- compute_imp(rs, mars_imp, control$var_imp)

  rs <- rs %>%
    replace_parsnip_terms() %>%
    mutate(
      model = map(model, function(x) {
        x$fit <- butcher::axe_data(x$fit)
        x$fit <- butcher::axe_call(x$fit)
        x$fit <- butcher::axe_fitted(x$fit)
        x
      }),
      mod_name = "mars",
      id = paste0(mod_name, stringr::str_pad(row_number(), 2, pad = "0"))
    ) %>%
    dplyr::select(-splits, -id, -fit_seed, -passed)

  list(model_df = rs, imp = imps)
}
#' @keywords internal
bagger_cart <- function(rs, control, processed, mod_spec) {
  rs <-
    rs %>%
    mutate(
      model = map2(fit_seed,
                   splits,
                   function(seed, split, .fn, ...) {
                     withr::with_seed(seed, .fn(split, ...))
                   },
                   .fn = cart_fit,
                   spec = mod_spec,
                   control = control
      )
    )

  rs <- bagger_extractor(rs)
  imps <- compute_imp(rs, cart_imp, control$var_imp)

  rs <-
    rs %>%
    replace_parsnip_terms() %>%
    mutate(
      model = map(model, function(x) {
        x$fit <- butcher::axe_data(x$fit)
        x$fit <- butcher::axe_ctrl(x$fit)
        x$fit <- butcher::axe_call(x$fit)
        x$fit <- butcher::axe_env(x$fit)
        x
      }),
      mod_name = "cart",
      id = paste0(mod_name, stringr::str_pad(row_number(), 2, pad = "0"))
    ) %>%
    dplyr::select(-splits, -id, -fit_seed, -passed)

  list(model_df = rs, imp = imps)
}
#' @keywords internal
down_sampler <- function(x) {
  if (!is.factor(x$.outcome)) {
    rlang::warn("Down-sampling is only used in classification models.",
                call. = FALSE
    )
    return(x)
  }

  min_n <- min(table(x$.outcome))
  x %>%
    group_by(.outcome) %>%
    sample_n(size = min_n, replace = TRUE) %>%
    ungroup()
}
