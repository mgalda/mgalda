#' random recipe para regresiones
#'
#' @rdname build_preprocess
#' @export
build_preprocess <-
  function(x, ...) {
    UseMethod("build_preprocess")
  }

#' @export
build_preprocess.default <-
  function(x, ...) {
    generic_default()
  }

#' @export
build_preprocess.data.frame <-
  function(x,
           formula,
           small = FALSE,
           add_step = NULL) {
    outcome <- rlang::f_lhs(formula)
    recs <- recipes::recipe(formula, x)

    recs <- recs %>% step_discrete(all_numeric_predictors())

    preds <- suppressall(get_colstyperec(recs))

    x <- recs %>%
      prep() %>%
      juice()

    if (!is_empty(add_step) & is_function(add_step)) {
      recs <- recs %>% add_step()
    }

    fct_preds <- names(preds)[preds == "nominal"]

    if (length(fct_preds) > 0) {
      recs <- clean_categorical(recs)
    }
    num_preds <- names(preds)[preds == "numeric"]


    df_steps <- select_step(x = recs, small = small)

    df_steps <- split(df_steps, df_steps$class)

    if (length(num_preds) > 0) {
      recs <-
        recs %>%
        step_limits_bounds(all_numeric_predictors())
      df_steps$numeric <- map2(
        df_steps$numeric$fn,
        df_steps$numeric$var,
        ~ get(.x)(rec = recs, var = .y)
      )
    }


    if (length(fct_preds) > 0) {
      df_steps$factor <- map2(
        df_steps$factor$fn,
        df_steps$factor$var,
        ~ get(.x)(rec = recs, var = .y)
      )
    }

    df_steps <-
      map_chr(
        .x = flatten(df_steps),
        .f = ~ deparse1(.x, collapse = "\n")
      )

    df_steps <-
      eval(str2lang(paste0(c("recs", df_steps), collapse = " %>%\n")))

    df_steps <- df_steps %>% step_nzv(all_numeric_predictors(),skip = T)

    if (!inherits(df_steps, "recipe")) {
      df_steps <- NULL
    }

    df_steps
  }

#' @export
build_preprocess.new_ensemble_models <-
  function(x,
           small = FALSE,
           add_step = NULL,
           ...) {
    datas <- map(x$full_fits$splits, rsample::analysis)
    formulas <- x$full_fits$formulas
    datas <-
      purrr::map2(formulas, datas, ~ model.frame(formula = .x, data = .y))

    recs <- list()

    for (i in seq_along(datas)) {
      recs[[i]] <-
        build_preprocess(
          x = datas[[i]],
          formula = as.formula(datas[[i]]),
          small = small,
          add_step = add_step
        )
    }

    recs <-
      map2(
        .x = x$full_fits$wflow,
        .y = recs,
        .f = ~ try(workflows::add_recipe(x = .x, recipe = .y))
      )

    recs <-
      map(.x = recs, .f = ~ finalize_blueprint(.x))

    recs <-
      map2(
        .x = recs,
        .y = datas,
        .f = ~ try(workflows::.fit_pre(
          workflow = .x,
          data = .y
        ))
      )

    x$full_fits$wflow <- recs
    x
  }



#' simple_rec
#'
#' @rdname simple_rec
#' @export
simple_rec <-
  function(x, ...) {
    UseMethod("simple_rec")
  }

#' @export
simple_rec.default <-
  function(x, ...) {
    generic_default()
  }

#' @export
simple_rec.data.frame <-
  function(x,
           formula,
           add_step = NULL) {
    outcome <- rlang::f_lhs(formula)
    recs <- recipes::recipe(formula, x)

    recs <- recs %>% step_discrete(all_numeric_predictors())

    preds <- suppressall(get_colstyperec(recs))

    x <- recs %>%
      prep() %>%
      juice()

    if (!is_empty(add_step) & is_function(add_step)) {
      recs <- recs %>% add_step()
    }

    fct_preds <- names(preds)[preds == "nominal"]

    if (length(fct_preds) > 0) {
      recs <- clean_categorical(recs)
    }
    num_preds <- names(preds)[preds == "numeric"]


    if (length(num_preds) > 0) {
      recs <-
        recs %>%
        step_limits_bounds(all_numeric_predictors()) %>%
        step_normalize(all_numeric_predictors()) %>%
        step_corr(all_numeric_predictors(),skip = T) %>%
        step_nzv(all_numeric_predictors(),skip = T)

    }


    if (length(fct_preds) > 0) {
      recs <- recs %>% step_dummy(all_nominal_predictors())
    }


    if (!inherits(recs, "recipe")) {
      recs <- NULL
    }

    recs
  }

#' @export
simple_rec.new_ensemble_models <-
  function(x,
           add_step = NULL,
           ...) {
    datas <- map(x$full_fits$splits, rsample::analysis)
    formulas <- x$full_fits$formulas
    datas <-
      map2(formulas, datas, ~ model.frame(formula = .x, data = .y))

    recs <- list()

    for (i in seq_along(datas)) {
      recs[[i]] <-
        simple_rec(
          x = datas[[i]],
          formula = as.formula(datas[[i]]),
          add_step = add_step
        )
    }

    recs <-
      map2(
        .x = x$full_fits$wflow,
        .y = recs,
        .f = ~ try(workflows::add_recipe(x = .x, recipe = .y))
      )

    recs <-
      map(.x = recs, .f = ~ finalize_blueprint(.x))

    recs <-
      map2(
        .x = recs,
        .y = datas,
        .f = ~ try(workflows::.fit_pre(
          workflow = .x,
          data = .y
        ))
      )

    x$full_fits$wflow <- recs
    x
  }

# recipes_list

#' recipes_list
#'
#' @rdname recipes_list
#' @export
recipes_list <- function(x, ...) {
  UseMethod("recipes_list")
}

#' @export
recipes_list.default <- function(x, ...) {
  generic_default()
}

#' @export
recipes_list.data.frame <- function(x, formula, times, rep) {
  outcome <- rlang::f_lhs(f = formula)

  dat <-
    eval_expr(random_rset(
      dataset = !!x,
      times = !!times,
      rep = !!rep,
      outcome = !!deparse(outcome)
    ))

  dat$splits <- lapply(dat$splits, rsample::analysis)

  dat$splits <-
    lapply(seq_along(dat$splits), function(i) {
      nm <- nmdiff(x, deparse(outcome))
      new_nm <-
        c(deparse(outcome), sample(nm, ceiling(runif(1, .25, .75) * length(nm))))
      dat$splits[[i]][, new_nm]
    })

  dat$formulas <-
    lapply(dat$splits, function(w) {
      eval_expr(make_formula(lhs = !!outcome,rhs = !!w))
    })

  dat$recs <-
    map2(
      .x = dat$formulas,
      .y = dat$splits,
      .f = ~ eval_expr(recipe(!!.x, data = !!.y))
    )

  wflow_id <-
    paste0("r", stringr::str_pad(seq_along(dat$recs), 3, pad = "0"))

  names(dat$recs) <- wflow_id
  names(dat$splits) <- wflow_id
  names(dat$formulas) <- wflow_id
  dat$id <- wflow_id
  names(dat$id) <- wflow_id
  dat$id2 <- NULL

  dat <- as_l(dat)
  class(dat) <- "recipes_list"
  attr(dat, "outcome") <- rlang::quo(!!outcome)

  dat
}

