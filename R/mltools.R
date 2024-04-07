#' mltools
#'
#'
#' @name mltools
#' @rdname mltools
#' @keywords internal
#' @examples
#'
#'
#' balance_data(
#'   .data = dplyr::select(mgalda::datalearn$credit, status, where(is_dblint)),
#'   class_var = "status"
#' )
#'
#' boots_cooks_dist(
#'   .data = dplyr::select_if(mgalda::datalearn$housing_market, is_dblint),
#'   response = sales_price,
#'   times = 3,
#'   rep = 4,
#'   everything()
#' )
#'
#' cooks_dist(
#'   .data = dplyr::select_if(mgalda::datalearn$housing_market, is_dblint),
#'   response = sales_price,
#'   everything()
#' )
#'
#' pop_linear(
#'   .data = dplyr::select_if(mgalda::datalearn$housing_market, is_dblint),
#'   response = sales_price
#' )
#'
#' pop_nonlinear(
#'   .data = dplyr::select_if(mgalda::datalearn$housing_market, is_dblint),
#'   x = sales_price_coefficient_variance,
#'   y = sales_price,
#'   times = 4,
#'   new_obs = 200
#' )
#'
#' pop_rollmean(
#'   .data = dplyr::select_if(mgalda::datalearn$housing_market, is_dblint),
#'   arrange_vars = sales20092010,
#'   var = sales_price,
#'   times = 4,
#'   new_obs = 100
#' )
#'
#' smooth_linear(
#'   sales_price ~ .,
#'   dplyr::select_if(mgalda::datalearn$housing_market, is_dblint),
#'   times = 4,
#'   rep = 5
#' )
#'
#' smooth_nonlinear(
#'   .data = dplyr::select_if(mgalda::datalearn$housing_market, is_dblint),
#'   x = sales_price_coefficient_variance,
#'   y = sales_price,
#'   times = 4,
#'   rep = 3
#' )
#'
#' smooth_outliers(
#'   .data = dplyr::select_if(mgalda::datalearn$housing_market, is_dblint),
#'   response = sales_price,
#'   times = 5,
#'   rep = 3,
#'   everything()
#' )
#'
#' smote(
#'   .data = dplyr::select(mgalda::datalearn$credit, status, where(is_dblint)),
#'   class_var = status
#' )
#'
#' xlm(sales_price ~ .,
#'   mgalda::datalearn$housing_market,
#'   times = 4,
#'   rep = 5
#' )
#'
#' xloess(
#'   sales_price ~ .,
#'   dplyr::select_if(mgalda::datalearn$housing_market, is_dblint),
#'   times = 4,
#'   rep = 5
#' )
NULL

# xlm  ----------------------------------

#' Fit a xlm
#'
#' @name xlm
#' @rdname xlm
#'
#' @export
#'

xlm <- function(x, ...) {
  UseMethod("xlm")
}

#' @export
#' @rdname xlm
xlm.default <- function(x, ...) {
  cat_stop('`xlm` is not defined for a "{class(x)[1]}".')
}

# XY method - data frame

#' @export
#' @rdname xlm
xlm.data.frame <- function(x, y, times = 5, rep = 3) {
  processed <- hardhat::mold(x, y)
  xlm_bridge(processed, times = times, rep = rep)
}

# XY method - matrix

#' @export
#' @rdname xlm
xlm.matrix <- function(x, y, times = 5, rep = 3) {
  processed <- hardhat::mold(x, y)
  xlm_bridge(processed, times = times, rep = rep)
}

# Formula method
frm_blueprint <-
  hardhat::default_formula_blueprint(
    indicators = "none",
    allow_novel_levels = TRUE
  )

#' @export
#' @rdname xlm
xlm.formula <- function(formula,
                        data,
                        times = 5,
                        rep = 3) {
  processed <- hardhat::mold(formula, data, blueprint = frm_blueprint)
  xlm_bridge(processed, times = times, rep = rep)
}

# Recipe method
rec_blueprint <-
  hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)

#' @export
#' @rdname xlm
xlm.recipe <- function(x, data, times = 5, rep = 3) {
  processed <- hardhat::mold(x, data, blueprint = rec_blueprint)
  xlm_bridge(processed, times = times, rep = rep)
}

# Bridge

xlm_bridge <- function(processed, times, rep) {

  fit <- xlm_impl(processed, times = times, rep = rep)

  new_xlm(fit = fit, blueprint = processed$blueprint)
}


# Implementation

xlm_impl <- function(processed, times, rep) {

  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  ndata <- cbind(.outcome = outcome, predictors)

  prepro <-
    xprepro(
      .data = ndata,
      everything(),
      outcome = quote(.outcome)
    )

  models <- list()

  for (i in seq_len(rep)) {
    models <- c(
      models,
      fit_with_boots(
        .data = predict(prepro,ndata),
        lhs = .outcome,
        rhs = .,
        .fn = lm,
        times = times,
        .seeds = sample(.Random.seed, size = times)
      )
    )
  }

  models <- map(models, function(x) {
    x$call$data <- quote(data)
    x$call[[1]] <- quote(lm)
    x
  })

  list(prepro = prepro, models = models)
}


new_xlm <- function(fit, blueprint) {
  hardhat::new_model(
    fit = fit,
    blueprint = blueprint,
    class = "xlm"
  )
}


#' Predict from a `xlm`
#'
#' @rdname mltools
#' @param object A `xlm` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#' @export
predict.xlm <- function(object, new_data) {
  forged <- hardhat::forge(new_data, object$blueprint)
  predict_xlm_bridge(object, forged$predictors)
}


# Bridge
predict_xlm_bridge <- function(model, predictors) {
  predict_xlm_numeric <- function(model, predictors) {
    predictors <- predict(model$fit$prepro, predictors)
    predictions <-
      sapply(model$fit$models, function(m) {
        predict(m, predictors)
      })
    hardhat::spruce_numeric(suppressall(rowMeans(predictions, na.rm = TRUE)))
  }
  predictors <- as_df(predictors)

  predictions <- predict_xlm_numeric(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

#' @export
print.xlm <- function(x) {
  cat_title_head("Modelos lineales basados en boots")
  cat_subtitle1(paste("Numero de modelos:", length(x$fit$models)))
  m <- purrr::map_dfr(x$fit$models, broom::tidy, .id = "id") %>%
    dplyr::select(id, term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    dplyr::select(!id) %>%
    as_df()
  print(m)
  print(x$fit$prepro)
}



# xloess  -------------------------------

#' Fit a xloess
#'
#' @name xloess
#' @rdname xloess
#'
#' @export
#'

xloess <- function(x, ...) {
  UseMethod("xloess")
}

#' @export
#' @rdname xloess
xloess.default <- function(x, ...) {
  cat_stop('`xloess` is not defined for a "{class(x)[1]}".')
}

# XY method - data frame

#' @export
#' @rdname xloess
xloess.data.frame <- function(x, y, times = 5, rep = 3) {
  processed <- hardhat::mold(x, y)
  xloess_bridge(processed, times = times, rep = rep)
}

# XY method - matrix

#' @export
#' @rdname xloess
xloess.matrix <- function(x, y, times = 5, rep = 3) {
  processed <- hardhat::mold(x, y)
  xloess_bridge(processed, times = times, rep = rep)
}

# Formula method

#' @export
#' @rdname xloess
xloess.formula <- function(formula,
                           data,
                           times = 5,
                           rep = 3) {
  processed <- hardhat::mold(formula, data)
  xloess_bridge(processed, times = times, rep = rep)
}

# Recipe method

#' @export
#' @rdname xloess
xloess.recipe <- function(x, data, times = 5, rep = 3) {
  processed <- hardhat::mold(x, data)
  xloess_bridge(processed, times = times, rep = rep)
}

# Bridge
xloess_bridge <- function(processed, times, rep) {
  predictors <- as_df(processed$predictors)
  outcome <- processed$outcomes[[1]]

  fit <-
    suppressall(xloess_impl(predictors, outcome, times = times, rep = rep))

  new_xloess(fit = fit, blueprint = processed$blueprint)
}


# Implementation

xloess_impl <- function(predictors, outcome, times, rep) {
  prepro <- xprepro(.data = predictors, everything())

  idx_folds <-
    kfolds_vec(
      x = seq_along(outcome),
      times = times,
      rep = rep
    )

  new_predictors <- predict(prepro, predictors)

  models <- lapply(
    X = idx_folds,
    FUN = function(i) {
      nms <- if (ncol(new_predictors) > 4) {
        sample(names(new_predictors), 4)
      } else {
        names(new_predictors)
      }
      stats::loess(
        formula = outcome ~ .,
        data = cbind(outcome = outcome[i], new_predictors[i, nms])
      )
    }
  )

  list(prepro = prepro, models = models)
}


new_xloess <- function(fit, blueprint) {
  hardhat::new_model(
    fit = fit,
    blueprint = blueprint,
    class = "xloess"
  )
}


#' Predict from a `xloess`
#'
#' @rdname mltools
#' @param object A `xloess` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @export
predict.xloess <- function(object, new_data) {
  forged <- hardhat::forge(new_data, object$blueprint)
  predict_xloess_bridge(object, forged$predictors)
}


# Bridge

predict_xloess_bridge <- function(model, predictors) {
  predict_xloess_numeric <- function(model, predictors) {
    predictors <- predict(model$fit$prepro, predictors)
    predictions <-
      sapply(model$fit$models, function(m) {
        predict(m, predictors)
      })
    hardhat::spruce_numeric(suppressall(rowMeans(predictions, na.rm = TRUE)))
  }
  predictors <- as_df(predictors)

  predictions <- predict_xloess_numeric(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

#' @export
print.xloess <- function(x) {
  cat_title_head("Modelos lineales basados en boots")
  cat_subtitle1(paste("Numero de modelos:", length(x$fit$models)))
  print(x$fit$prepro)
}


# pop_rollmean  -------------------------

#' @param .data dataset
#' @param ... variables a poblar
#' @param arrange_vars variables para ordenar el promedio movil
#' @param times repeticiones
#'
#' @rdname mltools
#' @export

pop_rollmean <-
  function(.data,
           var,
           arrange_vars,
           times = 5,
           new_obs = floor(.2 * nrow(.data))) {
    smooth_rollmean_impl <-
      function(.data, var, arrange_vars) {
        mutate_syms <- str2lang(var)

        grouping_vars <- names(select_if(.data, is_fctchr))

        arrange_syms <- rlang::syms(arrange_vars)

        sp <-
          map_dbl(
            .x = arrange_syms,
            .f = ~ significant_places(x = .data[[deparse(.x)]])
          )

        if (length(grouping_vars) > 0) {
          grouping_syms <- rlang::syms(grouping_vars)

          dg <- group_by(.data, !!!grouping_syms)
        } else {
          dg <- .data
        }

        dg <- arrange(dg, !!!arrange_syms)

        if (length(grouping_vars) > 0) {
          mutate_vars <-
            names(dplyr::select(
              .data,
              c(!!mutate_syms, !!!arrange_syms),
              -c(!!!grouping_syms)
            ))
        } else {
          mutate_vars <-
            names(dplyr::select(.data, c(!!mutate_syms, !!!arrange_syms)))
        }


        dg <-
          mutate_at(
            dg,
            .vars = vars(all_of(mutate_vars)),
            .funs = ~ slide_dbl(
              .x = .x,
              .fn = mean,
              .size = 2,
              .fill = FALSE,
              .partial = "center"
            )
          ) %>%
          drop_na() %>%
          ungroup()


        dg[, arrange_vars] <-
          purrr::map_dfc(.x = dg[, arrange_vars], .f = ~ round(x = .x, digits = sp))

        dg <-
          dplyr::anti_join(
            x = dg,
            y = .data[, c(arrange_vars, grouping_vars)],
            by = c(arrange_vars, grouping_vars)
          )

        bind_rows(dg[, names(.data)], .data) %>%
          arrange(!!!arrange_syms)
      }

    if (new_obs > .25 * nrow(.data)) {
      new_obs <- ceiling(.25 * nrow(.data))
      cat_warn("'new_obs' > .25*nrow('.data')")
    }

    smoothcall <- match.call(expand.dots = FALSE)
    smoothcall[[1L]] <- quote(smooth_rollmean_impl)
    if (!is.character(smoothcall$arrange_vars)) {
      smoothcall$arrange_vars <- deparse(smoothcall$arrange_vars)
    }
    smoothcall$times <- NULL
    smoothcall$new_obs <- NULL
    if (!is.character(smoothcall$var)) {
      smoothcall$var <- deparse(smoothcall$var)
    }

    sdata <- .data

    for (i in seq_len(times)) {
      smoothcall[[2L]] <- sdata
      sdata <- eval(smoothcall)
      cat(i, "\n")
    }
    slice_sample(.data = sdata, n = new_obs)
  }


# pop_nonlinear  ------------------------

#' @rdname mltools
#' @export

pop_nonlinear <-
  function(.data,
           x,
           y,
           times = 5,
           new_obs = floor(.2 * nrow(.data))) {
    if (new_obs > .25 * nrow(.data)) {
      new_obs <- ceiling(.25 * nrow(.data))
      cat_warn("'new_obs' > .25*nrow('.data')")
    }
    x <- substitute(x)
    y <- substitute(y)
    times <- eval(bquote_w(times), envir = env_call())

    .data <- dplyr::select(.data, c(!!x, !!y))

    folds_data <-
      lapply(
        eval_expr(
          manual_resamples(
            dataset = .data,
            times = !!times,
            outcome = !!y,
            rango = c(.7, .85)
          )$splits,
          envir = env_curr()
        ),
        rsample::analysis
      )

    expr_nls <- rlang::expr(fit_nonlinear(.x, !!x, !!y))
    # expr_nls <- rlang::expr(predict(!!expr_nls, datpop))
    # expr_nls <- rlang::expr(matrix(!!expr_nls, ncol = 1))

    .data <- .data[order(.data[[deparse(x)]]), ]
    datpop <-
      map(2:nrow(.data), ~ .data[[deparse(x)]][.x + c(-1, 0)])
    datpop <- map(datpop, sort)

    diffpop <- diff(sort(unique(.data[[deparse(x)]])))
    diffpop <- as_num(quantile(diffpop, .5))
    datpop <- datpop[map_dbl(datpop, diff) >= diffpop]
    datpop <-
      .un(map(datpop, ~ seq(.x[1], .x[2], length.out = 4)[2:3]))
    datpop <- datpop[!is_nonnum(datpop)]

    datpop_dist <- rowMeans(RANN::nn2(
      data = datpop,
      query = .data[[deparse(x)]],
      k = nsplits(new_obs)
    )$nn.dists)

    datpop <- datpop[order(-datpop_dist) <= new_obs]
    datpop <-
      as_df(tibble({{ x }} := datpop))

    ypop <-
      lapply(folds_data, function(.x) {
        try(eval(expr_nls), silent = T)
      })
    ypop <- sapply(ypop, function(x) {
      nullattr(predict(x, datpop))
    })
    ypop <- rowMeans(ypop)

    res <- mutate(datpop, {{ y }} := ypop)

    as_tbl(dplyr::relocate(res, rlang::expr(!!names(.data))))
  }


# pop_linear  ---------------------

#' @rdname mltools
#' @export
pop_linear <-
  function(.data,
           response,
           times = 5,
           rep = 3,
           new_obs = 1000) {
    response <- rlang::enexpr(response)

    if (is.name(rlang::enexpr(response))) {
      response_chr <- rlang::as_label(response)
    } else if (is.character(response)) {
      response_chr <- response
      response <- rlang::sym(response_chr)
    }

    objs <-
      smooth_linear(
        x = dplyr::select(.data, -all_of(response_chr)),
        y = dplyr::select(.data, all_of(response_chr)),
        times = times,
        rep = rep
      )

    objs_clip <-
      limits_bounds(.data = .data[, nmdiff(.data, response_chr)], where(is_dblint))
    nn <- length(setdiff(names(.data), response_chr))
    nn <- setNames(1:nn, nmdiff(.data, response_chr))

    new_data <-
      purrr::imap_dfc(nn, ~ .data[sample(seq_len(nrow(.data)), new_obs, new_obs >
                                           nrow(.data)), ][[.y]])

    new_data[, map_lgl(new_data, is_dbl)] <-
      purrr::map_dfc(new_data[, map_lgl(new_data, is_dbl)], roll_noise)

    new_data - predict(objs_clip, new_data)

    new_pred <- predict(object = objs, new_data)

    names(new_pred) <- response_chr

    cbind(new_pred, new_data)
  }



# nearmiss  -----------------------------

#' @rdname mltools
#' @export

nearmiss <-
  function(.data,
           class_var,
           ignore_vars = NULL,
           k = 5,
           under_ratio = 1) {
    downsample_count <- function(data, var, ratio) {
      min_count <- min(table(data[[var]]))
      ratio_target <- min_count * ratio
      which_class <- which(table(data[[var]]) > ratio_target)
      table(data[[var]])[which_class] - ratio_target
    }

    subset_to_matrix <-
      function(data, var, class, equal = TRUE) {
        if (equal) {
          return(as_mtx(data[data[[var]] == class, names(data) != var]))
        } else {
          return(as_mtx(data[data[[var]] != class, names(data) != var]))
        }
      }


    if (is_empty(ignore_vars)) {
      ignore_vars <- character(0)
    }
    if (is.name(rlang::enexpr(class_var))) {
      class_var <- rlang::enexpr(class_var)
      class_var <- rlang::as_label(class_var)
    }

    .data <- drop_na(.data)
    nms <- nmdiff(.data, class_var)
    new_data <-
      expr(do_xprepro(.data =as_df(!!.data), everything(),outcome = quote(!!sym(class_var))))
    new_data <- eval(new_data)
    classes <- downsample_count(new_data, class_var, under_ratio)

    deleted_rows <- integer()

    for (i in seq_along(classes)) {
      df_only <- new_data[, !names(new_data) %in% ignore_vars]
      class <-
        subset_to_matrix(df_only, class_var, names(classes)[i])
      not_class <-
        subset_to_matrix(df_only, class_var, names(classes)[i], FALSE)

      if (nrow(not_class) <= k) {
        cat_stop(paste0(
          "Not enough danger observations of '",
          names(classes)[i],
          "' to perform NEARMISS."
        ))
      }

      dists <-
        RANN::nn2(not_class[, !(colnames(not_class) %in% ignore_vars)],
                  class[, !(colnames(class) %in% ignore_vars)],
                  k = k
        )$nn.dists

      selected_ind <-
        order(rowMeans(dists)) <= (nrow(class) - classes[i])
      deleted_rows <-
        c(deleted_rows, which(new_data[[class_var]] %in% names(classes)[i])[!selected_ind])
    }

    if (length(deleted_rows) > 0) {
      .data <- .data[-deleted_rows, ]
    }
    .data
  }


# smooth_nonlinear  ---------------------

#' @rdname mltools
#' @export

smooth_nonlinear <-
  function(.data,
           x,
           y,
           times = 5,
           rep = 2) {
    smoothcall <- match.call()
    smoothcall[[1L]] <- quote(smooth_nonlinear_impl)
    smoothcall$rep <- NULL

    if (inherits(smoothcall$x, "call")) {
      smoothcall$x <- rlang::enexpr(x)
    }

    if (inherits(smoothcall$y, "call")) {
      smoothcall$y <- rlang::enexpr(y)
    }
    if (is.character(smoothcall$x)) {
      smoothcall$x <- rlang::sym(smoothcall$x)
    }
    if (is.character(smoothcall$y)) {
      smoothcall$y <- rlang::sym(smoothcall$y)
    }

    sdata <- .data

    for (i in seq_len(rep)) {
      smoothcall$.data <- sdata
      smoothcall$.data <-
        tryCatch(
          expr = eval(smoothcall),
          error = function(e) {
            NULL
          }
        )
      if (is.null(smoothcall$.data)) {
        smoothcall$.data <- sdata
      }

      cat(i, "\n")
    }
    smoothcall$.data
  }

#' @rdname mltools
#' @keywords internal

smooth_nonlinear_impl <-
  function(.data,
           x,
           y,
           times = 5) {
    x <- rlang::enexpr(x)
    y <- rlang::enexpr(y)
    times <- rlang::enexpr(times)

    dataset <- dplyr::select(.data, c(!!x, !!y))

    if (ncol(dplyr::select(.data, !!x)) > 1) {
      cat_stop("ncol(dplyr::select('.data',!!'x')) > 1 ")
    }

    folds_data <-
      lapply(
        manual_resamples(
          dataset = dataset,
          times = !!times,
          outcome = !!y,
          rango = c(.7, .85)
        )$splits,
        rsample::analysis
      )

    expr_nls <- rlang::expr(fit_nonlinear(x, !!x, !!y))
    expr_nls <- rlang::expr(predict(!!expr_nls, .data))
    expr_nls <- rlang::expr(matrix(!!expr_nls, ncol = 1))

    yhat <- rowMeans(sapply(folds_data, function(x) {
      eval(expr_nls)
    }))
    yreal <- .data[[deparse(y)]]


    .data[[deparse(y)]] <-
      sapply(seq_along(yreal), function(i) {
        mean(yhat[i], yreal[i])
      })
    .data
  }


# smooth_outliers  ----------------------

#' @rdname mltools
#' @export
smooth_outliers <-
  function(.data,
           response,
           times,
           rep,
           ...) {
    ckd <- match.call(expand.dots = F)$...
    response <- rlang::enexpr(response)

    if (is.name(rlang::enexpr(response))) {
      response_chr <- rlang::as_label(response)
    } else {
      response_chr <- response
      response <- rlang::sym(response_chr)
    }

    outbuild_idx <- outlier_locs(x = .data[[response_chr]])

    out_idx <-
      cooks_dist(
        .data = .data,
        response = !!response,
        everything()
      )$out

    if (sum(out_idx == "outlier") > 0) {
      fs <-
        as.formula(
          dplyr::select(.data, !!!ckd, !!response) %>% relocate(!!response, .before = everything())
        )
      objs <-
        smooth_linear(fs, .data[-outbuild_idx, ],
                      times = times,
                      rep = rep
        )

      lmit_obj <-
        limits_bounds(.data = .data[-outbuild_idx, ], everything())

      .data[out_idx == "outlier", ][[response_chr]] <-
        predict(objs, .data[out_idx == "outlier", ])$.pred

      new_data <- predict(lmit_obj, .data)
    } else {
      new_data <- .data
    }

    new_data
  }



# smooth_linear  ------------------------

#' Fit a smooth_linear
#'
#' @name smooth_linear
#' @rdname smooth_linear
#'
#' @export
#'

smooth_linear <- function(x, ...) {
  UseMethod("smooth_linear")
}

#' @export
#' @rdname smooth_linear
smooth_linear.default <- function(x, ...) {
  cat_stop('`smooth_linear` is not defined for a "{class(x)[1]}".',
           call. = FALSE
  )
}

# XY method - data frame

#' @export
#' @rdname smooth_linear
smooth_linear.data.frame <- function(x, y, times = 5, rep = 3) {
  processed <- hardhat::mold(x, y)
  smooth_linear_bridge(processed, times = times, rep = rep)
}

# XY method - matrix

#' @export
#' @rdname smooth_linear
smooth_linear.matrix <- function(x, y, times = 5, rep = 3) {
  processed <- hardhat::mold(x, y)
  smooth_linear_bridge(processed, times = times, rep = rep)
}

# Formula method

#' @export
#' @rdname smooth_linear
smooth_linear.formula <- function(formula,
                                  data,
                                  times = 5,
                                  rep = 3) {
  processed <- hardhat::mold(formula, data)
  smooth_linear_bridge(processed, times = times, rep = rep)
}

# Recipe method

#' @export
#' @rdname smooth_linear
smooth_linear.recipe <- function(x, data, times = 5, rep = 3) {
  processed <- hardhat::mold(x, data)
  smooth_linear_bridge(processed, times = times, rep = rep)
}

# Bridge

smooth_linear_bridge <- function(processed, times, rep) {
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  fit <-
    smooth_linear_impl(predictors, outcome, times = times, rep = rep)

  new_smooth_linear(fit = fit, blueprint = processed$blueprint)
}


# Implementation

smooth_linear_impl <- function(predictors, outcome, times, rep) {
  linear <- xlm(
    x = predictors,
    y = outcome,
    times = times,
    rep = rep
  )
  loess <- xloess(
    x = predictors,
    y = outcome,
    times = times,
    rep = rep
  )

  list(linear = linear, loess = loess)
}


new_smooth_linear <- function(fit, blueprint) {
  hardhat::new_model(
    fit = fit,
    blueprint = blueprint,
    class = "smooth_linear"
  )
}


#' Predict from a `smooth_linear`
#'
#' @rdname mltools
#' @param object A `smooth_linear` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @export
predict.smooth_linear <- function(object, new_data) {
  forged <- hardhat::forge(new_data, object$blueprint)
  predict_smooth_linear_bridge(object, forged$predictors)
}


# Bridge


predict_smooth_linear_bridge <- function(model, predictors) {
  predict_smooth_linear_numeric <- function(model, predictors) {
    p_linear <- predict(model$fit$linear, predictors)$.pred

    p_loess <- predict(model$fit$loess, predictors)$.pred

    predictions <- cbind(p_linear, p_loess)
    hardhat::spruce_numeric(suppressall(rowMeans(predictions, na.rm = TRUE)))
  }
  predictors <- as_df(predictors)

  predictions <-
    predict_smooth_linear_numeric(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}


# cooks_dist  ---------------------------

#' @rdname mltools
#' @export

boots_cooks_dist <-
  function(.data,
           response,
           times = 4,
           rep = 2,
           ...) {
    cdots <- dots(...)

    response <- rlang::enexpr(response)

    .data %<>% rowindex()

    dataset <- dplyr::select(.data, !!!cdots, !!response, row)

    .vars <- nmdiff(dataset, "row")

    xfolds <-
      do_call(
        "kfolds",
        list(
          dataset = .data,
          times = times,
          rep = rep,
          outcome = response
        )
      )

    out <- map(
      .x = rset2dflist(xfolds),
      .f = ~ do_call(
        f = "cooks_dist",
        .args = c(.data = list(.x), response = response, all_of(.vars))
      )
    )

    map_dfr(out, ~ select(.x, cd, row) %>% mutate(out = cd / attr(.x, "ts"))) %>%
      group_summarise_all(.groups = row, mean) %>%
      mutate(
        out = ifelse(out >= 1, "outlier", "normal"),
        out = factor(x = out, levels = c("outlier", "normal"))
      ) %>%
      inner_join(.data, by = "row") %>%
      select(!row)
  }


#' @rdname mltools
#' @export
cooks_dist <-
  function(.data, response, ...) {
    cdots <- dots(...)

    response <- rlang::enexpr(response)

    dataset <- dplyr::select(.data, !!!cdots, !!response)

    yfun <-
      paste0(setdiff(names(dataset), deparse(response)), collapse = " + ")

    yfun <- paste0(deparse(response), " ~ ", yfun)

    model <- eval(lm(formula(yfun), data = dataset))

    .data$cd <- cooks.distance(model)

    n <- length(.data$cd)

    k <- length(model$coefficients) - 1

    ts <-
      mean(c(4 / n, 4 / (n - k - 1), 1 / (n - k - 1), 3 * mean(.data$cd)))

    .data %>%
      mutate(
        out = cd / ts,
        out = ifelse(out >= 1, "outlier", "normal"),
        out = factor(x = out, levels = c("outlier", "normal"))
      ) %>%
      set_attr("ts", ts)
  }



# smote  --------------------------------

#' @export

smote <- function(.data,
                  class_var,
                  k = 5,
                  over_ratio = 1) {
  smote_impl <- function(df, var, k, over_ratio) {
    smote_data <-
      function(data,
               k,
               n_samples,
               smote_ids = seq_len(nrow(data))) {
        ids <- RANN::nn2(data, k = k + 1, searchtype = "priority")$nn.idx
        indexes <-
          rep(sample(smote_ids), length.out = n_samples)
        index_len <- tabulate(indexes, NROW(data))
        out <- matrix(0, nrow = n_samples, ncol = ncol(data))
        sampleids <- sample.int(k, n_samples, TRUE)
        runif_ids <- stats::runif(n_samples)

        iii <- 0
        for (row_num in smote_ids) {
          index_selection <- iii + seq_len(index_len[row_num])
          # removes itself as nearest neighbour
          id_knn <- ids[row_num, ids[row_num, ] != row_num]
          dif <- data[id_knn[sampleids[index_selection]], ] -
            data[rep(row_num, index_len[row_num]), ]
          gap <- dif * runif_ids[index_selection]
          out[index_selection, ] <-
            data[rep(row_num, index_len[row_num]), ] + gap
          iii <- iii + index_len[row_num]
        }

        out
      }
    data <- split(df, df[[var]])
    majority_count <- max(table(df[[var]]))
    ratio_target <- majority_count * over_ratio
    which_upsample <- which(table(df[[var]]) < ratio_target)
    samples_needed <-
      ratio_target - table(df[[var]])[which_upsample]
    min_names <- names(samples_needed)

    out_dfs <- list()

    for (i in seq_along(samples_needed)) {
      minority_df <- data[[min_names[i]]]
      minority <-
        as_mtx(minority_df[names(minority_df) != var])

      if (nrow(minority) <= k) {
        cat_stop(paste0(
          "Not enough observations of '",
          min_names[i],
          "' to perform SMOTE."
        ))
      }

      synthetic <-
        smote_data(minority, k = k, n_samples = samples_needed[i])
      out_df <- as_df(synthetic)
      out_df[var] <- data[[names(samples_needed)[i]]][[var]][1]
      names(out_df) <- names(df)
      out_dfs[[i]] <- out_df
    }

    final <- rbind(df, do.call(rbind, out_dfs))
    final[[var]] <-
      factor(final[[var]], levels = levels(df[[var]]))
    rownames(final) <- NULL
    final
  }

  if (is.name(rlang::enexpr(class_var))) {
    class_var <- rlang::enexpr(class_var)
    class_var <- rlang::as_label(class_var)
  }

  .data <- drop_na(.data)
  if (length(class_var) != 1) {
    cat_stop("Please select a single factor variable for `var`.")
  }

  class_var <- rlang::arg_match(class_var, colnames(.data))

  assert_engine(is_fctchr(.data[[class_var]]), severity = "stop")
  assert_engine(length(k) == 1, severity = "stop")
  assert_engine(k >= 1, severity = "stop")

  predictors <- setdiff(colnames(.data), class_var)

  assert_numeric_all(.data[, predictors])
  assert_na_any(dplyr::select(.data, -all_of(class_var)))

  suppressall(smote_impl(.data, class_var, k, over_ratio))
}


# balance_data  -------------------------

#' @rdname mltools
#' @export

balance_data <- function(.data,
                         class_var) {
  # k <- ncase(nrow(.data))

  if (length(class_var) > 1) {
    dataset_df <-
      .data %>%
      tidyr::unite("factor_col", all_of(!!!class_var), sep = "_x_") %>%
      as_df()
  } else {
    dataset_df <-
      .data %>%
      rename("factor_col" = all_of(!!class_var)) %>%
      as_df()
  }

  colsnames <-
    rlang::syms(setdiff(names(dataset_df), "factor_col"))
  colsnames <- rlang::quos(!!!colsnames, .named = TRUE)

  dataset_list <-
    split(
      x = dplyr::select(dataset_df, !factor_col),
      f = dataset_df$factor_col
    )

  current_n <- table(dataset_df$factor_col)

  dist_n <- diff(range(current_n)) / 3
  range_n <- c(min(current_n) + dist_n, max(current_n) - dist_n)

  for (i in names(current_n)) {
    if (current_n[names(current_n) == i] < range_n[1]) {
      dataset_list[[i]] <-
        suppressWarnings(smote_balance(
          dataset = dataset_list[[i]],
          !!!colsnames,
          range_n = range_n
        ))
    }

    if (current_n[names(current_n) == i] > range_n[2]) {
      dataset_list[[i]] <-
        suppressWarnings(nearmiss_balance(
          dataset = dataset_list[[i]],
          !!!colsnames,
          range_n = range_n
        ))
    }
  }

  if (length(class_var) > 1) {
    dataset_list <-
      bind_rows(dataset_list, .id = "factor_col") %>%
      as_tbl() %>%
      tidyr::separate(
        col = factor_col,
        sep = "_x_",
        into = class_var
      ) %>%
      mutate_at(.vars = vars(class_var), .funs = factor)
  } else {
    class_var <- rlang::sym(class_var)

    dataset_list <-
      bind_rows(dataset_list, .id = "factor_col") %>%
      as_tbl() %>%
      mutate({{ class_var }} := factor(factor_col),
             factor_col = NULL, .before = everything()
      )
  }


  dataset_list
}

#' @rdname mltools
#' @keywords internal
smote_balance <- function(dataset, ..., range_n) {
  colsnames <-
    rlang::enquos(..., .named = T)

  j <- 1
  repeat {
    out <- tryCatch(
      smote_balance_impl(dataset = dataset, !!!colsnames, range_n = range_n),
      error = function(e) {
        NULL
      }
    )
    if (!is.null(out) | j == 5) {
      break
    }
    j <- j + 1
  }
  if (is.null(out)) {
    out <- dataset
  }
  out
}

#' @rdname mltools
#' @keywords internal

smote_balance_impl <- function(dataset, ..., range_n) {
  colsnames <-
    rlang::enquos(..., .named = T)

  colsnames <- names(colsnames)

  pca_obj <-
    xpca(
      .data = dataset,
      c(!!!colsnames),
      threshold = .95,
      num_comp = 4
    )

  dataset$cluster <-
    fpc::clusterboot(
      data = select_if(predict(pca_obj, dataset), is.numeric),
      clustermethod = fpc::kmeansCBI,
      krange = abs((range_n[1] - nrow(dataset)) * .25),
      count = F
    )$result$partition

  dataset$cluster <-
    as_chr(dataset$cluster)

  while (min(table(dataset$cluster)) <= 5) {
    table_cluster <- table(dataset$cluster)
    table_cluster <- table_cluster[order(table_cluster)]
    dataset$cluster[dataset$cluster %in% names(table_cluster[1:2])] <-
      "00"
  }

  dataset$cluster <-
    as_fct(dataset$cluster)
  table_cluster <- table(dataset$cluster)
  max_table_cluster <- table_cluster[which.max(table_cluster)]
  or <-
    (range_n[1] - max_table_cluster) / (length(table_cluster) - 1) / max_table_cluster
  dataset <-
    smote(
      dataset,
      class_var = "cluster",
      k = min(table_cluster) * .5,
      over_ratio = or
    )
  dataset$cluster <- NULL
  dataset
}

#' @rdname mltools
#' @keywords internal

nearmiss_balance <- function(dataset, ..., range_n) {
  colsnames <-
    rlang::enquos(..., .named = T)
  j <- 1
  repeat {
    out <- tryCatch(
      nearmiss_balance_impl(dataset = dataset, !!!colsnames, range_n = range_n),
      error = function(e) {
        NULL
      }
    )
    if (!is.null(out) | j == 6) {
      break
    }
    j <- j + 1
  }
  if (is.null(out)) {
    out <- dataset
  }
  out
}

#' @rdname mltools
#' @keywords internal
nearmiss_balance_impl <- function(dataset, ..., range_n) {
  colsnames <-
    rlang::enquos(..., .named = T)

  colsnames <- names(colsnames)
  pca_obj <-
    xpca(
      .data = dataset,
      c(!!!colsnames),
      threshold = .95,
      num_comp = 4
    )

  dataset$cluster <-
    cluster_boot(
      .data = select_if(predict(pca_obj, dataset), is.numeric),
      krange = abs((range_n[2] - nrow(dataset)) * .25)
    )$partition

  dataset$cluster <-
    as_chr(dataset$cluster)

  while (min(table(dataset$cluster)) <= 5) {
    table_cluster <- table(dataset$cluster)
    table_cluster <- table_cluster[order(table_cluster)]
    dataset$cluster[dataset$cluster %in% names(table_cluster[1:2])] <-
      "00"
  }

  dataset$cluster <-
    as_fct(dataset$cluster)
  table_cluster <- table(dataset$cluster)
  max_table_cluster <- table_cluster[which.max(table_cluster)]
  min_table_cluster <- table_cluster[which.min(table_cluster)]
  ur <-
    (range_n[2] + max_table_cluster) / (length(table_cluster) - 1) / min_table_cluster
  dataset <-
    nearmiss(
      dataset,
      class_var = "cluster",
      k = min(table_cluster),
      under_ratio = ur
    )
  dataset$cluster <- NULL
  dataset
}


# -------------------------------------------------------------------------
