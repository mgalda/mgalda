# core --------------------------------------------------------------------


#' buscar la mejor normalizacion de las variables
#'
#'
#' @name bestnorm
#' @rdname bestnorm
#' @return normalizacion de variables
#' @param .data dataset
#' @param ... columnas a normalizar
#' @param x vector a normalizar
#' @description objeto de la mejor normalizacion: bestnorm
#'
#' @examples
#' credit <- mgalda::datalearn$credit
#' set.seed(7)
#' object <- bestnorm(.data = credit, where(is_dblint))
#' predict(object = object, new_data = credit)
#' autoplot(object)
#'
#' x <- mgalda::datalearn$credit$price
#' set.seed(7)
#' object <- bestnorm_transfor(x)
#' object
#'
#' x <- mgalda::datalearn$credit$price
#' set.seed(7)
#' fbest <- bestnorm_fn(x)
#' autoplot(fbest)
#'
#' x <- mgalda::datalearn$credit$price
#' set.seed(7)
#' plot(x, bestnorm_vec(x))
#' @export
bestnorm <- function(.data, ...) {
  UseMethod("bestnorm")
}

#' @export
bestnorm.default <-
  function(.data, ...) {
    generic_default()
  }

#' @export
bestnorm.data.frame <-
  function(.data, ...) {
    expr <- rlang::expr(c(...))

    pos <- tidyselect::eval_select(expr, data = .data)

    dataset <- rlang::set_names(.data[pos], names(pos))

    res <- map(.x = dataset, .f = ~ bestnorm_fn(x = .x))
    res <- compact(res)
    class(res) <- "bestnorm"
    attr(res, "transformed") <- names(res)
    res
  }

#' @export
print.bestnorm <- function(x) {
  generic_print(x)
}

#' @export
print.transform <- function(x) {
  cat_subtitle1(x$trans_nm)
  print(body(x$fn_transform))
}

#' @export
print.function_transform <- function(x) {
  cat_subtitle1(attr(x, "transform"))
  print(body(x))
}


#' transformacion posibles: bestnorm_transfor
#'
#' @rdname bestnorm
#' @description evaluacion de todos los metodos de
#' @examples
#'
#' x <- mgalda::datalearn$credit$price
#' set.seed(7)
#' bestnorm_transfor(x)
#' @export

bestnorm_transfor <- function(x) {
  x <- safe_vec(as_dbl(x))

  lst_tr <- fn_each(
    asinh = f(do_try_ier(transform_asinh(x))),
    asn = f(do_try_ier(transform_asn(x))),
    atanh = f(do_try_ier(transform_atanh(x))),
    binarize_dens = f(do_try_ier(transform_binarize_dens(x))),
    binarize_mean = f(do_try_ier(transform_binarize_mean(x))),
    exp = f(do_try_ier(transform_exp(x))),
    invasn = f(do_try_ier(transform_invasn(x))),
    invlogit = f(do_try_ier(transform_invlogit(x))),
    linear = f(do_try_ier(transform_linear(x))),
    log = f(do_try_ier(transform_log(x))),
    log10 = f(do_try_ier(transform_log10(x))),
    logit = f(do_try_ier(transform_logit(x))),
    michmen = f(do_try_ier(transform_michmen(x))),
    powerthree = f(do_try_ier(transform_powerthree(x))),
    powertwo = f(do_try_ier(transform_powertwo(x))),
    reciprocal = f(do_try_ier(transform_reciprocal(x))),
    sqrt = f(do_try_ier(transform_sqrt(x))),
    tanh = f(do_try_ier(transform_tanh(x))),
    yeojohnson = f(do_try_ier(transform_yeojohnson(x)))
  )

  lx <- lst_tr(x)
  lx <- lx[!map_lgl(lx, is_ier)]
  lx[map_lgl(lx,~sum(is.nan(.x$tx) | is.infinite(.x$tx))==0)]

}

#' @rdname bestnorm
#' @description respuesta de la funcion de transformacion optima: bestnorm_fn
#' @examples
#' x <- mgalda::datalearn$credit$price
#' set.seed(7)
#' autoplot(bestnorm_fn(x))
#' @export
bestnorm_fn <- function(x) {
  x <- as_dbl(x)
  if (!is.numeric(x)) {
    cat_stop("!is.numeric(x)")
  }
  do_try(bestnorm_var(x)$fn_transform)
}

#' @keywords internal
bestnorm_var <- function(x) {
  x <- as_dbl(x)

  bestt <- bestnorm_transfor(x)

  statistic_bestt <-
    purrr::map_dfr(
      .x = bestt,
      .f = ~ .x$statistic %>%
        dplyr::select(!p.value) %>%
        pivot_wider(
          names_from = metrica,
          values_from = statistic
        ),
      .id = "transfor"
    )

  if (nrow(drop_na(statistic_bestt)) == 1) {
    besttmtx <-
      drop_na(statistic_bestt) %>% pull(transfor)
  } else {
    besttmtx <-
      statistic_bestt %>%
      drop_na() %>%
      mutate(
        pearson = pearson * -1,
        cramervon = cramervon * -1
      ) %>%
      mutate_if(is_dblint, rank_percent)

    besttmtx <-
      besttmtx[which.max(rowSums(besttmtx[, -1])), 1][[1]]
  }

  bestt[[besttmtx]]
}

#' @export
autoplot.function_transform <- function(x) {
  data <-
    tibble(!!attr(x, "transform") := x(attr(x, "x")), x = attr(x, "x")) %>%
    mutate_all(~ rescalar(x = .x, to = c(0, 1))) %>%
    arrange(x) %>%
    mutate(n = row_number()) %>%
    pivot_longer(!n, names_to = "x", names_transform = factor)

  datalabs <-
    tibble(
      x = unique(data$x),
      n = round(quantile(data$n, c(.25, .75)))
    ) %>% inner_join(data, by = c("x", "n"))

  grf_line(value ~ n, colour = ~x, data = data) %>%
    grf_text(
      value ~ n,
      colour = ~x,
      label = ~x,
      data = datalabs,
      vjust = -.5,
      fontface = "bold"
    ) +
    gr_current_theme() +
    scale_pal_gr(scale = "discrete", aesthetics = "colour") +
    ylab("valor rescalado") +
    xlab("percentil") +
    gr_legend_acction(remove_legend = TRUE, remove_tittle = TRUE) +
    scale_y_continuous(labels = numdig_format)
}

#' @export
autoplot.bestnorm <- function(x) {
  gr_grid(gglist = map(x, ~ autoplot(.x) + theme(text = element_text(size = 12))))
}
# prediccion --------------------------------------------------------------



#' @export
predict.bestnorm <- function(object,
                             new_data = NULL,
                             keep = F) {
  nvars <- names(object)

  pred_data <-
    purrr::map_dfc(
      .x = nvars,
      .f = ~ tibble(!!.x := as_num(object[[.x]](new_data[[.x]])))
    )

  if (keep) {
    pred_data <-
      rename_with(
        .data = pred_data,
        .fn = ~ str_c("norm", .x, sep = "_")
      )
    pred_data <- bind_cols(new_data, pred_data)
  } else {
    pred_data <-
      bind_cols(dplyr::select(new_data, !dplyr::matches(nvars)), pred_data)
  }
  pred_data
}

# vector ------------------------------------------------------------------

#' @rdname bestnorm
#' @description normalizacion de un vector: bestnorm_vec
#'
#' @examples
#' x <- mgalda::datalearn$credit$price
#' set.seed(7)
#' bestnorm_vec(x)
#' @export

bestnorm_vec <- function(x) {
  if (!is.numeric(x)) {
    cat_stop("!is.numeric(x)")
  }

  x <- as_dbl(x)

  do_try(bestnorm_var(x)$tx, rep(NA, length(x)))
}


# helpers -----------------------------------------------------------------


#' @keywords internal
yeojohnson <- function(x,
                       limits = c(-3, 3),
                       num_unique = 5,
                       eps = .001,
                       na_rm = TRUE) {
  .transform_yj <- function(x, lambda, eps = .001) {
    if (is.na(lambda)) {
      return(x)
    }
    if (!inherits(x, "tbl_df") || is.data.frame(x)) {
      x <- unlist(x, use.names = FALSE)
    } else {
      if (!is.vector(x)) {
        x <- as_vct(x)
      }
    }

    not_neg <- which(x >= 0)
    is_neg <- which(x < 0)

    nn_trans <- function(x, lambda) {
      if (abs(lambda) < eps) {
        log(x + 1)
      } else {
        ((x + 1)^lambda - 1) / lambda
      }
    }

    ng_trans <- function(x, lambda) {
      if (abs(lambda - 2) < eps) {
        -log(-x + 1)
      } else {
        -((-x + 1)^(2 - lambda) - 1) / (2 - lambda)
      }
    }

    if (length(not_neg) > 0) {
      x[not_neg] <- nn_trans(x[not_neg], lambda)
    }

    if (length(is_neg) > 0) {
      x[is_neg] <- ng_trans(x[is_neg], lambda)
    }
    x
  }

  .objective_yj <- function(lam, dat, eps) {
    .loglikelihood_yj <- function(lambda, y, eps = .001) {
      y <- y[!is.na(y)]
      n <- length(y)
      y_t <- .transform_yj(y, lambda)
      var_t <- var(y_t) * (n - 1) / n
      const <- sum(sign(y) * log(abs(y) + 1))
      res <- -.5 * n * log(var_t) + (lambda - 1) * const
      res
    }
    dat <- dat[complete.cases(dat)]
    .loglikelihood_yj(
      lambda = lam,
      y = dat,
      eps = eps
    )
  }


  .lambda_yj <- function(dat, limits, num_unique, na_rm) {
    na_rows <- which(is.na(dat))
    if (length(na_rows) > 0) {
      if (na_rm) {
        dat <- dat[-na_rows]
      } else {
        cat_stop("Missing values in data. See `na_rm` option")
      }
    }

    if (length(unique(dat)) < num_unique) {
      return(NA)
    }
    res <- optimize(
      .objective_yj,
      interval = limits,
      maximum = TRUE,
      dat = dat,
      eps = eps,
      tol = .0001
    )
    lam <- res$maximum
    if (abs(limits[1] - lam) <= eps |
        abs(limits[2] - lam) <= eps) {
      lam <- NA
    }
    lam
  }


  lambda <-
    .lambda_yj(
      dat = x,
      limits = limits,
      num_unique = num_unique,
      na_rm = na_rm
    )

  eps <- enexpr(eps)
  lambda <- substitute(lambda)

  expr_yj <- body(.transform_yj)

  expr_yj <- eval(interp(quote(
    modify_body(
      expr = expr_yj,
      lambda = lambda,
      eps = eps
    )
  ),
  lambda = lambda,
  eps = eps
  ))
  fn <-
    make_function(
      args = alist(x = ),
      body = expr_yj
    )

  fn(x)
}


#' @keywords internal
newtrans <- function(x, name, expr) {
  expr_tr <- substitute(expr)
  acc_prec <- precision(x) / 10
  mu <- round_any(mean(eval(expr_tr), na.rm = TRUE), acc_prec)
  sigma <- round_any(sd(eval(expr_tr), na.rm = TRUE), acc_prec)

  fn <-
    interp(
      obj_ = quote((tx - mu) / sigma),
      tx = expr_tr,
      mu = mu,
      sigma = sigma
    )
  fn <-
    structure(
      make_function(args = alist(x = ), body = fn),
      class = c("function_transform", "function"),
      transform = name,
      x = sort_filter_vec(x, n = 100)
    )
  structure(
    list(
      trans_nm = name,
      tx = fn(x),
      x = x,
      trans = expr_tr,
      fn_transform = fn,
      statistic = normal_test(fn(x))
    ),
    class = c("transform", name)
  )
}

#' @keywords internal
transform_powertwo <- function(x) {
  newtrans(x, "powertwo", x^2)
}
#' @keywords internal
transform_powerthree <- function(x) {
  newtrans(x, "powerthree", x^3)
}
#' @keywords internal
transform_exp <- function(x) {
  newtrans(x, "exp", exp(x))
}
#' @keywords internal
transform_log <- function(x) {
  newtrans(x, "log", log(x))
}
#' @keywords internal
transform_log10 <- function(x) {
  newtrans(x, "log10", log10(x))
}
#' @keywords internal
transform_michmen <- function(x) {
  newtrans(x, "michmen", x / (1 + x))
}
#' @keywords internal
transform_sqrt <- function(x) {
  newtrans(x, "sqrt", sign(x) * sqrt(abs(x)))
}
#' @keywords internal
transform_asinh <- function(x) {
  newtrans(x, "asinh", asinh(x))
}
#' @keywords internal
transform_logit <- function(x) {
  newtrans(x, "logit", log(x / (1 + x), 10))
}
#' @keywords internal
transform_invlogit <- function(x) {
  newtrans(x, "invlogit", 1 / (1 + exp(x)))
}
#' @keywords internal
transform_linear <- function(x) {
  newtrans(x, "linear", (x - min(x)) / diff(range(x)))
}
#' @keywords internal
transform_binarize_mean <- function(x) {
  newtrans(x, "binarize_mean", ifelse(x > mean(x), 1, 0))
}
#' @keywords internal
transform_binarize_dens <- function(x) {
  newtrans(
    x,
    "binarize_dens",
    ifelse(x > density(x[!is.na(x)])$x[which.max(density(x[!is.na(x)])$y)], 1, 0)
  )
}
#' @keywords internal
transform_tanh <- function(x) {
  newtrans(x, "tanh", tanh(x))
}
#' @keywords internal
transform_invasn <- function(x) {
  newtrans(x, "invasn", sin(x / 2)^2)
}
#' @keywords internal
transform_reciprocal <- function(x) {
  newtrans(x, "reciprocal", 1 / x)
}
#' @keywords internal
transform_asn <- function(x) {
  newtrans(x, "asn", 2 * sign(x) * asin(sqrt(abs(x))))
}
#' @keywords internal
transform_atanh <- function(x) {
  newtrans(x, "atanh", atanh(x))
}
#' @keywords internal
transform_yeojohnson <- function(x,
                                 limits = c(-3, 3),
                                 num_unique = 5,
                                 eps = .001,
                                 na_rm = TRUE) {
  yj_call <- make_call(
    f = "yeojohnson",
    .args = list(
      x = quote(x),
      limits = limits,
      num_unique = num_unique,
      eps = eps,
      na_rm = na_rm
    )
  )

  do_call(
    f = "newtrans",
    .args = list(x = x, name = "yeojohnson", expr = yj_call)
  )
}
