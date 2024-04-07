#' @include util_env.R
NULL

# distrcont -
#' distrcont
#' @name distribution_models
#' @rdname distribution_models
#' @keywords internal
#'
#' @examples
#'
#' library(ggplot2)
#'
#' vec_cont <- xrandom(n = 1000, mean = 20, sd = 3)
#' vec_cont <- safe_vec(vec_cont)
#' vec_dist <- rbinom(1000, 20:35, .5)
#'
#' distr_cont <- distrcont(x = vec_cont)
#' distr_discr <- distrdiscr(x = vec_dist)
#'
#' distr_cont
#' distr_discr
#'
#' autoplot(distr_cont)
#' autoplot(distr_discr)
#'
#' distr_cont <- fit_custom_distr(x = vec_cont)
#' distr_discr <- fit_custom_distr(x = vec_dist)
#'
#' distr_cont
#' distr_discr
#'
#' autoplot(distr_cont)
#' autoplot(distr_discr)
#'
#' distr_cont <- density_integrate(x = vec_cont)
#' distr_discr <- density_integrate(x = vec_dist)
#'
#' distr_cont
#' distr_discr
#'
#' autoplot(distr_cont)
#' autoplot(distr_discr)
#'
#' distr_cont <- pdqr_distr("norm", mean = 3, sd = 4)
#' distr_discr <- pdqr_distr(f = "rndbinom", size = 15, prob = .4)
#'
#' distr_cont
#' distr_discr
#'
#' autoplot(distr_cont)
#' autoplot(distr_discr)
#'
#' q_cont <- quantile_dist(dist = distr_cont, p = c(.1, .9))
#' q_discr <- quantile_dist(dist = distr_discr, p = c(.1, .9))
#'
#' distr_cont <- truncate_distr(distr_cont, q_cont[2], q_cont[1])
#' distr_discr <- truncate_distr(distr_discr, q_discr[2], q_discr[1])
#'
#' distr_cont
#' distr_discr
#'
#' autoplot(distr_cont)
#' autoplot(distr_discr)
#'
#'
#' distr_cont <- fit_pdqr(x = vec_cont, fn = "norm")
#' distr_discr <- fit_pdqr(x = vec_dist, fn = "rndbinom")
#'
#' distr_cont
#' distr_discr
#'
#' autoplot(distr_cont)
#' autoplot(distr_discr)
NULL

#' @export
distrcont <- function(x) {
  UseMethod("distrcont")
}
#' @export
distrcont.default <- function(x) {
  generic_default()
}
#' @export
distrcont.numeric <- function(x) {
  x <- x[!is_nonnum(x)]
  if (length(x) < 500) {
    x <- rep(x, ceiling(500 / length(x)))
  }

  res <- eval_parse(mgalda::info_distribucion)

  res <- res %>%
    filter(type == "continuous")

  res$res <- suppressall(map(res$fn_opt, .f = ~ do_try(.x(x))))

  res <- res[!map_lgl(res$res, ~ is_empty(.x$par)), ]

  res <- res[map_lgl(res$res, ~ length(.x$par) > 0), ]

  res$res <-
    map2(res$dist_info, res$res, function(x, y) {
      y$valid_x <- x$valid_x
      y
    })

  res$res <- map(res$res, ~ distr_metrics(x = x, par = .x))

  res <- res[!map_lgl(res$res, is_try), ]

  res <- res[!map_lgl(res$res, is_empty), ]

  if (nrow(res) == 0) {
    cat_stop("no se detecta modelo, tratar con metodo custom")
  }

  if (nrow(res) > 1) {
    scores_data <-
      purrr::map_dfr(res$res, ~ .x$metrics, .id = "dist")

    scores_data <- scores_data[!scores_data$random_error, ]
    scores_data$random_error <- NULL

    scores_data <- scores_data[scores_data$density, ]
    scores_data$density <- NULL

    scf <- apply(scores_data[, 2:10], 1, function(w) {
      sum(is_nonnum(w))
    })

    scores_data <-
      scores_data[scf == 0, ]

    if (nrow(scores_data) == 0) {
      cat_stop("no se detecta modelo, tratar con metodo custom")
    }
    mm <-
      apply(scores_data[, 3:10], 2, function(w) {
        seq_along(w) %in% outlier_locs(w) & w > mean(w)
      })

    scores_data <-
      scores_data[rowSums(mm) == 0 | rowSums(mm) < max(rowSums(mm)), ]

    mm <-
      apply(scores_data[, 3:10], 2, function(w) {
        nn <- (sum(w) - max(w)) / (length(w) - 1)
        w <- w - min(w)
        w / nn > 3
      })

    mm <- mm[, apply(mm, 2, sum, na.rm = T) <= ceiling(nrow(mm) / 2)]

    scores_data <- scores_data[rowSums(mm) == 0, ]

    max_row <-
      select(scores_data, !c(dist)) %>%
      mutate_all(~ rescalar(-1 * .x)) %>%
      rowSums() %>%
      which.max()


    res <- res$res[[scores_data[max_row, ]$dist]]
  } else {
    res <- fitted[[1]]
  }

  res[["data"]] <- x
  class(res) <- c(class(res), "distrobj")
  res
}
#' @export
print.distrcont <- function(x) {
  fnname <- x$call_info$model
  par <- x$par
  if (is.list(par)) {
    par <- par[[1]]
  }
  if (is_call(par) | is_expr(par)) {
    par <- eval(par)
  }

  par <- vapply(par, round_sign, numeric(1))
  cat_title_head(paste("funcion de distribucion ", fnname))
  cat_subtitle2("  Parametros")
  cli::cli_bullets(purrr::set_names(
    x = paste0(names(par), " : ", par),
    nm = rep("*", length(par))
  ))
  cat_endline()
}
#' @export
autoplot.distrcont <- function(object) {
  check_inherits(object, "distrcont")

  densfns <- as_dens(dist = object)

  if (object$type == "discrete") {
    bins <- bins_break(object$data$data)

    data <-
      cbind(data = object$data$data, grp = rep(1, length(object$data$data)))
  } else {
    bins <- bins_break(object$data)

    data <-
      cbind(data = object$data, grp = rep(1, length(object$data)))
  }

  data <- data.frame(data)

  colors <-
    c(
      "Histogram" = "sienna1",
      "Densidad real" = "tomato3",
      "black"
    )

  names(colors)[3] <- object$call_info$model

  ggplot(data, aes(x = data)) +
    ggplot2::geom_histogram(
      aes(y = ..density.., color = "histogram"),
      fill = "gray",
      alpha = 0.5,
      position = "identity",
      bins = bins
    ) +
    ggplot2::geom_density(
      alpha = .2,
      aes(colour = "densidad real"),
      size = 1
    ) +
    ggplot2::geom_function(
      fun = densfns,
      linetype = "dashed",
      aes(colour = object$call_info$model),
      show.legend = T,
      size = 1
    ) +
    gr_current_theme() +
    scale_color_manual(values = colors)
}

# distrdiscr --

#' distrdiscr
#'

#' @export
distrdiscr <- function(x) {
  UseMethod("distrdiscr")
}
#' @export
distrdiscr.default <- function(x) {
  generic_default()
}
#' @export
distrdiscr.numeric <- function(x) {
  x <- x[!is_nonnum(x)]

  if (length(x) < 500) {
    x <- rep(x, ceiling(500 / length(x)))
  }

  res <- eval_parse(mgalda::info_distribucion)

  res <- res %>%
    filter(type == "discrete")

  res$res <- suppressall(map(res$fn_opt, .f = ~ do_try(.x(x))))

  res <- res[!map_lgl(res$res, ~ is_empty(.x$par)), ]

  res <- res[map_lgl(res$res, ~ length(.x$par) > 0), ]

  res$res <-
    map2(res$dist_info, res$res, function(x, y) {
      y$valid_x <- x$valid_x
      y
    })

  res$res <- map(res$res, ~ distr_metrics(x = x, par = .x))

  res <- res[!map_lgl(res$res, is_try), ]

  res <- res[!map_lgl(res$res, is_empty), ]

  if (nrow(res) == 0) {
    cat_stop("no se detecta modelo, tratar con metodo custom")
  }

  if (nrow(res) > 1) {
    scores_data <-
      purrr::map_dfr(res$res, ~ .x$metrics, .id = "dist")

    scores_data <- scores_data[!scores_data$random_error, ]
    scores_data$random_error <- NULL

    scores_data <- scores_data[scores_data$density, ]
    scores_data$density <- NULL
    scores_data <- scores_data[, 1:4]

    scf <- apply(scores_data[, 2:4], 1, function(w) {
      sum(is_nonnum(w))
    })

    scores_data <-
      scores_data[scf == 0, ]

    if (nrow(scores_data) == 0) {
      cat_stop("no se detecta modelo, tratar con metodo custom")
    }
    mm <-
      apply(scores_data[, 3:4], 2, function(w) {
        seq_along(w) %in% outlier_locs(w) & w > mean(w)
      })

    if (sum(mm) > 0) {
      scores_data <- scores_data[rowSums(mm) < max(rowSums(mm)), ]
    }
    mm <-
      apply(scores_data[, 3:4], 2, function(w) {
        nn <- (sum(w) - max(w)) / (length(w) - 1)
        w <- w - min(w)
        w / nn > 3
      })

    mm <- mm[, apply(mm, 2, sum, na.rm = T) <= ceiling(nrow(mm) / 2)]

    scores_data <- scores_data[rowSums(mm) == 0, ]

    max_row <-
      dplyr::select(scores_data, !c(dist)) %>%
      mutate_all(~ rescalar(-1 * .x)) %>%
      rowSums() %>%
      which.max()


    res <- res$res[[scores_data[max_row, ]$dist]]
  } else {
    res <- fitted[[1]]
  }

  res[["data"]] <- x
  class(res) <- c(class(res), "distrobj")
  res
}
#' @export
distrdiscr.integer <- function(x) {
  x <- as_dbl(x)
  distrdiscr(x)
}
#' @export
print.distrdiscr <- function(x) {
  fnname <- x$call_info$model
  par <- x$par
  if (is.list(par)) {
    par <- par[[1]]
  }
  if (is_call(par) | is_expr(par)) {
    par <- eval(par)
  }
  par <- sapply(par, round_sign)
  cat_title_head(paste("funcion de distribucion ", fnname))
  cat_subtitle2("  Parametros")
  cli::cli_bullets(purrr::set_names(
    x = paste0(names(par), " : ", par),
    nm = rep("*", length(par))
  ))
  cat_endline()
}
#' @export
autoplot.distrdiscr <- function(object) {
  check_inherits(object, "distrdiscr")

  densfns <- as_dens(dist = object)

  bins <- bins_break(object$data)

  data <-
    cbind(data = object$data, grp = rep(1, length(object$data)))

  data <- data.frame(data)

  colors <-
    c(
      "Histogram" = "sienna1",
      "Densidad real" = "tomato3",
      "black"
    )

  names(colors)[3] <- object$call_info$model

  ggplot(data, aes(x = data)) +
    ggplot2::geom_histogram(
      aes(y = ..density.., color = "histogram"),
      fill = "gray",
      alpha = 0.5,
      position = "identity",
      bins = bins
    ) +
    ggplot2::geom_density(
      alpha = .2,
      aes(colour = "densidad real"),
      size = 1
    ) +
    ggplot2::geom_function(
      fun = densfns,
      linetype = "dashed",
      aes(colour = object$call_info$model),
      show.legend = T,
      size = 1
    ) +
    gr_current_theme() +
    scale_color_manual(values = colors)
}

# fit_custom_distr --

#' fit_custom_distr
#'

#' @export
fit_custom_distr <- function(x, ...) {
  UseMethod("fit_custom_distr")
}



#' @export
fit_custom_distr.default <- function(x, ...) {
  generic_default()
}
#' @export

fit_custom_distr.numeric <-
  function(x,
           n = 512,
           n_sample = getOption("mgalda.n_samples", 1500) * 4) {
    assert_notempty(x = x, severity = "stop")
    assert_dblint(x = x, severity = "stop")

    type <- ifelse(is_discrete_distribution(x), "discrete", "continuous")

    outs <- outlier_locs(x = x, cutoff = .99)

    if (length(outs) > 0) {
      x <- x[-outs]
    }

    if (length(x) < n_sample) {
      rep <- ceiling(n_sample / length(x) / 8)

      x <- sample(.un(kfolds_vec(
        x = x,
        times = 10,
        rep = rep
      )), n_sample)
    }

    if (any(is_heavytail(x))) {
      res <- switch(type,
                    discrete = denstable_discrete_heavytail(x),
                    continuous = denstable_contin_heavytail(x, n = n)
      )
    } else {
      res <- switch(type,
                    discrete = denstable_discrete(x),
                    continuous = denstable_contin(x, n = n)
      )
    }

    if (type == "continuous") {
      support <- range(res[["x"]])
    } else {
      support <- range(x)
    }


    qfn <- build_custom_quantile(res, type)
    dfn <- build_custom_density(res, type)
    pfn <- build_custom_probability(res, type)

    out <- list(
      dist_table = res,
      quantile = qfn,
      density = dfn,
      probability = pfn
    )

    attr(out, "support") <- support
    attr(out, "type") <- type
    class(out) <- "fit_custom_distr"
    if (type == "continuous") {
      out[["high_dens_vect"]] <- high_dens_vect(
        dist = out,
        size_range = seq(50, 90, 10),
        n = 512
      )
      out$high_dens_vect$distributions <- NULL
    }
    out[["data"]] <- x
    class(out) <- c(class(out), "distrobj")
    out
  }

#' @keywords internal#'

#' @export
print.fit_custom_distr <- function(x) {
  cat_title_head("Custom distribution methods")

  num <- mgalda::num_format(x = range(x$dist_table$x))

  num <- paste0("[", num[1], " , ", num[2], "]")

  cat_subtitle2("Distribution Domain")

  cli::cli_bullets(text = c("*" = num))

  if (attr(x, "type") == "discrete") {
    cat_subtitle2("Distribution table")
    tb <- x$dist_table
    tb$cumprob <- mgalda::perc_format(tb$cumprob)
    tb$prob <- mgalda::perc_format(tb$prob)
    print(as_tbl(tb))
  }

  if (attr(x, "type") == "continuous") {
    rhd <- paste0(
      mgalda::perc_format(x = x$high_dens_vect$probability / 100),
      " [",
      mgalda::num_format(x = x$high_dens_vect$lower),
      " , ",
      mgalda::num_format(x = x$high_dens_vect$upper),
      "]"
    )
    cat_subtitle2(text = "highest density interval", sub_text = "(per decil)")
    cli::cli_bullets(text = purrr::set_names(rhd, rep("*", length(rhd))))
  }
  cat_endline()
}

#' @export
autoplot.fit_custom_distr <- function(object) {
  check_inherits(object, "fit_custom_distr")

  densfns <- as_dens(dist = object)

  bins <- bins_break(object$data)

  data <-
    cbind(data = object$data, grp = rep(1, length(object$data)))

  data <- data.frame(data)

  colors <-
    c(
      "Histogram" = "sienna1",
      "Densidad real" = "tomato3",
      "Custom distribution" = "black"
    )

  ggplot(data, aes(x = data)) +
    ggplot2::geom_histogram(
      aes(y = ..density.., color = "histogram"),
      fill = "gray",
      alpha = 0.5,
      position = "identity",
      bins = bins
    ) +
    ggplot2::geom_density(
      alpha = .2,
      aes(colour = "densidad real"),
      size = 1
    ) +
    ggplot2::geom_function(
      fun = densfns,
      linetype = "dashed",
      aes(colour = "Custom distribution"),
      show.legend = T,
      size = 1
    ) +
    gr_current_theme() +
    scale_color_manual(values = colors)
}

#' @keywords internal
denstable_discrete <- function(x) {
  x <- x[!is.na(x)]
  vals <- sort(unique(x))
  x_val_id <- match(x, vals)
  prob <- tabulate(x_val_id) / length(x)
  cumprob <- cumsum(prob)
  data.frame(
    x = vals,
    prob = prob,
    cumprob = cumprob
  )
}
#' @keywords internal
denstable_discrete_heavytail <- function(x) {
  fct2num <- function(x) {
    as_num(as_chr(x))
  }
  bool <- is_heavytail(x)
  inner_int <- as_num(quantile(unique(x), probs = c(.05, .95)))

  if (bool[1]) {
    nl <- abs(ncase(x[x <= inner_int[1]]))
    brks <-
      unique(round(quantile(x[x <= inner_int[1]], c(
        0, seq_len(nl)
      ) / nl)))
    lab <- brks[-length(brks)]

    xl <-
      fct2num(cut(
        x = x[x <= inner_int[1]],
        breaks = brks,
        labels = lab,
        include.lowest = T
      ))
    x <- c(xl, x[x > inner_int[1]])
  } else {
    inner_int[1] <- -Inf
  }

  if (bool[2]) {
    nu <- ncase(x[x >= inner_int[2]])
    brks <-
      unique(round(quantile(x[x >= inner_int[2]], c(
        0, seq_len(nu)
      ) / nu)))

    xu <-
      fct2num(cut(
        x = x[x >= inner_int[2]],
        breaks = brks,
        labels = brks[-1],
        include.lowest = T
      ))
    x <- c(xu, x[x < inner_int[2]])
  } else {
    inner_int[2] <- Inf
  }

  denstable_discrete(x = sort(x))
}
#' @keywords internal
denstable_contin <- function(x, n = 1001) {
  res <- density_piecelin(x, n = n)

  res[["cumprob"]] <-
    cum_composite(
      rule = trapezoid,
      x = res[["x"]],
      y = res[["y"]]
    )

  res
}
#' @keywords internal
denstable_contin_heavytail <- function(x, n) {
  bool <- is_heavytail(x)
  n <- nbins_dens(x, n = n * 10, type = "continuous")
  inner_int <- as_num(quantile(unique(x), probs = c(.05, .95)))
  res <- density_piecelin(x, n = n)
  res[["cumprob"]] <-
    cum_composite(
      rule = trapezoid,
      x = res[["x"]],
      y = res[["y"]]
    )

  ind_x <- interval_match(y = seq_along(res$x), x = res$x, inner_int)

  if (bool[1]) {
    lres <- res[c(1, ind_x[1]), ]
    lfn <- function(par) {
      abs(lres$cumprob[2] -
            trapez_integral(
              x = seq(lres$x[1], lres$x[2], length.out = 50),
              y = rep(par, 50)
            ))
    }

    lres <-
      data.frame(
        x = seq(lres$x[1], lres$x[2], length.out = 50),
        y = optimize(lfn, interval = c(0, 1))$objective
      )
  }

  if (bool[2]) {
    ures <- res[c(ind_x[2], nrow(res)), ]
    ufn <- function(par) {
      abs(diff(ures$cumprob) -
            trapez_integral(
              x = seq(ures$x[1], ures$x[2], length.out = 50),
              y = rep(par, 50)
            ))
    }
    ures <- data.frame(
      x = seq(ures$x[1], ures$x[2], length.out = 50),
      y = optimize(ufn, interval = c(0, 1))$objective
    )
  }

  res <- res[(ind_x[1] + 1):(ind_x[2] - 1), c("x", "y")]


  if (bool[1]) {
    res <- rbind(lres, res)
    res <- res[is_sorted(x = res$x), ]
  }
  if (bool[2]) {
    res <- rbind(res, ures)
    res <- res[is_sorted(x = res$x), ]
  }

  tot_integral <- composite(
    f = function(w) {
      ind <- findInterval(x = w, vec = res$x)
      res$y[ind]
    },
    lower = min(res$x),
    upper = max(res$x),
    n = length(res$x),
    rule = trapezoid
  )

  res$y <- res$y / tot_integral


  res[["cumprob"]] <-
    cum_composite(
      rule = trapezoid,
      x = res[["x"]],
      y = res[["y"]]
    )


  res
}


#' @keywords internal
build_custom_quantile <- function(dist, type = NULL) {
  if (is_empty(type)) {
    type <- attr(dist, "type")
  }
  qcustom_discrete <- function(dist) {
    type <- "discrete"
    support <- range(dist[["x"]])

    nenv <- env_curr()

    nenv$dist <- dist
    nenv$type <- type
    nenv$support <- support

    fn <- function(p) {
      res <- numeric(length(p))

      p_not_na <- !is_nonnum(p)
      res[!p_not_na] <- NA
      p <- p[p_not_na]

      is_prob <- (p >= 0) & (p <= 1)
      p_prob <- p[is_prob]
      p_ind <-
        findInterval(p_prob, dist[["cumprob"]], left.open = TRUE) + 1

      res[p_not_na][is_prob] <- dist[["x"]][p_ind]
      res[p_not_na][!is_prob] <- NaN

      res
    }

    env_fn(fn) <- nenv
    fn
  }

  qcustom_continuous <- function(dist) {
    type <- "continuous"
    support <- range(dist[["x"]])
    nenv <- env_curr()

    is_near <- function(x, y, tol = 10^(-8)) {
      abs(x - y) < tol
    }

    nenv$dist <- dist
    nenv$type <- type
    nenv$support <- support
    nenv$compute_piecelin_density_coeffs <-
      compute_piecelin_density_coeffs
    nenv$is_near <- is_near

    fn <- function(p) {
      if (!is.numeric(p)) {
        cat_stop("p must be 'numeric'.")
      }

      find_quant <-
        function(p,
                 cdf_start,
                 x_l,
                 slope,
                 intercept) {
          res <- numeric(length(p))

          is_quad <- !is_near(slope, 0)
          is_lin <- !(is_quad | is_near(intercept, 0))
          is_const <- !(is_quad | is_lin)

          a <- 0.5 * slope[is_quad]
          b <- 2 * a * x_l[is_quad] + intercept[is_quad]
          c <- cdf_start[is_quad] - p[is_quad]
          discr <- pmax(b * b - 4 * a * c, 0)
          res[is_quad] <-
            (-b + sqrt(discr)) / (2 * a) + x_l[is_quad]

          res[is_lin] <- x_l[is_lin] +
            (p[is_lin] - cdf_start[is_lin]) / intercept[is_lin]

          res[is_const] <- x_l[is_const]

          res
        }

      res <- numeric(length(p))

      p_not_na <- !is.na(p)
      res[!p_not_na] <- NA
      p <- p[p_not_na]

      x <- dist[["x"]]
      # y <- dist[["y"]]
      p_grid <- dist[["cumprob"]]

      is_inside <- (p >= 0) & (p <= 1)
      p_prob <- p[is_inside]

      p_ind <-
        findInterval(p_prob,
                     p_grid,
                     left.open = TRUE,
                     all.inside = TRUE
        )

      coeffs <- compute_piecelin_density_coeffs(dist, p_ind)

      res[p_not_na][is_inside] <- find_quant(
        p = p_prob,
        cdf_start = p_grid[p_ind],
        x_l = x[p_ind],
        slope = coeffs[["slope"]],
        intercept = coeffs[["intercept"]]
      )

      res[p_not_na][(p < 0) | (p > 1)] <- NaN

      res
    }
    env_fn(fn) <- nenv
    fn
  }


  impl_funs <-
    list(discrete = qcustom_discrete, continuous = qcustom_continuous)

  res <- switch(type,
                discrete = impl_funs[["discrete"]](dist),
                continuous = impl_funs[["continuous"]](dist)
  )
  res
}

#' @keywords internal
build_custom_density <- function(dist, type = NULL) {
  if (is_empty(type)) {
    type <- attr(dist, "type")
  }
  dcustom_continuous <- function(dist) {
    type <- "continuous"
    support <- range(dist[["x"]])

    nenv <- env_curr()

    nenv$dist <- dist
    nenv$type <- type
    nenv$support <- support

    fn <- function(x) {
      if (!is.numeric(x)) {
        cat_stop("`x` must be 'numeric'.")
      }

      res <- numeric(length(x))

      x_not_na <- !is.na(x)
      res[!x_not_na] <- NA
      x <- x[x_not_na]

      x_vec <- dist[["x"]]
      y_vec <- dist[["y"]]

      is_inside <- (x >= support[1]) & (x <= support[2])

      x_ind <-
        findInterval(x[is_inside], x_vec, rightmost.closed = TRUE)

      slopes <- (y_vec[x_ind + 1] - y_vec[x_ind]) /
        (x_vec[x_ind + 1] - x_vec[x_ind])

      res[x_not_na][is_inside] <-
        slopes * (x[is_inside] - x_vec[x_ind]) +
        y_vec[x_ind]

      res
    }
    env_fn(fn) <- nenv
    fn
  }
  dcustom_discrete <- function(dist) {
    type <- "discrete"
    support <- range(dist[["x"]])

    nenv <- env_curr()

    nenv$dist <- dist
    nenv$type <- type
    nenv$support <- support


    fn <- function(x) {
      if (!is.numeric(x)) {
        cat_stop("`x` must be 'numeric'.")
      }

      res <- numeric(length(x))

      x_not_na <- !is.na(x)
      res[!x_not_na] <- NA
      x <- x[x_not_na]

      x_ind <-
        match(round(x, digits = 10), dist[["x"]], nomatch = NA)
      x_ind_matched <- !is.na(x_ind)
      res[x_not_na][x_ind_matched] <-
        dist[["prob"]][x_ind[x_ind_matched]]

      res
    }
    env_fn(fn) <- nenv
    fn
  }

  impl_funs <-
    list(discrete = dcustom_discrete, continuous = dcustom_continuous)

  res <- switch(type,
                discrete = impl_funs[["discrete"]](dist),
                continuous = impl_funs[["continuous"]](dist)
  )
  res
}

#' @keywords internal
build_custom_probability <- function(dist, type = NULL) {
  if (is_empty(type)) {
    type <- attr(dist, "type")
  }
  pcustom_discrete <- function(dist) {
    type <- "discrete"
    support <- range(dist[["x"]])
    nenv <- env_curr()

    nenv$dist <- dist
    nenv$type <- type
    nenv$support <- support

    fn <- function(q) {
      if (!is.numeric(q)) {
        cat_stop("`q` must be 'numeric'.")
      }

      res <- numeric(length(q))

      q_not_na <- !is.na(q)
      res[!q_not_na] <- NA
      q <- q[q_not_na]

      q_ind <- findInterval(round(q, digits = 10), dist[["x"]])
      res[q_not_na][q_ind != 0] <- dist[["cumprob"]][q_ind]

      res
    }
    env_fn(fn) <- nenv
    fn
  }

  pcustom_continuous <- function(dist) {
    type <- "continuous"
    support <- range(dist[["x"]])

    nenv <- env_curr()

    nenv$dist <- dist
    nenv$type <- type
    nenv$support <- support

    fn <- function(q) {
      # Not using `assert_type()` for speed reasons
      if (!is.numeric(q)) {
        cat_stop("`q` must be numeric.")
      }

      res <- numeric(length(q))

      q_not_na <- !is.na(q)
      res[!q_not_na] <- NA
      q <- q[q_not_na]

      x <- dist[["x"]]
      # y <- dist[["y"]]
      p_grid <- dist[["cumprob"]]

      q_ind <- findInterval(q, x)

      is_q_small <- q_ind == 0
      res[q_not_na][is_q_small] <- 0
      is_q_large <- q_ind == length(x)
      res[q_not_na][is_q_large] <- 1

      is_q_between <- !(is_q_small | is_q_large)
      q_ind_bet <- q_ind[is_q_between]
      q_bet <- q[is_q_between]
      x_bet <- x[q_ind_bet]

      coeffs <- compute_piecelin_density_coeffs(dist, q_ind_bet)

      res_between <- p_grid[q_ind_bet] +
        0.5 * coeffs[["slope"]] * (q_bet + x_bet) * (q_bet - x_bet) +
        coeffs[["intercept"]] * (q_bet - x_bet)
      res[q_not_na][is_q_between] <-
        pmin(pmax(res_between, 0), 1)

      res
    }
    env_fn(fn) <- nenv
    fn
  }

  impl_funs <-
    list(discrete = pcustom_discrete, continuous = pcustom_continuous)

  res <- switch(type,
                discrete = impl_funs[["discrete"]](dist),
                continuous = impl_funs[["continuous"]](dist)
  )
  res
}


# density_integrate

#' density_integrate
#'

#' @export
density_integrate <- function(x, ...) {
  UseMethod("density_integrate")
}

#' @export
density_integrate.default <- function(x, ...) {
  generic_default()
}
#' @export
density_integrate.numeric <- function(x, ...) {
  check_inherits(x, c("numeric", "integer"))

  type <- ifelse(is_discrete_distribution(x), "discrete", "continuous")

  if (is.integer(x)) {
    x <- as_dbl(x)
  }

  dist <- tabulate_density_integrate(x, type = type, ...)
  dist <- as_l(dist)

  class(dist) <- "density_integrate"
  attr(dist, "type") <- type
  class(dist) <- c(class(dist), "distrobj")
  dist
}
#' @export
density_integrate.integer <- function(x, ...) {
  check_inherits(x, c("numeric", "integer"))

  type <- ifelse(is_discrete_distribution(x), "discrete", "continuous")

  if (is.integer(x)) {
    x <- as_dbl(x)
  }

  dist <- tabulate_density_integrate(x, type = type, ...)
  dist <- as_l(dist)

  class(dist) <- "density_integrate"
  attr(dist, "type") <- type
  class(dist) <- c(class(dist), "distrobj")
  dist
}
#' @export
density_integrate.density <- function(x, type, ...) {
  check_inherits(x, "density")
  dist <- x
  x <- as_rand(dist = x, type = "continuous")(1000)

  if (type == "discrete") {
    x <- round(x)
  }

  density_integrate(x)
}
#' @export
as_df.density_integrate <- function(x) {
  as_df(nullclass(x))
}
#' @export
as_tibble.density_integrate <- function(x) {
  as_tbl(nullclass(x))
}
#' @keywords internal
tabulate_density_integrate <- function(x, type, ...) {
  if (type == "discrete") {
    dens <- denstable_discrete(x)
    res <- data.frame(
      x = dens[["x"]],
      y = dens[["prob"]],
      prob = dens[["prob"]],
      cumprob = dens[["cumprob"]]
    )
  } else {
    dens <- density(x, ...)
    x_dens <- dens[["x"]]
    y_dens <- dens[["y"]]

    tot_integral <- composite(
      f = function(w) {
        ind <- findInterval(x = w, vec = x_dens)
        y_dens[ind]
      },
      lower = min(x_dens),
      upper = max(x_dens),
      n = length(x_dens),
      rule = trapezoid
    )

    res <- data.frame(x = x_dens, y = y_dens / tot_integral)

    res[["cumprob"]] <-
      cum_composite(
        rule = trapezoid,
        x = res[["x"]],
        y = res[["y"]]
      )

    res[["prob"]] <- res[["y"]]
  }

  attr(res, "type") <- type
  res
}
#' @export
print.density_integrate <- function(x) {
  rqnt <- function(x) {
    pl <- c(.25, .20, .15, .1, .05)

    pu <- 1 - pl
    list(
      probability = pu - pl,
      lower = quantile_dist(x, pl),
      upper = quantile_dist(x, pu)
    )
  }
  cat_title_head("Custom distribution methods: Density Table")
  num <- mgalda::num_format(x = range(x$x))
  num <- paste0("[", num[1], " , ", num[2], "]")
  cat_subtitle2("Distribution Domain")
  cli::cli_bullets(text = c(`*` = num))
  if (attr(x, "type") == "discrete") {
    cat_subtitle2("Distribution table")
    tb <- x
    tb$cumprob <- mgalda::perc_format(tb$cumprob)
    tb$prob <- mgalda::perc_format(tb$prob)
    print(as_tbl(tb))
  }
  if (attr(x, "type") == "continuous") {
    rhd <- rqnt(x)
    rhd <- paste0(
      mgalda::perc_format(x = rhd$probability),
      " [",
      mgalda::num_format(x = rhd$lower),
      " , ",
      mgalda::num_format(x = rhd$upper),
      "]"
    )
    cat_subtitle2(text = "highest density interval", sub_text = "(per decil)")
    cli::cli_bullets(text = purrr::set_names(rhd, rep(
      "*",
      length(rhd)
    )))
  }
  cat_endline()
}
#' @export
as_density_integrate <- function(x) {
  impute_data <- function(data) {
    impute_dis <- function(data) {
      # Rounding is needed due to some usage of `form_retype()`. This also aligns
      # with how d-functions of type "discrete" work (see `new_d_dis()` or
      # `new_p_dis()`).
      impute_prob <- function(prob) {
        tot_prob <- sum(prob)

        if (is_near(tot_prob, 1)) {
          prob
        } else {
          prob / tot_prob
        }
      }

      impute_vec <- function(vec, new_vec) {
        if (is.null(vec) || !all(is_near(vec, new_vec))) {
          new_vec
        } else {
          vec
        }
      }


      x <- round(data[["x"]], digits = 10)

      if (anyDuplicated(x) != 0) {
        # `data[["x"]]` is already sorted, so `vals` is automatically sorted too,
        # i.e. no need for `sort()`.
        vals <- unique(x)
        x_val_id <- match(x, vals)

        # `as_vct()` is used to remove extra attributes
        prob <- as_vct(tapply(data[["prob"]], x_val_id, sum))
        prob <- prob / sum(prob)

        data.frame(
          x = vals,
          prob = prob,
          cumprob = cumsum(prob)
        )
      } else {
        res <- data.frame(
          x = x,
          prob = impute_prob(data[["prob"]])
        )
        res[["cumprob"]] <-
          impute_vec(vec = data[["cumprob"]], new_vec = cumsum(res[["prob"]]))

        res
      }
    }
    impute_con <- function(data) {
      impute_prob <- function(prob) {
        tot_prob <- sum(prob)

        if (is_near(tot_prob, 1)) {
          prob
        } else {
          prob / tot_prob
        }
      }

      impute_y <- function(y, x) {
        tot_prob <- trapez_integral(x, y)

        if (is_near(tot_prob, 1)) {
          y
        } else {
          y / tot_prob
        }
      }

      impute_vec <- function(vec, new_vec) {
        if (is.null(vec) || !all(is_near(vec, new_vec))) {
          new_vec
        } else {
          vec
        }
      }

      res <- data.frame(
        x = data[["x"]],
        y = impute_y(data[["y"]], data[["x"]])
      )
      res[["cumprob"]] <- impute_vec(
        vec = data[["cumprob"]],
        new_vec = trapez_part_integral(res[["x"]], res[["y"]])
      )

      res
    }
    trapez_part_integral <- function(x, y) {
      n <- length(y)
      # `x` is assumed to be sorted increasingly (as after the `density()` call)
      c(0, cumsum((x[-1] - x[-n]) * (y[-1] + y[-n])) / 2)
    }

    type <-
      ifelse(is_discrete_distribution(data[["x"]]), "discrete", "continuous")

    if (is.unsorted(data[["x"]])) {
      data <- data[order(data[["x"]]), ]
    }

    if (type == "discrete") {
      impute_dis(data)
    } else if (type == "continuous") {
      impute_con(data)
    } else {
      cat_stop("Wrong `type`.")
    }
  }

  if (any(names(x) %in% c("x"))) {
    type <- ifelse(is_discrete_distribution(x$x), "discrete", "continuous")
    if (any(names(x) %in% c("y", "prob"))) {
      if ("y" %in% names(x) & !"prob" %in% names(x)) {
        x$prob <- x$y
      } else if (!"y" %in% names(x) & "prob" %in% names(x)) {
        x$y <- x$prob
      }

      x <- impute_data(x)
      if (!"y" %in% names(x)) {
        x[["y"]] <- x[["prob"]]
      } else if (!"prob" %in% names(x)) {
        x[["prob"]] <- x[["y"]]
      }
    }
  }

  class(x) <- c("density_integrate", "distrobj")
  attr(x, "type") <- type

  x
}
#' @export
autoplot.density_integrate <-
  function(object, n = getOption("mgalda.n_samples", 1500)) {
    check_inherits(object, "density_integrate")

    densfns <- as_dens(dist = object)
    randfns <- as_rand(dist = object)
    x <- randfns(n = n)


    bins <- bins_break(x)

    data <-
      cbind(data = x, grp = rep(1, length(x)))

    data <- data.frame(data)

    colors <-
      c(
        "Histogram" = "sienna1",
        "Densidad real" = "tomato3",
        "Custom distribution" = "black"
      )
    ggplot(data, aes(x = data)) +
      ggplot2::geom_histogram(
        aes(y = ..density.., color = "histogram"),
        fill = "gray",
        alpha = 0.5,
        position = "identity",
        bins = bins
      ) +
      ggplot2::geom_density(
        alpha = .2,
        aes(colour = "densidad real"),
        size = 1
      ) +
      ggplot2::geom_function(
        fun = densfns,
        linetype = "dashed",
        aes(colour = "Custom distribution"),
        show.legend = T,
        size = 1
      ) +
      gr_current_theme() +
      scale_color_manual(values = colors)
  }


# truncate_distr --

#' truncate_distr
#'

#' @export
truncate_distr <- function(dist, ...) {
  UseMethod("truncate_distr")
}
#' @export
truncate_distr.default <- function(dist, ...) {
  generic_default()
}
#' @export
truncate_distr.distrcont <-
  function(dist, upper = NULL, lower = NULL) {
    truncate_distr_impl(dist, upper, lower)
  }
#' @export
truncate_distr.distrdiscr <-
  function(dist, upper = NULL, lower = NULL) {
    truncate_distr_impl(dist, upper, lower)
  }
#' @export
truncate_distr.fit_custom_distr <-
  function(dist, upper = NULL, lower = NULL) {
    truncate_distr_impl(dist, upper, lower)
  }
#' @export
truncate_distr.density_integrate <-
  function(dist, upper = NULL, lower = NULL) {
    truncate_distr_impl(dist, upper, lower)
  }
#' @keywords internal
truncate_distr_impl <-
  function(dist, upper = NULL, lower = NULL) {
    if (is_emptyna(upper)) {
      upper <- max_qfn(fn = dist)
    }
    if (is_emptyna(lower)) {
      lower <- min_qfn(fn = dist)
    }

    pfn <- as_prob(dist)
    dfn <- as_dens(dist)
    qfn <- as_quan(dist)
    cdf_upr <- pfn(upper)
    cdf_lwr <- pfn(lower)

    nenv <- env_curr()
    nenv$pfn <- pfn
    nenv$qfn <- qfn
    nenv$dfn <- dfn
    nenv$cdf_upr <- cdf_upr
    nenv$cdf_lwr <- cdf_lwr
    nenv$upper <- upper
    nenv$lower <- lower

    new_dfn <- function(x) {
      in_lim <- x >= lower & x <= upper
      out <- numeric(length(x))
      out[in_lim] <-
        dfn(x = x[in_lim]) / (cdf_upr - cdf_lwr)
      out
    }
    new_pfn <- function(q) {
      out <- numeric(length(q))
      q_lwr <- q < lower # out[q_lwr <- q < lower] <- 0
      out[q_upr <- q > upper] <- 1
      q_mid <- !(q_lwr | q_upr)
      out[q_mid] <-
        (pfn(q = q[q_mid]) - cdf_lwr) / (cdf_upr - cdf_lwr)
      out
    }

    new_qfn <- function(p) {
      qt <- qfn(cdf_lwr + p * (cdf_upr - cdf_lwr))
      pmin(pmax(lower, qt), upper)
    }
    env_fn(new_qfn) <- nenv
    env_fn(new_pfn) <- nenv
    env_fn(new_dfn) <- nenv
    out <- list(
      origin = dist,
      q = new_qfn,
      p = new_pfn,
      d = new_dfn
    )
    class(out) <- "truncate_distr"
    class(out) <- c(class(out), "distrobj")
    attr(out,"type") <- ifelse(is_discrete_distribution(new_qfn(seq(0,1,.1))),"discrete","continuous")
    out
  }
#' @export
autoplot.truncate_distr <-
  function(object, n = getOption("mgalda.n_samples", 1500)) {
    check_inherits(object, "density_integrate")

    densfns <- as_dens(dist = object)
    randfns <- as_rand(dist = object)
    x <- randfns(n = n)


    bins <- bins_break(x)

    data <-
      cbind(data = x, grp = rep(1, length(x)))

    data <- data.frame(data)

    colors <-
      c(
        "Histogram" = "sienna1",
        "Densidad real" = "tomato3",
        "Custom distribution" = "black"
      )
    ggplot(data, aes(x = data)) +
      ggplot2::geom_histogram(
        aes(y = ..density.., color = "histogram"),
        fill = "gray",
        alpha = 0.5,
        position = "identity",
        bins = bins
      ) +
      ggplot2::geom_density(
        alpha = .2,
        aes(colour = "densidad real"),
        size = 1
      ) +
      ggplot2::geom_function(
        fun = densfns,
        linetype = "dashed",
        aes(colour = "Custom distribution"),
        show.legend = T,
        size = 1
      ) +
      gr_current_theme() +
      scale_color_manual(values = colors)
  }
#' @export
print.truncate_distr <- function(x) {
  cat_title_head("Truncate Distr")
  print(x$origin)
}

# pdqr -

#' from pdqr
#'

#' @export
pdqr_distr <- function(f, ...) {
  params <- dots(...)

  f <- switch(f,
              binom = "rndbinom",
              geom = "rndgeom",
              nbinom = "rndnbinom",
              pois = "rndpois",
              f
  )
  data_d <-
    eval_parse(mgalda::info_distribucion) %>%
    filter(dist == f)

  if (nrow(data_d) == 0) {
    cat_stop("{ f } no existe en la base")
  }
  if (length(f) != 1 & !is.character(f)) {
    cat_stop("{ f } debe ser character y de largo 1")
  }

  args <- data_d$dist_info[[1]]$args

  if (!all(args %in% names(params))) {
    stop_args <- args[!args %in% names(params)]
    stop_args <- paste("'", stop_args, "'", sep = "")
    stop_args <- commas(stop_args)
    cat_stop("{ stop_args } is missing, with no default")
  }

  res <- data_d$dist_info[[1]]


  x <- do.call(paste0("r", res$dist), c(n = 1000, params))

  out <- structure(
    list(
      par = unlist(params),
      call_info = list(
        model = res$model,
        quantile = c(res$pkg, paste("q", res$dist, sep = "")),
        probab = c(res$pkg, paste("p", res$dist, sep = "")),
        density = c(res$pkg, paste("d", res$dist, sep = "")),
        random = c(res$pkg, paste("r", res$dist, sep = "")),
        dist = res$dist,
        support = body(res$valid_x)
      ),
      bound = res$bound,
      type = res$type,
      valid_x = res$valid_x,
      metrics = structure(
        list(),
        class = c(
          "tbl_df", "tbl",
          "data.frame"
        ),
        row.names = integer(0),
        .Names = character(0)
      ),
      data = x
    ),
    class = ifelse(res$type == "discrete", "distrdiscr", "distrcont"),
    type = res$type
  )

  out <- distr_metrics(x = x, par = out)
  out
}

# fit_pdqr -

#' fit_pdqr
#'

#' @export
fit_pdqr <- function(x, ...) {
  UseMethod("fit_pdqr")
}

#' @export
fit_pdqr.default <- function(x, ...) {
  generic_default()
}
#' @export
fit_pdqr.numeric <- function(x, fn) {
  res <- eval_parse(mgalda::info_distribucion)

  fn <- switch(fn,
               binom = "rndbinom",
               geom = "rndgeom",
               nbinom = "rndnbinom",
               pois = "rndpois",
               fn
  )
  res <- res %>%
    filter(dist == fn)

  res$res <- try(list(suppressall(res$fn_opt[[1]](x = x))))

  if (length(res$res[[1]]$par) == 0) {
    return(NULL)
  }
  res$res[[1]]$valid_x <-
    res$dist_info[[1]]$valid_x

  res <- distr_metrics(x = x, par = res$res[[1]])

  res[["data"]] <- x
  class(res) <- c(class(res), "distrobj")
  res
}
#' @export
fit_pdqr.integer <- function(x, fn) {
  x <- as_dbl(x)
  fit_pdqr(x, fn)
}
