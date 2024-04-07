#' @include util_env.R
NULL

#' distr_utils
#'
#' @name distr_utils
#' @rdname distr_utils
#'
#' @examples
#'
#' vec_cont<-xrandom(n = 1000,mean = 20,sd = 3)
#' vec_cont <- safe_vec(vec_cont)
#' vec_dist <- rbinom(1000,20:35,.5)
#'
#' distr_cont <- distrcont(x = vec_cont)
#' distr_discr<- distrdiscr(x = vec_dist)
#'
#' distfn_domain_num(dist = distr_cont)
#' distfn_domain_num(dist = distr_discr)
#'
#' is_distrobj(distr_cont)
#' is_distrobj(distr_discr)
#'
#'
#' high_dens_int(dist = distr_cont,size = .9)
#' high_dens_int(dist = distr_discr,size = .9)
#'
#' high_dens_vect(dist = distr_cont)
#' high_dens_vect(dist = distr_discr)
#'
#'
#' is_heavytail(x = distr_cont)
#' is_heavytail(x = distr_discr)
#'
#'
#'
#' @keywords internal
NULL

#' @export
distfn_domain_num <- function(dist,tol = getOption("mgalda.zero_prob",1e-5)) {
  tol <- ifelse(log10(tol)>0,0,ceiling(abs(log10(tol))))
  if (inherits(dist, "density_integrate")) {
    range(dist$x[round(dist$y, tol) > 0])
  } else {
    max <- max_qfn(dist)
    min <- min_qfn(dist)
    c(min, max)
  }
}
#' @keywords internal
max_qfn <- function(fn) {
  optimise(
    f = function(x) {
      round(as_quan(fn)(x), 5)
    },
    interval = c(.99, 1),
    maximum = T
  )$objective
}
#' @keywords internal
min_qfn <- function(fn) {
  optimise(as_quan(fn),
           interval = c(0, .01),
           maximum = F
  )$objective
}

#' @export
is_distrobj <- function(x) {
  inherits(x, "distrobj")
}

# high density interval -

#' @export
high_dens_int <- function(dist, size = 95, n = 2 * 512) {
  x <- random_dist(dist, n = 2000)
  high_dens_int_num(
    x = x,
    size = size,
    n = n
  )
}
#' @export
high_dens_vect <- function(dist, size_range = seq(60, 90, 10), n = 512 * 2) {
  m <-
    sapply(size_range, function(x) {
      high_dens_int(
        dist = dist,
        size = x,
        n = n
      )
    })

  lower <- m[1, ]
  upper <- m[2, ]

  x <- random_dist(dist, 2000)
  out <-
    list(
      lower = lower,
      upper = upper,
      probability = size_range,
      distributions = density_integrate(x)
    )

  class(out) <- "high_dens_vect"
  out
}
#' @export
high_dens_int_num <- function(x, size = 95, n = 2 * 512) {
  # type <- ifelse(is_discrete_distribution(x), "discrete", "continuous")
  dist <- density_integrate(x)

  dist_x <-
    vapply(
      seq(0.5 / n, 1 - 0.5 / n, length.out = n),
      as_quan(dist = dist),
      numeric(1L)
    )

  dist_x <- unique(dist_x[!is_nonnum(dist_x)])

  dist_y <-
    vapply(dist_x, as_dens(dist = dist), numeric(1L))

  res <-
    crossing_alpha_num(
      size = size,
      x = dist_x,
      y = dist_y,
      fn = as_dens(dist)
    )

  res$x_val <- matrix(res$x_val[res$lgl_vec, ], ncol = 2)

  res$cum_dens <- res$cum_dens[res$lgl_vec]

  res$x_val[which.max(res$cum_dens / apply(res$x_val, 1, diff)), ]
}
#' @export
high_dens_vect_num <- function(x,size_range = seq(60, 90, 10),n = 2 * 512) {
  # type <- ifelse(is_discrete_distribution(x), "discrete", "continuous")
  res <- lapply(size_range, function(r) {
    high_dens_int_num(
      x = x,
      size = r,
      n = n
    )
  })
  dist <- density_integrate(x)

  out <- list(
    lower = sapply(res, function(x) {
      x[1]
    }),
    upper = sapply(res, function(x) {
      x[2]
    }),
    probability = sapply(res, function(x) {
      integrate(
        as_dens(dist),
        lower = x[1],
        upper = x[2],
        subdivisions = 1001,
        stop.on.error = F
      )$value
    }),
    distributions = dist
  )

  class(out) <- "high_dens_vect_num"
  out
}
#' @export
autoplot.high_dens_vect_num <- function(object) {
  check_inherits(object, "high_dens_vect_num")

  densfns <- as_dens(dist = object$distributions)
  # randfns <- as_rand(dist = object$distributions)

  nn <- length(object$probability)

  data <-
    purrr::map_dfr(seq_len(nn), function(i) {
      w <- data.frame(x = c(object$lower[i], object$upper[i]))
      w$y <- densfns(x = w$x)
      w$prob <- object$probability[i]
      w$label <- perc_format(w$prob)
      w
    })

  data$label <-
    factor(data$label, unique(data$label[order(data$prob)]))

  ggplot(data, aes(x = data))

  object
  ggplot(data, aes(x = x, y = y, colour = label)) +
    ggplot2::geom_line() +
    ggplot2::geom_function(
      fun = densfns,
      linetype = "dashed",
      colour = "tomato3",
      size = 1
    ) +
    theme_mgg()
}
#' @export
autoplot.high_dens_vect <- function(object) {
  check_inherits(object, "high_dens_vect")

  densfns <- as_dens(dist = object$distributions)
  # randfns <- as_rand(dist = object$distributions)

  nn <- length(object$probability)

  data <-
    purrr::map_dfr(seq_len(nn), function(i) {
      w <- data.frame(x = c(object$lower[i], object$upper[i]))
      w$y <- densfns(x = w$x)
      w$prob <- object$probability[i]
      w$label <- perc_format(w$prob)
      w
    })

  data$label <-
    factor(data$label, unique(data$label[order(data$prob)]))

  ggplot(data, aes(x = data))

  object
  ggplot(data, aes(x = x, y = y, colour = label)) +
    ggplot2::geom_line() +
    ggplot2::geom_function(
      fun = densfns,
      linetype = "dashed",
      colour = "tomato3",
      size = 1
    ) +
    theme_mgg()
}
crossing_alpha_num <- function(size, x, y, fn = NULL) {
  alpha <- quantile(y, probs = 1 - size / 100, na.rm = TRUE)

  it <- seq_len(length(y) - 1)

  dd <- y - alpha

  dd <- dd[it + 1] * dd[it]

  index <- it[dd <= 0]

  w <- 0
  x_val <-
    unique(vapply(index, function(.x) {
      xres <-
        do_try(stats::approx(y[.x + c(0, 1)], x[.x + c(0, 1)], xout = alpha)$y, NA)
      if (!is.na(xres)) {
        w <<- xres
      } else {
        xres <- w
      }
      xres
    }, numeric(1L)))

  x_val <- gtools::combinations(
    n = length(x_val),
    r = 2,
    v = x_val
  )

  x_val <- cbind(apply(x_val, 1, min), apply(x_val, 1, max))

  cum_dens <- sapply(seq_len(nrow(x_val)), function(r) {
    integrate_safely2(
      .fn = fn,
      lower = x_val[r, 1],
      upper = x_val[r, 2],
      n = 10001)
  })

  cum_dens <- ifelse(!is_nonnum(cum_dens), cum_dens * 100, 0)

  lgl_vec <- round(cum_dens) == size

  if (!any(lgl_vec)) {
    lgl_vec <- abs(cum_dens - size) == min(abs(cum_dens - size))
  }
  list(
    x_val = x_val,
    cum_dens = cum_dens,
    lgl_vec = lgl_vec
  )
}

# is_heavytail -

#' @export
is_heavytail <- function(x, alpha = .05) {
  eps <- .Machine$double.eps
  p_seq <- seq.default(
    from = eps,
    to = 1 - eps,
    length.out = 10000
  )

  x_dist <- quantile_dist(dist = x, p = p_seq)
  x_norm <- qnorm(p = p_seq, mean = 0, sd = 1)
  x_norm <- x_norm - min(x_norm) + eps

  low_tail <- function(x) {
    if (is.infinite(min(x))) {
      return(0)
    }
    x_dens <- density(x)
    x_dens <- x_dens$x[x_dens$x < median(x)]
    x <- sort(x[x < median(x)])

    n_unique(findInterval(x = x_dens, vec = x)) / length(x_dens)
  }
  up_tail <- function(x) {
    if (is.infinite(max(x))) {
      return(0)
    }
    x_dens <- density(x)
    x_dens <- x_dens$x[x_dens$x > median(x)]
    x <- sort(x[x > median(x)])

    n_unique(findInterval(x = x_dens, vec = x)) / length(x_dens)
  }


  l_x <- low_tail(x_dist) / low_tail(x_norm)
  u_x <- up_tail(x_dist) / up_tail(x_norm)
  c(
    lower = is_true(l_x < alpha),
    upper = is_true(u_x < alpha)
  )
}

# discretizar distribuciones -
#' @keywords internal
ddscrt <- function(dist, x, ...) {
  dfn <- eval(parse(text = paste0("d", dist)))
  pfn <- eval(parse(text = paste0("p", dist)))
  in_lim <- x >= 0
  x <- round(x)
  if (is_true(dfn(x = 0, ...) > 0)) {
    cum_p <- pfn(q = 0, ..., lower.tail = FALSE)
  } else {
    cum_p <- 1
  }

  pu <- pl <- out <- numeric(length(x))

  pl[in_lim] <- pfn(q = x[in_lim] - .5, ..., lower.tail = FALSE)
  pu[in_lim] <- pfn(q = x[in_lim] + .5, ..., lower.tail = FALSE)
  out <- pl - pu

  out / cum_p
}
#' @keywords internal
pdscrt <- function(dist, q, ..., lower.tail = TRUE) {
  pfn <- eval(parse(text = paste0("p", dist)))
  out <- numeric(length(q))
  q_lwr <- q < 0
  q <- round(q)
  # out[q_upr <- q > upper] <- 1
  if (is_true(pfn(q = 0, ..., lower.tail = TRUE) > 0)) {
    cum_p <- pfn(q = 0, ..., lower.tail = TRUE)
  } else {
    cum_p <- 0
  }
  out[!q_lwr] <-
    (pfn(q = q[!q_lwr], ..., lower.tail = lower.tail) - cum_p) / (1 - cum_p)
  out
}
#' @keywords internal
qdscrt <- function(dist, p, ..., lower.tail = TRUE) {
  pfn <- eval(parse(text = paste0("p", dist)))
  qfn <- eval(parse(text = paste0("q", dist)))
  if (is_true(pfn(q = 0, ..., lower.tail = TRUE) > 0)) {
    cum_p <- pfn(q = 0, ..., lower.tail = TRUE)
  } else {
    cum_p <- 0
  }

  qt <- qfn(cum_p + p * (1 - cum_p), ..., lower.tail = lower.tail)
  pmax(0, round(qt))
}
#' @keywords internal
rdscrt <- function(n, dist, ...) {
  qfn <- eval(parse(text = paste0("qd", dist)))
  qfn(p = runif(n, 0, 1), ..., lower.tail = TRUE)
}

# coerce2density_integrate
#' @export
coerce2density_integrate <- function(x) {
  UseMethod("coerce2density_integrate")
}
#' @export
coerce2density_integrate.default <- function(x) {
  generic_default()
}
#' @export
coerce2density_integrate.distrobj <- function(x) {
  x <- quantile_dist(dist = x, p = seq(0, 1, .005))
  density_integrate(x)
}

# is_density -

#' @export
is_density <- function(.fn, ...) {
  UseMethod("is_density")
}
#' @export
is_density.default <- function(.fn, ...) {
  generic_default()
  FALSE
}
#' @export
is_density.function <- function(.fn, .tol = 1e-4, n = 1001) {
  .capture_error <- function(code,
                             otherwise = NULL,
                             quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet) {
         cat_message("Error: ", e$message)
        }
        list(result = NA, error = e)
      },
      interrupt = function(e) {
        cat_stop("Terminated by user")
      }
    )
  }
  .fsafe <- function(.fn, x) {
    y <- sapply(x, function(x) .capture_error(.fn(x))$result)
    t1 <- map_lgl(y, ~ is_true(is_emptyna(.x)))
    t2 <- map_lgl(y, ~ is_true(is.nan(.x)) | is_true(is_nonnum(.x)))
    y[t1 | t2] <- 0
    y
  }

  integral <-
    integrate(
      f = function(x) {
        .fsafe(.fn, x)
      },
      lower = -Inf,
      upper = Inf,
      subdivisions = n,
      rel.tol = .tol
    )$value

  equals_one <- is_equal(x = integral, y = 1, .tol = .tol)
  equals_one
}
#' @export
is_density.distrcont <-
  function(.fn, ..., .tolerance = 1.5e-3, n = 1000) {
    trydfn <- function(x) {
      w <- tryCatch(
        expr = as_dens(.fn)(x),
        error = function(e) {
          0
        }
      )
      ifelse(is.nan(w), 0, w)
    }
    lower <- min_qfn(.fn) - 1
    upper <- max_qfn(.fn) + 1
    integral <-
      try(integrate_safely(
        .fn = trydfn,
        lower = lower,
        upper = upper,
        n = min(max(upper - lower + 2, 1001), 10001),
        target = 1
      ),
      silent = T
      )
    equals_one <- is_equal(x = integral, y = 1, .tol = .tolerance)
    equals_one
  }
#' @export
is_density.distrdiscr <- function(.fn, ..., .tolerance = 0.0015) {
  trydfn <- function(x) {
    tryCatch(
      expr = as_dens(.fn)(x),
      error = function(e) {
        0
      }
    )
  }
  lower <- min_qfn(.fn) - 1
  upper <- max_qfn(.fn) + 1
  integral <-
    try(integrate_safely(
      .fn = trydfn,
      lower = lower,
      upper = upper,
      n = min(max(upper - lower + 2, 1001), 10001),
      target = 1
    ),
    silent = T
    )
  equals_one <- is_equal(x = integral, y = 1, .tol = .tolerance)
  equals_one
}
#' @export
is_density.density_integrate <-
  function(.fn, ..., .tolerance = .05, n = 1000) {
    integral <- integrate_safely(
      .fn = as_dens(.fn),
      lower = min(.fn$x) - 1,
      upper = max(.fn$x) + 1,
      n = n,
      target = 1
    )
    equals_one <- is_equal(x = integral, y = 1, .tol = .tolerance)
    equals_one
  }
#' @export
is_density.fit_custom_distr <-
  function(.fn, ..., .tolerance = .05, n = 1000) {
    integral <- integrate_safely(
      .fn = as_dens(.fn),
      lower = min(.fn$dist_table$x) - 1,
      upper = max(.fn$dist_table$x) + 1,
      n = n,
      target = 1
    )
    equals_one <- is_equal(x = integral, y = 1, .tol = .tolerance)
    equals_one
  }

## integrate

#' @keywords internal
density_piecelin <- function(x, n) {
  dens <- stats::density(x, n = n)
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

  data.frame(x = x_dens, y = y_dens / tot_integral)
}

#' @keywords internal
compute_piecelin_density_coeffs <- function(dist, ind_vec) {
  n <- length(ind_vec)
  slope <- numeric(n)
  intercept <- numeric(n)

  x <- dist[["x"]]
  y <- dist[["y"]]

  ind_is_in <- (ind_vec >= 1) & (ind_vec < length(x))
  inds_in <- ind_vec[ind_is_in]

  slope[ind_is_in] <- (y[inds_in + 1] - y[inds_in]) /
    (x[inds_in + 1] - x[inds_in])
  intercept[ind_is_in] <-
    y[inds_in] - slope[ind_is_in] * x[inds_in]

  list(slope = slope, intercept = intercept)
}
#' @keywords internal
compute_density_crossings <- function(f, g) {
  compute_piecequad_crossings(piecequad_density(f), piecequad_density(g))
}
#' @keywords internal
compute_piecequad_crossings <- function(f, g) {
  # Regrid both `piecequad`s to have the same grid
  pair <- piecequad_pair_regrid(f, g)

  # Construct interval breaks to determine if roots lie inside intervals
  grid <- pair[[1]][["x"]]
  x_l <- grid[-length(grid)]
  x_r <- grid[-1]

  # Inside regridded intervals input piecewise quadratic functions are
  # quadratic. This means that all their crossings can be found as union of
  # roots for difference of quadratics (which is itself quadratic) inside every
  # interval. **Note** that root(s) based on certain interval should lie inside
  # that interval.
  a <- pair[[1]][["a"]] - pair[[2]][["a"]]
  b <- pair[[1]][["b"]] - pair[[2]][["b"]]
  c <- pair[[1]][["c"]] - pair[[2]][["c"]]

  # Compute candidates for quadratic solutions (`NA` if outside of interval)
  quad_solutions <- solve_piecequad(a, b, c, x_l, x_r)
  x_quad_1 <- quad_solutions[["solution_1"]]
  x_quad_2 <- quad_solutions[["solution_2"]]

  # Compute candidates for linear solutions (`NA` if outside of interval)
  x_lin <- na_outside(-c / b, x_l, x_r)

  # Compute candidates for constant solutions: both edges of interval in case
  # constants of original piecewise-quadratics are equal
  is_const_sol <- is_zero(c)
  is_const_sol[!is_const_sol] <- NA
  x_const_1 <- x_l[is_const_sol]
  x_const_2 <- x_r[is_const_sol]

  # Combine all solutions, filtering them by the type of equation on each
  # interval
  is_quad <- !is_zero(a)
  is_lin <- !(is_quad | is_zero(b))
  is_const <- !(is_quad | is_lin)
  res <- c(
    x_quad_1[is_quad], x_quad_2[is_quad],
    x_lin[is_lin],
    x_const_1[is_const], x_const_2[is_const]
  )

  sort(unique(res[is.finite(res)]))
}

#' @keywords internal
piecequad_pair_regrid <- function(f, g) {
  x_1 <- f[["x"]]
  x_2 <- g[["x"]]

  # Compute edges of intersection support
  left <- max(min(x_1), min(x_2))
  right <- min(max(x_1), max(x_2))

  # Account for edge case of no intersection
  if (left > right) {
    num <- numeric(0)

    return(list(
      piecequad_1 = list(
        x = num,
        a = num,
        b = num,
        c = num
      ),
      piecequad_2 = list(
        x = num,
        a = num,
        b = num,
        c = num
      )
    ))
  }

  grid <- sort(union(x_1, x_2))
  grid <- grid[(grid >= left) & (grid <= right)]

  list(
    piecequad_1 = piecequad_regrid(grid, f),
    piecequad_2 = piecequad_regrid(grid, g)
  )
}
#' @keywords internal
piecequad_density <- function(f) {
  x_tbl <- as_tbl(f)
  n <- nrow(x_tbl)

  coeffs <- compute_piecelin_density_coeffs(x_tbl, seq_len(n - 1))

  list(
    x = x_tbl[["x"]],
    a = rep(0, n - 1),
    b = coeffs[["slope"]],
    c = coeffs[["intercept"]]
  )
}
#' @keywords internal
piecequad_regrid <- function(grid, piecequad) {
  # Ensure that `grid` has at least two elements
  grid <- rep(grid, length.out = max(2, length(grid)))
  mids <- 0.5 * (grid[-1] + grid[-length(grid)])
  inds <- findInterval(mids, piecequad[["x"]], all.inside = TRUE)

  list(
    x = grid,
    a = piecequad[["a"]][inds],
    b = piecequad[["b"]][inds],
    c = piecequad[["c"]][inds]
  )
}

#' @keywords internal
solve_piecequad <- function(a, b, c, x_l, x_r) {
  a_new <- a
  b_new <- 2 * a * x_l + b
  c_new <- (a * x_l + b) * x_l + c

  # Solve modified equations
  discr_sqrt <- na_sqrt(b_new^2 - 4 * a_new * c_new)
  t_sol_1 <- (-b_new + discr_sqrt) / (2 * a_new)
  t_sol_2 <- (-b_new - discr_sqrt) / (2 * a_new)

  # Make reverse change of variables and check if answers lie inside intervals
  x_sol_1 <- na_outside(x_l + t_sol_1, x_l, x_r)
  x_sol_2 <- na_outside(x_l + t_sol_2, x_l, x_r)

  # For each equation two solutions are possible. Here `NA`s indicate absence of
  # solutions inside intervals.
  list(solution_1 = x_sol_1, solution_2 = x_sol_2)
}

#' @keywords internal
na_sqrt <- function(x) {
  res <- rep(NA_real_, length(x))
  x_is_pos <- is.finite(x) & (x >= 0)
  res[x_is_pos] <- sqrt(x[x_is_pos])

  res
}
#' @keywords internal
na_outside <- function(x, left, right) {
  x[!is.na(x) & ((x < left) | (x > right))] <- NA

  x
}


## rev
#' @keywords internal
obj_density_integrate <- function(dens, type) {
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

  res[["prob"]] <- y_dens
  if (type == "discrete") {
    res[["x"]] <- round(res[["x"]])
    res <-
      group_filter(
        .data = res,
        .groups = x,
        cumprob == max(cumprob)
      )
    res[["y"]] <-
      map_dbl(
        .x = seq_row(res),
        .f = ~ ifelse(.x == 1,
                      res[["cumprob"]][1],
                      res[["cumprob"]][.x] - res[["cumprob"]][.x - 1]
        )
      )
    res[["prob"]] <- res[["y"]]
  }

  attr(res, "type") <- type
  res
}
#' @keywords internal
nbins_dens <- function(x, n = 1000, type) {
  hdv <-
    high_dens_vect_num(x,
                       size_range = 90:99,
                       n = n
    )

  comp_rang <- hdv$upper[1] - hdv$lower[1]

  dif_u <- c(0, abs(diff(hdv$upper))) / comp_rang
  dif_l <- c(0, abs(diff(hdv$lower))) / comp_rang

  dif_r <- dif_u + dif_l > 1

  sel_r <- which(dif_r)[1] - 1
  sel_r <- c(hdv$lower[sel_r], hdv$upper[sel_r])

  xtrunc <- x[x > sel_r[1] & x < sel_r[2]]

  n <- max(ceiling(diff(range(x)) / 2), 500)
  ns <- mean(diff(density(xtrunc, n = n)[["x"]]))

  ceiling(diff(range(x)) / ns)
}
#' @keywords internal
distr_metrics <- function(x, par) {
  loglik <- function(par, x) {
    sum(log(density_dist(par, x) + .00001))
  }
  CvM <- function(par, x) {
    n <- length(x)
    s <- sort(x)
    thex <- as_prob(par)(s)
    1 / (12 * n) + sum((thex - (2 * 1:n - 1) / (2 * n))^2)
  }
  KS <- function(par, x) {
    n <- length(x)
    s <- sort(x)
    xpu <- seq(1, n) / n
    xpl <- seq(0, n - 1) / n
    thex <- as_prob(par)(s)
    max(pmax(abs(thex - xpu), abs(thex - xpl)))
  }
  AD <- function(par, x) {
    n <- length(x)
    s <- sort(x)
    thex <- as_prob(par)(s)
    -n - mean((2 * 1:n - 1) * (log(thex) + log(1 - rev(thex))))
  }
  ADR <- function(par, x) {
    n <- length(x)
    s <- sort(x)
    thex <- as_prob(par)(s)
    n / 2 - 2 * sum(thex) - mean((2 * 1:n - 1) * log(1 - rev(thex)))
  }
  ADL <- function(par, x) {
    n <- length(x)
    s <- sort(x)
    thex <- as_prob(par)(s)
    -3 * n / 2 + 2 * sum(thex) - mean((2 * 1:n - 1) * log(thex))
  }
  AD2L <- function(par, x) {
    n <- length(x)
    s <- sort(x)
    thex <- as_prob(par)(s)
    2 * sum(log(thex)) + mean((2 * 1:n - 1) / thex)
  }
  AD2R <- function(par, x) {
    n <- length(x)
    s <- sort(x)
    thex <- as_prob(par)(s)
    2 * sum(log(1 - thex)) + mean((2 * 1:n - 1) / (1 - rev(thex)))
  }
  AD2L <- function(par, x) {
    n <- length(x)
    s <- sort(x)
    thex <- as_prob(par)(s)
    2 * sum(log(thex)) + mean((2 * 1:n - 1) / thex)
  }
  AD2z <- function(par, x) {
    n <- length(x)
    s <- sort(x)
    thex <- as_prob(par)(s)
    2 * sum(log(thex) + log(1 - thex)) +
      mean(((2 * 1:n - 1) / thex) + ((2 * 1:n - 1) / (1 - rev(thex))))
  }
  random_error <- function(par) {
    rr <- random_dist(par, n = 1000)
    rr <- sum(is_nonnum(rr)) > 0
    qq <- quantile_dist(dist = par, p = runif(1000, 0, 1))
    qq <- sum(is_nonnum(qq)) > 0
    qq | rr
  }
  metrics <- tibble(
    logLik = loglik(par = par, x = x),
    cvm = CvM(
      par = par,
      x = x
    ),
    ks = KS(
      par = par,
      x = x
    ),
    ad = AD(
      par = par,
      x = x
    ),
    adr = ADR(
      par = par,
      x = x
    ),
    adl = ADL(
      par = par,
      x = x
    ),
    ad2r = AD2R(
      par = par,
      x = x
    ),
    ad2l = AD2L(
      par = par,
      x = x
    ),
    ad2z = AD2z(
      par = par,
      x = x
    ),
    random_error = random_error(par = par),
    density = mgalda::is_true(suppressall(is_density(.fn = par)))
  )
  par[["metrics"]] <- metrics
  par
}
#' @keywords internal
dist_mode <- function(dist, n) {
  dfun <- as_dens(dist)
  median <- dist_stepsize(dist, n = n, side = "left")[2]

  modefun <- function(x) {
    log(dfun(x))
  }
  step_high <- dist_stepsize(dist, n, side = "right")[1]

  high_x <- median + step_high

  while (dfun(high_x) > dfun(median)) {
    high_x <- high_x + step_high
  }

  step_low <- dist_stepsize(dist, n, side = "left")[1]

  low_x <- median - step_low
  while (dfun(low_x) > dfun(median)) {
    low_x <- low_x - step_low
  }

  range <- c(low_x, high_x)
  optimize(
    f = modefun,
    interval = range,
    maximum = TRUE
  )$maximum
}
#' @keywords internal
dist_stepsize <- function(dist,
                          n = 50,
                          side = c("right", "left")) {
  rfun <- as_rand(dist)
  side <- match.arg(side)
  sample <- rfun(n)

  mid <- median(sample)

  quans <- as_vct(quantile(sample, probs = c(0.25, 0.75)))
  step <- ifelse(side == "left", mid - quans[1],
                 quans[2] - mid
  )
  step <- c(step, mid)

  step
}
#' @keywords internal
formals_pdqr <- function(dist, pkg, model = NULL) {
  if (is_emptyna(model)) {
    model <- toupper(dist)
  }

  pdqr <-
    map(c("q", "p", "d", "r"), ~ paste0(pkg, "::", .x, dist))

  fn_pdqr <- map(pdqr, ~ do_try(eval_parse(.x)))

  if (any(sapply(fn_pdqr, is_empty))) {
    cat_stop("No se encontraron funciones de distribucion asociadas a: {dist} , ({model})")
    return(NULL)
  }

  fn_pdqr <- lapply(fn_pdqr, formals)

  map(fn_pdqr, ~ .x[nmdiff(.x, c("log", "log.p", "lower.tail"))])
}

# make obj fns
data_args_bounds <- function(dist, .inf = 1e5, small = 1e-5) {
  n_df <- function(args_d, n_seq) {
    x <- do_call("expand_grid", map(args_d, ~ sort(c(
      n_seq * -1, n_seq, 0
    ))))
    x
  }

  df_l <- function(x, q = seq(.01, .99, .01)) {
    map(split(x, seq_row(x)), ~ c(p = list(q), as_l(.x)))
  }
  fdist <- match.fun(paste0("q", dist))
  rep_args <- function(x) {
    x <- x[!x$res, ]
    x <- map(split(select(x, !res), seq_row(x)), as_l)
    expd_x <- function(w) {
      do_call(
        "expand_grid",
        map(w, ~ if (abs(.x) == 10) {
          .inf * sign(.x)
        } else if (abs(.x) == 3) {
          5 * sign(.x)
        } else if (abs(.x) == 1) {
          c(1 - small, 1 + small) * sign(.x)
        } else if (abs(.x) == .5) {
          .25 * sign(.x)
        } else if (.x == 0) {
          c(-small, small)
        })
      )
    }

    do_call(rbind, map(x, expd_x))
  }
  args_d <- formals(fdist)

  dist_args_list <- get("dist_args_list", envir = env_data)
  dist_args_list <- dist_args_list[[dist]]
  args_d <- args_d[dist_args_list]

  n_seq <- c(.5, 1, 3, 10)
  fdist <- substitute(fdist)
  fdist_chr <- lang2str(fdist)
  x_df <- n_df(args_d, n_seq)
  n_oks <-
    suppressall(map(df_l(x_df), ~ do_try(eval(make_call(
      f = fdist_chr, .args = .x
    )), rep(NA_real_, nrow(x_df)))))
  x_df$res <- map_lgl(n_oks, ~ sum(is.finite(.x)) > 0)
  if (any(!x_df$res)) {
    new_x_df <- rep_args(x_df)

    new_n_oks <-
      suppressall(map(df_l(new_x_df), ~ eval(make_call(
        f = fdist_chr, .args = .x
      ))))
    new_x_df$res <- map_lgl(new_n_oks, ~ sum(is.finite(.x)) > 0)
    x_df <- bind_rows(new_x_df, x_df)
  }
  id10 <-
    rowSums(apply(x_df[, are_dblint(x_df)], 2, function(x) {
      abs(x) == 10
    }))
  id10 <- id10 > 0 & x_df$res
  if (any(id10)) {
    max_x_df <- x_df[id10, ]
    max_x_df <-
      mutate_if(max_x_df, is_dblint, ~ ifelse(abs(.x) == 10, sign(.x) * .inf, .x))
    x_df <- bind_rows(max_x_df, x_df)
  }

  x_df
}
bound_nm <- function(df_bound, .inf = 1e5) {
  res <- df_bound$res

  df_bound$res <- NULL
  df_bound <- df_bound[res, ]
  out <-
    do_call(cbind, map(df_bound, range)) %rownm% c("lowb", "uppb")
  apply(out, 2, function(x) {
    ifelse(abs(x) == .inf, sign(x) * Inf, x)
  })
}
bound_type <- function(df_bound, dist, small = 1e-5) {
  df_l <- function(x, q = seq(.01, .99, .01)) {
    map(split(x, seq_row(x)), ~ c(p = list(q), as_l(.x)))
  }
  res <- df_bound$res

  df_bound$res <- NULL
  df_bound <- df_bound[res, ]

  df_bound <- distinct(bind_rows(
    mutate_all(df_bound, ~ ifelse(round(.x) == .x, .x + small, .x)),
    mutate_all(df_bound, ~ ifelse(round(.x) == .x, .x - small, .x)),
    df_bound
  ))

  res_q <- suppressall(map(df_l(df_bound), ~ eval(make_call(
    f = paste0("q", dist), .args = .x
  ))))

  df_bound <- df_bound[map_lgl(res_q, ~ sum(is.finite(.x)) > 0), ]

  map(df_bound, ~ if (is_integerish(.x)) {
    "integer"
  } else {
    "double"
  })
}
bound_fn <- function(num_b) {
  num_b <- as_tbl(num_b)

  map(num_b, ~ make_function(
    args = alist(x = ),
    body = interp(quote(all(x >= xmin &
                              x <= xmax)), xmin = min(.x), xmax = max(.x))
  ))
}
x_bounds <- function(df_bound, dist, .inf = 1e5) {
  df_l <- function(x, q = seq(.01, .99, .01)) {
    map(split(x, seq_row(x)), ~ c(p = list(q), as_l(.x)))
  }
  res <- df_bound$res

  df_bound$res <- NULL
  df_bound <- df_bound[res, ]


  res_q <- suppressall(map(df_l(df_bound), ~ eval(make_call(
    f = paste0("q", dist), .args = .x
  ))))
  res_q <- range(unlist(res_q), na.rm = TRUE)
  res_q <-
    ifelse(ceiling(abs(res_q)) >= .inf, sign(res_q) * Inf, res_q)
  res_q <- round_up(res_q, 5)
  make_function(
    args = alist(x = ),
    body = interp(
      quote(all(x >= xmin &
                  x <= xmax)),
      xmin = min(res_q),
      xmax = max(res_q)
    )
  )
}
default_crossmetrics <- function() {
  c_m <- get(x = "crossmetrics", envir = env_data)

  l1_m <- c_m$opsexprs
  l2_m <-
    expand_grid(
      operation = c_m$ops_chr[-2],
      e1 = c_m$opsexprs[c("mean", "median", "sd")],
      e2 = c_m$opsexprs[c("mean", "median", "sd")]
    )

  l2_m %<>%
    filter(names(e1) != names(e2)) %>%
    mutate(nms = pmap_chr(.l = list(
      x = names(e1),
      y = names(operation),
      z = names(e2)
    ), function(x, y, z) {
      if (y %in% c("subtraction", "division")) {
        paste(c(x, y, z), collapse = "_")
      } else {
        xz <- sort(c(x, z))
        paste(c(xz[1], y, xz[2]), collapse = "_")
      }
    })) %>%
    filter(cumcount(nms) == 1) %>%
    mutate(expr = pmap(
      .l = list(
        ops = operation,
        expr1 = e1,
        expr2 = e2
      ),
      .f = exprs_ops
    ))

  l2_m <- set_names(l2_m$expr, l2_m$nms)

  c(l1_m, l2_m)
}
make_init_list <-
  function(dist,
           pkg,
           model,
           .inf = 1e5,
           small = 1e-5) {
    .make_init_fn <-
      function(num_b,
               type_b,
               dist,
               df_bound,
               metrics_init,
               .inf = 1e5) {
        samp_fn_init <- function(num_b, type_b, dist, .inf = 1e5) {
          rand_df <- function(num_b, n = 100) {
            num_b <- apply(num_b, 2, function(x) {
              ifelse(is.infinite(x), sign(x) * .inf, x)
            })
            as_tbl(do_call("cbind", map(as_tbl(num_b), function(x) {
              if (amplitude(x) > 100) {
                f_rand <- rlogunif
              } else {
                f_rand <- runif
              }
              c(0, f_rand(
                n = n,
                min = min(x),
                max = max(x)
              ))
            })))
          }
          df_l <- function(x, q = seq(.01, .99, .01)) {
            map(split(x, seq_row(x)), ~ c(p = list(q), as_l(.x)))
          }
          f_res <- function(.x, dist) {
            call <- make_call(f = paste0("q", dist), .args = .x)
            res <- eval(call)
            .x$p <- NULL
            .x$res <- res
            .x
          }
          sim_ops <- rand_df(num_b)
          sim_ops <-
            purrr::map2_dfc(sim_ops, type_b, ~ if (.y == "integer") {
              round(.x)
            } else {
              .x
            })

          suppressall(map(df_l(sim_ops), ~ f_res(.x = .x, dist = dist)))
        }
        to_init_int <-
          map(
            .x = metrics_init,
            .f = ~ make_function(args = alist(x = ), body = interp(quote(round(
              y
            )), y = .x))
          )

        to_init <-
          map(
            .x = metrics_init,
            .f = ~ make_function(args = alist(x = ), .x)
          )

        res_q <- samp_fn_init(num_b, type_b, dist, .inf)

        res_q <- list(
          res = map_dfc(to_init, function(f) {
            .un(map_dbl(res_q, ~ f(.x$res)))
          }),
          args = map_dfr(res_q, function(x) {
            x$res <- NULL
            args <- bind_cols(x)
            args
          })
        )
        rsq_boot <-
          f(do_try(bootfn2(
            x = x,
            y = y,
            fn = f(rsq_vec(
              truth = x,
              estimate = y,
              na_rm = TRUE
            )),
            times = 5,
            rep = 10
          ), NA))


        nms_args <- nmdiff(data = df_bound, "res")
        inits_fns <-
          vector(mode = "list", length = length(nms_args))
        inits_fns <- set_names(inits_fns, nms_args)

        for (nm in nms_args) {
          tmp_type <- type_b[[nm]]

          if (tmp_type == "integer") {
            r <- map_dbl(res_q$res, ~ rsq_boot(res_q$args[[nm]], round(.x)))
          } else {
            r <- map_dbl(res_q$res, ~ rsq_boot(res_q$args[[nm]], .x))
          }
          r <- round(r, 4)
          r <- names(which.max(r))

          if (tmp_type == "integer") {
            inits_fns[[nm]] <- to_init_int[[r]]
          } else {
            inits_fns[[nm]] <- to_init[[r]]
          }
        }

        inits_fns
      }
    cat_step(
      mgs = model,
      head = "**",
      head_type = "green"
    )
    cat_step("generar muestras")
    df_bound <- data_args_bounds(dist, .inf = .inf, small = small)
    cat_step("numeric arguments bound")
    num_b <- bound_nm(df_bound, .inf = .inf)
    type_b <- bound_type(df_bound, dist, small = small)
    cat_step("function arguments bound")
    fn_b <- bound_fn(num_b)
    cat_step("function 'x' bound")
    fn_x <- x_bounds(df_bound, dist, .inf = .inf)

    cat_step("function inicial")
    init_fn_list <-
      suppressall(.make_init_fn(num_b, type_b, dist, df_bound, metrics_init, .inf))

    if (is_empty(init_fn_list)) {
      return(NULL)
    }
    assert_x_domain <-
      interp(quote(
        assert_engine(fn_x,
                      severity = "stop", msg = "'x' out of domain"
        )
      ), fn_x = body(fn_x))

    list(domain = assert_x_domain, init = init_fn_list)
  }
make_init_fn <- function(x) {
  assert_x_domain <- x$domain
  init_fn_list <- x$init

  eval(parse(
    text = paste(
      "function(x){",
      "  assert_na_any(x,severity = 'stop')",
      deparse1(assert_x_domain, collapse = "\n "),
      paste("  fn <- ", deparse1(init_fn_list, collapse = "\n  "), sep = ""),
      "  if (is.function(fn)) {",
      "    fn(x)",
      "  } else if (is.list(fn)) {",
      "    map_dbl(fn, ~ .x(x))",
      "  }",
      "}",
      sep = "\n"
    )
  ))
}
pdq2optm <- function(x, args) {
  dbtn_pkgdist <- function(x) {
    paste0(x$pkg, "::", c("q", "p", "d"), x$dist)
  }


  args <-
    set_names(paste0("par[", seq_along(args), "]"), args)


  fms_fn <- do_call(f = "formals_pdqr", .args = x)[-4]
  fms_fn <-
    map(fms_fn, function(x) {
      x[names(x) %in% c("p", "q", "x")][[1]] <-
        str2lang(names(x)[names(x) %in% c("p", "q", "x")])
      for (i in names(args)) {
        x[[i]] <- str2lang(args[names(args) == i])
      }
      x[names(x) %in% c(c("p", "q", "x"), names(args))]
    })
  names(fms_fn) <- c("q", "p", "d")
  dcall <-
    map2(
      dbtn_pkgdist(x),
      fms_fn,
      ~ chr_pair2call(.x, .y, default = T)
    )
  dist_opfn <-
    map2(
      dcall,
      c("p", "q", "x"),
      ~ make_function(chr2pairlist(c("par", .y)), .x)
    )

  dist_opfn
}
dens_valdid <- function(x) {
  ifelse(!is.finite(x), 0, x)
}
build_fnobj <- function(dist, pkg, model, num_b, df_bound) {
  .distr_type <- function(df_bound, dist, small = 1e-5) {
    df_l <- function(x, q = seq(.01, .99, .01)) {
      map(split(x, seq_row(x)), ~ c(p = list(q), as_l(.x)))
    }
    res <- df_bound$res

    df_bound$res <- NULL
    df_bound <- df_bound[res, ]

    df_bound <- distinct(bind_rows(
      mutate_all(df_bound, ~ ifelse(round(.x) == .x, .x + small, .x)),
      mutate_all(df_bound, ~ ifelse(round(.x) == .x, .x - small, .x)),
      df_bound
    ))

    res_q <- suppressall(map(df_l(df_bound), ~ eval(make_call(
      f = paste0("q", dist), .args = .x
    ))))

    res_q <- unlist(res_q)
    res_q <- res_q[!is_nonnum(res_q)]
    is_integerish(res_q)
  }

  x <- list(
    dist = dist,
    pkg = pkg,
    model = model
  )
  fns <- pdq2optm(x, args = colnames(num_b))

  body(fns[[3]]) <-
    interp(quote(dens_valdid(x)), x = body(fns[[3]]))

  discrete <- .distr_type(df_bound, dist)
  if (discrete) {
    x$type <- "discrete"
  } else {
    x$type <- "continuous"
  }

  if (!discrete) {
    objfns <- list(
      mle_obj = function(par, x) {
        fn <- function(par, x) {
          f(x, par)
        }
        -sum(log(fn(par = par, x = x)))
      },
      qme_obj = function(par, x) {
        fn <- function(par, p) {
          f(par, p)
        }
        valid <- function(x) {
          ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                 0, x
          )
        }
        p <- ppoints(7)
        q_theo <- valid(fn(par = par, p = p))
        q_emp <-
          as_num(quantile(x, probs = p, type = 7))
        sum((q_emp - q_theo)^2)
      },
      mge_obj = function(par, x) {
        fn <- function(par, q) {
          f(par, q)
        }
        sx <- c(-Inf, sort(x), Inf)
        n <- length(sx)
        Di <-
          fn(par = par, q = sx[-1]) - fn(par = par, q = sx[-n])
        mean(log(Di))
      },
      cvmmge_obj = function(par, x) {
        fn <- function(par, p) {
          f(par, p)
        }
        n <- length(x)
        s <- sort(x)
        theop <- fn(par, s)
        1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 * n))^2)
      },
      ksmge_obj = function(par, x) {
        fn <- function(par, p) {
          f(par, p)
        }
        n <- length(x)
        s <- sort(x)
        theop <- fn(par, s)
        obspu <- seq(1, n) / n
        obspl <- seq(0, n - 1) / n
        max(pmax(abs(theop - obspu), abs(theop - obspl)))
      },
      admge_obj = function(par, x) {
        fn <- function(par, p) {
          f(par, p)
        }
        n <- length(x)
        s <- sort(x)
        theop <- fn(par, s)
        -n - mean((2 * 1:n - 1) * (log(theop) + log(1 - rev(theop))))
      }
    )
    body(objfns$qme_obj)[[2]][[3]] <- fns[[1]]
    body(objfns$mge_obj)[[2]][[3]] <- fns[[2]]
    body(objfns$mle_obj)[[2]][[3]] <- fns[[3]]
    body(objfns$cvmmge_obj)[[2]][[3]] <- fns[[3]]
    body(objfns$ksmge_obj)[[2]][[3]] <- fns[[3]]
    body(objfns$admge_obj)[[2]][[3]] <- fns[[3]]
  } else {
    objfns <- list(
      mle_obj = function(par, x) {
        fn <- function(par, x) {
          f(x, par)
        }
        -sum(log(fn(par = par, x = x)))
      },
      cvmmge_obj = function(par, x) {
        fn <- function(par, p) {
          f(par, p)
        }
        x <- unique(x)
        n <- length(x)
        s <- sort(x)
        theop <- fn(par, s)
        1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 * n))^
                             2)
      },
      ksmge_obj = function(par, x) {
        fn <- function(par, p) {
          f(par, p)
        }
        x <- unique(x)
        n <- length(x)
        s <- sort(x)
        theop <- fn(par, s)
        obspu <- seq(1, n) / n
        obspl <- seq(0, n - 1) / n
        max(pmax(abs(theop - obspu), abs(theop - obspl)))
      },
      chi2_obj = function(par, x) {
        fn <- function(par, q) {
          f(par, q)
        }
        freq <- as_vct(table(x))
        count <- as_num(names(table(x)))
        nfreq <- rep(0, max(count) + 1)
        nfreq[count + 1] <- freq
        freq <- nfreq
        count <- 0:max(count)
        n <- length(count)
        p_hat <- diff(c(
          0, fn(par = par, q = count[-n]),
          1
        ))
        expected <- sum(freq) * p_hat
        sum((freq - expected)^2) / sum(expected)
      },
      qmediscr_obj = function(par, x) {
        fn <- function(par, p) {
          f(par, p)
        }
        (fn(par, 1 / 2) - median(x))^2
      }
    )
    body(objfns$mle_obj)[[2]][[3]] <- fns[[3]]
    body(objfns$cvmmge_obj)[[2]][[3]] <- fns[[3]]
    body(objfns$ksmge_obj)[[2]][[3]] <- fns[[3]]
    body(objfns$chi2_obj)[[2]][[3]] <- fns[[2]]
    body(objfns$qmediscr_obj)[[2]][[3]] <- fns[[1]]
  }
  bu_bnd <- num_b
  if (length(colnames(num_b)) > 1L) {
    o_method <- "L-BFGS-B"
  } else {
    o_method <- "Nelder-Mead"
  }
  objfns <-
    map(objfns, ~ make_function(args = alist(x = ), body = substitute(
      {
        fnobj <- function(par, x) {
          objetivo
        }
        do_try(
          optim(
            par = init(x),
            lower = LOWER,
            upper = UPPER,
            fn = fnobj,
            hessian = T,
            x = x,
            method = OMETHOD
          )
        )
      },
      list(
        UPPER = rlang::expr(!!as_num(num_b[2, ])),
        LOWER = rlang::expr(!!as_num(num_b[1, ])),
        objetivo = body(.x),
        OMETHOD = rlang::expr(!!o_method)
      )
    )))
  x[["fun_obj"]] <- objfns
  x[["optim.method"]] <- o_method
  x
}
optim_fn <- function(fun_obj, init_fn) {
  init_fn <-
    paste(c(paste("  ", deparse(body(
      init_fn
    ))), ""), collapse = "\n")
  init_fn[1] <-
    paste0("init <- function (x, na.rm = FALSE) ", init_fn)

  dvald <-
    "dens_valdid <- function (x) {\n  ifelse(!is.finite(x), 0, x)\n}"
  finfn <-
    "if (is_empty(res)) {\n  cat_return_null(msg='no result',type = 'warning')\n}\nvc <- do_try(solve(res$hessian))\nsds <- do_try(sqrt(diag(vc)))\n\nlist(\n  estimate = res$par,\n  convergence = res$convergence,\n  hessian = res$hessian,\n  sd = sds,\n  vcov = vc\n)"

  dep_x <- purrr::imap(fun_obj, function(x, y) {
    x <- deparse(x)
    x[1] <- paste0(y, " <- ", x[1])
    x
  })

  nm <- names(dep_x)
  fn01 <- c(
    "    if (length(x) > getOption('mgalda.n_samples',1500)) {\n        x <- sample(x, getOption('mgalda.n_samples',1500),F)\n    }\n",
    paste0("    res <- do_try(", nm[1], "(x))")
  )

  if (length(nm) > 1) {
    fn02 <- paste("    if (is_empty(res)) {",
                  paste0(
                    "        res <- do_try(",
                    nm[2:length(nm)], "(x))"
                  ),
                  "    }",
                  sep = "\n"
    )
  } else {
    fn02 <- ""
  }

  fns_ops <-
    paste0(map_chr(dep_x, ~ paste0(.x, collapse = "\n")),
           collapse = "\n"
    )
  eval_parse(c(
    "function (x) {\n",
    dvald,
    init_fn,
    fns_ops,
    fn01,
    c(paste0(fn02, collapse = "\n"), "\n"),
    finfn,
    "\n}"
  ))
}
distr_fingerprint <- function(x) {
  fun_obj <- x$fn_opts
  res <- list(par = c(), call_info = list())
  res$call_info[["model"]] <- x$model
  res$call_info[["quantile"]] <- c(x$pkg, paste0("q", x$dist))
  res$call_info[["probab"]] <- c(x$pkg, paste0("p", x$dist))
  res$call_info[["density"]] <- c(x$pkg, paste0("d", x$dist))
  res$call_info[["random"]] <- c(x$pkg, paste0("r", x$dist))
  res$call_info[["dist"]] <- x$dist
  res$call_info[["support"]] <- body(x$valid_x)
  res[["bound"]] <- x$bound
  res[["optim.method"]] <- x$optim.method
  res[["type"]] <- x$type
  class(res) <-
    ifelse(x$type == "discrete", "distrdiscr", "distrcont")
  nenv <-
    env_new(data = list(res = res, fun_obj = fun_obj))

  fun_obj <- rlang::expr_deparse(body(fun_obj))
  fun_obj[1] <- paste("fun_obj <- function(x) \n", fun_obj[1])
  res <- deparse(res)
  res[1] <- paste("res <- function(x) \n", res[1])
  eval_parse(
    paste0(
      "function(x) {\n",
      paste(fun_obj, collapse = "\n"),
      "  \n\n",
      paste(res, collapse = "\n"),
      "  \n\n",
      "  out <- fun_obj(x)\n",
      "  if (is.null(out)) {\n",
      "    return(NULL)\n",
      "  }\n",
      "  res <- res(x)\n",
      "  res$par <- out$estimate\n",
      "  res[[\"convergence\"]] <- out$convergence\n",
      "  res[[\"hessian\"]] <- out$hessian\n",
      "  res[[\"sd\"]] <- out$sd\n",
      "  res[[\"vcov\"]] <- out$vcov\n",
      "  res[[\"n\"]] <- length(x)\n",
      "  res$call_info[[\"call\"]] <- match.call()\n",
      "  res\n",
      "}\n",
      collapse = "\n"
    )
  )
}
rlogunif <- function(n, min = 0, max = 1) {
  min_sign <- sign(min)
  max_sign <- sign(max)
  r_d <- max - min
  r_d_log <- log10(r_d)
  (10^runif(
    n,
    min = .Machine$double.eps,
    max = r_d_log + .Machine$double.eps
  )) * sample(x = c(min_sign, max_sign), n, T)
}
distribution_info <-
  function(dist,
           pkg,
           model,
           metrics_init = default_crossmetrics(),
           .inf = 1e5,
           small = 1e-5) {
    cat_step(
      mgs = model,
      head = "**",
      head_type = "green"
    )
    init_list <- get(x = "init_list", envir = env_data)
    cat_step("generar muestras")
    df_bound <- data_args_bounds(dist, .inf = .inf, small = small)
    cat_step("numeric arguments bound")
    num_b <- bound_nm(df_bound, .inf = .inf)
    type_b <- bound_type(df_bound, dist, small = small)
    cat_step("function arguments bound")
    fn_b <- bound_fn(num_b)
    cat_step("function 'x' bound")
    fn_x <- x_bounds(df_bound, dist, .inf = .inf)

    cat_step("function objetivo")
    l_fnobj <- build_fnobj(dist, pkg, model, num_b, df_bound)

    cat_step(
      mgs = "done",
      head = "**",
      head_type = "green"
    )

    structure(
      list(
        args = colnames(num_b),
        model = model,
        dist = dist,
        pkg = pkg,
        bound = fn_b,
        valid_x = fn_x,
        fn_init = init_list[[dist]],
        type = l_fnobj$type,
        fun_obj = l_fnobj$fun_ob,
        bnd_num = num_b,
        optim.method = l_fnobj$optim.method,
        fn_opts = optim_fn(fun_obj = l_fnobj$fun_obj, init_fn = init_list[[dist]])
      ),
      class = "distribution_info",
      .inf = .inf,
      .small = small
    )
  }

#' @export
print.distribution_info <- function(x) {
  cat_title_head(x$model)
  w <- map(x$bound[x$args], ~ trimws(deparse(body(.x))))

  cat_subtitle1("Distribution Type")
  cat("*   ", stringr::str_to_sentence(x$type), "\n\n")
  cat_subtitle1("Bound")
  map(w, ~ if (any(.x == "TRUE")) {
    "-Inf - Inf"
  } else {
    stringr::str_sub(stringr::str_remove(.x, stringr::fixed("all(")), 1, -2)
  })
  purrr::iwalk(w, ~ cat(paste(
    "*   ", .y, ": ", paste(unlist(.x), collapse = " - "),
    sep = ""
  ), sep = "\n"))
  cat("\n")
  cat_subtitle1("Domain x")
  w <- trimws(deparse(body(x$valid_x)))
  w <- if (any(w == "TRUE")) {
    "-Inf - Inf"
  } else {
    stringr::str_sub(stringr::str_remove(w, stringr::fixed("all(")), 1, -2)
  }
  cat("*   ", w, "\n")
}
