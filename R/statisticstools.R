# Integracion ---

#' integrate
#'
#'
#' @name statisticstools
#' @rdname statisticstools
#' @keywords internal
NULL


#' media esperada
#'
#' @export
#' @rdname statisticstools
#' @examples
#'
#' expected_value( function(x)dunif(x, min = 10, max = 16))
#' expected_value(dnorm, .g = function(x) x^2)
#' expected_value(
#'   function(x) {
#'     dnorm(x, sd = 4)
#'   },
#'   .g = function(x) {
#'     (x - expected_value(function(x)dnorm(x, sd = 4)))^2
#'   }
#' )
expected_value <-
  function(.fn,
           .g = identity,
           lower = -Inf,
           upper = Inf,
           n = 1001,
           .tol = 1e5) {
    .fsafe <- function(.fn,x) {
      fx<-vapply(x,function(w) do_try(expr = .fn(w),error = NA),numeric(1))
      fx[is_valid(fx)]

    }
    integral <-
      integrate(
        function(x) {
          .g(x) * .fsafe(.fn,x)
        },
        lower = lower,
        upper = upper,
        subdivisions = n,
        rel.tol = .tol,
        stop.on.error = F
      )

    integral$value
  }


#' varianza esperda
#'
#' @export
#' @rdname statisticstools
#' @examples
#'
#' expected_var(function(x) dnorm(x, sd = 4))
#' expected_var(function(x) dunif(x, min = 10, max = 16))
#' expected_var(function(x) dexp(x, rate = 2))
expected_var <- function(.fn, ...) {
  mu <- expected_value(.fn, ...)

  identity_var <- function(x) {
    (x - mu) ^ 2
  }

  expected_value(.fn = .fn, ..., .g = identity_var)
}

#' composite
#'
#' @export
#'
#'
#' @rdname statisticstools
#' @examples
#'
#' #composite(sin, 0, pi, n = 10, rule = midpoint)
#' #composite(sin, 0, pi, n = 10, rule = trapezoid)
composite <- function(f, lower, upper, n = 10, rule) {
  points <- seq(lower, upper, length = n + 1)

  area <- 0
  for (i in seq_len(n)) {
    area <- area + rule(f, points[i], points[i + 1])
  }

  area
}
cum_composite <- function(rule, x, y) {
  cumcom <- 0

  .fn <- function(w) {
    ind <- findInterval(x = w, vec = x)
    y[ind]
  }

  for (i in 2:length(x)) {
    cumcom <- c(cumcom, rule(.fn, x[i - 1], x[i]))
  }
  cumsum(cumcom)
}
trapez_integral <- function(x, y) {
  trapez_integral_impl <- function(x, y) {
    n <- length(y)

    # `x` is assumed to be sorted increasingly (as after the `density()` call)
    (x[-1] - x[-n]) * (y[-1] + y[-n]) / 2
  }
  sum(trapez_integral_impl(x, y))
}


#' @export
#' @rdname statisticstools
#' @examples
#'
#' integrate_numerically(dnorm, -1.96, 1.96)
integrate_numerically <- function(f, lower, upper, n = 20, ...) {
  dx <- (upper - lower) / n
  x <- seq(lower, upper - dx, dx)
  sum(f(x, ...) * dx)
}

newton_cotes <- function(coef, open = FALSE) {
  n <- length(coef) + open

  function(.fn, lower, upper) {
    pos <- function(i) {
      lower + i * (upper - lower) / n
    }
    points <- pos(seq.int(0, length(coef) - 1))

    (upper - lower) / sum(coef) * sum(.fn(points) * coef)
  }
}
boole <- function(.fn, lower, upper) {
  pos <- function(i) {
    lower + i * (upper - lower) / 4
  }
  fi <- function(i) {
    .fn(pos(i))
  }

  (upper - lower) / 90 *

    (7 * fi(0) + 32 * fi(1) + 12 * fi(2) + 32 * fi(3) + 7 * fi(4))
}
simpson <- function(.fn, lower, upper) {
  (upper - lower) / 6 * (.fn(lower) + 4 * .fn((lower + upper) / 2) + .fn(upper))
}
midpoint <- function(.fn, lower, upper) {
  (upper - lower) * .fn((lower + upper) / 2)
}
trapezoid <- function(.fn, lower, upper) {
  (upper - lower) / 2 * (.fn(lower) + .fn(upper))
}

midpoint_composite <- function(.fn, lower, upper, n = 10) {
  points <- seq(lower, upper, length = n + 1)
  h <- (upper - lower) / n

  area <- 0
  for (i in seq_len(n)) {
    area <- area + h * .fn((points[i] + points[i + 1]) / 2)
  }
  area
}
trapezoid_composite <- function(.fn, lower, upper, n = 10) {
  points <- seq(lower, upper, length = n + 1)
  h <- (upper - lower) / n

  area <- 0
  for (i in seq_len(n)) {
    area <- area + h / 2 * (.fn(points[i]) + .fn(points[i + 1]))
  }
  area
}

#' varianza esperda
#'
#' @export
#' @rdname statisticstools
#' @examples
#'
#'
#' integrate_composite(sin, 0, pi, n = 10, rule = 'midpoint')
#' integrate_composite(sin, 0, pi, n = 10, rule = 'trapezoid')
#' integrate_composite(sin, 0, pi, n = 10, rule = 'simpson')
#' integrate_composite(sin, 0, pi, n = 10, rule = 'boole')
#' integrate_composite(sin, 0, pi, n = 10, rule = 'newton_cotes',coef = c(1,6,8))
#'
#' integrate_composite(sin, 0, pi, n = 10, rule = "midpoint", point = "q")
#' integrate_composite(sin, 0, pi, n = 10, rule = "trapezoid", point = "q")
#' integrate_composite(sin, 0, pi, n = 10, rule = "simpson", point = "q")
#' integrate_composite(sin, 0, pi, n = 10, rule = "boole", point = "q")
#'
integrate_composite <-
  function(.fn,
           lower,
           upper,
           n = 10,
           rule = c("midpoint", "trapezoid", "simpson", "boole", "newton_cotes"),
           output = c("total", "cumulative", "marginal"),
           point = c("l", "q"),
           coef = NULL,
           open = FALSE) {
    rule <- match.arg(rule)
    output <- match.arg(output)
    point <- match.arg(point)

    points <- switch(point,
                     l = seq(lower, upper, length = n + 1),
                     q = .seq_quan(.fn, lower, upper, n + 1, m = 10)
    )

    .frule <- composite_rule(
      rule = rule,
      coef = coef,
      open = open
    )

    area <- 0
    for (i in seq_len(n)) {
      area <- c(area, .frule(.fn, points[i], points[i + 1]))
    }
    area <- ifelse(is.na(area), 0, area)
    switch(output,
           total = sum(area, na.rm = TRUE),
           cumulative = cumsum(area),
           marginal = area
    )
  }
.seq_quan <- function(.fn, lower, upper, n, m) {
  vec_p <- seq.default(0 + .0001, 1 - .0001, length.out = m)
  res <- numeric(0)
  q <- lower
  m <- ceiling(n / m)
  m <- ifelse(m == 1, 2, m)

  v <- integrate(
    f = .fn,
    lower = lower,
    upper = upper,
    stop.on.error = F
  )$value
  for (i in 2:length(vec_p)) {
    .opfn <- function(x) {
      abs(integrate(
        f = .fn,
        lower = lower,
        upper = x,
        stop.on.error = F
      )$value / v - vec_p[i])
    }
    q <-
      c(q, optimize(
        f = .opfn,
        interval = c(lower, upper),
        maximum = F
      )$minimum)

    res <- c(res, seq(q[i - 1], q[i], length.out = m))
  }
  res
}
composite_rule <-
  function(rule = c("midpoint", "trapezoid", "simpson", "boole", "newton_cotes"),
           coef = NULL,
           open = FALSE) {
    rule <- match.arg(rule)

    if (rule == "newton_cotes") {
      if (is_empty(coef) || !is_dblint(coef)) {
        cat_stop("newton_cotes rule need coef")
      }
    }

    switch(rule,
           midpoint = midpoint,
           trapezoid = trapezoid,
           simpson = simpson,
           boole = boole,
           newton_cotes = newton_cotes(coef = coef, open = open)
    )
  }

#' @export
#' @rdname statisticstools
#' @examples
#'
#' integrate_lite(dnorm, -1.96, 1.96)
integrate_lite <- function(f, lower, upper, n = 20,discrete= FALSE,...) {
  if(discrete){
    g <- base::round
  } else{
    g <- base::identity
  }
  dx <- (upper - lower) / n
  x <- seq(lower, upper - dx, dx)
  sum(f(g(x), ...) * dx,na.rm = TRUE)
}

#' @export
#' @rdname statisticstools
#' @examples
#'
#'  integrate_safely(.fn = dnorm,lower = -2,upper = 2)
#' integrate_safely(
#'   .fn = function(x) {
#'     ddnorm(x, mean = 1, sd = 1)
#'   },
#'   lower = 0,
#'   upper = 4
#' )
#' integrate_safely(
#'   .fn = dnorm,
#'   lower = -2,
#'   upper = 2,
#'   target = 1
#' )
#' integrate_safely(
#'   .fn = function(x) {
#'     ddnorm(x, mean = 1, sd = 1)
#'   },
#'   lower = 0,
#'   upper = 4,
#'   target = 1
#' )
#'


integrate_safely <- function(.fn,
                             lower,
                             upper,
                             n = 10001,
                             rel.tol = 1e-3,
                             target = NULL) {
  integrate_try <- function(.fn, lower, upper, n) {
    do_try(
      expr = integrate(
        f = .fn,
        lower = lower,
        upper = upper,
        subdivisions = n,
        stop.on.error = FALSE
      )$value,
      error = Inf
    )
  }
  composite_try <- function(.fn, lower, upper, n) {
    do_try(
      expr = integrate_composite(
        .fn = .fn,
        lower = lower,
        upper = upper,
        n = n
      ),
      error = Inf
    )
  }
  numerically_try <- function(.fn, lower, upper, n, is_discr) {
    do_try(
      expr =
        integrate_lite(
          f = .fn,
          lower = lower,
          upper = upper,
          n = n,
          discrete = is_discr
        ),
      error = Inf
    )
  }

  value <- integrate_try(.fn, lower, upper, n)

  if (is_true(abs(target - value) < rel.tol)) {
    return(value)
  }

  t_value <- composite_try(.fn, lower, upper, n)

  value <-
    ifelse(abs(target - value) > abs(target - t_value), t_value, value)

  if (is_true(abs(target - value) < rel.tol)) {
    return(value)
  }

  is_discr <- is_discrete_distribution(.fn, lower, upper)

  t_value <- numerically_try(.fn, lower, upper, n, is_discr)

  value <-
    ifelse(abs(target - value) > abs(target - t_value), t_value, value)

  value
}

integrate_safely2 <- function(.fn, lower, upper, n = 10001) {
  composite_try <- function(.fn, lower, upper, n) {
    do_try(
      expr = integrate_composite(
        .fn = .fn,
        lower = lower,
        upper = upper,
        n = n
      ),
      error = Inf
    )
  }
  numerically_try <- function(.fn, lower, upper, n, is_discr) {
    do_try(
      expr = integrate_lite(
        f = .fn,
        lower = lower,
        upper = upper,
        n = n,
        discrete = is_discr
      ),
      error = 0
    )
  }

  is_discr <- is_discrete_distribution(.fn, lower, upper)

  if (!is_discr) {
    value <- composite_try(.fn, lower, upper, n)
    if (is_true(is.finite(value))) {
      return(value)
    }
  }
  numerically_try(.fn, lower, upper, n, is_discr)
}

#' @export
#' @rdname statisticstools
#' @examples
#'
#' trapez_integral(x = seq(-2,2,.01),y = dnorm(x = seq(-2,2,.01)))
trapez_integral <- function(x, y) {
  trapez_integral_impl <- function(x, y) {
    n <- length(y)

    # `x` is assumed to be sorted increasingly (as after the `density()` call)
    (x[-1] - x[-n]) * (y[-1] + y[-n]) / 2
  }
  sum(trapez_integral_impl(x, y))
}

#' @export
#' @rdname statisticstools
#' @examples
#'
#' is_discrete_distribution(sample(1:10,100,T))
#' is_discrete_distribution(runif(100))
#'

is_discrete_distribution <- function(x, ...) {
  UseMethod("is_discrete_distribution")
}
#' @export
is_discrete_distribution.default <- function(x, ...) {
  generic_default()
}
#' @export
is_discrete_distribution.function <- function(x, lower, upper) {
  s <- runif(
    min(ncase(upper - lower), 100), ceiling(lower),
    trunc(upper)
  )
  s <- round(s)
  s <- s[s > lower & s < upper]
  is_true(all(x(s - runif(
    length(s), 0.01, 0.49
  )) == x(s -
            runif(
              length(s), 0.01, 0.49
            ))))
}

#' @export
is_discrete_distribution.numeric <- function(x) {
  x <- x[!is_nonnum(x)]
  xr <- round(x)
  is_true(all(x == xr))
}

#' @export
is_discrete_distribution.distrobj <- function(x) {
  x <- random_dist(dist = x, n = 1000)
  x <- x[!is_nonnum(x)]
  xr <- round(x)
  is_true(all(x == xr))
}

# optims ---


#' optim_tools
#'
#'
#' @name statisticstools
#' @rdname statisticstools
#' @keywords internal
NULL

#' @param x parametros
#' @param fun funcion a optimizar
#' @param ... otros
#'
#' @rdname statisticstools
#' @export

tsHessian <-
  function(x, fun, ...) {
    # Description:
    #   Computes two sided (TS) approximated Hessian

    # Source:
    #   A function borrowed from Kevin Sheppard's Matlab garch toolbox
    #   as implemented by Alexios Ghalanos in his rgarch package

    # Notes:
    #   optionally requires package Matrix (added as suggestion)

    # FUNCTION:

    # Settings:
    n <- length(x)
    fx <- fun(x, ...)
    eps <- .Machine$double.eps

    # Compute the stepsize:
    h <- eps ^ (1 / 3) *
      apply(
        as_df(x),
        1,
        FUN = function(z) {
          max(abs(z), 1.0e-2)
        }
      )
    xh <- x + h
    h <- xh - x
    ee <- diag(h) # Matrix(diag(h), sparse = TRUE)

    # Compute forward and backward steps:
    gp <- vector(mode = "numeric", length = n)

    for (i in 1:n) {
      gp[i] <- fun(x + ee[, i], ...)
    }
    gm <- vector(mode = "numeric", length = n)

    for (i in 1:n) {
      gm[i] <- fun(x - ee[, i], ...)
    }
    H <- h %*% t(h)
    Hm <- H
    Hp <- H

    # Compute double forward and backward steps:
    for (i in 1:n) {
      for (j in i:n) {
        Hp[i, j] <- fun(x + ee[, i] + ee[, j], ...)
        Hp[j, i] <- Hp[i, j]
        Hm[i, j] <- fun(x - ee[, i] - ee[, j], ...)
        Hm[j, i] <- Hm[i, j]
      }
    }

    # Compute the Hessian:
    for (i in 1:n) {
      for (j in i:n) {
        H[i, j] <- ((Hp[i, j] - gp[i] - gp[j] + fx + fx - gm[i] - gm[j] +
                       Hm[i, j]) / H[i, j]) / 2
        H[j, i] <- H[i, j]
      }
    }

    # Return Value:
    return(H)
  }

# non linear  ---

#' non_linear
#'
#'
#' @name statisticstools
#' @rdname statisticstools
#' @keywords internal
#'
#' @examples
#'
#' x <- runif(100,1,20)
#' y <- sqrt(x)*2-2
#'
#' data <- data.frame(x = x,y = y)
#'
#' exprs <-nls2expr(.data = data,x = x,y = y)
#'
#'
#' #plot(x = y,y = eval(exprs,envir = env_curr()))
#'
#' exprs <-fit_nls_curve(.data = data,x = x,y = y)
#'
#' #plot(x = y,y = eval(exprs,envir = env_curr()))
#'
#' obj <-fit_nonlinear(.data = data,x = x,y = y)
#'
#' #plot(x = y,y = predict(obj,newdata = x))
#'
NULL

#' @export
nls2expr <- function(.data, x, y) {
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)

  .nls_exprs <-
    list(
      ss_asymp = "a + ((b - a) * exp(((-exp(c)) * x)))",
      ss_asympoff = "a * (1 - exp(((-exp(b)) * (x - c))))",
      ss_asymporig = "a * (1 - exp(((-exp(b)) * x)))",
      ss_biexp = "(a * exp(((-exp(b)) * x))) + (c * exp(((-exp(e)) * x)))",
      ss_fol = "Dose * exp((a + b) - c) * (exp(-exp(a) * x) - exp(-exp(b) * x))/(exp(b) - exp(a))",
      ss_fpl = "a + (b - a)/(1 + exp((c - x)/d))",
      ss_gompertz = "a * exp(-b * c^x)",
      ss_logis = "a/(1 + exp((b - x)/c))",
      ss_micmen = "a * x/(b + x)",
      ss_weibull = "a - (b * exp(-(exp(c) * x^d)))",
      ss_bragg3 = "d * exp(-b * (x - e) ^ 2)",
      ss_bragg4 = "c + (d - c) * exp(-b * (x - e) ^ 2)",
      ss_e2 = "1 - exp(-exp(b * (x - e)))",
      ss_e3 = "d * (1 - exp(-exp(b * (x - e))))",
      ss_e4 = "c + (d - c) * (1 - exp(-exp(b * (x - e))))",
      ss_linear = "a + b * x",
      ss_loglogistic2 = "1 / (1 + exp(-b * (x - e)))",
      ss_loglogistic3 = "d / (1 + exp(-b * (x - e)))",
      ss_loglogistic4 = "c + (d - c) / (1 + exp(-b * (x - e)))",
      ss_poly2 = "a + b * x + c * (x ^ 2)",
      ss_power = "a * (x ^ b)"
    )
  .nls <- fit_nonlinear(.data = .data, x = !!x, y = !!y)


  .nls_frm <- do_try(formula(.nls)[[3]][[1]])

  .nls_frm

  if (inherits(.nls_frm, "try-error")) {
    return(NULL)
  }

  .nls_exprs <- parse(text = .nls_exprs[[.nls_frm]])
  .nls_coef <-
    sapply(coef(.nls), function(x) {
      round_sign(x)
    })
  .nls_env <- env_new(as_prls(.nls_coef))

  new_nls_expr <-
    do.call(substitute, list(as_l(.nls_exprs)[[1]], env = .nls_env))

  interp(new_nls_expr,x = x)
}
#' @export
fit_nls_curve <- function(.data, x, y, .times = 5, envir = env_call()) {
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)

  .data <- dplyr::select(.data, c(!!x, !!y)) %>%
    drop_na()

  res <- nls_get_inicial(.data = .data, x = !!x, y = !!y)

  res <- set_names(res, map_chr(res, ~ .x$nombre$expr))
  out <- vector(mode = "list", length = length(res))


  for (i in seq_along(res)) {
    tmp <- do_try(do_call(
      "fit_with_boots",
      list(
        .fn = nls,
        lhs = rlang::f_lhs(res[[i]]$formulas$expr),
        rhs = rlang::f_rhs(res[[i]]$formulas$expr),
        .data = .data,
        start = as.list(res[[i]]$start$expr),
        times = .times
      )
    ))
    if (!is.null(tmp)) {
      names(tmp) <- paste0(res[[i]]$nombre$expr, "_", seq_along(tmp))
    }

    out[[i]] <- tmp
  }
  out <- set_names(out, names(res))

  nls_params <-
    map(out, ~ map_dfr(.x, function(x) {
      r2_pseudo(object = x, .data = .data)
    }, .id = "model"))


  nls_params <-
    map_dfr(nls_params,
            ~ select_if(.x, is.numeric) %>% summarise_all(.funs = mean),
            .id = "group"
    ) %>%
    dplyr::select(!logLik) %>%
    drop_na() %>%
    mutate(across(
      contains("rsq"),
      ~ dsrb_max(
        x = .x,
        low = median(.x),
        high = 1,
        scale = TRUE
      )
    )) %>%
    mutate(across(
      !contains("rsq") & where(is_dblint),
      ~ dsrb_min(
        x = .x,
        low = min(.x),
        high = median(.x),
        scale = TRUE
      )
    )) %>%
    mutate(overall = dsrb_overall(across(where(is_dblint)))) %>%
    arrange(desc(overall))


  if (nrow(nls_params) == 0) {
    return(NULL)
  }

  .nls <- nls_params$group[1]

  out <- out[[.nls]]


  rset <- random_rset(dataset = .data, times = .times, rep = 5)
  rset <- map(rset$splits, rsample::analysis)

  metrics <-
    imap_dfr(out, function(x, y) {
      map_dfr(rset, ~ r2_pseudo(object = x, .data = .x) %>%
                mutate(
                  model = y,
                  logLik = NULL
                ))
    }) %>%
    group_by(model) %>%
    summarise_all(mean) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(across(
      contains("rsq"),
      ~ dsrb_max(
        x = .x,
        low = min(.x),
        high = max(.x),
        scale = TRUE
      )
    )) %>%
    mutate(across(
      !contains("rsq") & where(is_dblint),
      ~ dsrb_min(
        x = .x,
        low = min(.x),
        high = max(.x),
        scale = TRUE
      )
    )) %>%
    mutate(overall = dsrb_overall(across(where(is_dblint)))) %>%
    arrange(desc(overall)) %>%
    slice(1)

  out <- out[[metrics$model]]
  res <- res[[.nls]]

  nres <- do.call(substitute, list(res$formulas$expr, env = env_new(as_l(
    out$m$getPars()
  ))))

  nres <- rlang::f_rhs(nres)
  attr(nres,"call_info") <- res
  attr(nres, "metricas") <- metrics

  nres
}
#' @export
fit_nonlinear <- function(.data, x, y, envir = env_call()) {
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)

  .data <- dplyr::select(.data, c(!!x, !!y))

  .expr <- list(
    asymp = rlang::expr(ss_asymp(.x = !!x, a, b, c)),
    asympoff = rlang::expr(ss_asympoff(.x = !!x, a, b, c)),
    asymporig = rlang::expr(ss_asymporig(.x = !!x, a, b)),
    biexp = rlang::expr(ss_biexp(input = !!x, A1, lrc1, A2, lrc2)),
    bragg3 = rlang::expr(ss_bragg3(.x = !!x, b, d, e)),
    bragg4 = rlang::expr(ss_bragg4(.x = !!x, b, c, d, e)),
    e2 = rlang::expr(ss_e2(.x = !!x, b, e)),
    e3 = rlang::expr(ss_e3(.x = !!x, b, d, e)),
    e4 = rlang::expr(ss_e4(.x = !!x, b, c, d, e)),
    fol = rlang::expr(ss_fol(Dose, .x = !!x, a, b, c)),
    fpl = rlang::expr(ss_fpl(.x = !!x, a, b, c, d)),
    gompertz = rlang::expr(ss_gompertz(.x = !!x, a, b, c)),
    linear = rlang::expr(ss_linear(.x = !!x, a, b)),
    logis = rlang::expr(ss_logis(.x = !!x, a, b, c)),
    loglogistic2 = rlang::expr(ss_loglogistic2(.x = !!x, b, e)),
    loglogistic3 = rlang::expr(ss_loglogistic3(.x = !!x, b, d, e)),
    loglogistic4 = rlang::expr(ss_loglogistic4(.x = !!x, b, c, d, e)),
    micmen = rlang::expr(ss_micmen(.x = !!x, a, b)),
    poly2 = rlang::expr(ss_poly2(.x = !!x, a, b, c)),
    power = rlang::expr(ss_power(.x = !!x, a, b)),
    weibull = rlang::expr(ss_weibull(.x = !!x, a, b, c, d))
  )

  .expr <-
    map(
      .x = .expr,
      .f = ~ rlang::expr(do_try(stats::nls(!!y ~ !!.x, data = !!.data)))
    )

  .expr <- map(.x = .expr, .f = ~ eval(expr = .x, envir = envir))

  .expr <- compact(.expr)

  if (length(.expr) == 0) {
    return(NULL)
  }

  if (length(.expr) == 1) {
    return(.expr[[1]])
  }

  .best <-
    purrr::map_dfr(
      .x = .expr,
      .f = ~ r2_pseudo(.x, .data),
      .id = "group"
    ) %>%
    dplyr::select(!logLik) %>%
    drop_na() %>%
    mutate(across(
      contains("rsq"),
      ~ dsrb_max(
        x = .x,
        low = min(.x),
        high = max(.x),
        scale = TRUE
      )
    )) %>%
    mutate(across(
      !contains("rsq") & where(is_dblint),
      ~ dsrb_min(
        x = .x,
        low = min(.x),
        high = max(.x),
        scale = TRUE
      )
    )) %>%
    mutate(overall = dsrb_overall(across(where(is_dblint)))) %>%
    arrange(desc(overall)) %>%
    slice(1) %>%
    pull(group)

  .expr[[.best]]
}
nls_get_inicial <- function(.data, x, y) {
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)

  curvas <- get("curv_data", asNamespace("mgalda"))

  bw <- map(curvas$fn_obj, ~ body(.x)[[3]])
  bw <- set_names(bw, curvas$nombre)
  bw <- map(bw, ~ interp(.x, x = x, y = y))

  for (i in seq_along(bw)) {
    body(curvas$fn_obj[[i]])[[3]] <- bw[[i]]
  }

  curvas$res <-
    map(
      .x = seq_along(curvas$fn_obj),
      .f = ~ do_try(
        expr =
          optim(
            par = rep(1, length(curvas$args[[.x]]) - 1),
            fn = curvas$fn_obj[[.x]],
            method = curvas$omethod[[.x]],
            data = .data
          ),
        error = ier()
      )
    )

  curvas <-
    curvas[!map_lgl(.x = curvas$res, is_ier), ]

  curvas$res <-
    map2(curvas$res, curvas$args, function(x, y) {
      x <- x$par
      names(x) <- y[-length(y)]
      x
    })

  for (i in seq_along(curvas$res)) {
    curvas$curva[[i]] <- interp(obj_ = curvas$curva[[i]], x = x)
  }

  curvas$curva <-
    map(curvas$curva, ~ make_formula(lhs = y, rhs = .x, env = .data))

  map(
    .x = seq_row(curvas),
    ~ as_lazy(
      list(
        formulas = curvas$curva[[.x]],
        start = curvas$res[[.x]],
        nombre = curvas$nombre[[.x]]
      )
    )
  )
}
r2_pseudo <- function(object, .data) {
  if (!inherits(object, "nls") & !inherits(object, "drc")) {
    cat_stop("use only with \"nls\" or \"drc\" objects")
  }

  formula <- as_frm(summary(object)$formula)
  var_nm <- all.vars(formula)
  Y <- .data[[var_nm[1]]]
  df_mod <- summary(object)$df[2]
  sigma <- summary(object)$sigma

  ss_m <- sum((fitted(object) - mean(Y))^2) # Regression deviance
  ss_t <- deviance(lm(Y ~ 1)) # Total deviance about the mean
  ss_r <- deviance(object) # Residual deviance

  pseudo_rsq <-
    (ss_t - ss_r) / ss_t # rsq as in Schebenberger Eq. 5.24 (pag. 211)

  rsq <- ss_m / ss_t # rsq traditional as in Scebenberger Eq. 5.23

  ms_t <- ss_t / (length(Y) - 1)
  ms_r <- ss_r / df_mod
  rsq_adj <- 1 - ms_r / ms_t # Adjusted rsq

  MSE <- ss_r / df_mod
  RMSE <- sigma
  RRMSE <- RMSE / mean(Y)

  # rsq generalised: to be done for GLMs
  ll1 <- as_num(logLik(object))
  ll2 <- as_num(logLik(lm(Y ~ 1)))
  rsq_gen <-
    1 - exp(-2 / length(Y) * (ll1 - ll2)) # Schabenberger, 6.46 pag 343
  rsq_gen_resc <-
    rsq_gen / (1 - exp(2 / length(Y) * ll2)) # Schabenberger, 6.48 pag 344

  res <- tibble(
    pseudo_rsq = pseudo_rsq,
    rsq = rsq,
    rsq_adj = rsq_adj,
    rsq_gen = rsq_gen,
    logLik = as_num(logLik(object)),
    MSE = MSE,
    RMSE = RMSE,
    RRMSE = RRMSE
  )
  res
}

#' @export
ss_weibull <- # selfStart( ~ a - b * exp(-exp(c)*.x^d),
  selfStart(
    model = function(.x, a, b, c, d) {
      .expr1 <- exp(c)
      .expr3 <- .x^d
      .expr5 <- exp(-(ee <- .expr1 * .expr3))
      .value <- a - (De <- b * .expr5)
      .actualArgs <-
        as_l(match.call()[c("a", "b", "c", "d")])
      if (all(vapply(.actualArgs, is.name, NA))) {
        .grad <- array(
          0, c(length(.value), 4L),
          list(NULL, c("a", "b", "c", "d"))
        )
        .grad[, "a"] <- 1
        .grad[, "b"] <- -.expr5
        .grad[, "c"] <- c <- De * ee
        .grad[, "d"] <- c * log(.x)
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
      }
      .value
    },
    initial = function(mCall, data, LHS, ...) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      if (nrow(xy) < 5) {
        cat_stop("too few distinct input values to fit the Weibull growth model")
      }
      if (any(xy[["x"]] < 0)) {
        cat_stop("all 'x' values must be non-negative to fit the Weibull growth model")
      }
      Rasym <- NLSstRtAsymptote(xy)
      Lasym <- NLSstLfAsymptote(xy)
      pars <-
        coef(lm(log(-log((Rasym - y) / (Rasym - Lasym))) ~ log(x),
                data = xy, subset = x > 0
        ))
      setNames(
        coef(nls(
          y ~ cbind(1, -exp(-exp(c) * x^d)),
          data = xy,
          start = c(c = pars[[1L]], d = pars[[2L]]),
          algorithm = "plinear",
          ...
        ))[c(3, 4, 1, 2)],
        mCall[c("a", "b", "c", "d")]
      )
    },
    parameters =
      c("a", "b", "c", "d")
  )
#' @export
ss_logis <-
  selfStart(
    model = function(.x, a, b, c) {
      .expr1 <- b - .x
      .expr3 <- exp(.e2 <- .expr1 / c)
      .expr4 <- 1 + .expr3
      .value <- a / .expr4
      .actualArgs <-
        as_l(match.call()[c("a", "b", "c")])
      if (all(vapply(.actualArgs, is.name, NA))) {
        .expr10 <- .expr4^2
        .grad <-
          array(0, c(length(.value), 3L), list(NULL, c(
            "a",
            "b", "c"
          )))
        .grad[, "a"] <- 1 / .expr4
        .grad[, "b"] <-
          -(xm <- a * .expr3 / c / .expr10)
        .grad[, "c"] <- xm * .e2
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
      }
      .value
    },
    initial = function(mCall, data, LHS, ...) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      if (nrow(xy) < 4) {
        cat_stop("too few distinct .x values to fit a logistic model")
      }
      z <- xy[["y"]]
      rng <- range(z)
      dz <- diff(rng)
      z <- (z - rng[1L] + 0.05 * dz) / (1.1 * dz)
      xy[["z"]] <- log(z / (1 - z))
      aux <- coef(lm(x ~ z, xy))
      pars <- coef(nls(
        y ~ 1 / (1 + exp((b - x) / c)),
        data = xy,
        start = list(b = aux[[1L]], c = aux[[2L]]),
        algorithm = "plinear",
        ...
      ))
      setNames(pars[c(".lin", "b", "c")], mCall[c(
        "a",
        "b", "c"
      )])
    },
    parameters = c("a", "b", "c")
  )

#' @export
ss_asymp <-
  selfStart(
    function(.x, a, b, c) {
      .expr1 <- b - a
      .expr2 <- exp(c)
      .expr5 <- exp(((-.expr2) * .x))
      .value <- a + (.expr1 * .expr5)
      .actualArgs <-
        as_l(match.call()[c("a", "b", "c")])
      if (all(vapply(.actualArgs, is.name, NA))) {
        .grad <- array(
          0, c(length(.value), 3L),
          list(NULL, c("a", "b", "c"))
        )
        .grad[, "a"] <- 1 - .expr5
        .grad[, "b"] <- .expr5
        .grad[, "c"] <-
          -(.expr1 * (.expr5 * (.expr2 * .x)))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
      }
      .value
    },
    initial = function(mCall, data, LHS, ...) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      if (nrow(xy) < 3) {
        cat_stop("too few distinct .x values to fit an asymptotic regression model")
      }
      if (nrow(xy) > 3) {
        xy$ydiff <- abs(xy$y - NLSstRtAsymptote(xy))
        xy <- data.frame(xy)
        c <- log(-coef(lm(log(ydiff) ~ x, data = xy))[[2L]])
        ## This gives an estimate of the log (rate constant).  Use that
        ## with a partially linear nls algorithm
        pars <- coef(nls(
          y ~ cbind(
            1 - exp(-exp(c) * x),
            exp(-exp(c) * x)
          ),
          data = xy,
          start = list(c = c),
          algorithm = "plinear",
          ...
        ))
      } else {
        ## nrow(.) == 3
        ydiff <- diff(xy$y)
        if (prod(ydiff) <= 0) {
          cat_stop("cannot fit an asymptotic regression model to these data")
        }
        avg.resp <- xy$y
        frac <-
          (avg.resp[3L] - avg.resp[1L]) / (avg.resp[2L] - avg.resp[1L])
        xunique <- unique(xy$x)
        xdiff <- diff(xunique)
        if (xdiff[1L] == xdiff[2L]) {
          # equal spacing - can use a shortcut
          expmRd <- frac - 1
          rc <- -log(expmRd) / xdiff[1L]
          c <- log(rc)
          expmRx1 <- exp(-rc * xunique[1L])
          bma <- ydiff[1L] / (expmRx1 * (expmRd - 1))
          a <- avg.resp[1L] - bma * expmRx1
          pars <- c(
            c = c,
            a = a,
            b = bma + a
          )
        } else {
          cat_stop("too few observations to fit an asymptotic regression model")
        }
      }
      setNames(pars[c(2L, 3L, 1L)], mCall[c("a", "b", "c")])
    },
    parameters = c("a", "b", "c")
  )
#' @export
ss_asympoff <-
  selfStart(
    function(.x, a, b, c) {
      .expr1 <- exp(b)
      .expr3 <- .x - c
      .expr5 <- exp(((-.expr1) * .expr3))
      .expr6 <- 1 - .expr5
      .value <- a * .expr6
      .actualArgs <-
        as_l(match.call()[c("a", "b", "c")])
      if (all(vapply(.actualArgs, is.name, NA))) {
        .grad <-
          array(0, c(length(.value), 3L), list(NULL, c("a", "b", "c")))
        .grad[, "a"] <- .expr6
        .grad[, "b"] <- a * (.expr5 * (.expr1 * .expr3))
        .grad[, "c"] <- -(a * (.expr5 * .expr1))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
      }
      .value
    },
    initial = function(mCall, data, LHS, ...) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      if (nrow(xy) < 4) {
        cat_stop("too few distinct .x values to fit the 'asympOff' model")
      }
      xy$ydiff <- abs(xy$y - NLSstRtAsymptote(xy))
      xy <- data.frame(xy)
      b <-
        log(-coef(lm(log(ydiff) ~ x, data = xy))[[2L]]) # log( rate constant)
      pars <- coef(nls(
        y ~ cbind(1, exp(-exp(b) * x)),
        data = xy,
        start = list(b = b),
        algorithm = "plinear",
        ...
      ))
      setNames(
        c(pars[[2L]], pars[["b"]], exp(-pars[[1L]]) * log(-pars[[3L]] / pars[[2L]])),
        mCall[c("a", "b", "c")]
      )
    },
    parameters = c("a", "b", "c")
  )
#' @export
ss_asymporig <-
  selfStart(
    function(.x, a, b) {
      .expr1 <- exp(b)
      .expr4 <- exp(((-.expr1) * .x))
      .expr5 <- 1 - .expr4
      .value <- a * .expr5
      .actualArgs <- as_l(match.call()[c("a", "b")])
      if (all(vapply(.actualArgs, is.name, NA))) {
        .grad <-
          array(0, c(length(.value), 2L), list(NULL, c("a", "b")))
        .grad[, "a"] <- .expr5
        .grad[, "b"] <- a * (.expr4 * (.expr1 * .x))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
      }
      .value
    },
    initial = function(mCall, data, LHS) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      if (nrow(xy) < 3) {
        cat_stop("too few distinct .x values to fit the 'asympOrig' model")
      }
      ## get a preliminary estimate for A
      A0 <- NLSstRtAsymptote(xy)
      ## get a least squares estimate for log of the rate constant
      b <-
        log(abs(mean(log(1 - xy$y / A0) / xy$x, na.rm = TRUE)))
      ## use the partially linear form to converge quickly
      xy <- data.frame(xy)
      pars <- coef(nls(
        y ~ 1 - exp(-exp(b) * x),
        data = xy,
        start = list(b = b),
        algorithm = "plinear"
      ))
      setNames(
        pars[c(".lin", "b")],
        mCall[c("a", "b")]
      )
    },
    parameters = c("a", "b")
  )

#' @export
ss_biexp <-
  selfStart(
    function(.x, a, b, c, e) {
      .expr1 <- exp(b)
      .expr4 <- exp(((-.expr1) * .x))
      .expr6 <- exp(e)
      .expr9 <- exp(((-.expr6) * .x))
      .value <- (a * .expr4) + (c * .expr9)
      .actualArgs <-
        as_l(match.call()[c("a", "b", "c", "e")])
      if (all(vapply(.actualArgs, is.name, NA))) {
        .grad <- array(
          0, c(length(.value), 4L),
          list(NULL, c("a", "b", "c", "e"))
        )
        .grad[, "a"] <- .expr4
        .grad[, "b"] <- -(a * (.expr4 * (.expr1 * .x)))
        .grad[, "c"] <- .expr9
        .grad[, "e"] <- -(c * (.expr9 * (.expr6 * .x)))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
      }
      .value
    },
    initial = function(mCall, data, LHS, ...) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      if (nrow(xy) < 5) {
        cat_stop("too few distinct input values to fit a biexponential")
      }
      ndistinct <- nrow(xy)
      nlast <-
        max(3, round(ndistinct / 2)) # take at least half the data
      dlast <- xy[(ndistinct + 1 - nlast):ndistinct, ]
      pars2 <- coef(lm(log(y) ~ x, data = dlast))
      lrc2 <- log(abs(pars2[2L])) # log of the slope
      xy[["res"]] <-
        xy[["y"]] - exp(pars2[1L]) * exp(-exp(lrc2) * xy[["x"]])
      dfirst <- xy[1L:(ndistinct - nlast), ]
      pars1 <- coef(lm(log(abs(res)) ~ x, data = dfirst))
      lrc1 <- log(abs(pars1[2L]))
      pars <-
        coef(nls(
          y ~ cbind(exp(-exp(lrc1) * x), exp(-exp(lrc2) * x)),
          data = xy,
          start = list(lrc1 = lrc1, lrc2 = lrc2),
          algorithm = "plinear",
          ...
        ))
      setNames(
        pars[c(3L, 1L, 4L, 2L)],
        mCall[c("a", "b", "c", "e")]
      )
    },
    parameters = c("a", "b", "c", "e")
  )

#' @export
ss_fol <- selfStart(
  model =
    function(Dose, .x, a, b, c) {
      .expr4 <- Dose * exp((a + b) - c)
      .expr5 <- exp(a)
      .expr8 <- exp(-.expr5 * .x)
      .expr9 <- exp(b)
      .expr12 <- exp(-.expr9 * .x)
      .expr14 <- .expr4 * (.expr8 - .expr12)
      .expr15 <- .expr9 - .expr5
      .expr16 <- .expr14 / .expr15
      .expr23 <- .expr15^2
      .value <- .expr16
      .actualArgs <-
        as_l(match.call()[c("a", "b", "c")])
      if (all(vapply(.actualArgs, is.name, NA))) {
        .grad <-
          array(0, c(length(.value), 3L), list(NULL, c("a", "b", "c")))
        .grad[, "a"] <-
          (.expr14 - .expr4 * (.expr8 * (.expr5 * .x))) /
          .expr15 + .expr14 * .expr5 / .expr23
        .grad[, "b"] <-
          (.expr14 + .expr4 * (.expr12 * (.expr9 * .x))) /
          .expr15 - .expr14 * .expr9 / .expr23
        .grad[, "c"] <- -.expr16
        dimnames(.grad) <- list(NULL, .actualArgs) # extra
        attr(.value, "gradient") <- .grad
      }
      .value
    },
  initial = function(mCall, data, LHS, ...) {
    data <- data.frame(data)
    resp <- eval(LHS, data)
    input <- eval(mCall[["input"]], data)
    Dose <- eval(mCall[["Dose"]], data)
    n <- length(resp)
    if (length(input) != n) {
      cat_stop("must have length of response = length of second argument to 'SSfol'")
    }
    if (n < 4) {
      cat_stop("must have at least 4 observations to fit an 'SSfol' model")
    }
    rmaxind <- which.max(resp)
    a <- if (rmaxind == n) {
      -2.5
    } else {
      log((log(resp[rmaxind]) - log(resp[n])) / (input[n] - input[rmaxind]))
    }
    cond.lin <-
      nls(
        resp ~ (exp(-input * exp(a)) - exp(-input * exp(b))) * Dose,
        data = list(
          resp = resp,
          input = input,
          Dose = Dose,
          a = a
        ),
        start = list(b = a + 1),
        algorithm = "plinear",
        ...
      )
    pars <- coef(cond.lin)
    cond.lin <- nls(
      resp ~ (Dose * (exp(-input * exp(
        a
      )) -
        exp(-input * exp(
          b
        )))) / (exp(b) - exp(a)),
      data = data.frame(list(
        resp = resp,
        input = input,
        Dose = Dose
      )),
      start = list(b = pars[["b"]], a = a),
      algorithm = "plinear",
      ...
    )
    pars <- coef(cond.lin)
    b <- pars[["b"]]
    a <- pars[["a"]]
    setNames(
      c(a, b, a + b - log(pars[[3L]])),
      c("a", "b", "c")
    )
  },
  parameters = c("a", "b", "c")
)
#' @export
ss_fpl <-
  selfStart(
    model = function(.x, a, b, c, d) {
      .expr1 <- b - a
      .expr2 <- c - .x
      .expr4 <- exp(.e2 <- .expr2 / d)
      .expr5 <- 1 + .expr4
      .value <- a + .expr1 / .expr5
      .actualArgs <-
        as_l(match.call()[c("a", "b", "c", "d")])
      if (all(vapply(.actualArgs, is.name, NA))) {
        .expr8 <- 1 / .expr5
        .expr13 <- .expr5^2
        .grad <- array(
          0, c(length(.value), 4L),
          list(NULL, c("a", "b", "c", "d"))
        )
        .grad[, "a"] <- 1 - .expr8
        .grad[, "b"] <- .expr8
        .grad[, "c"] <-
          -(xm <- .expr1 * .expr4 / d / .expr13)
        .grad[, "d"] <- xm * .e2
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
      }
      .value
    },
    initial = function(mCall, data, LHS, ...) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      if (nrow(xy) < 5) {
        cat_stop("too few distinct input values to fit a four-parameter logistic")
      }
      ## convert the response to a proportion (i.e. contained in (0,1))
      rng <- range(xy$y)
      drng <- diff(rng)
      xy$prop <- (xy$y - rng[1L] + 0.05 * drng) / (1.1 * drng)
      ## inverse regression of the x values on the proportion
      ir <-
        as_vct(coef(lm(x ~ I(log(
          prop / (1 - prop)
        )), data = xy)))
      pars <-
        as_vct(coef(
          nls(
            y ~ cbind(1, 1 / (1 + exp((c - x) / exp(lscal)))),
            data = xy,
            start = list(
              c = ir[1L],
              lscal = log(abs(ir[2L]))
            ),
            algorithm = "plinear",
            ...
          )
        ))
      setNames(c(pars[3L], pars[3L] + pars[4L], pars[1L], exp(pars[2L])),
               nm = mCall[c("a", "b", "c", "d")]
      )
    },
    parameters = c("a", "b", "c", "d")
  )
#' @export
ss_micmen <-
  selfStart(
    model =
      function(.x, a, b) {
        .expr1 <- a * .x
        .expr2 <- b + .x
        .value <- .expr1 / .expr2
        .actualArgs <- as_l(match.call()[c("a", "b")])
        if (all(vapply(.actualArgs, is.name, NA))) {
          .grad <- array(0, c(length(.value), 2L), list(NULL, c("a", "b")))
          .grad[, "a"] <- .x / .expr2
          .grad[, "b"] <- -(.expr1 / .expr2^2)
          dimnames(.grad) <- list(NULL, .actualArgs)
          attr(.value, "gradient") <- .grad
        }
        .value
      },
    initial = function(mCall, data, LHS, ...) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      if (nrow(xy) < 3) {
        cat_stop("too few distinct input values to fit a Michaelis-Menten model")
      }
      ## take the inverse transformation
      pars <- as_vct(coef(lm(1 / y ~ I(1 / x), data = xy)))
      ## use the partially linear form to converge quickly
      pars <- coef(nls(
        y ~ x / (b + x),
        data = xy,
        start = list(b = abs(pars[2L] / pars[1L])),
        algorithm = "plinear",
        ...
      ))
      setNames(
        pars[c(".lin", "b")],
        mCall[c("a", "b")]
      )
    },
    parameters = c("a", "b")
  )

#' @export
ss_gompertz <- selfStart(
  model =
    function(.x, a, b, c) {
      .expr2 <- c^.x
      .expr4 <- exp(-b * .expr2)
      .value <- a * .expr4
      .actualArgs <- as_l(match.call()[c("a", "b", "c")])
      if (all(vapply(.actualArgs, is.name, NA))) {
        .grad <- array(
          0, c(length(.value), 3L),
          list(NULL, c("a", "b", "c"))
        )
        .grad[, "a"] <- .expr4
        .grad[, "b"] <- -a * (.expr4 * .expr2)
        .grad[, "c"] <-
          -a * (.expr4 * (b * (c^(.x - 1) * .x)))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
      }
      .value
    },
  initial = function(mCall, data, LHS, ...) {
    xy <- sortedXyData(mCall[[".x"]], LHS, data)
    if (nrow(xy) < 4) {
      cat_stop("too few distinct input values to fit the Gompertz model")
    }
    xyL <- xy
    xyL$y <- log(abs(xyL$y))
    pars <- NLSstAsymptotic(xyL)
    pars <- coef(nls(
      y ~ exp(-b * c^x),
      data = xy,
      start = c(
        b = pars[["b1"]],
        c = exp(-exp(pars[["lrc"]]))
      ),
      algorithm = "plinear",
      ...
    ))
    setNames(
      pars[c(".lin", "b", "c")],
      mCall[c("a", "b", "c")]
    )
  },
  parameters =
    c("a", "b", "c")
)

#' @export
ss_bragg3 <-
  selfStart(
    model = function(.x, b, d, e) {
      d * exp(-b * (.x - e)^2)
    },
    initial = function(mCall, LHS, data) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      x <- xy[, "x"]
      y <- xy[, "y"]

      d <- max(y)
      e <- x[which.max(y)]

      ## Linear regression on pseudo-y and pseudo-x
      pseudoY <- log((y + 0.0001) / d)
      pseudoX <- (x - e)^2
      coefs <- coef(lm(pseudoY ~ pseudoX - 1))
      b <- -coefs[1]
      start <- c(b, d, e)
      names(start) <- mCall[c("b", "d", "e")]
      start
    },
    parameters = c("b", "d", "e")
  )

#' @export
ss_bragg4 <-
  selfStart(
    model = function(.x, b, c, d, e) {
      c + (d - c) * exp(-b * (.x - e)^2)
    },
    initial = function(mCall, LHS, data) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      x <- xy[, "x"]
      y <- xy[, "y"]

      d <- max(y)
      c <- min(y) * 0.95
      e <- x[which.max(y)]


      ## Linear regression on pseudo-y and pseudo-x
      pseudoY <- log(((y + 0.0001) - c) / d)
      pseudoX <- (x - e)^2
      coefs <- coef(lm(pseudoY ~ pseudoX - 1))
      b <- -coefs[1]
      start <- c(b, c, d, e)
      names(start) <- mCall[c("b", "c", "d", "e")]
      start
    },
    parameters = c("b", "c", "d", "e")
  )

#' @export
ss_loglogistic4 <-
  selfStart(
    model = function(.x, b, c, d, e) {
      x <- .x
      c + (d - c) / (1 + exp(-b * (x - e)))
    },
    initial = function(mCall, LHS, data) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      x <- xy[, "x"]
      y <- xy[, "y"]
      d <- max(y) * 1.05
      c <- min(y)

      ## Linear regression on pseudo y values
      pseudoY <- log((d - y) / (y + 0.00001 - c))
      coefs <- coef(lm(pseudoY ~ x))
      k <- coefs[1]
      b <- -coefs[2]
      e <- k / b
      value <- c(b, c, d, e)
      names(value) <- mCall[c("b", "c", "d", "e")]
      value
    },
    parameters = c("b", "c", "d", "e")
  )

#' @export
ss_loglogistic3 <- selfStart(
  model = function(.x, b, d, e) {
    x <- .x
    d / (1 + exp(-b * (x - e)))
  },
  initial = function(mCall, LHS, data) {
    xy <- sortedXyData(mCall[[".x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]
    # y <- beetGrowth$weightFree; x <- beetGrowth$DAE
    d <- max(y) * 1.05
    # print(d)
    ## Linear regression on pseudo y values
    pseudoY <- log((d - y) / (y + 0.00001))
    coefs <- coef(lm(pseudoY ~ x))
    k <- coefs[1]
    b <- -coefs[2]
    e <- k / b
    value <- c(b, d, e)
    names(value) <- mCall[c("b", "d", "e")]
    value
  },
  parameters = c("b", "d", "e")
)

#' @export
ss_loglogistic2 <- selfStart(
  model = function(.x, b, e) {
    x <- .x
    1 / (1 + exp(-b * (x - e)))
  },
  initial = function(mCall, LHS, data) {
    xy <- sortedXyData(mCall[[".x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]
    d <- 1
    ## Linear regression on pseudo y values
    pseudoY <- log((d - y) / (y + 0.00001))
    coefs <- coef(lm(pseudoY ~ x))
    k <- coefs[1]
    b <- -coefs[2]
    e <- k / b
    value <- c(b, e)
    names(value) <- mCall[c("b", "e")]
    value
  },
  parameters = c("b", "e")
)

#' @export
ss_linear <-
  selfStart(
    model = function(.x, a, b) {
      a + b * .x
    },
    initial = function(mCall, LHS, data) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      lmFit <- lm((xy[, "y"]) ~ xy[, "x"])
      coefs <- coef(lmFit)
      a <- coefs[1]
      b <- coefs[2]
      value <- c(a, b)
      names(value) <- mCall[c("a", "b")]
      value
    },
    parameters = c("a", "b")
  )

#' @export
ss_e4 <-
  selfStart(
    model = function(.x, b, c, d, e) {
      x <- .x
      c + (d - c) * (1 - exp(-exp(b * (x - e))))
    },
    initial = function(mCall, LHS, data) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      x <- xy[, "x"]
      y <- xy[, "y"]

      y <- as_num(tapply(y, factor(x), mean))
      x <- as_num(tapply(x, factor(x), mean))
      mod <- nls(y ~ NLS.L4(x, b, c, d, e))
      value <- as_num(coef(mod))

      names(value) <- mCall[c("b", "c", "d", "e")]
      value
    },
    parameters = c("b", "c", "d", "e")
  )

#' @export
ss_e3 <- selfStart(
  model = function(.x, b, d, e) {
    x <- .x
    d * (1 - exp(-exp(b * (x - e))))
  },
  initial = function(mCall, LHS, data) {
    xy <- sortedXyData(mCall[[".x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    y <- as_num(tapply(y, factor(x), mean))
    x <- as_num(tapply(x, factor(x), mean))
    mod <- nls(y ~ NLS.L3(x, b, d, e))
    value <- as_num(coef(mod))

    names(value) <- mCall[c("b", "d", "e")]
    value
  },
  parameters = c("b", "d", "e")
)

#' @export
ss_e2 <- selfStart(
  model = function(.x, b, e) {
    x <- .x
    1 - exp(-exp(b * (x - e)))
  },
  initial = function(mCall, LHS, data) {
    xy <- sortedXyData(mCall[[".x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    ## Linear regression on pseudo y values
    y <- as_num(tapply(y, factor(x), mean))
    x <- as_num(tapply(x, factor(x), mean))
    mod <- nls(y ~ NLS.L2(x, b, e))
    value <- as_num(coef(mod))

    names(value) <- mCall[c("b", "e")]
    value
  },
  parameters = c("b", "e")
)

#' @export
ss_power <-
  selfStart(
    model = function(.x, a, b) {
      a * (.x^b)
    },
    initial = function(mCall, LHS, data) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      lmFit <- lm(log(xy[, "y"]) ~ log(xy[, "x"]))
      coefs <- coef(lmFit)
      a <- exp(coefs[1])
      b <- coefs[2]
      value <- c(a, b)
      names(value) <- mCall[c("a", "b")]
      value
    },
    parameters = c("a", "b")
  )

#' @export
ss_poly2 <-
  selfStart(
    model = function(.x, a, b, c) {
      a + b * .x + c * (.x^2)
    },
    initial = function(mCall, LHS, data) {
      xy <- sortedXyData(mCall[[".x"]], LHS, data)
      lmFit <- lm((xy[, "y"]) ~ xy[, "x"] + I(xy[, "x"]^2))
      coefs <- coef(lmFit)
      a <- coefs[1]
      b <- coefs[2]
      c <- coefs[3]
      value <- c(a, b, c)
      names(value) <- mCall[c("a", "b", "c")]
      value
    },
    parameters = c("a", "b", "c")
  )


# round_any ---



#' round_any
#'
#'
#' @name statisticstools
#' @rdname statisticstools
#' @export
#'
#' @examples
#'
#' round_any(135, 10)
#' round_any(135, 100)
#' round_any(135, 25)
#' round_any(135, 10, floor)
#' round_any(135, 100, floor)
#' round_any(135, 25, floor)
#' round_any(135, 10, ceiling)
#' round_any(135, 100, ceiling)
#' round_any(135, 25, ceiling)
#'
#' round_any(Sys.time() + 1:10, 5)
#' round_any(Sys.time() + 1:10, 5, floor)
#' round_any(Sys.time(), 3600)
#'

round_any <- function(x, accuracy, f = round) {
  UseMethod("round_any")
}

#' @export
round_any.numeric <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' @export
round_any.POSIXct <- function(x, accuracy, f = round) {
  tz <- format(x[1], "%Z")
  xr <- round_any(as_num(x), accuracy, f)
  as.POSIXct(xr, origin = "1970-01-01 00:00.00 UTC", tz = tz)
}

#' @export
significant_places <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }
  z <- diff(r <- range(x[!mgalda::is_nonnum(x)]))
  if (z == 0) {
    z <- abs(r[1L])
  }
  if (z == 0) {
    z <- 1
  }

  3 - floor(log10(z))
}

#' @export
round_sign <- function(x) {
  round(x, significant_places(x))
}

#' @export
round_up <- function(x, digits) {
  xr <- round(x, digits = digits)
  ifelse(xr < x, xr + 10^(-digits), xr)
}

#' @export
round_digit <- function(d) {
  .rdig <- function(x) {
    as.integer(floor(log10(abs(x))))
  }
  if (d > 1) {
    round(d)
  } else {
    round(d, -.rdig(d))
  }
}

# otras --

#' statisticstools
#'
#'
#' @name statisticstools
#' @rdname statisticstools
#' @keywords internal
#'
#' @examples
#' x <- runif(100,-20,20)
#' center_scale(x)
#' cov_alt(x = x,runif(100))
#' xout<-create_outliers(x = x)
#' d2sigmoid(x = x)
#' dsigmoid(x = x)
#' entropy(x = x[x>0])
#' find_near(x = x,y = runif(100,-10,10))
#' geo_mean(x[x>0])
#' gini(x)
#' heaviside(x)
#' iqrize(x)
#' kurtosis(x)
#' logdesv(x,y = x*.9)
#' moment(x = x,order = 2)
#' outlier_locs(x = xout)
#' precision(x)
#' qsr(x)
#' quartile_skewness(x)
#' rescalar_outliers(xout)
#' sigmoid(x)
#' skewness(x)
#'

NULL


#' @export
precision <- function(x) {
  x <- unique(x)
  # ignore NA and Inf/-Inf
  x <- x[is.finite(x)]

  if (length(x) <= 1) {
    return(1)
  }

  smallest_diff <- min(diff(sort(x)))
  if (smallest_diff < sqrt(.Machine$double.eps)) {
    1
  } else {
    # Never return precision bigger than 1
    pmin(10^(floor(log10(smallest_diff)) - 1), 1)
  }
}

#' @export
center_scale <-
  function(x,
           center = TRUE,
           scale = TRUE,
           keep_attr = FALSE) {
    ffin <- if (is_dblint(x)) {
      as_num
    } else if (is_df(x)) {
      as_tbl
    } else {
      identify
    }

    x <- as.matrix(x)
    nc <- ncol(x)
    if (is.logical(center)) {
      if (center) {
        center <- colMeans(x, na.rm = TRUE)
        x <- sweep(x, 2L, center, check.margin = FALSE)
      }
    } else {
      if (!is.numeric(center)) {
        center <- as_num(center)
      }
      if (length(center) == nc) {
        x <- sweep(x, 2L, center, check.margin = FALSE)
      } else {
        cat_stop("length of 'center' must equal the number of columns of 'x'")
      }
    }
    if (is.logical(scale)) {
      if (scale) {
        f <- function(v) {
          v <- v[!is.na(v)]
          sqrt(sum(v^2) / max(1, length(v) - 1L))
        }
        scale <- apply(x, 2L, f)
        x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
      }
    } else {
      if (!is.numeric(scale)) {
        scale <- as_num(scale)
      }
      if (length(scale) == nc) {
        x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
      } else {
        cat_stop("length of 'scale' must equal the number of columns of 'x'")
      }
    }
    if (keep_attr) {
      if (is.numeric(center)) {
        attr(x, "scaled:center") <- center
      }
      if (is.numeric(scale)) {
        attr(x, "scaled:scale") <- scale
      }
    }
    ffin(x)
  }
#' @export
qsr <- function(x,na.rm =FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(NA)
  }

  qs <- quantile(x, c(0.8, .2),na.rm = na.rm)
  sum(x[x > qs[1]],na.rm = na.rm) / sum(x[x < qs[2]],na.rm = na.rm)
}
#' @export
iqrize <- function(x) {
  med <- median(x)
  iqr <- IQR(x, na.rm = TRUE)
  (x - med) / iqr
}
#' @export
amplitude <- function(x,na.rm = FALSE){
  diff(range(x,na.rm = na.rm))
}

#' @export
pseudo_rsq_model <- function(object, dataset) {
  if (!inherits(object, "workflow")) {
    cat_stop("'object' debe ser clase clase workflow, tidymodels")
  }

  x <- workflows::extract_fit_parsnip(x = object)
  rec <- workflows::extract_preprocessor(x = object)


  if (!inherits(x$fit, c("multinom", "glm"))) {
    cat_stop("'object' debe ser clase modelo multinom o glm")
  }

  ycol <- rec$var_info$variable[rec$var_info$role == "outcome"]

  null_data <- dataset[, as_chr(ycol), drop = FALSE]

  null_formula <- paste(ycol, " ~ 1", sep = "")

  null_formula <-
    as_frm(str2lang(null_formula), env = env_curr())

  l_full <- logLik(x$fit)
  d_full <- -2 * l_full

  null_mod <-
    fit(
      object = x$spec,
      data = null_data,
      formula = null_formula
    )

  l_base <- logLik(null_mod$fit)

  if (inherits(x = x$fit, what = "multinom")) {
    edf <- x$fit$edf
  } else if (inherits(x = x$fit, what = "glm")) {
    edf <- x$fit$rank
  }

  d_base <- -2 * l_base # deviance(update(x, ~1))
  g2 <- -2 * (l_base - l_full)

  # n <- if(length(weights(x)))
  #   sum(weights(x))
  # else
  n <-
    attr(l_full, "nobs") # alternative: n <- dim(x$residuals)[1]

  # mc_fadden
  mc_fadden <- 1 - (l_full / l_base)
  # adjusted to penalize for the number of predictors (k) in the model
  mc_fadden_adj <- 1 - ((l_full - edf) / l_base)

  # nagelkerke / CraggUhler
  nagelkerke <-
    (1 - exp((d_full - d_base) / n)) / (1 - exp(-d_base / n))

  # CoxSnell / Maximum Likelihood R2
  cox_snell <- 1 - exp(-g2 / n)

  res <- c(
    mc_fadden = mc_fadden,
    mc_fadden_adj = mc_fadden_adj,
    cox_snell = cox_snell,
    nagelkerke = nagelkerke,
    AldrichNelson = NA,
    VeallZimmermann = NA,
    Efron = NA,
    McKelveyZavoina = NA,
    Tjur = NA
  )

  if (inherits(x = x$fit, what = "glm")) {
    fam <- x$fit$family$family
    link <- x$fit$family$link
    y <- x$fit$y

    s2 <- switch(link,
                 probit = 1,
                 logit = pi^2 / 3,
                 NA
    )

    res["AldrichNelson"] <- g2 / (g2 + n)

    res["VeallZimmermann"] <-
      res["AldrichNelson"] * (2 * l_base - n) / (2 * l_base)

    y.hat <-
      predict(x$fit, type = "link")

    y.hat.resp <-
      predict(x$fit, type = "response")

    sse <- sum((y.hat - mean(y.hat))^2)
    res["McKelveyZavoina"] <- sse / (n * s2 + sse)

    # EfronR2
    res["Efron"] <- (1 - (sum((y - y.hat.resp)^2)) /
                       (sum((y - mean(
                         y
                       ))^2)))

    # Tjur's D
    # compare with binomTools::Rsq.glm()
    if (identical(fam, "binomial")) {
      res["Tjur"] <-
        unname(diff(tapply(y.hat.resp, y, mean, na.rm = TRUE)))
    }
  }
  res[!is.na(res)]
}
#' @export
entropy <- function(x,na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(NA)
  }

  x <- x / sum(x)
  -sum(ifelse(x > 0, x * log2(x), 0))
}
#' @export
gini <- function(x,na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(NA)
  }

  n <- length(x)
  x <- sort(x)
  gini <- sum(x * 1L:n)
  (2 * gini / sum(x) - (n + 1L)) / n
}
#' @export
geo_mean <- function(x, na.rm = FALSE) {
  if (any(x[!is.na(x)] <= 0)) {
    return(0)
  }
  exp(sum(log(x), na.rm = na.rm) / sum(!is.na(x)))
}
#' @export
skewness <- function(x,na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(NA)
  }

  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3 / 2)
}
#' @export
quartile_skewness <- function(x) {
  q <- quantile(x, c(.25, .50, .75))

  ((q[3] - q[2]) - (q[2] - q[1])) / (q[3] - q[1])
}
#' @export
kurtosis <- function(x,na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(NA)
  }

  n <- length(x)
  n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}
#' @export
cov_alt <- function(x, y) {
  n <- length(x)
  (sum(x * y) / (n - 1)) - (n / (n - 1)) * mean(x) * mean(y)
}
#' @export
heaviside <- function(x, a = 0) {
  (sign(x - a) + 1) / 2
}
#' @export
outlier_locs <-
  function(x, cutoff = 0.95) {
    x <- as_df(x)
    out <-
      OutlierDetection::maha(x, cutoff = cutoff)$"Location of Outlier"

    if (ncol(x) > 1) {
      out2 <-
        OutlierDetection::disp(x, cutoff = cutoff)$"Location of Outlier"
      out <- base::intersect(out2, out)
    }
    out
  }
#' @export
create_outliers <- function (x, na.rm = TRUE,size = .01) {
  assert_engine(is_number(size),is_prob(size),severity = "stop")

  n <- size_n(x)
  nn <- ceiling(n * size)
  xiqr <- IQR(x, na.rm = na.rm)
  minx <- min(x, na.rm = na.rm)
  maxx <- max(x, na.rm = na.rm)
  nmin <- minx - xiqr * runif(nn, 3, 10)
  nmax <- maxx + xiqr * runif(nn, 3, 10)
  new_x <- c(nmax, nmin, x)
  sample(new_x,n,F)
}

#' @export
find_near <- function(x, y) {
  if (!is_df(x)) {
    x <- matrix(x, ncol = 1)
  }
  if (!is_df(y)) {
    y <- matrix(y, ncol = 1)
  }
  as_int(RANN::nn2(data = x, query = y, k = 1)$nn.idx)
}
#' @export
moment <- function(x,
                   order = 1,
                   center = FALSE,
                   absolute = FALSE,
                   na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (center) {
    x <- x - mean(x)
  }
  if (absolute) {
    x <- abs(x)
  }
  sum(x^order) / length(x)
}
#' @export
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}
#' @export
dsigmoid <- function(x) {
  sigmoid(x) * (1 - sigmoid(x))
}
#' @export
d2sigmoid <- function(x) {
  dsigmoid(x) * (1 - 2 * sigmoid(x))
}
#' @export
logdesv <- function(x, y, na_rm = TRUE) {
  if (na_rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  mean(abs(log(x / y)))
}
#' @keywords  internal
zero_range <- function(x, tol = 1000 * .Machine$double.eps) {
  if (length(x) == 1) {
    return(TRUE)
  }
  if (length(x) != 2) {
    cat_stop("x must be length 1 or 2")
  }
  if (any(is.na(x))) {
    return(NA)
  }
  if (x[1] == x[2]) {
    return(TRUE)
  }
  if (all(is.infinite(x))) {
    return(FALSE)
  }
  m <- min(abs(x))
  if (m == 0) {
    return(FALSE)
  }
  abs((x[1] - x[2]) / m) < tol
}
#' @export
correlate_all <-
  function(data,
           use = "everything",
           method = c("pearson", "kendall", "spearman")) {
    cor_cramer <- function(x, y = NULL, digits = 4, ...) {
      cv <- NULL

      if (is.factor(x)) {
        x <- as.vector(x)
      }
      if (is.factor(y)) {
        y <- as.vector(y)
      }
      if (is.vector(x) & is.vector(y)) {
        .n <- length(x)
        chi_sq <-
          suppressall(chisq.test(x, y, correct = FALSE, ...)$statistic)
        phi <- chi_sq / .n
        .row <- length(unique(x))
        .c <- length(unique(y))
        cv <- sqrt(phi / min(.row - 1, .c - 1))
      }

      if (is.matrix(x)) {
        x <- as.table(x)
      }
      if (is.table(x)) {
        .table <- x
        .n <- sum(.table)
        chi_sq <-
          suppressall(chisq.test(.table, correct = FALSE, ...)$statistic)
        phi <- chi_sq / .n
        .row <- nrow(x)
        .c <- ncol(x)
        cv <- sqrt(phi / min(.row - 1, .c - 1))
      }

      signif(as.numeric(cv), digits = digits)
    }
    nms <- names(data)
    res <- data.frame()
    for (j in nms) {
      w <- map_dfc(
        .x = data,
        .f = ~ if (is_dblint(.x) & is_dblint(data[[j]])) {
          cor(data[[j]], .x, use = use, method = method)
        } else if (is_fctchr(.x) & is_fctchr(data[[j]])) {
          cor_cramer(data[[j]], .x)
        } else if (is_fctchr(.x) & is_dblint(data[[j]])) {
          summary(lm(y ~ x, data = tibble(y = data[[j]], x = .x)))$r.squared
        } else if (is_dblint(.x) & is_fctchr(data[[j]])) {
          summary(lm(y ~ x, data = tibble(x = data[[j]], y = .x)))$r.squared
        }
      )
      w <- as.data.frame(w)
      rownames(w) <- j
      res <- bind_rows(res, w)
    }
    res <- as.matrix(res[, rownames(res)])

    assert_engine(ncol(res) == nrow(res),severity = "stop")
    res
  }

#' Mcfadden's Pseudo R-sqaured
#'
#' @param x a data matrix.
#' @param lf_alt Observed logistic factocbind_wrapsrs.
#' @param lf_null Null logistic factors.
#' @export
pseudo_rsq <- function(x, lf_alt, lf_null = NULL) {
  .mcfadden_rsq <- function(snp, p1, p0) {
    if (missing(snp)) {
      cat_stop("`snp` is required!")
    }
    if (missing(p1)) {
      cat_stop("`p1` is required!")
    }
    if (missing(p0)) {
      cat_stop("`p0` is required!")
    }

    # check for p's = 0 or 1
    indx <- (p0 != 0) & (p0 != 1) & (p1 != 0) & (p1 != 1)
    p1 <- p1[indx]
    p0 <- p0[indx]
    snp <- snp[indx]

    llalt <- sum(snp * log(p1) + (2 - snp) * log(1 - p1))
    llnull <- sum(snp * log(p0) + (2 - snp) * log(1 - p0))

    1 - (llalt / llnull)
  }
  if (missing(x)) {
    cat_stop("`x` is required!")
  }
  if (missing(lf_alt)) {
    cat_stop("`lf_alt` is required!")
  }
  if (!is.matrix(x)) {
    if (is.numeric(x)) {
      x <- as.matrix(x)
    }
  }
  if (!is.matrix(lf_alt)) {
    if (is.numeric(lf_alt)) {
      lf_alt <- as.matrix(lf_alt)
    }
  }


  if (is.null(lf_null)) {
    lf_null <- matrix(1, ncol(x), 1)
  }

  m <- nrow(x)

  f_alt <- lfa::af(x, lf_alt)
  f_null <- lfa::af(x, lf_null)

  sapply(1:m, function(i) {
    .mcfadden_rsq(
      x[i, ],
      f_alt[i, ],
      f_null[i, ]
    )
  })
}

#' Efron's Pseudo R-sqaured
#'
#' @param x a data matrix.
#' @param lf Observed logistic factors.
#' @author Wei Hao
#' @export
efron_rsq <- function(x, lf) {
  .efron_rsq <- function(snp, p1) {
    if (missing(snp)) {
      cat_stop("`snp` is required!")
    }
    if (missing(p1)) {
      cat_stop("`p1` is required!")
    }

    indx <- (p1 != 0) & (p1 != 1)
    p1 <- p1[indx]
    snp <- snp[indx]

    y <- as_num(c(snp > 0, snp == 2))
    p <- c(p1, p1)
    ybar <- mean(y)

    1 - sum((y - p)^2) / sum((y - ybar)^2)
  }
  if (missing(x)) {
    cat_stop("`x` is required!")
  }
  if (missing(lf)) {
    cat_stop("`lf` is required!")
  }
  if (!is.matrix(x)) {
    if (is.numeric(x)) {
      x <- as.matrix(x)
    }
  }
  if (!is.matrix(lf)) {
    if (is.numeric(lf)) {
      lf <- as.matrix(lf)
    }
  }

  m <- nrow(x)

  f <- lfa::af(x, lf)

  sapply(1:m, function(i) {
    .efron_rsq(x[i, ], f[i, ])
  })
}

#' @export
bins_break <- function(x, mode = c("sturges", "ncostt", "fd")) {
  nscott <- function(x) {
    x <- x[!is_nonnum(x)]
    h <- 3.5 * sqrt(stats::var(x)) * length(x)^(-1 / 3)
    if (h > 0) {
      max(1, ceiling(diff(range(x)) / h))
    } else {
      1L
    }
  }
  sturges <- function(x) {
    x <- x[!is_nonnum(x)]
    ceiling(log2(length(x)) + 1)
  }
  fd <- function(x) {
    x <- x[!is_nonnum(x)]
    h <- 2 * stats::IQR(xs <- signif(x, digits = 5))
    if (h == 0) {
      xs <- sort(xs)
      al <- 1 / 4
      al_min <- 1 / 512
      while (h == 0 &&
             (al <- al / 2) >= al_min) {
        h <- diff(stats::quantile(xs,
                                  c(al, 1 - al),
                                  names = FALSE
        )) /
          (1 - 2 * al)
      }
    }
    if (h == 0) {
      h <- 3.5 * sqrt(stats::var(x))
    }
    if (h > 0) {
      ceiling(diff(range(x)) / h * length(x)^(1 / 3))
    } else {
      1L
    }
  }

  mode <- match.arg(mode)
  switch(mode,
         sturges = sturges(x),
         nscott = nscott(x),
         fd = fd(x)
  )
}


## test

#' @rdname statisticstools
#' @description conjunto de test de normalidad: normal_test
#' @examples
#'
#' x <- mgalda::datalearn$credit$price
#' set.seed(7)
#' normal_test(x)
#'
#' @export

normal_test <- function(x) {
  tibble_impl <-
    function(x) {
      bind_rows(nullclass(x)[1:2])
    }

  if(length(x)> 5000){
    x <- sort_filter_vec(x,4999)
  }

  bind_rows(
    shapiro = tibble_impl(stats::shapiro.test(x)),
    pearson = tibble_impl(nortest::pearson.test(x)),
    cramervon = tibble_impl(nortest::cvm.test(x)),
    .id = "metrica"
  )
}

#' t.test data.frame
#' @name statisticstools
#' @importFrom dplyr inner_join
#' @aliases t.test.data.frame
#' @method t.test data.frame
#' @export
t.test.data.frame <- function(.data, group_var, test_var) {
  group_var <- rlang::enexpr(group_var)
  test_var <- rlang::enexpr(test_var)
  model <-
    paste(rlang::as_label(test_var),
          rlang::as_label(group_var),
          sep = " ~ "
    )
  model <- str2lang(model)
  if (n_unique(.data[[rlang::as_label(group_var)]]) == 2) {
    surce_vector <-
      as_chr(unique(.data[[rlang::as_label(group_var)]]))
    e <- rlang::expr(t.test(!!model, .data))
    m <- round(eval(e)$p.value, 5)
    m <- eval_expr(
      tribble(
        ~from,
        ~ !!sym(surce_vector[1]),
        ~ !!sym(surce_vector[2]),
        !!surce_vector[1],
        NA,
        !!m,
        !!surce_vector[2],
        !!m,
        NA
      )
    )
  } else {
    surce_vector <- unique(.data[[rlang::as_label(group_var)]])
    if (is.factor(surce_vector)) {
      surce_vector <- as_chr(surce_vector)
    }
    combinations_matrix <-
      gtools::permutations(
        n = length(surce_vector),
        r = 2,
        v = surce_vector
      )
    m <- c()
    cvec <- as_chr(.data[[rlang::as_label(group_var)]])
    for (i in 1:nrow(combinations_matrix)) {
      cmi <- combinations_matrix[i, ]
      data_group <- .data[cvec %in% cmi, ]
      e <- rlang::expr(t.test(!!model, data = data_group))
      m[[i]] <- try(eval(e), silent = T)
      if (is_try(m[[i]])) {
        m[[i]] <- NA
      } else {
        m[[i]] <- m[[i]]$p.value
      }
    }
    m <- unlist(m)
    m <-
      tibble(
        from = combinations_matrix[, 1],
        to = combinations_matrix[
          ,
          2
        ],
        ttest = ifelse(!is.na(m), round_sign(m),
                       m
        )
      ) %>% pivot_wider(names_from = to, values_from = ttest)
  }
  m
}


##

# rescalar --


#' Rescale continuous vector to have specified minimum and maximum
#'
#' @name statisticstools
#' @rdname statisticstools
#' @param x continuous vector of values to manipulate.
#' @param to output range (numeric vector of length two)
#' @param from input range (vector of length two).  If not given, is
#'   calculated from the range of `x`
#' @param ... other arguments passed on to methods
#' @keywords manip
#' @export
#' @examples
#' rescalar(1:100)
#' rescalar(runif(50))
#' rescalar(1)
rescalar <- function(x, to, from, ...) {
  UseMethod("rescalar")
}



#' @export
rescalar.numeric <-
  function(x,
           to = c(0, 1),
           from = range(x, na.rm = TRUE, finite = TRUE),
           ...) {
    if (zero_range(from) || zero_range(to)) {
      return(ifelse(is.na(x), NA, mean(to)))
    }
    (x - from[1]) / diff(from) * diff(to) + to[1]
  }

#' @export
rescalar.NULL <- function(...) {
  NULL
}


#' @export
rescalar.dist <- rescalar.numeric



#' @export
rescalar.logical <- rescalar.numeric


#' @export
rescalar.POSIXt <-
  function(x,
           to = c(0, 1),
           from = range(x, na.rm = TRUE, finite = TRUE),
           ...) {
    x <- as_num(x)
    from <- as_num(from)
    rescalar.numeric(x = x, to = to, from = from)
  }


#' @export
rescalar.Date <- rescalar.POSIXt


#' @export
rescalar.integer64 <-
  function(x,
           to = c(0, 1),
           from = range(x, na.rm = TRUE),
           ...) {
    if (zero_range(from, tol = 0) || zero_range(to)) {
      return(ifelse(is.na(x), NA, mean(to)))
    }
    (x - from[1]) / diff(from) * diff(to) + to[1]
  }


#' @export
rescalar.data.frame <-   function(x,
                                  to = c(0, 1),
                                  from = range(x, na.rm = TRUE),
                                  ...) {
  as_df(apply(data,2,function(x) rescalar(x,to,from,...)))
}

# rescalar_mid --

#' rescalar vector to have specified minimum, midpoint, and maximum
#'
#' @rdname statisticstools
#' @export
#' @param x vector of values to manipulate.
#' @param to output range (numeric vector of length two)
#' @param from input range (vector of length two).  If not given, is
#'   calculated from the range of `x`
#' @param mid mid-point of input range
#' @param ... other arguments passed on to methods
#' @examples
#' rescalar_mid(1:100, mid = 50.5)
#' rescalar_mid(runif(50), mid = 0.5)
#' rescalar_mid(1)
rescalar_mid <- function(x, to, from, mid, ...) {
  UseMethod("rescalar_mid")
}


#' @export
rescalar_mid.numeric <-
  function(x,
           to = c(0, 1),
           from = range(x, na.rm = TRUE),
           mid = 0,
           ...) {
    if (zero_range(from) || zero_range(to)) {
      return(ifelse(is.na(x), NA, mean(to)))
    }

    extent <- 2 * max(abs(from - mid))
    (x - mid) / extent * diff(to) + mean(to)
  }

#' @export
rescalar_mid.NULL <- function(...) {
  NULL
}


#' @export
rescalar_mid.logical <- rescalar_mid.numeric


#' @export
rescalar_mid.dist <- rescalar_mid.numeric



#' @export
rescalar_mid.POSIXt <-
  function(x,
           to = c(0, 1),
           from = range(x, na.rm = TRUE),
           mid,
           ...) {
    x <- as_num(as.POSIXct(x))
    if (!is.numeric(from)) {
      from <- as_num(as.POSIXct(from))
    }
    if (!is.numeric(mid)) {
      mid <- as_num(as.POSIXct(mid))
    }
    rescalar_mid.numeric(
      x = x,
      to = to,
      from = from,
      mid = mid
    )
  }


#' @export
rescalar_mid.Date <- rescalar_mid.POSIXt


#' @export
rescalar_mid.integer64 <-
  function(x,
           to = c(0, 1),
           from = range(x, na.rm = TRUE),
           mid = 0,
           ...) {
    if (zero_range(from, tol = 0) || zero_range(to)) {
      return(ifelse(is.na(x), NA, mean(to)))
    }
    extent <- 2 * max(abs(from - mid))
    (x - mid) / extent * diff(to) + mean(to)
  }


# rescalar_max --



#' rescalar numeric vector to have specified maximum
#'
#' @rdname statisticstools
#'
#' @export
#' @param x numeric vector of values to manipulate.
#' @param to output range (numeric vector of length two)
#' @param from input range (numeric vector of length two).  If not given, is
#'   calculated from the range of `x`
#' @examples
#' rescalar_max(1:100)
#' rescalar_max(runif(50))
#' rescalar_max(1)
rescalar_max <-
  function(x,
           to = c(0, 1),
           from = range(x, na.rm = TRUE)) {
    x / from[2] * to[2]
  }


# rescaler --

#' rescaler
#'
#' @rdname statisticstools
#' @export
rescaler <- function(x, type = "sd", ...) {
  UseMethod("rescaler", x)
}

#' @export
rescaler.default <- function(x, type = "sd", ...) {
  switch(type,
         rank = rank(x, ...),
         var = ,
         sd = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
         robust = (x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE),
         I = x,
         range = (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
  )
}

#' @export
rescaler.data.frame <- function(x, type = "sd", ...) {
  continuous <- sapply(x, is.numeric)
  x[continuous] <- lapply(x[continuous], rescaler, type = type, ...)
  x
}

#' @export
rescaler.matrix <- function(x, type = "sd", ...) {
  apply(x, 2, rescaler, type = type, ...)
}



# unscale --

#' unscale
#'
#' @rdname statisticstools
#' @export
unscale <- function(x, ...) {
  UseMethod("unscale", x)
}

#' @export
unscale.default <- function(x, center = 1, scale = 1) {
  if (is.null(center)) {
    center <- attr(x, "scaled:center")
  }
  if (is.null(scale)) {
    scale <- attr(x, "scaled:scale")
  }
  if (is.null(center) | is.null(scale)) {
    cat_stop("Cannot extract center and/or scale from the data and none is provided.")
  }
  x <- scale(x,
             center = (-center / scale),
             scale = 1 / scale
  )
  attr(x, "scaled:center") <- attr(x, "scaled:scale") <- NULL
  as_num(x)
}

#' @export
unscale.data.frame <- function(x, center = 1, scale = 1) {
  continuous <- sapply(x, is.numeric)
  x[continuous] <-
    lapply(x[continuous], unscale, center = center, scale = scale)
  x
}

#' @export
unscale.matrix <- function(x, center = 1, scale = 1) {
  apply(x, 2, unscale, center = center, scale = scale)
}


#' @export
center_scale <-
  function(x,
           center = TRUE,
           scale = TRUE,
           keep_attr = FALSE) {
    ffin <- if (is_dblint(x)) {
      as_num
    } else if (is_df(x)) {
      as_tbl
    } else {
      identify
    }

    x <- as.matrix(x)
    nc <- ncol(x)
    if (is.logical(center)) {
      if (center) {
        center <- colMeans(x, na.rm = TRUE)
        x <- sweep(x, 2L, center, check.margin = FALSE)
      }
    } else {
      if (!is.numeric(center)) {
        center <- as_num(center)
      }
      if (length(center) == nc) {
        x <- sweep(x, 2L, center, check.margin = FALSE)
      } else {
        cat_stop("length of 'center' must equal the number of columns of 'x'")
      }
    }
    if (is.logical(scale)) {
      if (scale) {
        f <- function(v) {
          v <- v[!is.na(v)]
          sqrt(sum(v^2) / max(1, length(v) - 1L))
        }
        scale <- apply(x, 2L, f)
        x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
      }
    } else {
      if (!is.numeric(scale)) {
        scale <- as_num(scale)
      }
      if (length(scale) == nc) {
        x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
      } else {
        cat_stop("length of 'scale' must equal the number of columns of 'x'")
      }
    }
    if (keep_attr) {
      if (is.numeric(center)) {
        attr(x, "scaled:center") <- center
      }
      if (is.numeric(scale)) {
        attr(x, "scaled:scale") <- scale
      }
    }
    ffin(x)
  }
#' @export
rescalar_outliers <-
  function(x,
           cutoff = 0.95,
           fn = "simple",
           limite = "ambos",
           method = c("maha", "quantile"),
           times = 3,
           rep = 3) {
    xbu <- x
    fn <- match.arg(
      arg = fn,
      choices = c(
        "normal", "simple",
        "slide"
      )
    )
    limite <- match.arg(arg = limite, c("ambos", "upper", "lower"))
    method <- match.arg(
      arg = method,
      c(
        "quantile", "dens", "knn",
        "maha", "box"
      ),
      several.ok = T
    )
    check_inherits(x = x, what = "numeric")
    if (!is_prob(cutoff)) {
      cat_warn("!is_prob(cutoff)")
      return(x)
    }
    for (i in 1:rep) {
      x <- cbind(x, sapply(method, function(m) {
        do_call("clip_bounds_vec",c(
          x = list(x),
          cutoff = cutoff,
          fn = fn,
          limite = limite,
          method = m,
          times = times,
          rep = 1
        ))
      }))
      x <- apply(x, 2, function(x) {
        ifelse(is_nonnum(x), NA, x)
      })
      x <- rowMeans(x, na.rm = TRUE)
      x <- ifelse(is_nonnum(x), xbu, x)
    }
    x
  }
