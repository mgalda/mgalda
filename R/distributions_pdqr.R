#' @include util_env.R
NULL


#' Distributions
#'
#'
#' @name dens_cumdistr_quan_rand
#' @rdname dens_cumdistr_quan_rand
#' @keywords internal
NULL


###  CONTINUOUS -

# Generalized Error -
#' @rdname dens_cumdistr_quan_rand
#' @export
dged <- function(x,
                 mean = 0,
                 sd = 1,
                 nu = 2,
                 log = FALSE) {
  if (!is_num(mean)) {
    cat_warn("bad input for argument argument 'mean'")
    return(rep(NA,length(x)))
  }
  if (!is_num(sd)) {
    cat_warn("bad input for argument argument 'sd'")
    return(rep(NA,length(x)))
  }
  if (!is_num(nu)) {
    cat_warn("bad input for argument argument 'nu'")
    return(rep(NA,length(x)))
  }
  if (length(mean) == 3) {
    nu <- mean[3]
    sd <- mean[2]
    mean <- mean[1]
  }
  z <- (x - mean) / sd
  lambda <- sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
  g <- nu / (lambda * (2^(1 + 1 / nu)) * gamma(1 / nu))
  result <- g * exp(-0.5 * (abs(z / lambda))^nu) / sd
  if (log) {
    result <- log(result)
  }
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pged <- function(q,
                 mean = 0,
                 sd = 1,
                 nu = 2,
                 lower.tail = TRUE) {
  if (!is_num(mean)) {
    cat_warn("bad input for argument argument 'mean'")
    return(rep(NA,length(q)))
  }
  if (!is_num(sd)) {
    cat_warn("bad input for argument argument 'sd'")
    return(rep(NA,length(q)))
  }
  if (!is_num(nu)) {
    cat_warn("bad input for argument argument 'nu'")
    return(rep(NA,length(q)))
  }

  q <- (q - mean) / sd
  lambda <- sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
  g <- nu / (lambda * (2^(1 + 1 / nu)) * gamma(1 / nu))
  h <- 2^(1 / nu) * lambda * g * gamma(1 / nu) / nu
  s <- 0.5 * (abs(q) / lambda)^nu
  res <- 0.5 + sign(q) * h * pgamma(s, 1 / nu)
  if (!lower.tail) {
    res <- 1 - res
  }

  res
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qged <- function(p,
                 mean = 0,
                 sd = 1,
                 nu = 2,
                 lower.tail = TRUE) {
  if (!is_num(mean)) {
    cat_warn("bad input for argument argument 'mean'")
    return(rep(NA,length(p)))
  }
  if (!is_num(sd)) {
    cat_warn("bad input for argument argument 'sd'")
    return(rep(NA,length(p)))
  }
  if (!is_num(nu)) {
    cat_warn("bad input for argument argument 'nu'")
    return(rep(NA,length(p)))
  }

  if (!lower.tail) {
    p <- 1 - p
  }

  lambda <- sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
  q <- lambda * (2 * qgamma((abs(2 * p - 1)), 1 / nu))^(1 / nu)
  result <- q * sign(2 * p - 1) * sd + mean
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rged <- function(n,
                 mean = 0,
                 sd = 1,
                 nu = 2) {
  if (!is_num(mean)) {
    cat_warn("bad input for argument argument 'mean'")
  }
  if (!is_num(sd)) {
    cat_warn("bad input for argument argument 'sd'")
  }
  if (!is_num(nu)) {
    cat_warn("bad input for argument argument 'nu'")
  }

  lambda <- sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
  r <- rgamma(n, 1 / nu)
  z <- lambda * (2 * r)^(1 / nu) * sign(runif(n) - 1 / 2)
  result <- z * sd + mean
  result
}

# Skew Generalized Error -
#' @rdname dens_cumdistr_quan_rand
#' @export
dsged <-
  function(x,
           mean = 0,
           sd = 1,
           nu = 2,
           xi = 1.5,
           log = FALSE) {
    if (!is_num(mean)) {
      cat_warn("bad input for argument argument 'mean'")
      return(rep(NA,length(x)))
    }
    if (!is_num(sd)) {
      cat_warn("bad input for argument argument 'sd'")
      return(rep(NA,length(x)))
    }
    if (!is_num(nu)) {
      cat_warn("bad input for argument argument 'nu'")
      return(rep(NA,length(x)))
    }
    if (!is_num(xi)) {
      cat_warn("bad input for argument argument 'xi'")
      return(rep(NA,length(x)))
    }
    dsged_impl <-
      function(x, nu, xi) {
        lambda <- sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
        g <- nu / (lambda * (2^(1 + 1 / nu)) * gamma(1 / nu))
        m1 <-
          2^(1 / nu) * lambda * gamma(2 / nu) / gamma(1 / nu)
        mu <- m1 * (xi - 1 / xi)
        sigma <-
          sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
        z <- x * sigma + mu
        Xi <- xi^sign(z)
        g <- 2 / (xi + 1 / xi)
        Density <- g * dged(x = z / Xi, nu = nu)
        Density * sigma
      }

    if (length(mean) == 4) {
      xi <- mean[4]
      nu <- mean[3]
      sd <- mean[2]
      mean <- mean[1]
    }
    result <- dsged_impl(
      x = (x - mean) / sd,
      nu = nu,
      xi = xi
    ) / sd
    result[result < 0] <- 0
    if (log) {
      result <- log(result)
    }
    result
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
psged <-
  function(q,
           mean = 0,
           sd = 1,
           nu = 2,
           xi = 1.5) {
    if (!is_num(mean)) {
      cat_warn("bad input for argument argument 'mean'")
      return(rep(NA,length(q)))
    }
    if (!is_num(sd)) {
      cat_warn("bad input for argument argument 'sd'")
      return(rep(NA,length(q)))
    }
    if (!is_num(nu)) {
      cat_warn("bad input for argument argument 'nu'")
      return(rep(NA,length(q)))
    }
    if (!is_num(xi)) {
      cat_warn("bad input for argument argument 'xi'")
      return(rep(NA,length(q)))
    }

    psged_impl <-
      function(q, nu, xi) {
        lambda <- sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
        g <- nu / (lambda * (2^(1 + 1 / nu)) * gamma(1 / nu))
        m1 <-
          2^(1 / nu) * lambda * gamma(2 / nu) / gamma(1 / nu)
        mu <- m1 * (xi - 1 / xi)
        sigma <-
          sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
        z <- q * sigma + mu
        Xi <- xi^sign(z)
        g <- 2 / (xi + 1 / xi)
        Probability <-
          heaviside(z) - sign(z) * g * Xi * pged(
            q = -abs(z) / Xi, nu =
              nu
          )
        Probability
      }
    result <- psged_impl(
      q = (q - mean) / sd,
      nu = nu,
      xi = xi
    )
    result
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
qsged <-
  function(p,
           mean = 0,
           sd = 1,
           nu = 2,
           xi = 1.5) {
    if (!is_num(mean)) {
      cat_warn("bad input for argument argument 'mean'")
      return(rep(NA,length(p)))
    }
    if (!is_num(sd)) {
      cat_warn("bad input for argument argument 'sd'")
      return(rep(NA,length(p)))
    }
    if (!is_num(nu)) {
      cat_warn("bad input for argument argument 'nu'")
      return(rep(NA,length(p)))
    }
    if (!is_num(xi)) {
      cat_warn("bad input for argument argument 'xi'")
      return(rep(NA,length(p)))
    }

    qsged_impl <-
      function(p, nu, xi) {
        lambda <- sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
        g <- nu / (lambda * (2^(1 + 1 / nu)) * gamma(1 / nu))
        m1 <-
          2^(1 / nu) * lambda * gamma(2 / nu) / gamma(1 / nu)
        mu <- m1 * (xi - 1 / xi)
        sigma <-
          sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
        g <- 2 / (xi + 1 / xi)
        sig <- sign(p - 1 / 2)
        Xi <- xi^sig
        p <- (heaviside(p - 1 / 2) - sig * p) / (g * Xi)
        Quantile <-
          (-sig * qged(p = p, sd = Xi, nu = nu) - mu) / sigma
        Quantile
      }

    result <- qsged_impl(p = p, nu = nu, xi = xi) * sd + mean
    result
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
rsged <-
  function(n,
           mean = 0,
           sd = 1,
           nu = 2,
           xi = 1.5) {
    if (!is_num(mean)) {
      cat_warn("bad input for argument argument 'mean'")
    }
    if (!is_num(sd)) {
      cat_warn("bad input for argument argument 'sd'")
    }
    if (!is_num(nu)) {
      cat_warn("bad input for argument argument 'nu'")
    }
    if (!is_num(xi)) {
      cat_warn("bad input for argument argument 'xi'")
    }

    rsged_impl <-
      function(n, nu, xi) {
        weight <- xi / (xi + 1 / xi)
        z <- runif(n, -weight, 1 - weight)
        Xi <- xi^sign(z)
        Random <- -abs(rged(n, nu = nu)) / Xi * sign(z)
        lambda <-
          sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
        # g <- nu / (lambda * (2^(1 + 1 / nu)) * gamma(1 / nu))
        m1 <-
          2^(1 / nu) * lambda * gamma(2 / nu) / gamma(1 / nu)
        mu <- m1 * (xi - 1 / xi)
        sigma <-
          sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
        Random <- (Random - mu) / sigma
        Random
      }
    result <- rsged_impl(n = n, nu = nu, xi = xi) * sd + mean
    result
  }

# Skew Normal -
#' @rdname dens_cumdistr_quan_rand
#' @export
dsnorm <- function(x,
                   mean = 0,
                   sd = 1,
                   xi = 1.5,
                   log = FALSE) {
  if (!is_num(mean)) {
    cat_warn("bad input for argument argument 'mean'")
    return(rep(NA,length(x)))
  }
  if (!is_num(sd)) {
    cat_warn("bad input for argument argument 'sd'")
    return(rep(NA,length(x)))
  }
  if (!is_num(xi)) {
    cat_warn("bad input for argument argument 'xi'")
    return(rep(NA,length(x)))
  }

  dsnorm_impl <- function(x, xi) {
    m1 <- 2 / sqrt(2 * pi)
    mu <- m1 * (xi - 1 / xi)
    sigma <-
      sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
    z <- x * sigma + mu
    Xi <- xi^sign(z)
    g <- 2 / (xi + 1 / xi)
    Density <- g * dnorm(x = z / Xi)
    Density * sigma
  }
  if (length(mean) == 3) {
    xi <- mean[3]
    sd <- mean[2]
    mean <- mean[1]
  }
  res <- dsnorm_impl(x = (x - mean) / sd, xi = xi) / sd
  res[res < 0] <- 0
  if (log) {
    res <- log(res)
  }
  res
}
#' @rdname dens_cumdistr_quan_rand
#' @export
psnorm <- function(q,
                   mean = 0,
                   sd = 1,
                   xi = 1.5) {
  if (!is_num(mean)) {
    cat_warn("bad input for argument argument 'mean'")
    return(rep(NA,length(q)))
  }
  if (!is_num(sd)) {
    cat_warn("bad input for argument argument 'sd'")
    return(rep(NA,length(q)))
  }
  if (!is_num(xi)) {
    cat_warn("bad input for argument argument 'xi'")
    return(rep(NA,length(q)))
  }

  psnorm_impl <- function(q, xi) {
    m1 <- 2 / sqrt(2 * pi)
    mu <- m1 * (xi - 1 / xi)
    sigma <-
      sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
    z <- q * sigma + mu
    Xi <- xi^sign(z)
    g <- 2 / (xi + 1 / xi)
    Probability <-
      heaviside(z) - sign(z) * g * Xi * pnorm(q = -abs(z) / Xi)
    Probability
  }
  result <- psnorm_impl(q = (q - mean) / sd, xi = xi)
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qsnorm <- function(p,
                   mean = 0,
                   sd = 1,
                   xi = 1.5) {
  if (!is_num(mean)) {
    cat_warn("bad input for argument argument 'mean'")
    return(rep(NA,length(p)))
  }
  if (!is_num(sd)) {
    cat_warn("bad input for argument argument 'sd'")
    return(rep(NA,length(p)))
  }
  if (!is_num(xi)) {
    cat_warn("bad input for argument argument 'xi'")
    return(rep(NA,length(p)))
  }

  qsnorm_impl <- function(p, xi) {
    m1 <- 2 / sqrt(2 * pi)
    mu <- m1 * (xi - 1 / xi)
    sigma <-
      sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
    g <- 2 / (xi + 1 / xi)
    sig <- sign(p - 1 / 2)
    Xi <- xi^sig
    p <- (heaviside(p - 1 / 2) - sig * p) / (g * Xi)
    Quantile <- (-sig * qnorm(p = p, sd = Xi) - mu) / sigma
    Quantile
  }
  result <- qsnorm_impl(p = p, xi = xi) * sd + mean
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rsnorm <- function(n,
                   mean = 0,
                   sd = 1,
                   xi = 1.5) {
  if (!is_num(mean)) {
    cat_warn("bad input for argument argument 'mean'")
  }
  if (!is_num(sd)) {
    cat_warn("bad input for argument argument 'sd'")
  }
  if (!is_num(xi)) {
    cat_warn("bad input for argument argument 'xi'")
  }

  rsnorm_impl <- function(n, xi) {
    weight <- xi / (xi + 1 / xi)
    z <- runif(n, -weight, 1 - weight)
    Xi <- xi^sign(z)
    Random <- -abs(rnorm(n)) / Xi * sign(z)
    m1 <- 2 / sqrt(2 * pi)
    mu <- m1 * (xi - 1 / xi)
    sigma <-
      sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
    Random <- (Random - mu) / sigma
    Random
  }
  result <- rsnorm_impl(n = n, xi = xi) * sd + mean
  result
}

# Skew Student-t -
#' @rdname dens_cumdistr_quan_rand
#' @export
dsstd <- function(x,
                  mean = 0,
                  sd = 1,
                  nu = 5,
                  xi = 1.5,
                  log = FALSE) {
  dsstd_impl <- function(x, nu, xi) {
    if (!exists("beta")) {
      beta <- function(a, b) {
        exp(lgamma(a) + lgamma(b) - lgamma(a + b))
      }
    }
    m1 <- 2 * sqrt(nu - 2) / (nu - 1) / beta(1 / 2, nu / 2)
    mu <- m1 * (xi - 1 / xi)
    sigma <-
      sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
    z <- x * sigma + mu
    Xi <- xi^sign(z)
    g <- 2 / (xi + 1 / xi)
    Density <- g * dstd(x = z / Xi, nu = nu)
    Density * sigma
  }
  if (length(mean) == 4) {
    xi <- mean[4]
    nu <- mean[3]
    sd <- mean[2]
    mean <- mean[1]
  }
  result <- dsstd_impl(
    x = (x - mean) / sd,
    nu = nu,
    xi = xi
  ) / sd
  if (log) {
    result <- log(result)
  }
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
psstd <- function(q,
                  mean = 0,
                  sd = 1,
                  nu = 5,
                  xi = 1.5) {
  psstd_impl <- function(q, nu, xi) {
    if (!exists("beta")) {
      beta <- function(a, b) {
        exp(lgamma(a) + lgamma(b) - lgamma(a + b))
      }
    }
    m1 <- 2 * sqrt(nu - 2) / (nu - 1) / beta(1 / 2, nu / 2)
    mu <- m1 * (xi - 1 / xi)
    sigma <-
      sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
    z <- q * sigma + mu
    Xi <- xi^sign(z)
    g <- 2 / (xi + 1 / xi)
    Probability <-
      heaviside(z) - sign(z) * g * Xi * pstd(
        q = -abs(z) / Xi,
        nu = nu
      )
    Probability
  }
  result <- psstd_impl(
    q = (q - mean) / sd,
    nu = nu,
    xi = xi
  )
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qsstd <- function(p,
                  mean = 0,
                  sd = 1,
                  nu = 5,
                  xi = 1.5) {
  qsstd_impl <-
    function(p, nu, xi) {
      if (!exists("beta")) {
        beta <-
          function(a, b) {
            exp(lgamma(a) + lgamma(b) - lgamma(a + b))
          }
      }
      m1 <- 2 * sqrt(nu - 2) / (nu - 1) / beta(1 / 2, nu / 2)
      mu <- m1 * (xi - 1 / xi)
      sigma <-
        sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
      g <- 2 / (xi + 1 / xi)
      sig <- sign(p - 1 / 2)
      Xi <- xi^sig
      p <- (heaviside(p - 1 / 2) - sig * p) / (g * Xi)
      Quantile <-
        (-sig * qstd(p = p, sd = Xi, nu = nu) - mu) / sigma
      Quantile
    }
  result <- qsstd_impl(p = p, nu = nu, xi = xi) * sd + mean
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rsstd <- function(n,
                  mean = 0,
                  sd = 1,
                  nu = 5,
                  xi = 1.5) {
  rsstd_impl <-
    function(n, nu, xi) {
      if (!exists("beta")) {
        beta <-
          function(a, b) {
            exp(lgamma(a) + lgamma(b) - lgamma(a + b))
          }
      }
      weight <- xi / (xi + 1 / xi)
      z <- runif(n, -weight, 1 - weight)
      Xi <- xi^sign(z)
      Random <- -abs(rstd(n, nu = nu)) / Xi * sign(z)
      m1 <- 2 * sqrt(nu - 2) / (nu - 1) / beta(1 / 2, nu / 2)
      mu <- m1 * (xi - 1 / xi)
      sigma <-
        sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
      Random <- (Random - mu) / sigma
      Random
    }
  result <- rsstd_impl(n = n, nu = nu, xi = xi) * sd + mean
  result
}

# Student-t -
#' @rdname dens_cumdistr_quan_rand
#' @export
dstd <- function(x,
                 mean = 0,
                 sd = 1,
                 nu = 5,
                 log = FALSE) {
  if (length(mean) == 3) {
    nu <- mean[3]
    sd <- mean[2]
    mean <- mean[1]
  }
  s <- sqrt(nu / (nu - 2))
  z <- (x - mean) / sd
  result <- dt(x = z * s, df = nu) * s / sd
  if (log) {
    result <- log(result)
  }
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pstd <- function(q,
                 mean = 0,
                 sd = 1,
                 nu = 5) {
  s <- sqrt(nu / (nu - 2))
  z <- (q - mean) / sd
  result <- pt(q = z * s, df = nu)
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qstd <- function(p,
                 mean = 0,
                 sd = 1,
                 nu = 5) {
  s <- sqrt(nu / (nu - 2))
  result <- qt(p = p, df = nu) * sd / s + mean
  result
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rstd <- function(n,
                 mean = 0,
                 sd = 1,
                 nu = 5) {
  s <- sqrt(nu / (nu - 2))
  result <- rt(n = n, df = nu) * sd / s + mean
  result
}

# Power -
#' @rdname dens_cumdistr_quan_rand
#' @export
dpower <-
  function(x, alpha, beta, log = FALSE) {
    d <-
      ifelse(x > alpha |
               x < 0, 0, (beta * x^(beta - 1)) / (alpha^beta))

    if (log) {
      d <- log(d)
    }
    d
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
ppower <-
  function(q,
           alpha,
           beta,
           lower.tail = TRUE) {
    p <- (beta * q^(beta - 1)) / (alpha^beta)

    p <- ifelse(p > 1, 1, p)
    p <- ifelse(p < 0, 0, p)

    if (!lower.tail) {
      p <- 1 - p
    }
    p
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
qpower <-
  function(p,
           alpha,
           beta,
           lower.tail = TRUE) {
    if (!lower.tail) {
      p <- 1 - p
    }

    ifelse(p <= 1 & p >= 0, alpha * p^(1 / beta), NaN)
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
rpower <- function(n, alpha, beta) {
  qpower(runif(n, 0, 1), alpha = alpha, beta = beta)
}

# Pareto -
#' @rdname dens_cumdistr_quan_rand
#' @export
dpareto <- function(x, shape, scale, log = FALSE) {
  actuar::dpareto(x, shape, scale, log = log)
}
#' @rdname dens_cumdistr_quan_rand
#' @export
ppareto <-
  function(q,
           shape,
           scale,
           lower.tail = TRUE,
           log.p = FALSE) {
    actuar::ppareto(q, shape, scale, lower.tail = lower.tail, log.p = log.p)
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
qpareto <-
  function(p,
           shape,
           scale,
           lower.tail = TRUE,
           log.p = FALSE) {
    actuar::qpareto(p, shape, scale, lower.tail = lower.tail, log.p = log.p)
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
rpareto <- function(n, shape, scale) {
  actuar::rpareto(n, shape, scale)
}

# Laplace --
#' @rdname dens_cumdistr_quan_rand
#' @export
dlaplace <- function(x,
                     mu = 0,
                     sigma = 1,
                     log = FALSE) {
  l <- -abs(x - mu) / sigma - log(2 * sigma)
  if (log == FALSE) {
    l <- exp(l)
  }
  l
}
#' @rdname dens_cumdistr_quan_rand
#' @export
plaplace <-
  function(q,
           mu = 0,
           sigma = 1,
           lower.tail = TRUE) {
    p <-
      ifelse(q < mu, 1 / 2 * exp((q - mu) / sigma), 1 - 1 / 2 * exp((mu - q) /
                                                                      sigma))

    p <- ifelse(p > 1, 1, p)
    p <- ifelse(p < 0, 0, p)

    if (!lower.tail) {
      p <- 1 - p
    }


    p
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
qlaplace <-
  function(p,
           mu = 0,
           sigma = 1,
           lower.tail = TRUE) {
    if (!lower.tail) {
      p <- 1 - p
    }

    ifelse(p <= 1 &
             p >= 0, ifelse(p < 0.5, mu + sigma * log(2 * p), mu - sigma * log(2 * (1 -
                                                                                      p))), NaN)
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
rlaplace <- function(n,
                     mu = 0,
                     sigma = 1) {
  qlaplace(runif(n), mu = mu, sigma = sigma)
}

# Gumbel -
#' @rdname dens_cumdistr_quan_rand
#' @export
dgumbel <- function(x,
                    location = 0,
                    scale = 1,
                    log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1) {
    cat_warn("bad input for argument 'log'")
  }
  rm(log)

  zedd <- (x - location) / scale
  logdensity <- -zedd - exp(-zedd) - log(scale)
  logdensity[is.infinite(x)] <- log(0) # 20141209 KaiH
  if (log.arg) {
    logdensity
  } else {
    exp(logdensity)
  }
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qgumbel <- function(p,
                    location = 0,
                    scale = 1,
                    lower.tail = TRUE,
                    log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }

  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }

  if (lower.tail) {
    if (log.p) {
      ln.p <- p
      ans <- location - scale * log(-ln.p)
    } else {
      ans <- location - scale * log(-log(p))
      ans[p == 0] <- -Inf
      ans[p == 1] <- Inf
    }
  } else {
    if (log.p) {
      ln.p <- p
      ans <- location - scale * log(-log(-expm1(ln.p)))
      ans[ln.p > 0] <- NaN
    } else {
      ans <- location - scale * log(-log1p(-p))
      ans[p == 0] <- Inf
      ans[p == 1] <- -Inf
    }
  }
  ans[scale <= 0] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pgumbel <- function(q,
                    location = 0,
                    scale = 1,
                    lower.tail = TRUE,
                    log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }
  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }

  if (lower.tail) {
    if (log.p) {
      ans <- -exp(-(q - location) / scale)
      ans[q <= -Inf] <- -Inf
      ans[q == Inf] <- 0
    } else {
      ans <- exp(-exp(-(q - location) / scale))
      ans[q <= -Inf] <- 0
      ans[q == Inf] <- 1
    }
  } else {
    if (log.p) {
      ans <- log(-expm1(-exp(-(q - location) / scale)))
      ans[q <= -Inf] <- 0
      ans[q == Inf] <- -Inf
    } else {
      ans <- -expm1(-exp(-(q - location) / scale))
      ans[q <= -Inf] <- 1
      ans[q == Inf] <- 0
    }
  }

  ans[scale <= 0] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rgumbel <- function(n, location = 0, scale = 1) {
  answer <- location - scale * log(-log(runif(n)))
  answer[scale <= 0] <- NaN
  answer
}

# Rayleigh --
#' @rdname dens_cumdistr_quan_rand
#' @export
drayleigh <- function(x, scale = 1, log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1) {
    cat_warn("bad input for argument 'log'")
  }
  rm(log)

  L <- max(length(x), length(scale))
  if (length(x) != L) {
    x <- rep_len(x, L)
  }
  if (length(scale) != L) {
    scale <- rep_len(scale, L)
  }

  logdensity <- rep_len(log(0), L)
  xok <- (x > 0)
  logdensity[xok] <- log(x[xok]) - 0.5 * (x[xok] / scale[xok])^2 -
    2 * log(scale[xok])
  logdensity[is.infinite(x)] <- log(0) # 20141208 KaiH
  if (log.arg) {
    logdensity
  } else {
    exp(logdensity)
  }
}
#' @rdname dens_cumdistr_quan_rand
#' @export
prayleigh <- function(q,
                      scale = 1,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }

  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }


  if (lower.tail) {
    if (log.p) {
      ans <- log(-expm1(-0.5 * (q / scale)^2))
      ans[q <= 0] <- -Inf
    } else {
      ans <- -expm1(-0.5 * (q / scale)^2)
      ans[q <= 0] <- 0
    }
  } else {
    if (log.p) {
      ans <- -0.5 * (q / scale)^2
      ans[q <= 0] <- 0
    } else {
      ans <- exp(-0.5 * (q / scale)^2)
      ans[q <= 0] <- 1
    }
  }
  ans[scale < 0] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qrayleigh <- function(p,
                      scale = 1,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }

  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }

  if (lower.tail) {
    if (log.p) {
      ln.p <- p
      ans <- scale * sqrt(-2 * log(-expm1(ln.p)))
      ans[ln.p > 0] <- NaN
    } else {
      ans <- scale * sqrt(-2 * log1p(-p))
      ans[p < 0] <- NaN
      ans[p == 0] <- 0
      ans[p == 1] <- Inf
    }
  } else {
    if (log.p) {
      ln.p <- p
      ans <- scale * sqrt(-2 * ln.p)
      ans[ln.p > 0] <- NaN
      ans
    } else {
      ans <- scale * sqrt(-2 * log(p))
      ans[p > 1] <- NaN
    }
  }
  ans[scale <= 0] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rrayleigh <- function(n, scale = 1) {
  ans <- scale * sqrt(-2 * log(runif(n)))
  ans[scale <= 0] <- NaN
  ans
}


# Invchisq --
#' @rdname dens_cumdistr_quan_rand
#' @export
dinvchisq <- function(x, df, log = FALSE) {
  if (log) {
    dchisq(
      x = 1 / x,
      df = ceiling(df * 4) / 4,
      ncp = 0,
      log = TRUE
    ) - 2 * log(x)
  } else {
    dchisq(
      x = 1 / x,
      df = ceiling(df * 4) / 4,
      ncp = 0,
      log = FALSE
    ) * (1 / x^2)
  }
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pinvchisq <- function(q,
                      df,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  pchisq(
    q = 1 / q,
    ncp = 0,
    df = ceiling(df * 4) / 4,
    lower.tail = !lower.tail,
    log.p = log.p
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qinvchisq <- function(p,
                      df,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  1 / qchisq(
    p = p,
    df = ceiling(df * 4) / 4,
    ncp = 0,
    lower.tail = !lower.tail,
    log.p = log.p
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rinvchisq <- function(n, df) {
  1 / rchisq(n = n, df = ceiling(df * 4) / 4, ncp = 0)
}

# Student-t 2 degrees -
#' @rdname dens_cumdistr_quan_rand
#' @export
dsc_t2 <- function(x,
                   location = 0,
                   scale = 1,
                   log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1) {
    cat_warn("bad input for argument 'log'")
  }
  rm(log)
  zedd <-
    (x - location) / scale
  zedd[scale <= 0] <- NaN
  if (log.arg) {
    log(0.25) - 1.5 * log1p((zedd / 2)^2) - log(scale)
  } else {
    2 / (scale * (4 + zedd^2)^1.5)
  }
}
#' @rdname dens_cumdistr_quan_rand
#' @export
psc_t2 <- function(q,
                   location = 0,
                   scale = 1,
                   lower.tail = TRUE,
                   log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }
  if (!is.logical(log.p) ||
      length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  zedd <-
    (q - location) / scale
  zedd[scale <= 0] <- NaN
  if (lower.tail) {
    if (log.p) {
      ans <- log(0.5) + log1p(zedd / sqrt(4 + zedd^2))
      ans[q == -Inf] <- log(0)
      ans[q == Inf] <- log(1)
    } else {
      ans <- 0.5 * (1 + zedd / sqrt(4 + zedd^2))
      ans[q == -Inf] <- 0
      ans[q == Inf] <- 1
    }
  } else {
    if (log.p) {
      ans <- log(0.5) + log1p(-zedd / sqrt(4 + zedd^2))
      ans[q == -Inf] <- log(1)
      ans[q == Inf] <- log(0)
    } else {
      ans <- 0.5 * exp(log1p(-zedd / sqrt(4 + zedd^2)))
      ans[q == -Inf] <- 1
      ans[q == Inf] <- 0
    }
  }
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qsc_t2 <- function(p,
                   location = 0,
                   scale = 1,
                   lower.tail = TRUE,
                   log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }
  if (!is.logical(log.p) ||
      length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  if (lower.tail) {
    if (log.p) {
      ln.p <- p
      ans <-
        exp(0.5 * (ln.p - log(-expm1(ln.p)))) -
        exp(0.5 * (log(-expm1(ln.p)) - ln.p))
      ans[ln.p > 0] <- NaN
    } else {
      ans <- exp(0.5 * (log(p) - log1p(-p))) -
        exp(0.5 * (log1p(-p) - log(p)))
      ans[p < 0] <- NaN
      ans[p == 0] <- -Inf
      ans[p == 1] <- Inf
      ans[p > 1] <- NaN
    }
  } else {
    if (log.p) {
      ln.p <- p
      ans <-
        exp(0.5 * (log(-expm1(ln.p)) - ln.p)) -
        exp(0.5 * (ln.p - log(-expm1(ln.p))))
      ans[ln.p > 0] <- NaN
      ans
    } else {
      ans <- exp(0.5 * (log1p(-p) - log(p))) -
        exp(0.5 * (log(p) - log1p(-p)))
      ans[p < 0] <- NaN
      ans[p == 0] <- Inf
      ans[p == 1] <- -Inf
      ans[p > 1] <- NaN
    }
  }
  answer <-
    ans * scale + location
  answer[scale <= 0] <- NaN
  answer
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rsc_t2 <-
  function(n, location = 0, scale = 1) {
    answer <- qsc_t2(runif(n)) * scale + location
    answer[scale <= 0] <- NaN
    answer
  }
# Generalized Extreme -
#' @rdname dens_cumdistr_quan_rand
#' @export
rgev <-
  function(n,
           location = 0,
           scale = 1,
           shape = 0) {
    use.n <- if ((length.n <- length(n)) > 1) {
      length.n
    } else
      if (!is_num(x = n,.int = TRUE,.pos = TRUE)) {
        cat_warn("bad input for argument 'n'")
      } else {
        n
      }
    if (!is_num(location)) {
      cat_warn("bad input for argument argument 'location'")
    }
    if (!is_num(shape)) {
      cat_warn("bad input for argument argument 'shape'")
    }
    ans <-
      numeric(use.n)
    if (length(shape) != use.n) {
      shape <- rep_len(shape, use.n)
    }
    if (length(location) != use.n) {
      location <- rep_len(location, use.n)
    }
    if (length(scale) != use.n) {
      scale <- rep_len(scale, use.n)
    }
    scase <-
      abs(shape) < sqrt(.Machine$double.eps)
    nscase <-
      sum(scase)
    if (use.n - nscase) {
      ans[!scase] <- location[!scase] + scale[!scase] *
        ((-log(runif(use.n - nscase)))^(-shape[!scase]) - 1) / shape[!scase]
    }
    if (nscase) {
      ans[scase] <- rgumbel(nscase,
                            location = location[scase],
                            scale = scale[scase]
      )
    }
    ans[scale <= 0] <-
      NaN
    ans
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
dgev <-
  function(x,
           location = 0,
           scale = 1,
           shape = 0,
           log = FALSE) {
    tolshape0 <- sqrt(.Machine$double.eps)
    oobounds.log <- -Inf # 20160412; No longer an argument.
    if (!is.logical(log.arg <-
                    log) || length(log) != 1) {
      cat_warn("bad input for argument 'log'")
    }
    rm(log)
    if (!is_num(tolshape0, .pos = TRUE)) {
      cat_warn("bad input for argument 'tolshape0'")
    }
    use.n <-
      max(length(x), length(location), length(scale), length(shape))
    if (length(shape) != use.n) {
      shape <- rep_len(shape, use.n)
    }
    if (length(location) != use.n) {
      location <- rep_len(location, use.n)
    }
    if (length(scale) != use.n) {
      scale <- rep_len(scale, use.n)
    }
    if (length(x) != use.n) {
      x <- rep_len(x, use.n)
    }
    logdensity <-
      rep_len(log(0), use.n)
    scase <-
      (abs(shape) < tolshape0)
    nscase <-
      sum(scase)
    if (use.n - nscase) {
      zedd <- 1 + shape * (x - location) / scale
      xok <-
        (!scase) & (zedd > 0)
      logdensity[xok] <-
        -log(scale[xok]) - zedd[xok]^(-1 / shape[xok]) -
        (1 + 1 / shape[xok]) * log(zedd[xok])
      outofbounds <-
        (!scase) & (zedd <= 0)
      if (any(outofbounds)) {
        logdensity[outofbounds] <- oobounds.log
        no.oob <-
          sum(outofbounds)
      }
    }
    if (nscase) {
      logdensity[scase] <- dgumbel(x[scase],
                                   location = location[scase],
                                   scale = scale[scase],
                                   log = TRUE
      )
    }
    logdensity[scale <= 0] <-
      NaN
    logdensity[is.infinite(x)] <-
      log(0) # 20141209 KaiH
    if (log.arg) {
      logdensity
    } else {
      exp(logdensity)
    }
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
pgev <-
  function(q,
           location = 0,
           scale = 1,
           shape = 0,
           lower.tail = TRUE,
           log.p = FALSE) {
    if (!is.logical(lower.tail) || length(lower.tail) != 1) {
      cat_warn("bad input for argument 'lower.tail'")
    }
    if (!is.logical(log.arg <-
                    log.p) ||
        length(log.p) != 1) {
      cat_warn("bad input for argument 'log.p'")
    }
    use.n <-
      max(
        length(q), length(location),
        length(scale), length(shape)
      )
    if (length(shape) != use.n) {
      shape <- rep_len(shape, use.n)
    }
    if (length(location) != use.n) {
      location <- rep_len(location, use.n)
    }
    if (length(scale) != use.n) {
      scale <- rep_len(scale, use.n)
    }
    if (length(q) != use.n) {
      q <- rep_len(q, use.n)
    }
    scase0 <-
      abs(shape) < sqrt(.Machine$double.eps) # Effectively 0
    zedd <-
      (q - location) / scale
    use.zedd <-
      pmax(0, 1 + shape * zedd)
    if (lower.tail) {
      if (log.p) {
        ans <- -use.zedd^(-1 / shape)
      } else {
        ans <- exp(-use.zedd^(-1 / shape))
      }
    } else {
      if (log.p) {
        ans <- log(-expm1(-use.zedd^(-1 / shape)))
      } else {
        ans <- -expm1(-use.zedd^(-1 / shape))
      }
    }
    if (any(scase0)) {
      ans[scase0] <- pgumbel(
        q[scase0],
        location = location[scase0],
        scale = scale[scase0],
        lower.tail = lower.tail,
        log.p = log.p
      )
    }
    ans[scale <= 0] <-
      NaN
    ans
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
qgev <-
  function(p,
           location = 0,
           scale = 1,
           shape = 0,
           lower.tail = TRUE,
           log.p = FALSE) {
    if (!is.logical(log.p) || length(log.p) != 1) {
      cat_warn("bad input for argument 'log.p'")
    }
    use.n <-
      max(
        length(p), length(location),
        length(scale), length(shape)
      )
    if (length(shape) != use.n) {
      shape <- rep_len(shape, use.n)
    }
    if (length(location) != use.n) {
      location <- rep_len(location, use.n)
    }
    if (length(scale) != use.n) {
      scale <- rep_len(scale, use.n)
    }
    if (length(p) != use.n) {
      p <- rep_len(p, use.n)
    }
    scase0 <-
      abs(shape) < sqrt(.Machine$double.eps)
    if (lower.tail) {
      if (log.p) {
        ln.p <- p
        ans <-
          location + scale * ((-ln.p)^(-shape) - 1) / shape
        ans[ln.p > 0] <-
          NaN
      } else {
        ans <- location + scale * ((-log(p))^(-shape) - 1) / shape
        ans[p == 1] <-
          Inf
        ans[p > 1] <-
          NaN
      }
    } else {
      if (log.p) {
        ln.p <- p
        ans <-
          location + scale *
          ((-log1p(-exp(ln.p)))^(-shape) - 1) / shape
        ans[ln.p > 0] <-
          NaN
      } else {
        ans <- location + scale * ((-log1p(-p))^(-shape) - 1) / shape
        ans[p == 1] <-
          Inf
        ans[p > 1] <-
          NaN
        ans[p < 0] <-
          NaN
      }
    }
    if (any(scase0)) {
      ans[scase0] <- qgumbel(
        p[scase0],
        location = location[scase0],
        scale = scale[scase0],
        lower.tail = lower.tail,
        log.p = log.p
      )
    }
    ans[scale <= 0] <-
      NaN
    ans
  }
# Generalized Pareto -
#' @rdname dens_cumdistr_quan_rand
#' @export
rgpd <-
  function(n,
           location = 0,
           scale = 1,
           shape = 0) {
    use.n <- if ((length.n <- length(n)) > 1) {
      length.n
    } else
      if (!is_num(n,.int = TRUE,.pos = TRUE)) {
        cat_warn("bad input for argument 'n'")
      } else {
        n
      }
    if (!is_num(location)) {
      cat_warn("bad input for argument 'location'")
    }
    if (!is_num(shape)) {
      cat_warn("bad input for argument 'shape'")
    }
    ans <-
      numeric(use.n)
    if (length(shape) != use.n) {
      shape <- rep_len(shape, use.n)
    }
    if (length(location) != use.n) {
      location <- rep_len(location, use.n)
    }
    if (length(scale) != use.n) {
      scale <- rep_len(scale, use.n)
    }
    scase <-
      abs(shape) < sqrt(.Machine$double.eps)
    nscase <-
      sum(scase)
    if (use.n - nscase) {
      ans[!scase] <- location[!scase] +
        scale[!scase] *
        ((runif(use.n - nscase))^(-shape[!scase]) - 1) / shape[!scase]
    }
    if (nscase) {
      ans[scase] <-
        location[scase] - scale[scase] * log(runif(nscase))
    }
    ans[scale <= 0] <-
      NaN
    ans
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
dgpd <-
  function(x,
           location = 0,
           scale = 1,
           shape = 0,
           log = FALSE) {
    tolshape0 <- sqrt(.Machine$double.eps)
    if (!is.logical(log.arg <- log) || length(log) != 1) {
      cat_warn("bad input for argument 'log'")
    }
    rm(log)
    oobounds.log <-
      -Inf
    if (!is_num(tolshape0, .pos = TRUE)) {
      cat_warn("bad input for argument 'tolshape0'")
    }
    L <-
      max(length(x), length(location), length(scale), length(shape))
    if (length(shape) != L) {
      shape <- rep_len(shape, L)
    }
    if (length(location) != L) {
      location <- rep_len(location, L)
    }
    if (length(scale) != L) {
      scale <- rep_len(scale, L)
    }
    if (length(x) != L) {
      x <- rep_len(x, L)
    }
    logdensity <-
      rep_len(log(0), L)
    scase <-
      abs(shape) < tolshape0
    nscase <-
      sum(scase)
    if (L - nscase) {
      zedd <- (x - location) / scale
      xok <-
        (!scase) &
        (zedd > 0) & (1 + shape * zedd > 0)
      logdensity[xok] <-
        -(1 + 1 / shape[xok]) * log1p(shape[xok] * zedd[xok]) -
        log(scale[xok])
      outofbounds <-
        (!scase) &
        ((zedd <= 0) |
           (1 + shape * zedd <= 0))
      if (any(outofbounds)) {
        logdensity[outofbounds] <- oobounds.log
      }
    }
    if (nscase) {
      xok <- scase & (x > location)
      logdensity[xok] <-
        -(x[xok] - location[xok]) / scale[xok] -
        log(scale[xok])
      outofbounds <-
        scase & (x <= location)
      if (any(outofbounds)) {
        logdensity[outofbounds] <- oobounds.log
      }
    }
    logdensity[scale <= 0] <-
      NaN
    if (log.arg) {
      logdensity
    } else {
      exp(logdensity)
    }
  } # dgpd
#' @rdname dens_cumdistr_quan_rand
#' @export
pgpd <-
  function(q,
           location = 0,
           scale = 1,
           shape = 0,
           lower.tail = TRUE,
           log.p = FALSE) {
    if (!is.logical(lower.tail) || length(lower.tail) != 1) {
      cat_warn("bad input for argument 'lower.tail'")
    }
    if (!is.logical(log.p) ||
        length(log.p) != 1) {
      cat_warn("bad input for argument 'log.p'")
    }
    use.n <-
      max(length(q), length(location), length(scale), length(shape))
    ans <-
      numeric(use.n)
    if (length(shape) != use.n) {
      shape <- rep_len(shape, use.n)
    }
    if (length(location) != use.n) {
      location <- rep_len(location, use.n)
    }
    if (length(scale) != use.n) {
      scale <- rep_len(scale, use.n)
    }
    if (length(q) != use.n) {
      q <- rep_len(q, use.n)
    }
    zedd <-
      (q - location) / scale
    use.zedd <-
      pmax(zedd, 0)
    scase0 <-
      abs(shape) < sqrt(.Machine$double.eps)
    nscase0 <-
      sum(scase0)
    if (use.n - nscase0) {
      ans <- 1 - pmax(1 + shape * use.zedd, 0)^(-1 / shape)
    }
    if (nscase0) {
      pos <- (zedd >= 0)
      ind9 <-
        (pos & scase0)
      ans[ind9] <-
        -expm1(-use.zedd[ind9])
      ind9 <-
        (!pos & scase0)
      ans[ind9] <-
        0
    }
    ans[scale <= 0] <-
      NaN
    if (lower.tail) {
      if (log.p) {
        log(ans)
      } else {
        ans
      }
    } else {
      if (log.p) {
        log1p(-ans)
      } else {
        1 - ans
      }
    }
  } # dgpd
#' @rdname dens_cumdistr_quan_rand
#' @export
qgpd <-
  function(p,
           location = 0,
           scale = 1,
           shape = 0,
           lower.tail = TRUE,
           log.p = FALSE) {
    if (!is.logical(lower.tail) || length(lower.tail) != 1) {
      cat_warn("bad input for argument 'lower.tail'")
    }
    if (!is.logical(log.arg <-
                    log.p) ||
        length(log.p) != 1) {
      cat_warn("bad input for argument 'log.p'")
    }
    rm(log.p)
    if (lower.tail) {
      if (log.arg) {
        p <- exp(p)
      }
    } else {
      p <- if (log.arg) {
        -expm1(p)
      } else {
        1 - p
      }
    }
    use.n <-
      max(
        length(p), length(location),
        length(scale), length(shape)
      )
    ans <-
      numeric(use.n)
    if (length(shape) != use.n) {
      shape <- rep_len(shape, use.n)
    }
    if (length(location) != use.n) {
      location <- rep_len(location, use.n)
    }
    if (length(scale) != use.n) {
      scale <- rep_len(scale, use.n)
    }
    if (length(p) != use.n) {
      p <- rep_len(p, use.n)
    }
    scase <-
      abs(shape) < sqrt(.Machine$double.eps)
    nscase <-
      sum(scase)
    if (use.n - nscase) {
      ans[!scase] <- location[!scase] + scale[!scase] *
        ((1 - p[!scase])^(-shape[!scase]) - 1) / shape[!scase]
    }
    if (nscase) {
      ans[scase] <- location[scase] - scale[scase] * log1p(-p[scase])
    }
    ans[p < 0] <-
      NaN
    ans[p > 1] <-
      NaN
    ans[(p == 0)] <-
      location[p == 0]
    ans[(p == 1) &
          (shape >= 0)] <- Inf
    ind5 <-
      (p == 1) & (shape < 0)
    ans[ind5] <-
      location[ind5] - scale[ind5] / shape[ind5]
    ans[scale <= 0] <-
      NaN
    ans
  } # qgpd
# Log-Laplace  --
#' @rdname dens_cumdistr_quan_rand
#' @export
rloglap <- function(n,
                    location_ald = 0,
                    scale_ald = 1,
                    tau = 0.5) {
  kappa <- sqrt(tau / (1 - tau))
  use.n <- if ((length.n <- length(n)) > 1) {
    length.n
  } else
    if (!is_num(n,.int = TRUE,.pos = TRUE)) {
      cat_warn("bad input for argument 'n'")
    } else {
      n
    }
  location_ald <- rep_len(location_ald, use.n)
  scale_ald <- rep_len(scale_ald, use.n)
  tau <- rep_len(tau, use.n)
  kappa <- rep_len(kappa, use.n)
  ans <- exp(location_ald) *
    (runif(use.n)^kappa / runif(use.n)^(1 / kappa))^(scale_ald / sqrt(2))
  indexTF <-
    (scale_ald > 0) & (tau > 0) & (tau < 1) & (kappa > 0) # &
  ans[!indexTF] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
dloglap <- function(x,
                    location_ald = 0,
                    scale_ald = 1,
                    tau = 0.5,
                    log = FALSE) {
  kappa <- sqrt(tau / (1 - tau))
  if (!is.logical(log.arg <- log) || length(log) != 1) {
    cat_warn("bad input for argument 'log'")
  }
  rm(log)
  scale <- scale_ald
  location <- location_ald
  NN <- max(
    length(x),
    length(location),
    length(scale),
    length(kappa),
    length(tau)
  )
  if (length(x) != NN) {
    x <- rep_len(x, NN)
  }
  if (length(location) != NN) {
    location <- rep_len(location, NN)
  }
  if (length(scale) != NN) {
    scale <- rep_len(scale, NN)
  }
  if (length(kappa) != NN) {
    kappa <- rep_len(kappa, NN)
  }
  if (length(tau) != NN) {
    tau <- rep_len(tau, NN)
  }
  Alpha <- sqrt(2) * kappa / scale_ald
  Beta <- sqrt(2) / (scale_ald * kappa)
  Delta <- exp(location_ald)
  exponent <- ifelse(x >= Delta, -(Alpha + 1), (Beta - 1)) *
    (log(x) - location_ald)
  logdensity <- -location_ald + log(Alpha) + log(Beta) -
    log(Alpha + Beta) + exponent
  indexTF <-
    (scale_ald > 0) & (tau > 0) & (tau < 1) & (kappa > 0) # &
  logdensity[!indexTF] <- NaN
  logdensity[x < 0 & indexTF] <- -Inf
  if (log.arg) {
    logdensity
  } else {
    exp(logdensity)
  }
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qloglap <- function(p,
                    location_ald = 0,
                    scale_ald = 1,
                    tau = 0.5,
                    lower.tail = TRUE,
                    log.p = FALSE) {
  kappa <- sqrt(tau / (1 - tau))
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }
  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  NN <- max(
    length(p),
    length(location_ald),
    length(scale_ald),
    length(kappa)
  )
  p <- rep_len(p, NN)
  # location <- rep_len(location_ald, NN)
  # scale <- rep_len(scale_ald, NN)
  kappa <- rep_len(kappa, NN)
  tau <- rep_len(tau, NN)
  Alpha <- sqrt(2) * kappa / scale_ald
  Beta <- sqrt(2) / (scale_ald * kappa)
  Delta <- exp(location_ald)
  temp9 <- Alpha + Beta
  if (lower.tail) {
    if (log.p) {
      ln.p <- p
      ans <- ifelse((exp(ln.p) > Alpha / temp9),
                    Delta * (-expm1(ln.p) * temp9 / Beta)^(-1 / Alpha),
                    Delta * (exp(ln.p) * temp9 / Alpha)^(1 / Beta)
      )
      ans[ln.p > 0] <- NaN
    } else {
      ans <- ifelse((p > Alpha / temp9),
                    Delta * exp((-1 / Alpha) * (log1p(-p) +
                                                  log(temp9 / Beta))),
                    Delta * (p * temp9 / Alpha)^(1 / Beta)
      )
      ans[p < 0] <- NaN
      ans[p == 0] <- 0
      ans[p == 1] <- Inf
      ans[p > 1] <- NaN
    }
  } else {
    if (log.p) {
      ln.p <- p
      ans <- ifelse((-expm1(ln.p) > Alpha / temp9),
                    Delta * (exp(ln.p) * temp9 / Beta)^(-1 / Alpha),
                    Delta * (-expm1(ln.p) * temp9 / Alpha)^(1 / Beta)
      )
      ans[ln.p > 0] <- NaN
    } else {
      ans <- ifelse((p < (temp9 - Alpha) / temp9),
                    Delta * (p * temp9 / Beta)^(-1 / Alpha),
                    Delta * exp((1 / Beta) * (log1p(-p) + log(temp9 / Alpha)))
      )
      ans[p < 0] <- NaN
      ans[p == 0] <- Inf
      ans[p == 1] <- 0
      ans[p > 1] <- NaN
    }
  }
  indexTF <- (scale_ald > 0) & (tau > 0) & (tau < 1) & (kappa > 0)
  ans[!indexTF] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
ploglap <- function(q,
                    location_ald = 0,
                    scale_ald = 1,
                    tau = 0.5,
                    lower.tail = TRUE,
                    log.p = FALSE) {
  kappa <- sqrt(tau / (1 - tau))
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }
  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  NN <- max(
    length(q),
    length(location_ald),
    length(scale_ald),
    length(kappa)
  )
  # location <- rep_len(location_ald, NN)
  # scale <- rep_len(scale_ald, NN)
  kappa <- rep_len(kappa, NN)
  q <- rep_len(q, NN)
  tau <- rep_len(tau, NN)
  Alpha <- sqrt(2) * kappa / scale_ald
  Beta <- sqrt(2) / (scale_ald * kappa)
  Delta <- exp(location_ald)
  temp9 <- Alpha + Beta
  index1 <- (Delta <= q)
  if (lower.tail) {
    if (log.p) {
      ans <- log((Alpha / temp9) * (q / Delta)^(Beta))
      ans[index1] <-
        log1p((-(Beta / temp9) * (Delta / q)^(Alpha))[index1])
      ans[q <= 0] <- -Inf
      ans[q == Inf] <- 0
    } else {
      ans <- (Alpha / temp9) * (q / Delta)^(Beta)
      ans[index1] <- -expm1((log(Beta / temp9) +
                               Alpha * log(Delta / q)))[index1]
      ans[q <= 0] <- 0
      ans[q == Inf] <- 1
    }
  } else {
    if (log.p) {
      ans <- log1p(-(Alpha / temp9) * (q / Delta)^(Beta))
      ans[index1] <-
        log(((Beta / temp9) * (Delta / q)^(Alpha))[index1])
      ans[q <= 0] <- 0
      ans[q == Inf] <- -Inf
    } else {
      ans <- -expm1(log(Alpha / temp9) + Beta * log(q / Delta))
      ans[index1] <-
        ((Beta / temp9) * (Delta / q)^(Alpha))[index1]
      ans[q <= 0] <- 1
      ans[q == Inf] <- 0
    }
  }
  indexTF <-
    (scale_ald > 0) & (tau > 0) & (tau < 1) & (kappa > 0) # &
  ans[!indexTF] <- NaN
  ans
}
# Huber's Least Favourable  -
#' @rdname dens_cumdistr_quan_rand
#' @export
dhuber <- function(x,
                   k = 0.862,
                   mu = 0,
                   sigma = 1,
                   log = FALSE) {
  edhuber <- function(x,
                      k = 0.862,
                      mu = 0,
                      sigma = 1,
                      log = FALSE) {
    if (!is.logical(log.arg <- log) || length(log) != 1) {
      cat_warn("bad input for argument 'log'")
    }
    rm(log)
    zedd <- (x - mu) / sigma
    fk <- dnorm(k)
    eps <- 1 - 1 / (pnorm(k) - pnorm(-k) + 2 * fk / k)
    ceps <- 1 / (pnorm(k) - pnorm(-k) + 2 * fk / k)
    if (log.arg) {
      val <- log(ceps) + dnorm(zedd, log = TRUE)
      val[zedd < (-k)] <-
        (log(ceps) + log(fk) + (k * (zedd + k)))[zedd < (-k)]
      val[zedd > (+k)] <-
        (log(ceps) + log(fk) + (-k * (zedd - k)))[zedd > (+k)]
    } else {
      val <- (ceps) * dnorm(zedd)
      val[zedd < (-k)] <-
        ((ceps) * fk * exp(k * (zedd + k)))[zedd < (-k)]
      val[zedd > (+k)] <-
        ((ceps) * fk * exp(-k * (zedd - k)))[zedd > (+k)]
    }
    list(
      val = if (log.arg) {
        val - log(sigma)
      } else {
        val / sigma
      },
      eps = eps
    )
  }
  edhuber(x, k, mu, sigma, log = log)$val
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rhuber <- function(n,
                   k = 0.862,
                   mu = 0,
                   sigma = 1) {
  qhuber(
    p = runif(n, 0, 1),
    k = k,
    mu = mu,
    sigma = sigma
  )
}

#' @rdname dens_cumdistr_quan_rand
#' @export
qhuber <- function(p,
                   k = 0.862,
                   mu = 0,
                   sigma = 1,
                   lower.tail = TRUE,
                   log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }
  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  cnorm <- sqrt(2 * pi) * ((2 * pnorm(k) - 1) + 2 * dnorm(k) / k)
  if (lower.tail) {
    if (log.p) {
      ln.p <- p
      x <- pmin(exp(ln.p), -expm1(ln.p))
    } else {
      x <- pmin(p, 1 - p)
    }
  } else {
    if (log.p) {
      ln.p <- p
      x <- pmin(-expm1(ln.p), exp(ln.p))
    } else {
      x <- pmin(1 - p, p)
    }
  }
  q <- ifelse(x <= sqrt(2 * pi) * dnorm(k) / (k * cnorm),
              log(k * cnorm * x) / k - k / 2,
              qnorm(abs(
                1 - pnorm(k) + x * cnorm / sqrt(2 * pi) -
                  dnorm(k) / k
              ))
  )
  ans <- if (lower.tail) {
    if (log.p) {
      ifelse(exp(ln.p) < 0.5, mu + q * sigma, mu - q * sigma)
    } else {
      ifelse(p < 0.5, mu + q * sigma, mu - q * sigma)
    }
  } else {
    if (log.p) {
      ifelse(exp(ln.p) > 0.5, mu + q * sigma, mu - q * sigma)
    } else {
      ifelse(p > 0.5, mu + q * sigma, mu - q * sigma)
    }
  }
  ans[k <= 0 | sigma <= 0] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
phuber <- function(q,
                   k = 0.862,
                   mu = 0,
                   sigma = 1,
                   lower.tail = TRUE,
                   log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }
  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  A1 <- (2 * dnorm(k) / k - 2 * pnorm(-k))
  eps <- A1 / (1 + A1)
  zedd <- (q - mu) / sigma
  x <- -abs(zedd)
  p <- ifelse(x <= -k,
              exp(k^2 / 2) / k * exp(k * x) / sqrt(2 * pi),
              dnorm(k) / k + pnorm(x) - pnorm(-k)
  )
  if (lower.tail) {
    if (log.p) {
      ans <- ifelse(zedd <= 0, log(p) + log1p(-eps),
                    log1p(exp(log(p) + log1p(-eps)))
      )
    } else {
      ans <- ifelse(zedd <= 0, exp(log(p) + log1p(-eps)),
                    -expm1(log(p) + log1p(-eps))
      )
    }
  } else {
    if (log.p) {
      ans <- ifelse(zedd <= 0, log1p(exp(log(p) + log1p(-eps))),
                    log(p) + log1p(-eps)
      )
    } else {
      ans <- ifelse(zedd <= 0, -expm1(log(p) + log1p(-eps)),
                    exp(log(p) + log1p(-eps))
      )
    }
  }
  ans
}
# Exponential Geometric --
#' @rdname dens_cumdistr_quan_rand
#' @export
dexpgeom <- function(x,
                     scale = 1,
                     shape,
                     log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1) {
    cat_warn("bad input for argument 'log'")
  }
  rm(log)
  N <- max(length(x), length(scale), length(shape))
  if (length(x) != N) {
    x <- rep_len(x, N)
  }
  if (length(scale) != N) {
    scale <- rep_len(scale, N)
  }
  if (length(shape) != N) {
    shape <- rep_len(shape, N)
  }
  logdensity <- rep_len(log(0), N)
  if (any(xok <- (x > 0))) {
    temp1 <- -x[xok] / scale[xok]
    logdensity[xok] <- -log(scale[xok]) + log1p(-shape[xok]) +
      temp1 - 2 * log1p(-shape[xok] * exp(temp1))
  }
  logdensity[(scale <= 0) | (shape <= 0) | (shape >= 1)] <- NaN
  if (log.arg) {
    logdensity
  } else {
    exp(logdensity)
  }
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pexpgeom <- function(q, scale = 1, shape) {
  temp1 <- -q / scale
  ans <- -expm1(temp1) / (1 - shape * exp(temp1))
  ans[q <= 0] <- 0
  ans[(scale <= 0) | (shape <= 0) | (shape >= 1)] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qexpgeom <- function(p, scale = 1, shape) {
  ans <- (-scale) * log((p - 1) / (p * shape - 1))
  ans[(scale <= 0) | (shape <= 0) | (shape >= 1)] <- NaN
  ans[p < 0] <- NaN
  ans[p > 1] <- NaN
  ans[p == 0] <- 0
  ans[p == 1] <- Inf
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rexpgeom <- function(n, scale = 1, shape) {
  ans <- qexpgeom(runif(n), shape = shape, scale = scale)
  ans[(scale <= 0) | (shape <= 0) | (shape >= 1)] <- NaN
  ans
}
# Exponential Logarithmic  -
#' @rdname dens_cumdistr_quan_rand
#' @export
dexplog <- function(x,
                    scale = 1,
                    shape,
                    log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1) {
    cat_warn("bad input for argument 'log'")
  }
  rm(log)
  N <- max(length(x), length(scale), length(shape))
  if (length(x) != N) {
    x <- rep_len(x, N)
  }
  if (length(scale) != N) {
    scale <- rep_len(scale, N)
  }
  if (length(shape) != N) {
    shape <- rep_len(shape, N)
  }
  logdensity <- rep_len(log(0), N)
  if (any(xok <- (x > 0))) {
    temp1 <- -x[xok] / scale[xok]
    logdensity[xok] <- -log(-log(shape[xok])) - log(scale[xok]) +
      log1p(-shape[xok]) + temp1 -
      log1p(-(1 - shape[xok]) * exp(temp1))
  }
  logdensity[(scale <= 0) | (shape <= 0) | (shape >= 1)] <- NaN
  if (log.arg) {
    logdensity
  } else {
    exp(logdensity)
  }
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pexplog <- function(q, scale = 1, shape) {
  ans <- 1 - log1p(-(1 - shape) * exp(-q / scale)) / log(shape)
  ans[q <= 0] <- 0
  ans[(scale <= 0) | (shape <= 0) | (shape >= 1)] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qexplog <- function(p, scale = 1, shape) {
  ans <- -scale * (log1p(-shape^(1.0 - p)) - log1p(-shape))
  ans[(scale <= 0) | (shape <= 0) | (shape >= 1)] <- NaN
  ans[p < 0] <- NaN
  ans[p > 1] <- NaN
  ans[p == 0] <- 0
  ans[p == 1] <- Inf
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rexplog <- function(n, scale = 1, shape) {
  qexplog(
    p = runif(n, min = 0, max = 1),
    scale = scale,
    shape = shape
  )
}
# Positive-Normal Distribution -
#' @rdname dens_cumdistr_quan_rand
#' @export
dposnorm <- function(x,
                     mean = 0,
                     sd = 1,
                     log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1) {
    cat_warn("bad input for argument 'log'")
  }
  rm(log)
  L <- max(length(x), length(mean), length(sd))
  if (length(x) != L) {
    x <- rep_len(x, L)
  }
  if (length(mean) != L) {
    mean <- rep_len(mean, L)
  }
  if (length(sd) != L) {
    sd <- rep_len(sd, L)
  }
  if (log.arg) {
    ifelse(x < 0,
           log(0),
           dnorm(
             x,
             mean = mean,
             sd = sd,
             log = TRUE
           ) -
             pnorm(mean / sd, log.p = TRUE)
    )
  } else {
    ifelse(x < 0, 0, dnorm(x = x, mean = mean, sd = sd) / pnorm(mean / sd))
  }
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pposnorm <- function(q,
                     mean = 0,
                     sd = 1,
                     lower.tail = TRUE,
                     log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }
  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  ans <- (pnorm(q, mean = mean, sd = sd) -
            pnorm(0, mean = mean, sd = sd)) / pnorm(mean / sd)
  ans[q <= 0] <- 0
  if (lower.tail) {
    if (log.p) {
      log(ans)
    } else {
      ans
    }
  } else {
    if (log.p) {
      log1p(-ans)
    } else {
      1 - ans
    }
  }
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qposnorm <- function(p,
                     mean = 0,
                     sd = 1,
                     lower.tail = TRUE,
                     log.p = FALSE) {
  if (!is.logical(log.arg <- log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  rm(log.p) # 20150102 KaiH
  if (lower.tail) {
    if (log.arg) {
      p <- exp(p)
    }
  } else {
    p <- if (log.arg) {
      -expm1(p)
    } else {
      1 - p
    }
  }
  qnorm(
    p = p + (1 - p) * pnorm(0, mean = mean, sd = sd),
    mean = mean,
    sd = sd
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rposnorm <- function(n, mean = 0, sd = 1) {
  qnorm(
    p = runif(n, min = pnorm(0, mean = mean, sd = sd)),
    mean = mean,
    sd = sd
  )
}
# Beta-Normal  --
#' @rdname dens_cumdistr_quan_rand
#' @export
dbetanorm <-
  function(x,
           shape1,
           shape2,
           mean = 0,
           sd = 1,
           log = FALSE) {
    if (!is.logical(log.arg <- log) || length(log) != 1) {
      cat_warn("bad input for argument 'log'")
    }
    rm(log)
    logden <-
      dnorm(
        x = x,
        mean = mean,
        sd = sd,
        log = TRUE
      ) +
      (shape1 - 1) * pnorm(
        q = x,
        mean = mean,
        sd = sd,
        log.p = TRUE
      ) +
      (shape2 - 1) * pnorm(
        q = x,
        mean = mean,
        sd = sd,
        log.p = TRUE,
        lower.tail = FALSE
      ) -
      lbeta(shape1, shape2)
    logden[is.infinite(x)] <- log(0) # 20141210 KaiH
    if (log.arg) {
      logden
    } else {
      exp(logden)
    }
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
pbetanorm <- function(q,
                      shape1,
                      shape2,
                      mean = 0,
                      sd = 1,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  pbeta(
    q = pnorm(q = q, mean = mean, sd = sd),
    shape1 = shape1,
    shape2 = shape2,
    lower.tail = lower.tail,
    log.p = log.p
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qbetanorm <- function(p,
                      shape1,
                      shape2,
                      mean = 0,
                      sd = 1,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  qnorm(
    p = qbeta(
      p = p,
      shape1 = shape1,
      shape2 = shape2,
      lower.tail = lower.tail,
      log.p = log.p
    ),
    mean = mean,
    sd = sd
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rbetanorm <- function(n,
                      shape1,
                      shape2,
                      mean = 0,
                      sd = 1) {
  qnorm(
    p = qbeta(
      p = runif(n),
      shape1 = shape1,
      shape2 = shape2
    ),
    mean = mean,
    sd = sd
  )
}
# Folded-Normal  -
#' @rdname dens_cumdistr_quan_rand
#' @export
dfoldnorm <- function(x,
                      mean = 0,
                      sd = 1,
                      a1 = 1,
                      a2 = 1,
                      log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1) {
    cat_warn("bad input for argument 'log'")
  }
  rm(log)
  ans <- dnorm(x = x / (a1 * sd) - mean / sd) / (a1 * sd) +
    dnorm(x = x / (a2 * sd) + mean / sd) / (a2 * sd)
  ans[x < 0] <- 0
  ans[a1 <= 0 | a2 <= 0] <- NA
  ans[sd <= 0] <- NA
  if (log.arg) {
    log(ans)
  } else {
    ans
  }
} # dfoldnorm
#' @rdname dens_cumdistr_quan_rand
#' @export
pfoldnorm <- function(q,
                      mean = 0,
                      sd = 1,
                      a1 = 1,
                      a2 = 1,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  if (!is.logical(lower.tail) || length(lower.tail) != 1) {
    cat_warn("bad input for argument 'lower.tail'")
  }
  if (!is.logical(log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  if (lower.tail) {
    if (log.p) {
      ans <- log(pnorm(q = q / (a1 * sd) - mean / sd) -
                   pnorm(q = -q / (a2 * sd) - mean / sd))
      ans[q <= 0] <- -Inf
      ans[q == Inf] <- 0
    } else {
      ans <- pnorm(q = q / (a1 * sd) - mean / sd) -
        pnorm(q = -q / (a2 * sd) - mean / sd)
      ans[q <= 0] <- 0
      ans[q == Inf] <- 1
    }
  } else {
    if (log.p) {
      ans <-
        log(pnorm(
          q = q / (a1 * sd) - mean / sd,
          lower.tail = FALSE
        ) +
          pnorm(q = -q / (a2 * sd) - mean / sd))
      ans[q <= 0] <- 0
      ans[q == Inf] <- -Inf
    } else {
      ans <- pnorm(
        q = q / (a1 * sd) - mean / sd,
        lower.tail = FALSE
      ) +
        pnorm(q = -q / (a2 * sd) - mean / sd)
      ans[q <= 0] <- 1
      ans[q == Inf] <- 0
    }
  }
  ans[a1 <= 0 | a2 <= 0] <- NaN
  ans[sd <= 0] <- NaN
  ans
} # pfoldnorm
#' @rdname dens_cumdistr_quan_rand
#' @export
qfoldnorm <- function(p,
                      mean = 0,
                      sd = 1,
                      a1 = 1,
                      a2 = 1,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  if (!is.logical(log.arg <- log.p) || length(log.p) != 1) {
    cat_warn("bad input for argument 'log.p'")
  }
  rm(log.p)
  if (lower.tail) {
    if (log.arg) {
      p <- exp(p)
    }
  } else {
    p <- if (log.arg) {
      -expm1(p)
    } else {
      1 - p
    }
  }
  L <- max(
    length(p), length(mean), length(sd),
    length(a1), length(a2)
  )
  if (length(p) != L) {
    p <- rep_len(p, L)
  }
  if (length(mean) != L) {
    mean <- rep_len(mean, L)
  }
  if (length(sd) != L) {
    sd <- rep_len(sd, L)
  }
  if (length(a1) != L) {
    a1 <- rep_len(a1, L)
  }
  if (length(a2) != L) {
    a2 <- rep_len(a2, L)
  }
  ans <- p + mean + sd + a1 + a2
  bad0 <- !is.finite(mean) | !is.finite(sd) | sd <= 0 |
    !is.finite(a1) | !is.finite(a2)
  bad <- bad0 | !is.finite(p) | p <= 0 | 1 <= p
  is.easy <- !bad & a1 == 1 & a2 == 1
  if (FALSE && any(is.easy)) {
    ans[is.easy] <- sqrt(qchisq(p[is.easy], 1,
                                ncp = (mean[is.easy] / sd[is.easy])^2
    )) *
      sd[is.easy]
  }
  lo <- numeric(L) - 0.5
  approx.ans <- lo # True at lhs
  hi <- 2 * sd + 10.5
  dont.iterate <- bad # | is.easy
  done <- dont.iterate | p <= pfoldnorm(hi, mean, sd, a1, a2)
  iter <- 0
  max.iter <- round(log2(.Machine$double.xmax)) - 2
  max.iter <- round(log2(1e300)) - 2
  while (!all(done) && iter < max.iter) {
    lo[!done] <- hi[!done]
    hi[!done] <- 2 * hi[!done] + 10.5 # Bug fixed
    done[!done] <-
      (p[!done] <= pfoldnorm(
        hi[!done],
        mean = mean[!done],
        sd = sd[!done],
        a1 = a1[!done],
        a2 = a2[!done]
      ))
    iter <- iter + 1
  }
  foo <- function(q, mean, sd, a1, a2, p) {
    pfoldnorm(q, mean, sd, a1, a2) - p
  }
  lhs <- dont.iterate # | (p <= dfoldnorm(0, mean, sd, a1, a2))
  if (any(!lhs)) {
    approx.ans[!lhs] <-
      bisection_basic(
        foo,
        lo[!lhs],
        hi[!lhs],
        # tol = 1e-8,
        mean = mean[!lhs],
        sd = sd[!lhs],
        a1 = a1[!lhs],
        a2 = a2[!lhs],
        p = p[!lhs]
      )
    ans[!lhs] <- approx.ans[!lhs] # tmp
  }
  ans[!bad0 & !is.na(p) & p == 0] <- 0
  ans[!bad0 & !is.na(p) & p == 1] <- Inf
  ans[!bad0 & !is.na(p) & p < 0] <- NaN
  ans[!bad0 & !is.na(p) & p > 1] <- NaN
  ans[bad0] <- NaN
  ans
} # qfoldnorm
#' @rdname dens_cumdistr_quan_rand
#' @export
rfoldnorm <- function(n,
                      mean = 0,
                      sd = 1,
                      a1 = 1,
                      a2 = 1) {
  X <- rnorm(n, mean = mean, sd = sd)
  ans <- pmax(a1 * X, -a2 * X)
  ans[a1 <= 0 | a2 <= 0] <- NA
  ans[sd <= 0] <- NA
  ans
}
bisection_basic <-
  function(f,
           a,
           b,
           tol = 1e-9,
           nmax = NULL,
           ...) {
    if (any(is.infinite(b))) {
      cat_warn("replacing 'b' values of Inf by a large value")
      b[is.infinite(b)] <- .Machine$double.xmax / 4
    }

    if (is.null(nmax)) {
      nmax <- round(log2(max(b - a)) - log2(min(tol))) + 4
      if (!is.finite(nmax)) {
        nmax <- log2(.Machine$double.xmax) - 5
      }
    }
    signtest <- (sign(f(a, ...)) * sign(f(b, ...)) <= 0)

    if (!all(signtest)) {
      cat_warn(
        "roots do not exist between 'a' and 'b'. ",
        "Some answers may be misleading."
      )
    }

    N <- 1
    while (N <= nmax) {
      mid <- (a + b) / 2
      save.f <- f(mid, ...)
      if (all(save.f == 0 | (b - a) / 2 < tol)) {
        return(mid)
      }
      N <- N + 1
      vecTF <- sign(save.f) == sign(f(a, ...))
      a[vecTF] <- mid[vecTF]
      b[!vecTF] <- mid[!vecTF]
    }

    cat_warn("did not coverge. Returning final root")
    mid
  }

# The Burr -
#' @rdname dens_cumdistr_quan_rand
#' @export
dburr <-
  function(x,
           shape1 = 2,
           shape2 = 1,
           scale = 0.5,
           log = FALSE) {
    d <-
      (log((shape1 * shape2) / scale) + (shape2 - 1) * log(x / scale)) - (shape1 + 1) * log(1 + (x / scale)^
                                                                                              shape2)

    if (!log) {
      d <- exp(d)
    }
    return(d)
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
pburr <-
  function(q,
           shape1 = 2,
           shape2 = 1,
           scale = 0.5,
           log.p = FALSE,
           lower.tail = TRUE) {
    p <- 1 - 1 / ((1 + (q / scale)^shape2)^shape1)

    p <- if (lower.tail) {
      p
    } else {
      1 - p
    }
    p <- if (log.p) {
      log(p)
    } else {
      p
    }

    return(p)
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
qburr <-
  function(p,
           shape1 = 2,
           shape2 = 1,
           scale = 0.5,
           log.p = FALSE,
           lower.tail = TRUE) {
    p <- if (log.p) {
      exp(p)
    } else {
      p
    }
    p <- if (lower.tail) {
      p
    } else {
      1 - p
    }

    q <- (((1 - p)^(-1 / shape1) - 1)^(1 / shape2)) * scale

    return(q)
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
rburr <- function(n,
                  shape1 = 2,
                  shape2 = 1,
                  scale = 0.5) {
  r <-
    qburr(runif(n),
          shape1 = shape1,
          shape2 = shape2,
          scale = scale
    )
  return(r)
}


# Loglogistic  --

#' @rdname dens_cumdistr_quan_rand
#' @export
dllogis <-
  function(x,
           shape,
           rate = 1,
           scale = 1 / rate,
           log = FALSE) {
    actuar::dllogis(
      x = x,
      shape = shape,
      scale = scale,
      rate = rate,
      log = log
    )
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
pllogis <- function(q,
                    shape,
                    rate = 1,
                    scale = 1 / rate,
                    lower.tail = TRUE,
                    log.p = FALSE) {
  actuar::pllogis(
    q = q,
    shape = shape,
    scale = scale,
    rate = rate,
    lower.tail = lower.tail,
    log.p = log.p
  )
}

#' @rdname dens_cumdistr_quan_rand
#' @export
qllogis <- function(p,
                    shape,
                    rate = 1,
                    scale = 1 / rate,
                    lower.tail = TRUE,
                    log.p = FALSE) {
  actuar::qllogis(
    p = p,
    shape = shape,
    scale = scale,
    rate = rate,
    lower.tail = lower.tail,
    log.p = log.p
  )
}


#' @rdname dens_cumdistr_quan_rand
#' @export
rllogis <- function(n,
                    shape,
                    rate = 1,
                    scale = 1 / rate) {
  actuar::rllogis(
    n = n,
    shape = shape,
    scale = scale,
    rate = rate
  )
}

# BetaPrime -

#' @rdname dens_cumdistr_quan_rand
#' @export
dbetaprime <- function(x, alpha, beta, log = FALSE) {
  out <-
    (alpha - 1) * log(x) - (alpha + beta) * log1p(x) - lbeta(alpha, beta)

  if (!log) {
    out <- exp(out)
  }

  return(out)
}

#' @rdname dens_cumdistr_quan_rand
#' @export
pbetaprime <-
  function(q,
           alpha,
           beta,
           lower.tail = TRUE,
           log.p = FALSE) {
    x <- q / (1 + q)

    out <-
      pbeta(x, alpha, beta, lower.tail = lower.tail, log.p = log.p)

    return(out)
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
qbetaprime <-
  function(p,
           alpha,
           beta,
           lower.tail = TRUE,
           log.p = FALSE) {
    x <- qbeta(p, alpha, beta, lower.tail = lower.tail, log.p = log.p)

    q <- x / (1 - x)

    return(q)
  }

#' @rdname dens_cumdistr_quan_rand
#' @export
rbetaprime <- function(n, alpha, beta) {
  x <- rbeta(n, alpha, beta)

  return(x / (1 - x))
}


# HalfNorm -

#' @rdname dens_cumdistr_quan_rand
#' @export
dhalfnorm <- function(x,
                      mean = 0,
                      theta = sqrt(pi / 2),
                      log = FALSE) {
  sd.norm <- theta2sd(theta)



  if (log) {
    d <- ifelse(x - mean < 0,
                -Inf,
                log(2) + dnorm(
                  x - mean,
                  mean = 0,
                  sd = sd.norm,
                  log = TRUE
                )
    )
  } else {
    d <-
      ifelse(x - mean < 0, 0, 2 * dnorm(x - mean, mean = 0, sd = sd.norm))
  }

  d
}
#' @rdname dens_cumdistr_quan_rand
#' @export
phalfnorm <-
  function(q,
           mean = 0,
           theta = sqrt(pi / 2),
           lower.tail = TRUE,
           log.p = FALSE) {
    sd.norm <- theta2sd(theta)



    p <-
      ifelse(q - mean < 0, 0, 2 * pnorm(q - mean, mean = 0, sd = sd.norm) -
               1)
    if (lower.tail == FALSE) {
      p <- 1 - p
    }
    if (log.p) {
      p <- log(p)
    }

    p
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
qhalfnorm <-
  function(p,
           mean = 0,
           theta = sqrt(pi / 2),
           lower.tail = TRUE,
           log.p = FALSE) {
    sd.norm <- theta2sd(theta)

    if (log.p) {
      p <- exp(p)
    }
    if (lower.tail == FALSE) {
      p <- 1 - p
    }
    ifelse(p < 0, NaN, qnorm((p + 1) / 2, mean = 0, sd = sd.norm)) + mean
  }
#' @rdname dens_cumdistr_quan_rand
#' @export
rhalfnorm <- function(n,
                      mean = 0,
                      theta = sqrt(pi / 2)) {
  sd.norm <- theta2sd(theta)
  abs(rnorm(n, mean = 0, sd = sd.norm)) + mean
}

sd2theta <- function(sd) {
  sqrt(pi / 2) / sd
}
theta2sd <- function(theta) {
  sqrt(pi / 2) / theta
}


# Chi-Squared  --

#' @rdname dens_cumdistr_quan_rand
#' @export
qchisq <- function(p,
                   df,
                   ncp = 0,
                   lower.tail = TRUE,
                   log.p = FALSE) {
  stats::qchisq(
    p = p,
    df = ceiling(df * 4) / 4,
    ncp = round(ncp, 1),
    lower.tail = lower.tail,
    log.p = log.p
  )
}

#' @rdname dens_cumdistr_quan_rand
#' @export
pchisq <- function(q,
                   df,
                   ncp = 0,
                   lower.tail = TRUE,
                   log.p = FALSE) {
  stats::pchisq(
    q = q,
    df = ceiling(df * 4) / 4,
    ncp = round(ncp, 1),
    lower.tail = lower.tail,
    log.p = log.p
  )
}

#' @rdname dens_cumdistr_quan_rand
#' @export
dchisq <- function(x, df, ncp = 0, log = FALSE) {
  stats::dchisq(
    x = x,
    df = ceiling(df * 4) / 4,
    ncp = round(ncp, 1),
    log = log
  )
}

#' @rdname dens_cumdistr_quan_rand
#' @export
rchisq <- function(n, df, ncp = 0) {
  stats::rchisq(
    n = n,
    df = ceiling(df * 4) / 4,
    ncp = round(ncp, 1)
  )
}


### DISCRETE --

# Discrete normal -
#' @rdname dens_cumdistr_quan_rand
#' @export
ddnorm <- function(x, mean, sd) {
  ddscrt(
    x = x,
    dist = "norm",
    mean = mean,
    sd = sd
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qdnorm <- function(p, mean, sd, lower.tail = TRUE) {
  q <- qdscrt(
    p = p,
    dist = "norm",
    mean = mean,
    sd = sd,
    lower.tail = lower.tail
  )
  ifelse(q < 0, 0, q)
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pdnorm <- function(q, mean, sd, lower.tail = TRUE) {
  pdscrt(
    q = q,
    dist = "norm",
    mean = mean,
    sd = sd,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rdnorm <- function(n, mean, sd) {
  rdscrt(
    n = n,
    dist = "norm",
    mean = mean,
    sd = sd
  )
}

# Discrete lognormal --
#' @rdname dens_cumdistr_quan_rand
#' @export
ddlnorm <- function(x, meanlog, sdlog) {
  ddscrt(
    x = x,
    dist = "lnorm",
    meanlog = meanlog,
    sdlog = sdlog
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qdlnorm <- function(p, meanlog, sdlog, lower.tail = TRUE) {
  q <- qdscrt(
    p = p,
    dist = "lnorm",
    meanlog = meanlog,
    sdlog = sdlog,
    lower.tail = lower.tail
  )
  ifelse(q <= 0, 0, q)
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pdlnorm <- function(q, meanlog, sdlog, lower.tail = TRUE) {
  pdscrt(
    q = q,
    dist = "lnorm",
    meanlog = meanlog,
    sdlog = sdlog,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rdlnorm <- function(n, meanlog, sdlog) {
  rdscrt(
    n = n,
    dist = "lnorm",
    meanlog = meanlog,
    sdlog = sdlog
  )
}

# Discrete exp -
#' @rdname dens_cumdistr_quan_rand
#' @export
ddexp <- function(x, rate) {
  ddscrt(
    x = x,
    dist = "exp",
    rate = rate
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qdexp <- function(p, rate, lower.tail = TRUE) {
  q <- qdscrt(
    p = p,
    dist = "exp",
    rate = rate,
    lower.tail = lower.tail
  )
  ifelse(q < 0, 0, q)
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pdexp <- function(q, rate, lower.tail = TRUE) {
  pdscrt(
    q = q,
    dist = "exp",
    rate = rate,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rdexp <- function(n, rate) {
  rdscrt(
    n = n,
    dist = "exp",
    rate = rate
  )
}

# Discrete cauchy -
#' @rdname dens_cumdistr_quan_rand
#' @export
ddcauchy <- function(x, location, scale) {
  ddscrt(
    x = x,
    dist = "cauchy",
    location = location,
    scale = scale
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qdcauchy <- function(p, location, scale, lower.tail = TRUE) {
  q <- qdscrt(
    p = p,
    dist = "cauchy",
    location = location,
    scale = scale,
    lower.tail = lower.tail
  )
  ifelse(q < 0, 0, q)
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pdcauchy <- function(q, location, scale, lower.tail = TRUE) {
  pdscrt(
    q = q,
    dist = "cauchy",
    location = location,
    scale = scale,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rdcauchy <- function(n, location, scale) {
  rdscrt(
    n = n,
    dist = "cauchy",
    location = location,
    scale = scale
  )
}


# Discrete Generalized Error --

#' @rdname dens_cumdistr_quan_rand
#' @export
ddged <- function(x, mean, sd, nu) {
  ddscrt(
    x = x,
    dist = "ged",
    mean = mean,
    sd = round(sd * 4) / 4,
    nu = ceiling(nu * 10) / 10
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qdged <- function(p, mean, sd, nu, lower.tail = TRUE) {
  q <- qdscrt(
    p = p,
    dist = "ged",
    mean = mean,
    sd = round(sd * 4) / 4,
    nu = ceiling(nu * 10) / 10,
    lower.tail = lower.tail
  )
  ifelse(q < 0, 0, q)
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pdged <- function(q, mean, sd, nu, lower.tail = TRUE) {
  pdscrt(
    q = q,
    dist = "ged",
    mean = mean,
    sd = round(sd * 4) / 4,
    nu = ceiling(nu * 10) / 10,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rdged <- function(n, mean, sd, nu) {
  rdscrt(
    n = n,
    dist = "ged",
    mean = mean,
    sd = round(sd * 4) / 4,
    nu = ceiling(nu * 10) / 10
  )
}


# Discrete Uniform Distribution -

#' @rdname dens_cumdistr_quan_rand
#' @export
ddunif <- function(x, min, max) {
  ddscrt(
    x = x,
    dist = "unif",
    min = min,
    max = max
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qdunif <- function(p, min, max, lower.tail = TRUE) {
  q <- qdscrt(
    p = p,
    dist = "unif",
    min = min,
    max = max,
    lower.tail = lower.tail
  )
  ifelse(q < 0, 0, q)
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pdunif <- function(q, min, max, lower.tail = TRUE) {
  pdscrt(
    q = q,
    dist = "unif",
    min = min,
    max = max,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rdunif <- function(n, min, max) {
  rdscrt(
    n = n,
    dist = "unif",
    min = min,
    max = max
  )
}

# Round Geometric -
#' @rdname dens_cumdistr_quan_rand
#' @export
drndgeom <- function(x, prob, log = FALSE) {
  stats::dgeom(
    x = round(x),
    prob = prob,
    log = log
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qrndgeom <- function(p, prob, lower.tail = TRUE) {
  stats::qgeom(
    p = p,
    prob = prob,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
prndgeom <- function(q, prob, lower.tail = TRUE) {
  stats::pgeom(
    q = round(q),
    prob = prob,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rrndgeom <- function(n, prob) {
  qrndgeom(p = runif(n, 0, 1), prob = prob)
}

# Round Poisson --
#' @rdname dens_cumdistr_quan_rand
#' @export
drndpois <- function(x, lambda, log = FALSE) {
  stats::dpois(
    x = round(x),
    lambda = lambda,
    log = log
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qrndpois <- function(p, lambda, lower.tail = TRUE) {
  stats::qpois(
    p = p,
    lambda = lambda,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
prndpois <- function(q, lambda, lower.tail = TRUE) {
  stats::ppois(
    q = round(q),
    lambda = lambda,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rrndpois <- function(n, lambda) {
  qrndpois(p = runif(n, 0, 1), lambda = lambda)
}

# Round Binomial -
#' @rdname dens_cumdistr_quan_rand
#' @export
drndbinom <- function(x, size, prob, log = FALSE) {
  stats::dbinom(
    x = round(x),
    size = round(size),
    prob = prob,
    log = log
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qrndbinom <- function(p, size, prob, lower.tail = TRUE) {
  stats::qbinom(
    p = p,
    size = round(size),
    prob = prob,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
prndbinom <- function(q, size, prob, lower.tail = TRUE) {
  stats::pbinom(
    q = round(q, 0),
    size = round(size),
    prob = prob,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rrndbinom <- function(n, size, prob) {
  qrndbinom(
    p = runif(n, 0, 1),
    size = round(size),
    prob = prob
  )
}

# Round Negative binomial --
#' @rdname dens_cumdistr_quan_rand
#' @export
drndnbinom <- function(x, size, mu, log = FALSE) {
  stats::dnbinom(
    x = round(x),
    size = size,
    mu = mu,
    log = log
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qrndnbinom <- function(p, size, mu, lower.tail = TRUE) {
  stats::qnbinom(
    p = p,
    size = size,
    mu = mu,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
prndnbinom <- function(q, size, mu, lower.tail = TRUE) {
  stats::pnbinom(
    q = round(q),
    size = size,
    mu = mu,
    lower.tail = lower.tail
  )
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rrndnbinom <- function(n, size, mu) {
  qrndnbinom(
    p = runif(n, 0, 1),
    size = size,
    mu = mu
  )
}
# Zero-Inflated Poisson --

#' @rdname dens_cumdistr_quan_rand
#' @export
dzipois <- function(x,
                    lambda,
                    pstr0 = 0,
                    log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1) {
    cat_warn("bad input for argument 'log'")
  }
  rm(log)

  LLL <- max(length(x), length(lambda), length(pstr0))
  if (length(x) != LLL) {
    x <- rep_len(x, LLL)
  }
  if (length(lambda) != LLL) {
    lambda <- rep_len(lambda, LLL)
  }
  if (length(pstr0) != LLL) {
    pstr0 <- rep_len(pstr0, LLL)
  }

  ans <- x + lambda + pstr0



  index0 <- (x == 0)
  if (log.arg) {
    ans[index0] <- log(pstr0[index0] + (1 - pstr0[index0]) *
                         drndpois(x[index0], lambda[index0]))
    ans[!index0] <- log1p(-pstr0[!index0]) +
      drndpois(x[!index0], lambda[!index0], log = TRUE)
  } else {
    ans[index0] <- pstr0[index0] + (1 - pstr0[index0]) *
      drndpois(x[index0], lambda[index0])
    ans[!index0] <- (1 - pstr0[!index0]) *
      drndpois(x[!index0], lambda[!index0])
  }


  deflat.limit <- -1 / expm1(lambda)
  ans[pstr0 < deflat.limit] <- NaN
  ans[pstr0 > 1] <- NaN

  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
pzipois <- function(q, lambda, pstr0 = 0) {
  LLL <- max(length(pstr0), length(lambda), length(q))
  if (length(pstr0) != LLL) {
    pstr0 <- rep_len(pstr0, LLL)
  }
  if (length(lambda) != LLL) {
    lambda <- rep_len(lambda, LLL)
  }
  if (length(q) != LLL) {
    q <- rep_len(q, LLL)
  }

  ans <- prndpois(q, lambda)
  ans <- ifelse(q < 0, 0, pstr0 + (1 - pstr0) * ans)


  deflat.limit <- -1 / expm1(lambda)
  ans[pstr0 < deflat.limit] <- NaN
  ans[pstr0 > 1] <- NaN


  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
qzipois <- function(p, lambda, pstr0 = 0) {
  LLL <- max(length(p), length(lambda), length(pstr0))
  if (length(p) != LLL) {
    p <- rep_len(p, LLL)
  }
  if (length(lambda) != LLL) {
    lambda <- rep_len(lambda, LLL)
  }
  if (length(pstr0) != LLL) {
    pstr0 <- rep_len(pstr0, LLL)
  }
  ans <- rep_len(NA_real_, LLL)
  deflat.limit <- -1 / expm1(lambda)

  ans[p <= pstr0] <- 0
  pindex <- (pstr0 < p) & (deflat.limit <= pstr0)
  ans[pindex] <-
    qrndpois((p[pindex] - pstr0[pindex]) / (1 - pstr0[pindex]),
             lambda = lambda[pindex]
    )

  ans[pstr0 < deflat.limit] <- NaN
  ans[1 < pstr0] <- NaN


  ans[lambda < 0] <- NaN
  ans[p < 0] <- NaN
  ans[1 < p] <- NaN
  ans
}
#' @rdname dens_cumdistr_quan_rand
#' @export
rzipois <- function(n, lambda, pstr0 = 0) {
  qzipois(runif(n), lambda, pstr0 = pstr0)
}

