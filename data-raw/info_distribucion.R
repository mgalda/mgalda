structure(
  list(
    dist = c(
      "beta",
      "betanorm",
      "betaprime",
      "burr",
      "cauchy",
      "chisq",
      "exp",
      "expgeom",
      "explog",
      "f",
      "gamma",
      "ged",
      "gev",
      "gpd",
      "gumbel",
      "halfnorm",
      "huber",
      "invchisq",
      "laplace",
      "llogis",
      "lnorm",
      "logis",
      "loglap",
      "norm",
      "pareto",
      "posnorm",
      "power",
      "rayleigh",
      "sc_t2",
      "sged",
      "snorm",
      "sstd",
      "std",
      "unif",
      "weibull",
      "dcauchy",
      "dexp",
      "dged",
      "dlnorm",
      "dnorm",
      "dunif",
      "rndbinom",
      "rndgeom",
      "rndnbinom",
      "rndpois",
      "zipois"
    ),
    type = c(
      beta = "continuous",
      betanorm = "continuous",
      betaprime = "continuous",
      burr = "continuous",
      cauchy = "continuous",
      chisq = "continuous",
      exp = "continuous",
      expgeom = "continuous",
      explog = "continuous",
      f = "continuous",
      gamma = "continuous",
      ged = "continuous",
      gev = "continuous",
      gpd = "continuous",
      gumbel = "continuous",
      halfnorm = "continuous",
      huber = "continuous",
      invchisq = "continuous",
      laplace = "continuous",
      llogis = "continuous",
      lnorm = "continuous",
      logis = "continuous",
      loglap = "continuous",
      norm = "continuous",
      pareto = "continuous",
      posnorm = "continuous",
      power = "continuous",
      rayleigh = "continuous",
      sc_t2 = "continuous",
      sged = "continuous",
      snorm = "continuous",
      sstd = "continuous",
      std = "continuous",
      unif = "continuous",
      weibull = "continuous",
      dcauchy = "discrete",
      dexp = "discrete",
      dged = "discrete",
      dlnorm = "discrete",
      dnorm = "discrete",
      dunif = "discrete",
      rndbinom = "discrete",
      rndgeom = "discrete",
      rndnbinom = "discrete",
      rndpois = "discrete",
      zipois = "discrete"
    ),
    model = c(
      "Beta",
      "BetaNormal",
      "BetaPrime",
      "TheBurr",
      "Cauchy",
      "ChiSquared",
      "Exponential",
      "ExponentialGeometric",
      "ExponentialLogarithmic",
      "F",
      "Gamma",
      "GeneralizedError",
      "GeneralizedExtreme",
      "GeneralizedPareto",
      "Gumbel",
      "HalfNorm",
      "HubersLeastFavourable",
      "Invchisq",
      "Laplace",
      "Loglogistic",
      "Lognormal",
      "Logistic",
      "LogLaplace",
      "Normal",
      "Pareto",
      "PositiveNormalDistribution",
      "PowerDist",
      "Rayleigh",
      "Studenttdegrees",
      "SkewGeneralizedError",
      "SkewNormal",
      "SkewStudentt",
      "Studentt",
      "Uniform",
      "Weibull",
      "DiscreteCauchy",
      "DiscreteExponential",
      "DiscreteGeneralizedError",
      "DiscreteLognormal",
      "DiscreteNormal",
      "Discrete Uniform Distribution",
      "Binomial",
      "Geometric",
      "NegativeBinomial",
      "Poisson",
      "ZeroInflatedPoisson"
    ),
    pkg = c(
      "stats",
      "mgalda",
      "mgalda",
      "mgalda",
      "stats",
      "mgalda",
      "stats",
      "mgalda",
      "mgalda",
      "stats",
      "stats",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "stats",
      "stats",
      "mgalda",
      "stats",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "stats",
      "stats",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda",
      "mgalda"
    ),
    dist_info = list(
      beta = structure(
        list(
          args = c(
            "shape1",
            "shape2", "ncp"
          ),
          model = "Beta",
          dist = "beta",
          pkg = "stats",
          bound = list(
            shape1 = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            shape2 = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            ncp = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= 1)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            assert_engine(
              max(x) < 1,
              min(x) > 0,
              env = parent.frame(),
              msg = "values must be > 0",
              severity = "stop"
            )
            n <- length(x)
            m <- mean(x)
            v <- (n - 1) / n * var(x)
            aux <- m * (1 - m) / v - 1
            c(
              shape1 = m * aux,
              shape2 = (1 - m) * aux,
              ncp = 0
            )
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dbeta(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qbeta(
                  p = p,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pbeta(
                  q = q,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dbeta(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dbeta(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dbeta(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              0.00001, Inf, 0.00001, Inf, 0,
              Inf
            ),
            .Dim = 2:3,
            .Dimnames = list(c("lowb", "uppb"), c(
              "shape1",
              "shape2", "ncp"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              assert_engine(
                max(x) < 1,
                min(x) > 0,
                env = parent.frame(),
                msg = "values must be > 0",
                severity = "stop"
              )
              n <- length(x)
              m <- mean(x)
              v <- (n - 1) / n * var(x)
              aux <- m * (1 - m) / v - 1
              c(
                shape1 = m * aux,
                shape2 = (1 - m) * aux,
                ncp = 0
              )
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dbeta(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001, 0.00001,
                    0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qbeta(
                  p = p,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001, 0.00001,
                    0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pbeta(
                  q = q,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001, 0.00001,
                    0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dbeta(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001, 0.00001,
                    0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dbeta(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001, 0.00001,
                    0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dbeta(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001, 0.00001,
                    0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(
                x, getOption("mgalda.n_samples", 1500),
                F
              )
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      betanorm = structure(
        list(
          args = c(
            "shape1", "shape2", "mean",
            "sd"
          ),
          model = "BetaNormal",
          dist = "betanorm",
          pkg = "mgalda",
          bound = list(
            shape1 = function(x) {
              all(x >= 0 & x <= Inf)
            },
            shape2 = function(x) {
              all(x >= 0 & x <= Inf)
            },
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(
              shape1 = 1,
              shape2 = 1,
              mean = 0,
              sd = 1
            )
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetanorm(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qbetanorm(
                  p = p,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pbetanorm(
                  q = q,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetanorm(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetanorm(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetanorm(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              0, Inf, 0, Inf, -Inf, Inf,
              0, Inf
            ),
            .Dim = c(2L, 4L),
            .Dimnames = list(c(
              "lowb",
              "uppb"
            ), c("shape1", "shape2", "mean", "sd"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(
                shape1 = 1,
                shape2 = 1,
                mean = 0,
                sd = 1
              )
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetanorm(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0, 0, -Inf,
                    0
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qbetanorm(
                  p = p,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0, 0, -Inf,
                    0
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pbetanorm(
                  q = q,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0, 0, -Inf,
                    0
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetanorm(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0, 0, -Inf,
                    0
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetanorm(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0, 0, -Inf,
                    0
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetanorm(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  mean = par[3],
                  sd = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0, 0, -Inf,
                    0
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      betaprime = structure(
        list(
          args = c("alpha", "beta"),
          model = "BetaPrime",
          dist = "betaprime",
          pkg = "mgalda",
          bound = list(
            alpha = function(x) {
              all(x >= 0 & x <= Inf)
            },
            beta = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            assert_engine(
              max(x) < 1,
              min(x) > 0,
              env = parent.frame(),
              msg = "values must be > 0",
              severity = "stop"
            )
            val1 <- mean(log(x / (x + 1)))
            val2 <- mean(log(1 - x / (x + 1)))
            G1 <- exp(val1)
            G2 <- exp(val2)
            denom <- 1 / 2 * (1 / (1 - G1 - G2))
            start <- c(1 / 2 + G1 * denom, 1 / 2 + G2 * denom)
            names(start) <- c("alpha", "beta")
            start
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetaprime(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qbetaprime(
                  p = p,
                  alpha = par[1],
                  beta = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pbetaprime(
                  q = q,
                  alpha = par[1],
                  beta = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetaprime(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetaprime(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetaprime(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("alpha", "beta"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              assert_engine(
                max(x) < 1,
                min(x) > 0,
                env = parent.frame(),
                msg = "values must be > 0",
                severity = "stop"
              )
              val1 <- mean(log(x / (x + 1)))
              val2 <- mean(log(1 - x / (x + 1)))
              G1 <- exp(val1)
              G2 <- exp(val2)
              denom <- 1 / 2 * (1 / (1 - G1 - G2))
              start <- c(1 / 2 + G1 * denom, 1 / 2 + G2 * denom)
              names(start) <- c("alpha", "beta")
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetaprime(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qbetaprime(
                  p = p,
                  alpha = par[1],
                  beta = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pbetaprime(
                  q = q,
                  alpha = par[1],
                  beta = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetaprime(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetaprime(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dbetaprime(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      burr = structure(
        list(
          args = c("shape1", "shape2", "scale"),
          model = "TheBurr",
          dist = "burr",
          pkg = "mgalda",
          bound = list(
            shape1 = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            shape2 = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            scale = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(
              shape1 = 2,
              shape2 = 1,
              scale = 0.5
            )
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dburr(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf, -Inf),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qburr(
                  p = p,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf, -Inf),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pburr(
                  q = q,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf, -Inf),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dburr(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf, -Inf),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dburr(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf, -Inf),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dburr(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf, -Inf),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, -Inf, Inf, -Inf, Inf),
            .Dim = 2:3,
            .Dimnames = list(c("lowb", "uppb"), c(
              "shape1",
              "shape2", "scale"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(
                shape1 = 2,
                shape2 = 1,
                scale = 0.5
              )
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dburr(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qburr(
                  p = p,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pburr(
                  q = q,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dburr(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dburr(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dburr(
                  x = x,
                  shape1 = par[1],
                  shape2 = par[2],
                  scale = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(
                x, getOption("mgalda.n_samples", 1500),
                F
              )
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      cauchy = structure(
        list(
          args = c("location", "scale"),
          model = "Cauchy",
          dist = "cauchy",
          pkg = "stats",
          bound = list(
            location = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            start <- c(location = median(x), scale = IQR(x) / 2)
            start
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qcauchy(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pcauchy(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c(
              "location",
              "scale"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              start <- c(location = median(x), scale = IQR(x) / 2)
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qcauchy(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pcauchy(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      chisq = structure(
        list(
          args = c("df", "ncp"),
          model = "ChiSquared",
          dist = "chisq",
          pkg = "mgalda",
          bound = list(
            df = function(x) {
              all(x >= 0 & x <= Inf)
            },
            ncp = function(x) {
              all(x >= -0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= Inf)
          },
          fn_init = function(x) {
            assert_engine(!any(x <= 0),
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            assert_na_any(x, severity = "stop")
            c(df = (mean(x) + sd(x)) / 2, ncp = 0)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dchisq(
                  x = x,
                  df = par[1],
                  ncp = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qchisq(
                  p = p,
                  df = par[1],
                  ncp = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pchisq(
                  q = q,
                  df = par[1],
                  ncp = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dchisq(
                  x = x,
                  df = par[1],
                  ncp = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dchisq(
                  x = x,
                  df = par[1],
                  ncp = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dchisq(
                  x = x,
                  df = par[1],
                  ncp = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0, Inf, -0.00001, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("df", "ncp"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_engine(!any(x <= 0),
                            env = parent.frame(),
                            msg = "values must be > 0",
                            severity = "stop"
              )
              assert_na_any(x, severity = "stop")
              c(df = (mean(x) + sd(x)) / 2, ncp = 0)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dchisq(
                  x = x,
                  df = par[1],
                  ncp = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qchisq(
                  p = p,
                  df = par[1],
                  ncp = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pchisq(
                  q = q,
                  df = par[1],
                  ncp = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dchisq(
                  x = x,
                  df = par[1],
                  ncp = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dchisq(
                  x = x,
                  df = par[1],
                  ncp = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dchisq(
                  x = x,
                  df = par[1],
                  ncp = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      exp = structure(
        list(
          args = "rate",
          model = "Exponential",
          dist = "exp",
          pkg = "stats",
          bound = list(
            rate = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= Inf)
          },
          fn_init = function(x) {
            assert_engine(!any(x < 0),
                          env = parent.frame(),
                          msg = "values must be >= 0",
                          severity = "stop"
            )
            assert_na_any(x, severity = "stop")
            start <- 1 / mean(x)
            names(start) <- "rate"
            start
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dexp(
                  x = x,
                  rate = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            qme_obj = function(x) {
              fnobj <-
                function(par, x) {{ fn <- function(par, p) {
                  stats::qexp(p = p, rate = par[1])
                }
                valid <- function(x) {
                  ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                         0, x
                  )
                }
                p <- ppoints(7)
                q_theo <- valid(fn(par = par, p = p))
                q_emp <- as.num(quantile(x, probs = p, type = 7))
                sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            mge_obj = function(x) {
              fnobj <-
                function(par, x) {{ fn <- function(par, q) {
                  stats::pexp(q = q, rate = par[1])
                }
                sx <- c(-Inf, sort(x), Inf)
                n <- length(sx)
                Di <- fn(par = par, q = sx[-1]) - fn(
                  par = par,
                  q = sx[-n]
                )
                mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dexp(
                  x = x,
                  rate = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dexp(
                  x = x,
                  rate = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dexp(
                  x = x,
                  rate = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
          ),
          bnd_num = structure(
            c(0.00001, Inf),
            .Dim = 2:1,
            .Dimnames = list(c("lowb", "uppb"), "rate")
          ),
          optim.method = "Nelder-Mead",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_engine(!any(x < 0),
                            env = parent.frame(),
                            msg = "values must be >= 0",
                            severity = "stop"
              )
              assert_na_any(x, severity = "stop")
              start <- 1 / mean(x)
              names(start) <- "rate"
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dexp(
                  x = x,
                  rate = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qexp(
                  p = p,
                  rate = par[1]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pexp(
                  q = q,
                  rate = par[1]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dexp(
                  x = x,
                  rate = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dexp(
                  x = x,
                  rate = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dexp(
                  x = x,
                  rate = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      expgeom = structure(
        list(
          args = c("scale", "shape"),
          model = "ExponentialGeometric",
          dist = "expgeom",
          pkg = "mgalda",
          bound = list(
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            shape = function(x) {
              all(x >= 0.00001 & x <= 0.99999)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= Inf)
          },
          fn_init = function(x) {
            c(scale = 1, shape = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexpgeom(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qexpgeom(
                  p = p,
                  scale = par[1],
                  shape = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pexpgeom(
                  q = q,
                  scale = par[1],
                  shape = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexpgeom(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexpgeom(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexpgeom(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0.00001, Inf, 0.00001, 0.99999),
            .Dim = c(2L, 2L),
            .Dimnames = list(c("lowb", "uppb"), c("scale", "shape"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(scale = 1, shape = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexpgeom(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qexpgeom(
                  p = p,
                  scale = par[1],
                  shape = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pexpgeom(
                  q = q,
                  scale = par[1],
                  shape = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexpgeom(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexpgeom(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexpgeom(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      explog = structure(
        list(
          args = c("scale", "shape"),
          model = "ExponentialLogarithmic",
          dist = "explog",
          pkg = "mgalda",
          bound = list(
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            shape = function(x) {
              all(x >= 0.00001 & x <= 0.99999)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= Inf)
          },
          fn_init = function(x) {
            c(scale = 1,shape = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexplog(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qexplog(
                  p = p,
                  scale = par[1],
                  shape = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pexplog(
                  q = q,
                  scale = par[1],
                  shape = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexplog(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexplog(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexplog(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0.00001, Inf, 0.00001, 0.99999),
            .Dim = c(2L, 2L),
            .Dimnames = list(c("lowb", "uppb"), c("scale", "shape"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(scale = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexplog(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qexplog(
                  p = p,
                  scale = par[1],
                  shape = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pexplog(
                  q = q,
                  scale = par[1],
                  shape = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexplog(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexplog(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dexplog(
                  x = x,
                  scale = par[1],
                  shape = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      f = structure(
        list(
          args = c("df1", "df2", "ncp"),
          model = "F",
          dist = "f",
          pkg = "stats",
          bound = list(
            df1 = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            df2 = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            ncp = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= Inf)
          },
          fn_init = function(x) {
            c(df1 = 1, df2 = 1, ncp = 0)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::df(
                  x = x,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qf(
                  p = p,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pf(
                  q = q,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::df(
                  x = x,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::df(
                  x = x,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::df(
                  x = x,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, 0.00001,
                  0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              0.00001, Inf, 0.00001, Inf,
              0, Inf
            ),
            .Dim = 2:3,
            .Dimnames = list(c("lowb", "uppb"), c("df1", "df2", "ncp"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(df1 = 1, df2 = 1, ncp = 0)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::df(
                  x = x,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001, 0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qf(
                  p = p,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001, 0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pf(
                  q = q,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001, 0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::df(
                  x = x,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001, 0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::df(
                  x = x,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001, 0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::df(
                  x = x,
                  df1 = par[1],
                  df2 = par[2],
                  ncp = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001, 0
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      gamma = structure(
        list(
          args = c("shape", "rate"),
          model = "Gamma",
          dist = "gamma",
          pkg = "stats",
          bound = list(
            shape = function(x) {
              all(x >= 0 & x <= Inf)
            },
            rate = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            assert_engine(min(x) > 0,
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            n <- length(x)
            m <- mean(x)
            v <- (n - 1) / n * var(x)
            c(shape = m^2 / v, rate = m / v)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dgamma(
                  x = x,
                  shape = par[1],
                  rate = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qgamma(
                  p = p,
                  shape = par[1],
                  rate = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pgamma(
                  q = q,
                  shape = par[1],
                  rate = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dgamma(
                  x = x,
                  shape = par[1],
                  rate = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dgamma(
                  x = x,
                  shape = par[1],
                  rate = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dgamma(
                  x = x,
                  shape = par[1],
                  rate = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("shape", "rate"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              assert_engine(min(x) > 0,
                            env = parent.frame(),
                            msg = "values must be > 0",
                            severity = "stop"
              )
              n <- length(x)
              m <- mean(x)
              v <- (n - 1) / n * var(x)
              c(shape = m^2 / v, rate = m / v)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dgamma(
                  x = x,
                  shape = par[1],
                  rate = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qgamma(
                  p = p,
                  shape = par[1],
                  rate = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pgamma(
                  q = q,
                  shape = par[1],
                  rate = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dgamma(
                  x = x,
                  shape = par[1],
                  rate = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dgamma(
                  x = x,
                  shape = par[1],
                  rate = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dgamma(
                  x = x,
                  shape = par[1],
                  rate = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      ged = structure(
        list(
          args = c("mean", "sd", "nu"),
          model = "GeneralizedError",
          dist = "ged",
          pkg = "mgalda",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            nu = function(x) {
              all(x >= 0.5 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(mean = 0, sd = 1, nu = 2)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qged(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pged(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              -Inf, Inf, -Inf, Inf, 0.5,
              Inf
            ),
            .Dim = 2:3,
            .Dimnames = list(
              c("lowb", "uppb"),
              c("mean", "sd", "nu")
            )
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(mean = 0, sd = 1, nu = 2)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.5
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qged(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.5
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pged(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.5
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.5
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.5
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.5
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      gev = structure(
        list(
          args = c("location", "scale", "shape"),
          model = "GeneralizedExtreme",
          dist = "gev",
          pkg = "mgalda",
          bound = list(
            location = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            shape = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(
              location = 0,
              scale = 1,
              shape = 0
            )
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgev(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qgev(
                  p = p,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pgev(
                  q = q,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgev(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgev(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgev(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              -Inf, Inf, 0.00001, Inf, -Inf,
              Inf
            ),
            .Dim = 2:3,
            .Dimnames = list(
              c("lowb", "uppb"),
              c("location", "scale", "shape")
            )
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(
                location = 0,
                scale = 1,
                shape = 0
              )
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgev(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qgev(
                  p = p,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pgev(
                  q = q,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgev(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgev(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgev(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      gpd = structure(
        list(
          args = c("location", "scale", "shape"),
          model = "GeneralizedPareto",
          dist = "gpd",
          pkg = "mgalda",
          bound = list(
            location = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            shape = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(
              location = 0,
              scale = 1,
              shape = 0
            )
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgpd(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qgpd(
                  p = p,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pgpd(
                  q = q,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgpd(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgpd(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgpd(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              -Inf, Inf, 0.00001, Inf, -Inf,
              Inf
            ),
            .Dim = 2:3,
            .Dimnames = list(
              c("lowb", "uppb"),
              c("location", "scale", "shape")
            )
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(
                location = 0,
                scale = 1,
                shape = 0
              )
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgpd(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qgpd(
                  p = p,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pgpd(
                  q = q,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgpd(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgpd(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgpd(
                  x = x,
                  location = par[1],
                  scale = par[2],
                  shape = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    -Inf
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      gumbel = structure(
        list(
          args = c("location", "scale"),
          model = "Gumbel",
          dist = "gumbel",
          pkg = "mgalda",
          bound = list(
            location = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(location = 0, scale = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgumbel(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qgumbel(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pgumbel(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgumbel(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgumbel(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgumbel(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0.00001, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c(
              "location",
              "scale"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(location = 0, scale = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgumbel(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qgumbel(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pgumbel(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgumbel(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgumbel(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dgumbel(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      halfnorm = structure(
        list(
          args = c("mean", "theta"),
          model = "HalfNorm",
          dist = "halfnorm",
          pkg = "mgalda",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            theta = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(mean = 0, theta = sqrt(pi / 2))
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhalfnorm(
                  x = x,
                  mean = par[1],
                  theta = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qhalfnorm(
                  p = p,
                  mean = par[1],
                  theta = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::phalfnorm(
                  q = q,
                  mean = par[1],
                  theta = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhalfnorm(
                  x = x,
                  mean = par[1],
                  theta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhalfnorm(
                  x = x,
                  mean = par[1],
                  theta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhalfnorm(
                  x = x,
                  mean = par[1],
                  theta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0.00001, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("mean", "theta"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(mean = 0, theta = sqrt(pi / 2))
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhalfnorm(
                  x = x,
                  mean = par[1],
                  theta = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qhalfnorm(
                  p = p,
                  mean = par[1],
                  theta = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::phalfnorm(
                  q = q,
                  mean = par[1],
                  theta = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhalfnorm(
                  x = x,
                  mean = par[1],
                  theta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhalfnorm(
                  x = x,
                  mean = par[1],
                  theta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhalfnorm(
                  x = x,
                  mean = par[1],
                  theta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      huber = structure(
        list(
          args = c("k", "mu", "sigma"),
          model = "HubersLeastFavourable",
          dist = "huber",
          pkg = "mgalda",
          bound = list(
            k = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            mu = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sigma = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(k = 0.862, mu = 0, sigma = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhuber(
                  x = x,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qhuber(
                  p = p,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::phuber(
                  q = q,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhuber(
                  x = x,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhuber(
                  x = x,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhuber(
                  x = x,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  0.00001, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              0.00001, Inf, -Inf, Inf, 0.00001,
              Inf
            ),
            .Dim = 2:3,
            .Dimnames = list(
              c("lowb", "uppb"),
              c("k", "mu", "sigma")
            )
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(k = 0.862, mu = 0, sigma = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhuber(
                  x = x,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    -Inf, 0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qhuber(
                  p = p,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    -Inf, 0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::phuber(
                  q = q,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    -Inf, 0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhuber(
                  x = x,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    -Inf, 0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhuber(
                  x = x,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    -Inf, 0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dhuber(
                  x = x,
                  k = par[1],
                  mu = par[2],
                  sigma = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    -Inf, 0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      invchisq = structure(
        list(
          args = "df",
          model = "Invchisq",
          dist = "invchisq",
          pkg = "mgalda",
          bound = list(
            df = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= Inf)
          },
          fn_init = function(x) {
            c(df = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dinvchisq(
                  x = x,
                  df = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qinvchisq(
                  p = p,
                  df = par[1]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pinvchisq(
                  q = q,
                  df = par[1]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dinvchisq(
                  x = x,
                  df = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dinvchisq(
                  x = x,
                  df = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dinvchisq(
                  x = x,
                  df = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
          ),
          bnd_num = structure(
            c(0.00001, Inf),
            .Dim = 2:1,
            .Dimnames = list(c("lowb", "uppb"), "df")
          ),
          optim.method = "Nelder-Mead",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(df = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dinvchisq(
                  x = x,
                  df = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qinvchisq(
                  p = p,
                  df = par[1]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pinvchisq(
                  q = q,
                  df = par[1]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dinvchisq(
                  x = x,
                  df = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dinvchisq(
                  x = x,
                  df = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dinvchisq(
                  x = x,
                  df = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      laplace = structure(
        list(
          args = c("mu", "sigma"),
          model = "Laplace",
          dist = "laplace",
          pkg = "mgalda",
          bound = list(
            mu = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sigma = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(mu = 0, sigma = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dlaplace(
                  x = x,
                  mu = par[1],
                  sigma = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qlaplace(
                  p = p,
                  mu = par[1],
                  sigma = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::plaplace(
                  q = q,
                  mu = par[1],
                  sigma = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dlaplace(
                  x = x,
                  mu = par[1],
                  sigma = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dlaplace(
                  x = x,
                  mu = par[1],
                  sigma = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dlaplace(
                  x = x,
                  mu = par[1],
                  sigma = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, -Inf, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("mu", "sigma"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(mu = 0, sigma = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dlaplace(
                  x = x,
                  mu = par[1],
                  sigma = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qlaplace(
                  p = p,
                  mu = par[1],
                  sigma = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::plaplace(
                  q = q,
                  mu = par[1],
                  sigma = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dlaplace(
                  x = x,
                  mu = par[1],
                  sigma = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dlaplace(
                  x = x,
                  mu = par[1],
                  sigma = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dlaplace(
                  x = x,
                  mu = par[1],
                  sigma = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      llogis = structure(
        list(
          args = c("shape", "scale"),
          model = "Loglogistic",
          dist = "llogis",
          pkg = "mgalda",
          bound = list(
            shape = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            assert_engine(min(x) > 0,
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            p25 <- as.numeric(quantile(x, 0.25))
            p75 <- as.numeric(quantile(x, 0.75))
            shape <- 2 * log(3) / (log(p75) - log(p25))
            scale <- exp(log(p75) + log(p25)) / 2
            c(shape = shape, scale = scale)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dllogis(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qllogis(
                  p = p,
                  shape = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pllogis(
                  q = q,
                  shape = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dllogis(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dllogis(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dllogis(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0.00001, Inf, 0.00001, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("shape", "scale"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              assert_engine(min(x) > 0,
                            env = parent.frame(),
                            msg = "values must be > 0",
                            severity = "stop"
              )
              p25 <- as.numeric(quantile(x, 0.25))
              p75 <- as.numeric(quantile(x, 0.75))
              shape <- 2 * log(3) / (log(p75) - log(p25))
              scale <- exp(log(p75) + log(p25)) / 2
              c(shape = shape, scale = scale)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dllogis(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qllogis(
                  p = p,
                  shape = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pllogis(
                  q = q,
                  shape = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dllogis(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dllogis(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dllogis(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      lnorm = structure(
        list(
          args = c("meanlog", "sdlog"),
          model = "Lognormal",
          dist = "lnorm",
          pkg = "stats",
          bound = list(
            meanlog = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sdlog = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            lx <- log(x)
            n <- length(x)
            sd0 <- sqrt((n - 1) / n) * sd(lx)
            mx <- mean(lx)
            start <- c(mx, sd0)
            names(start) <- c("meanlog", "sdlog")
            start
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qlnorm(
                  p = p,
                  meanlog = par[1],
                  sdlog = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::plnorm(
                  q = q,
                  meanlog = par[1],
                  sdlog = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c(
              "meanlog",
              "sdlog"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              lx <- log(x)
              n <- length(x)
              sd0 <- sqrt((n - 1) / n) * sd(lx)
              mx <- mean(lx)
              start <- c(mx, sd0)
              names(start) <- c("meanlog", "sdlog")
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qlnorm(
                  p = p,
                  meanlog = par[1],
                  sdlog = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::plnorm(
                  q = q,
                  meanlog = par[1],
                  sdlog = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      logis = structure(
        list(
          args = c("location", "scale"),
          model = "Logistic",
          dist = "logis",
          pkg = "stats",
          bound = list(
            location = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            start <- c(location = median(x), scale = IQR(x) / 2)
            start
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlogis(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qlogis(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::plogis(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlogis(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlogis(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlogis(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c(
              "location",
              "scale"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              start <- c(location = median(x), scale = IQR(x) / 2)
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlogis(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qlogis(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::plogis(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlogis(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlogis(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dlogis(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      loglap = structure(
        list(
          args = c(
            "location_ald", "scale_ald",
            "tau"
          ),
          model = "LogLaplace",
          dist = "loglap",
          pkg = "mgalda",
          bound = list(
            location_ald = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            scale_ald = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            tau = function(x) {
              all(x >= 0.00001 & x <= 0.99999)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            c(
              location_ald = 0,
              scale_ald = 1,
              tau = 0.5
            )
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dloglap(
                  x = x,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qloglap(
                  p = p,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::ploglap(
                  q = q,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dloglap(
                  x = x,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dloglap(
                  x = x,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dloglap(
                  x = x,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              -Inf, Inf, 0.00001, Inf, 0.00001,
              0.99999
            ),
            .Dim = 2:3,
            .Dimnames = list(c("lowb", "uppb"), c("location_ald", "scale_ald", "tau"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(
                location_ald = 0,
                scale_ald = 1,
                tau = 0.5
              )
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dloglap(
                  x = x,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qloglap(
                  p = p,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::ploglap(
                  q = q,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dloglap(
                  x = x,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dloglap(
                  x = x,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dloglap(
                  x = x,
                  location_ald = par[1],
                  scale_ald = par[2],
                  tau = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, 0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf, 0.99999),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      norm = structure(
        list(
          args = c("mean", "sd"),
          model = "Normal",
          dist = "norm",
          pkg = "stats",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            n <- length(x)
            sd0 <- sqrt((n - 1) / n) * sd(x)
            mx <- mean(x)
            start <- c(mx, sd0)
            names(start) <- c("mean", "sd")
            start
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qnorm(
                  p = p,
                  mean = par[1],
                  sd = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pnorm(
                  q = q,
                  mean = par[1],
                  sd = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("mean", "sd"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              n <- length(x)
              sd0 <- sqrt((n - 1) / n) * sd(x)
              mx <- mean(x)
              start <- c(mx, sd0)
              names(start) <- c("mean", "sd")
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qnorm(
                  p = p,
                  mean = par[1],
                  sd = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pnorm(
                  q = q,
                  mean = par[1],
                  sd = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      pareto = structure(
        list(
          args = c("shape", "scale"),
          model = "Pareto",
          dist = "pareto",
          pkg = "mgalda",
          bound = list(
            shape = function(x) {
              all(x >= 0.25 & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            assert_engine(!any(x < 0),
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            m1 <- mean(x)
            m2 <- mean(x^2)
            scale <- (m1 * m2) / (m2 - 2 * m1^2)
            shape <- 2 * (m2 - m1^2) / (m2 - 2 * m1^2)
            start <- c(shape = shape, scale = scale)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpareto(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qpareto(
                  p = p,
                  shape = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::ppareto(
                  q = q,
                  shape = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpareto(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpareto(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpareto(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0.25, Inf, 0.00001, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("shape", "scale"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              assert_engine(!any(x < 0),
                            env = parent.frame(),
                            msg = "values must be > 0",
                            severity = "stop"
              )
              m1 <- mean(x)
              m2 <- mean(x^2)
              scale <- (m1 * m2) / (m2 - 2 * m1^2)
              shape <- 2 * (m2 - m1^2) / (m2 - 2 * m1^2)
              start <- c(shape = shape, scale = scale)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpareto(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0.25, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qpareto(
                  p = p,
                  shape = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0.25, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::ppareto(
                  q = q,
                  shape = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0.25, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpareto(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0.25, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpareto(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0.25, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpareto(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0.25, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      posnorm = structure(
        list(
          args = c("mean", "sd"),
          model = "PositiveNormalDistribution",
          dist = "posnorm",
          pkg = "mgalda",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= Inf)
          },
          fn_init = function(x) {
            c(mean = 0, sd = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dposnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qposnorm(
                  p = p,
                  mean = par[1],
                  sd = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pposnorm(
                  q = q,
                  mean = par[1],
                  sd = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dposnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dposnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dposnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("mean", "sd"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(mean = 0, sd = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dposnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qposnorm(
                  p = p,
                  mean = par[1],
                  sd = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pposnorm(
                  q = q,
                  mean = par[1],
                  sd = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dposnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dposnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dposnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      power = structure(
        list(
          args = c("alpha", "beta"),
          model = "PowerDist",
          dist = "power",
          pkg = "mgalda",
          bound = list(
            alpha = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            beta = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(alpha = 1, beta = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpower(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qpower(
                  p = p,
                  alpha = par[1],
                  beta = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::ppower(
                  q = q,
                  alpha = par[1],
                  beta = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpower(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpower(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpower(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, -Inf, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("alpha", "beta"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(alpha = 1, beta = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpower(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qpower(
                  p = p,
                  alpha = par[1],
                  beta = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::ppower(
                  q = q,
                  alpha = par[1],
                  beta = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpower(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpower(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dpower(
                  x = x,
                  alpha = par[1],
                  beta = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      rayleigh = structure(
        list(
          args = "scale",
          model = "Rayleigh",
          dist = "rayleigh",
          pkg = "mgalda",
          bound = list(
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0.00001 & x <= Inf)
          },
          fn_init = function(x) {
            c(scale = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drayleigh(
                  x = x,
                  scale = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrayleigh(
                  p = p,
                  scale = par[1]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prayleigh(
                  q = q,
                  scale = par[1]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drayleigh(
                  x = x,
                  scale = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drayleigh(
                  x = x,
                  scale = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drayleigh(
                  x = x,
                  scale = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
          ),
          bnd_num = structure(
            c(0.00001, Inf),
            .Dim = 2:1,
            .Dimnames = list(c("lowb", "uppb"), "scale")
          ),
          optim.method = "Nelder-Mead",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(scale = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drayleigh(
                  x = x,
                  scale = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrayleigh(
                  p = p,
                  scale = par[1]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prayleigh(
                  q = q,
                  scale = par[1]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drayleigh(
                  x = x,
                  scale = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drayleigh(
                  x = x,
                  scale = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drayleigh(
                  x = x,
                  scale = par[1]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      sc_t2 = structure(
        list(
          args = c("location", "scale"),
          model = "Studenttdegrees",
          dist = "sc_t2",
          pkg = "mgalda",
          bound = list(
            location = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(location = 0, scale = 1)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsc_t2(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qsc_t2(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::psc_t2(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsc_t2(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsc_t2(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsc_t2(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0.00001, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c(
              "location",
              "scale"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(location = 0, scale = 1)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsc_t2(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qsc_t2(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::psc_t2(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsc_t2(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsc_t2(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsc_t2(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0.00001),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      sged = structure(
        list(
          args = c("mean", "sd", "nu", "xi"),
          model = "SkewGeneralizedError",
          dist = "sged",
          pkg = "mgalda",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            nu = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            xi = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(
              mean = 0,
              sd = 1,
              nu = 2,
              xi = 1.5
            )
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qsged(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::psged(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              -Inf, Inf, -Inf, Inf, -Inf,
              Inf, -Inf, Inf
            ),
            .Dim = c(2L, 4L),
            .Dimnames = list(c(
              "lowb",
              "uppb"
            ), c("mean", "sd", "nu", "xi"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(
                mean = 0,
                sd = 1,
                nu = 2,
                xi = 1.5
              )
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qsged(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::psged(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    -Inf, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      snorm = structure(
        list(
          args = c("mean", "sd", "xi"),
          model = "SkewNormal",
          dist = "snorm",
          pkg = "mgalda",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            xi = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(mean = 0, sd = 1, xi = 1.5)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qsnorm(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::psnorm(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              -Inf, Inf, -Inf, Inf, 0.00001,
              Inf
            ),
            .Dim = 2:3,
            .Dimnames = list(
              c("lowb", "uppb"),
              c("mean", "sd", "xi")
            )
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(mean = 0, sd = 1, xi = 1.5)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qsnorm(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::psnorm(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  xi = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      sstd = structure(
        list(
          args = c("mean", "sd", "nu", "xi"),
          model = "SkewStudentt",
          dist = "sstd",
          pkg = "mgalda",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            nu = function(x) {
              all(x >= 3 & x <= Inf)
            },
            xi = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(
              mean = 0,
              sd = 1,
              nu = 5,
              xi = 1.5
            )
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qsstd(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::psstd(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              -Inf, Inf, -Inf, Inf, 3, Inf,
              -Inf, Inf
            ),
            .Dim = c(2L, 4L),
            .Dimnames = list(c(
              "lowb",
              "uppb"
            ), c("mean", "sd", "nu", "xi"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(
                mean = 0,
                sd = 1,
                nu = 5,
                xi = 1.5
              )
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qsstd(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::psstd(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dsstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3],
                  xi = par[4]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3, -Inf
                  ),
                  upper = c(Inf, Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      std = structure(
        list(
          args = c("mean", "sd", "nu"),
          model = "Studentt",
          dist = "std",
          pkg = "mgalda",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            nu = function(x) {
              all(x >= 3 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            c(mean = 0, sd = 1, nu = 5)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qstd(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pstd(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, -Inf, Inf, 3, Inf),
            .Dim = 2:3,
            .Dimnames = list(c("lowb", "uppb"), c(
              "mean",
              "sd", "nu"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(mean = 0, sd = 1, nu = 5)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qstd(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pstd(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dstd(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    3
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      unif = structure(
        list(
          args = c("min", "max"),
          model = "Uniform",
          dist = "unif",
          pkg = "stats",
          bound = list(
            min = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            max = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= -Inf & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            l <- bootfn(
              x = x,
              fn = f(min(x, na.rm = TRUE)),
              times = 5,
              rep = 2
            )
            u <- bootfn(
              x = x,
              fn = f(max(x, na.rm = TRUE)),
              times = 5,
              rep = 2
            )
            c(min = l, max = u)
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qunif(
                  p = p,
                  min = par[1],
                  max = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::punif(
                  q = q,
                  min = par[1],
                  max = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, -Inf, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("min", "max"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              l <- bootfn(
                x = x,
                fn = f(min(x, na.rm = TRUE)),
                times = 5,
                rep = 2
              )
              u <- bootfn(
                x = x,
                fn = f(max(x, na.rm = TRUE)),
                times = 5,
                rep = 2
              )
              c(min = l, max = u)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qunif(
                  p = p,
                  min = par[1],
                  max = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::punif(
                  q = q,
                  min = par[1],
                  max = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      weibull = structure(
        list(
          args = c("shape", "scale"),
          model = "Weibull",
          dist = "weibull",
          pkg = "stats",
          bound = list(
            shape = function(x) {
              all(x >= 0.00001 & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_engine(!any(x <= 0),
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            assert_na_any(x, severity = "stop")
            lx <- log(x)
            m <- mean(lx)
            v <- var(lx)
            shape <- 1.2 / sqrt(v)
            scale <- exp(m + 0.572 / shape)
            start <- c(shape = shape, scale = scale)
            start
          },
          type = "continuous",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dweibull(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qme_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qweibull(
                  p = p,
                  shape = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            mge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pweibull(
                  q = q,
                  shape = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dweibull(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dweibull(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            admge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dweibull(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(optim(
                par = init(x),
                lower = c(0.00001, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0.00001, Inf, 0.00001, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("shape", "scale"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_engine(!any(x <= 0),
                            env = parent.frame(),
                            msg = "values must be > 0",
                            severity = "stop"
              )
              assert_na_any(x, severity = "stop")
              lx <- log(x)
              m <- mean(lx)
              v <- var(lx)
              shape <- 1.2 / sqrt(v)
              scale <- exp(m + 0.572 / shape)
              start <- c(shape = shape, scale = scale)
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dweibull(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qme_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                stats::qweibull(
                  p = p,
                  shape = par[1],
                  scale = par[2]
                )
              }
              valid <- function(x) {
                ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                       0, x
                )
              }
              p <- ppoints(7)
              q_theo <- valid(fn(par = par, p = p))
              q_emp <- as.num(quantile(x, probs = p, type = 7))
              sum((q_emp - q_theo)^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            mge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                stats::pweibull(
                  q = q,
                  shape = par[1],
                  scale = par[2]
                )
              }
              sx <- c(-Inf, sort(x), Inf)
              n <- length(sx)
              Di <- fn(par = par, q = sx[-1]) - fn(
                par = par,
                q = sx[-n]
              )
              mean(log(Di)) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dweibull(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dweibull(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            admge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(stats::dweibull(
                  x = x,
                  shape = par[1],
                  scale = par[2]
                ))
              }
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                            rev(theop)))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    0.00001,
                    0.00001
                  ),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(qme_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(mge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(admge_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      dcauchy = structure(
        list(
          args = c("location", "scale"),
          model = "DiscreteCauchy",
          dist = "dcauchy",
          pkg = "mgalda",
          bound = list(
            location = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            scale = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            start <- c(location = median(x), scale = IQR(x) / 2)
            start
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdcauchy(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdcauchy(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c(
              "location",
              "scale"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              start <- c(location = median(x), scale = IQR(x) / 2)
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddcauchy(
                  x = x,
                  location = par[1],
                  scale = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdcauchy(
                  q = q,
                  location = par[1],
                  scale = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdcauchy(
                  p = p,
                  location = par[1],
                  scale = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      dexp = structure(
        list(
          args = "rate",
          model = "DiscreteExponential",
          dist = "dexp",
          pkg = "mgalda",
          bound = list(
            rate = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_engine(!any(x < 0),
                          env = parent.frame(),
                          msg = "values must be >= 0",
                          severity = "stop"
            )
            assert_na_any(x, severity = "stop")
            start <- 1 / mean(x)
            names(start) <- "rate"
            start
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddexp(
                  x = x,
                  rate = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddexp(
                  x = x,
                  rate = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddexp(
                  x = x,
                  rate = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdexp(
                  q = q,
                  rate = par[1]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdexp(
                  p = p,
                  rate = par[1]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
          ),
          bnd_num = structure(
            c(0.00001, Inf),
            .Dim = 2:1,
            .Dimnames = list(c("lowb", "uppb"), "rate")
          ),
          optim.method = "Nelder-Mead",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_engine(!any(x < 0),
                            env = parent.frame(),
                            msg = "values must be >= 0",
                            severity = "stop"
              )
              assert_na_any(x, severity = "stop")
              start <- 1 / mean(x)
              names(start) <- "rate"
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddexp(
                  x = x,
                  rate = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddexp(
                  x = x,
                  rate = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddexp(
                  x = x,
                  rate = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdexp(
                  q = q,
                  rate = par[1]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdexp(
                  p = p,
                  rate = par[1]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      dged = structure(
        list(
          args = c("mean", "sd", "nu"),
          model = "DiscreteGeneralizedError",
          dist = "dged",
          pkg = "mgalda",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            nu = function(x) {
              all(x >= 0.00001 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            c(mean = 0, sd = 1, nu = 2)
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdged(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdged(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(
              -Inf, Inf, -Inf, Inf, 0.00001,
              Inf
            ),
            .Dim = 2:3,
            .Dimnames = list(
              c("lowb", "uppb"),
              c("mean", "sd", "nu")
            )
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(mean = 0, sd = 1, nu = 2)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddged(
                  x = x,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdged(
                  q = q,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdged(
                  p = p,
                  mean = par[1],
                  sd = par[2],
                  nu = par[3]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(
                    -Inf, -Inf,
                    0.00001
                  ),
                  upper = c(Inf, Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      dlnorm = structure(
        list(
          args = c("meanlog", "sdlog"),
          model = "DiscreteLognormal",
          dist = "dlnorm",
          pkg = "mgalda",
          bound = list(
            meanlog = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sdlog = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            lx <- log(x)
            n <- length(x)
            sd0 <- sqrt((n - 1) / n) * sd(lx)
            mx <- mean(lx)
            start <- c(mx, sd0)
            names(start) <- c("meanlog", "sdlog")
            start
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdlnorm(
                  q = q,
                  meanlog = par[1],
                  sdlog = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdlnorm(
                  p = p,
                  meanlog = par[1],
                  sdlog = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c(
              "meanlog",
              "sdlog"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              lx <- log(x)
              n <- length(x)
              sd0 <- sqrt((n - 1) / n) * sd(lx)
              mx <- mean(lx)
              start <- c(mx, sd0)
              names(start) <- c("meanlog", "sdlog")
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddlnorm(
                  x = x,
                  meanlog = par[1],
                  sdlog = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdlnorm(
                  q = q,
                  meanlog = par[1],
                  sdlog = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdlnorm(
                  p = p,
                  meanlog = par[1],
                  sdlog = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      dnorm = structure(
        list(
          args = c("mean", "sd"),
          model = "DiscreteNormal",
          dist = "dnorm",
          pkg = "mgalda",
          bound = list(
            mean = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            sd = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            n <- length(x)
            sd0 <- sqrt((n - 1) / n) * sd(x)
            mx <- mean(x)
            start <- c(mx, sd0)
            names(start) <- c("mean", "sd")
            start
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdnorm(
                  q = q,
                  mean = par[1],
                  sd = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdnorm(
                  p = p,
                  mean = par[1],
                  sd = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, 0, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("mean", "sd"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              n <- length(x)
              sd0 <- sqrt((n - 1) / n) * sd(x)
              mx <- mean(x)
              start <- c(mx, sd0)
              names(start) <- c("mean", "sd")
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddnorm(
                  x = x,
                  mean = par[1],
                  sd = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdnorm(
                  q = q,
                  mean = par[1],
                  sd = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdnorm(
                  p = p,
                  mean = par[1],
                  sd = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, 0),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      dunif = structure(
        list(
          args = c("min", "max"),
          model = "Discrete Uniform Distribution",
          dist = "dunif",
          pkg = "mgalda",
          bound = list(
            min = function(x) {
              all(x >= -Inf & x <= Inf)
            },
            max = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            l <- bootfn(
              x = x,
              fn = f(min(x, na.rm = TRUE)),
              times = 5,
              rep = 2
            )
            u <- bootfn(
              x = x,
              fn = f(max(x, na.rm = TRUE)),
              times = 5,
              rep = 2
            )
            c(min = l, max = u)
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdunif(
                  q = q,
                  min = par[1],
                  max = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdunif(
                  p = p,
                  min = par[1],
                  max = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-Inf, Inf, -Inf, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("min", "max"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              l <- bootfn(
                x = x,
                fn = f(min(x, na.rm = TRUE)),
                times = 5,
                rep = 2
              )
              u <- bootfn(
                x = x,
                fn = f(max(x, na.rm = TRUE)),
                times = 5,
                rep = 2
              )
              c(min = l, max = u)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::ddunif(
                  x = x,
                  min = par[1],
                  max = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pdunif(
                  q = q,
                  min = par[1],
                  max = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qdunif(
                  p = p,
                  min = par[1],
                  max = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-Inf, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      rndbinom = structure(
        list(
          args = c("size", "prob"),
          model = "Binomial",
          dist = "rndbinom",
          pkg = "mgalda",
          bound = list(
            size = function(x) {
              all(x >= -0.5 & x <= Inf)
            },
            prob = function(x) {
              all(x >= 0 & x <= 1)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            c(size = mean(x), prob = sum(x > mean(x)) / length(x))
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndbinom(
                  x = x,
                  size = par[1],
                  prob = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndbinom(
                  x = x,
                  size = par[1],
                  prob = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndbinom(
                  x = x,
                  size = par[1],
                  prob = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prndbinom(
                  q = q,
                  size = par[1],
                  prob = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrndbinom(
                  p = p,
                  size = par[1],
                  prob = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(-0.5, Inf, 0, 1),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("size", "prob"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              c(size = mean(x), prob = sum(x > mean(x)) / length(x))
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndbinom(
                  x = x,
                  size = par[1],
                  prob = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-0.5, 0),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndbinom(
                  x = x,
                  size = par[1],
                  prob = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-0.5, 0),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndbinom(
                  x = x,
                  size = par[1],
                  prob = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-0.5, 0),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prndbinom(
                  q = q,
                  size = par[1],
                  prob = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-0.5, 0),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrndbinom(
                  p = p,
                  size = par[1],
                  prob = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(-0.5, 0),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      rndgeom = structure(
        list(
          args = "prob",
          model = "Geometric",
          dist = "rndgeom",
          pkg = "mgalda",
          bound = list(
            prob = function(x) {
              all(x >= 0.00001 & x <= 1)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            start <- 1 / (1 + mean(x))
            names(start) <- "prob"
            start
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndgeom(
                  x = x,
                  prob = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndgeom(
                  x = x,
                  prob = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndgeom(
                  x = x,
                  prob = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prndgeom(
                  q = q,
                  prob = par[1]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrndgeom(
                  p = p,
                  prob = par[1]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
          ),
          bnd_num = structure(
            c(0.00001, 1),
            .Dim = 2:1,
            .Dimnames = list(c("lowb", "uppb"), "prob")
          ),
          optim.method = "Nelder-Mead",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              start <- 1 / (1 + mean(x))
              names(start) <- "prob"
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndgeom(
                  x = x,
                  prob = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndgeom(
                  x = x,
                  prob = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndgeom(
                  x = x,
                  prob = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prndgeom(
                  q = q,
                  prob = par[1]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrndgeom(
                  p = p,
                  prob = par[1]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0.00001,
                  upper = 1,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      rndnbinom = structure(
        list(
          args = c("size", "mu"),
          model = "NegativeBinomial",
          dist = "rndnbinom",
          pkg = "mgalda",
          bound = list(
            size = function(x) {
              all(x >= 0 & x <= Inf)
            },
            mu = function(x) {
              all(x >= -Inf & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            m <- mean(x)
            v <- var(x)
            size <- if (v > m) {
              m^2 / (v - m)
            } else {
              100
            }
            start <- c(size = size, mu = m)
            start
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndnbinom(
                  x = x,
                  size = par[1],
                  mu = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndnbinom(
                  x = x,
                  size = par[1],
                  mu = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndnbinom(
                  x = x,
                  size = par[1],
                  mu = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prndnbinom(
                  q = q,
                  size = par[1],
                  mu = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrndnbinom(
                  p = p,
                  size = par[1],
                  mu = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  Inf
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0, Inf, -Inf, Inf),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c("size", "mu"))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              m <- mean(x)
              v <- var(x)
              size <- if (v > m) {
                m^2 / (v - m)
              } else {
                100
              }
              start <- c(size = size, mu = m)
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndnbinom(
                  x = x,
                  size = par[1],
                  mu = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndnbinom(
                  x = x,
                  size = par[1],
                  mu = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndnbinom(
                  x = x,
                  size = par[1],
                  mu = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prndnbinom(
                  q = q,
                  size = par[1],
                  mu = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrndnbinom(
                  p = p,
                  size = par[1],
                  mu = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, Inf),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      rndpois = structure(
        list(
          args = "lambda",
          model = "Poisson",
          dist = "rndpois",
          pkg = "mgalda",
          bound = list(
            lambda = function(x) {
              all(x >= 0 & x <= Inf)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            assert_na_any(x, severity = "stop")
            start <- mean(x)
            names(start) <- "lambda"
            start
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndpois(
                  x = x,
                  lambda = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndpois(
                  x = x,
                  lambda = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndpois(
                  x = x,
                  lambda = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prndpois(
                  q = q,
                  lambda = par[1]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrndpois(
                  p = p,
                  lambda = par[1]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
          ),
          bnd_num = structure(
            c(0, Inf),
            .Dim = 2:1,
            .Dimnames = list(c("lowb", "uppb"), "lambda")
          ),
          optim.method = "Nelder-Mead",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              assert_na_any(x, severity = "stop")
              start <- mean(x)
              names(start) <- "lambda"
              start
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndpois(
                  x = x,
                  lambda = par[1]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndpois(
                  x = x,
                  lambda = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::drndpois(
                  x = x,
                  lambda = par[1]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::prndpois(
                  q = q,
                  lambda = par[1]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qrndpois(
                  p = p,
                  lambda = par[1]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = 0,
                  upper = Inf,
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "Nelder-Mead"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      ),
      zipois = structure(
        list(
          args = c("lambda", "pstr0"),
          model = "ZeroInflatedPoisson",
          dist = "zipois",
          pkg = "mgalda",
          bound = list(
            lambda = function(x) {
              all(x >= 0 & x <= Inf)
            },
            pstr0 = function(x) {
              all(x >= -Inf & x <= 1)
            }
          ),
          valid_x = function(x) {
            all(x >= 0 & x <= Inf)
          },
          fn_init = function(x) {
            c(lambda = 1, pstr0 = 0)
          },
          type = "discrete",
          fun_obj = list(
            mle_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dzipois(
                  x = x,
                  lambda = par[1],
                  pstr0 = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            cvmmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dzipois(
                  x = x,
                  lambda = par[1],
                  pstr0 = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            ksmge_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dzipois(
                  x = x,
                  lambda = par[1],
                  pstr0 = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop - obspl))) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            chi2_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pzipois(
                  q = q,
                  lambda = par[1],
                  pstr0 = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            },
            qmediscr_obj = function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qzipois(
                  p = p,
                  lambda = par[1],
                  pstr0 = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(
                  Inf,
                  1
                ),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              ))
            }
          ),
          bnd_num = structure(
            c(0, Inf, -Inf, 1),
            .Dim = c(
              2L,
              2L
            ),
            .Dimnames = list(c("lowb", "uppb"), c(
              "lambda",
              "pstr0"
            ))
          ),
          optim.method = "L-BFGS-B",
          fn_opts = function(x) {
            dens_valdid <- function(x) {
              ifelse(!is.finite(x), 0, x)
            }
            init <- function(x, na.rm = FALSE) {
              c(lambda = 1, pstr0 = 0)
            }
            mle_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dzipois(
                  x = x,
                  lambda = par[1],
                  pstr0 = par[2]
                ))
              }
              -sum(log(fn(par = par, x = x))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            cvmmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dzipois(
                  x = x,
                  lambda = par[1],
                  pstr0 = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                             n))^2) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            ksmge_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, x) {
                dens_valdid(mgalda::dzipois(
                  x = x,
                  lambda = par[1],
                  pstr0 = par[2]
                ))
              }
              x <- unique(x)
              n <- length(x)
              s <- sort(x)
              theop <- fn(par, s)
              obspu <- seq(1, n) / n
              obspl <- seq(0, n - 1) / n
              max(pmax(abs(theop - obspu), abs(theop -
                                                 obspl))) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            chi2_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, q) {
                mgalda::pzipois(
                  q = q,
                  lambda = par[1],
                  pstr0 = par[2]
                )
              }
              freq <- as.vct(table(x))
              count <- as.num(names(table(x)))
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
              sum((freq - expected)^2) / sum(expected) }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            qmediscr_obj <- function(x) {
              fnobj <- function(par, x) {{ fn <- function(par, p) {
                mgalda::qzipois(
                  p = p,
                  lambda = par[1],
                  pstr0 = par[2]
                )
              }
              (fn(par, 1 / 2) - median(x))^2 }}
              do_try(
                optim(
                  par = init(x),
                  lower = c(0, -Inf),
                  upper = c(Inf, 1),
                  fn = fnobj,
                  hessian = T,
                  x = x,
                  method = "L-BFGS-B"
                )
              )
            }
            if (length(x) > getOption("mgalda.n_samples", 1500)) {
              x <- sample(x, getOption(
                "mgalda.n_samples",
                1500
              ), F)
            }
            res <- do_try(mle_obj(x))
            if (is_empty(res)) {
              res <- do_try(cvmmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(ksmge_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(chi2_obj(x))
            }
            if (is_empty(res)) {
              res <- do_try(qmediscr_obj(x))
            }
            if (is_empty(res)) {
              cat_null("no result", type = "warn")
            }
            vc <- do_try(solve(res$hessian))
            sds <- do_try(sqrt(diag(vc)))
            list(
              estimate = res$par,
              convergence = res$convergence,
              hessian = res$hessian,
              sd = sds,
              vcov = vc
            )
          }
        ),
        class = "distribution_info",
        .inf = 100000,
        .small = 0.00001
      )
    ),
    fn_opt = list(
      beta = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            assert_engine(
              max(x) < 1,
              min(x) > 0,
              env = parent.frame(),
              msg = "values must be > 0",
              severity = "stop"
            )
            n <- length(x)
            m <- mean(x)
            v <- (n - 1) / n * var(x)
            aux <- m * (1 - m) / v - 1
            c(
              shape1 = m * aux,
              shape2 = (1 - m) * aux,
              ncp = 0
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dbeta(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                ncp = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qbeta(
                p = p,
                shape1 = par[1],
                shape2 = par[2],
                ncp = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::pbeta(
                q = q,
                shape1 = par[1],
                shape2 = par[2],
                ncp = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dbeta(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                ncp = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dbeta(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                ncp = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dbeta(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                ncp = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Beta",
                quantile = c("stats", "qbeta"),
                probab = c(
                  "stats",
                  "pbeta"
                ),
                density = c("stats", "dbeta"),
                random = c(
                  "stats",
                  "rbeta"
                ),
                dist = "beta",
                support = all(x >= 0.00001 &
                                x <= 1)
              ),
              bound = list(
                shape1 = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                },
                shape2 = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                },
                ncp = function(x) {
                  all(x >= 0 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      betanorm = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              shape1 = 1,
              shape2 = 1,
              mean = 0,
              sd = 1
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dbetanorm(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                mean = par[3],
                sd = par[4]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qbetanorm(
                p = p,
                shape1 = par[1],
                shape2 = par[2],
                mean = par[3],
                sd = par[4]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pbetanorm(
                q = q,
                shape1 = par[1],
                shape2 = par[2],
                mean = par[3],
                sd = par[4]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dbetanorm(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                mean = par[3],
                sd = par[4]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dbetanorm(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                mean = par[3],
                sd = par[4]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dbetanorm(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                mean = par[3],
                sd = par[4]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0, 0, -Inf,
                  0
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "BetaNormal",
                quantile = c("mgalda", "qbetanorm"),
                probab = c(
                  "mgalda",
                  "pbetanorm"
                ),
                density = c("mgalda", "dbetanorm"),
                random = c("mgalda", "rbetanorm"),
                dist = "betanorm",
                support = all(x >= -Inf & x <= Inf)
              ),
              bound = list(
                shape1 = function(x) {
                  all(x >=
                        0 & x <= Inf)
                },
                shape2 = function(x) {
                  all(x >= 0 &
                        x <= Inf)
                },
                mean = function(x) {
                  all(x >= -Inf & x <=
                        Inf)
                },
                sd = function(x) {
                  all(x >= 0 & x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      betaprime = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            assert_engine(
              max(x) < 1,
              min(x) > 0,
              env = parent.frame(),
              msg = "values must be > 0",
              severity = "stop"
            )
            val1 <- mean(log(x / (x + 1)))
            val2 <- mean(log(1 - x / (x + 1)))
            G1 <- exp(val1)
            G2 <- exp(val2)
            denom <- 1 / 2 * (1 / (1 - G1 - G2))
            start <- c(1 / 2 + G1 * denom, 1 / 2 + G2 * denom)
            names(start) <- c("alpha", "beta")
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dbetaprime(
                x = x,
                alpha = par[1],
                beta = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qbetaprime(
                p = p,
                alpha = par[1],
                beta = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pbetaprime(
                q = q,
                alpha = par[1],
                beta = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dbetaprime(
                x = x,
                alpha = par[1],
                beta = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dbetaprime(
                x = x,
                alpha = par[1],
                beta = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dbetaprime(
                x = x,
                alpha = par[1],
                beta = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "BetaPrime",
                quantile = c("mgalda", "qbetaprime"),
                probab = c(
                  "mgalda",
                  "pbetaprime"
                ),
                density = c("mgalda", "dbetaprime"),
                random = c("mgalda", "rbetaprime"),
                dist = "betaprime",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                alpha = function(x) {
                  all(x >=
                        0 & x <= Inf)
                },
                beta = function(x) {
                  all(x >= 0 & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      burr = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              shape1 = 2,
              shape2 = 1,
              scale = 0.5
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dburr(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                scale = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qburr(
                p = p,
                shape1 = par[1],
                shape2 = par[2],
                scale = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pburr(
                q = q,
                shape1 = par[1],
                shape2 = par[2],
                scale = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dburr(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                scale = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dburr(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                scale = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dburr(
                x = x,
                shape1 = par[1],
                shape2 = par[2],
                scale = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "TheBurr",
                quantile = c("mgalda", "qburr"),
                probab = c(
                  "mgalda",
                  "pburr"
                ),
                density = c("mgalda", "dburr"),
                random = c(
                  "mgalda",
                  "rburr"
                ),
                dist = "burr",
                support = all(x >= -Inf &
                                x <= Inf)
              ),
              bound = list(
                shape1 = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                shape2 = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                },
                scale = function(x) {
                  all(x >= -Inf & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      cauchy = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            start <- c(location = median(x), scale = IQR(x) / 2)
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dcauchy(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qcauchy(
                p = p,
                location = par[1],
                scale = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::pcauchy(
                q = q,
                location = par[1],
                scale = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dcauchy(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dcauchy(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dcauchy(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Cauchy",
                quantile = c("stats", "qcauchy"),
                probab = c(
                  "stats",
                  "pcauchy"
                ),
                density = c("stats", "dcauchy"),
                random = c("stats", "rcauchy"),
                dist = "cauchy",
                support = all(x >= -Inf & x <= Inf)
              ),
              bound = list(
                location = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                scale = function(x) {
                  all(x >= 0 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      chisq = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_engine(!any(x <= 0),
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            assert_na_any(x, severity = "stop")
            c(df = (mean(x) + sd(x)) / 2, ncp = 0)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dchisq(
                x = x,
                df = par[1],
                ncp = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qchisq(
                p = p,
                df = par[1],
                ncp = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pchisq(
                q = q,
                df = par[1],
                ncp = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dchisq(
                x = x,
                df = par[1],
                ncp = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dchisq(
                x = x,
                df = par[1],
                ncp = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dchisq(
                x = x,
                df = par[1],
                ncp = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "ChiSquared",
                quantile = c("mgalda", "qchisq"),
                probab = c(
                  "mgalda",
                  "pchisq"
                ),
                density = c("mgalda", "dchisq"),
                random = c(
                  "mgalda",
                  "rchisq"
                ),
                dist = "chisq",
                support = all(x >=
                                0.00001 & x <= Inf)
              ),
              bound = list(
                df = function(x) {
                  all(x >=
                        0 & x <= Inf)
                },
                ncp = function(x) {
                  all(x >= -0.00001 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      exp = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_engine(!any(x < 0),
                          env = parent.frame(),
                          msg = "values must be >= 0",
                          severity = "stop"
            )
            assert_na_any(x, severity = "stop")
            start <- 1 / mean(x)
            names(start) <- "rate"
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dexp(
                x = x,
                rate = par[1]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qexp(
                p = p,
                rate = par[1]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::pexp(
                q = q,
                rate = par[1]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dexp(
                x = x,
                rate = par[1]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dexp(
                x = x,
                rate = par[1]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dexp(
                x = x,
                rate = par[1]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Exponential",
                quantile = c("stats", "qexp"),
                probab = c(
                  "stats",
                  "pexp"
                ),
                density = c("stats", "dexp"),
                random = c(
                  "stats",
                  "rexp"
                ),
                dist = "exp",
                support = all(x >= 0.00001 &
                                x <= Inf)
              ),
              bound = list(
                rate = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                }
              ),
              optim.method = "Nelder-Mead",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      expgeom = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(scale = 1, shape = 1)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dexpgeom(
                x = x,
                scale = par[1],
                shape = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qexpgeom(
                p = p,
                scale = par[1],
                shape = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pexpgeom(
                q = q,
                scale = par[1],
                shape = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dexpgeom(
                x = x,
                scale = par[1],
                shape = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dexpgeom(
                x = x,
                scale = par[1],
                shape = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dexpgeom(
                x = x,
                scale = par[1],
                shape = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "ExponentialGeometric",
                quantile = c("mgalda", "qexpgeom"),
                probab = c(
                  "mgalda",
                  "pexpgeom"
                ),
                density = c("mgalda", "dexpgeom"),
                random = c("mgalda", "rexpgeom"),
                dist = "expgeom",
                support = all(x >= 0.00001 & x <= Inf)
              ),
              bound = list(
                scale = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                },
                shape = function(x) {
                  all(x >=
                        0.00001 & x <= 0.99999)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      explog = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(scale = 1,shape = 1)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dexplog(
                x = x,
                scale = par[1],
                shape = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qexplog(
                p = p,
                scale = par[1],
                shape = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pexplog(
                q = q,
                scale = par[1],
                shape = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dexplog(
                x = x,
                scale = par[1],
                shape = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dexplog(
                x = x,
                scale = par[1],
                shape = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dexplog(
                x = x,
                scale = par[1],
                shape = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "ExponentialLogarithmic",
                quantile = c("mgalda", "qexplog"),
                probab = c(
                  "mgalda",
                  "pexplog"
                ),
                density = c("mgalda", "dexplog"),
                random = c("mgalda", "rexplog"),
                dist = "explog",
                support = all(x >= 0.00001 & x <= Inf)
              ),
              bound = list(
                scale = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                },
                shape = function(x) {
                  all(x >=
                        0.00001 & x <= 0.99999)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      f = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              df1 = 1,
              df2 = 1,
              ncp = 0
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::df(
                x = x,
                df1 = par[1],
                df2 = par[2],
                ncp = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qf(
                p = p,
                df1 = par[1],
                df2 = par[2],
                ncp = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::pf(
                q = q,
                df1 = par[1],
                df2 = par[2],
                ncp = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::df(
                x = x,
                df1 = par[1],
                df2 = par[2],
                ncp = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::df(
                x = x,
                df1 = par[1],
                df2 = par[2],
                ncp = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::df(
                x = x,
                df1 = par[1],
                df2 = par[2],
                ncp = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001, 0
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "F",
                quantile = c("stats", "qf"),
                probab = c(
                  "stats",
                  "pf"
                ),
                density = c("stats", "df"),
                random = c(
                  "stats",
                  "rf"
                ),
                dist = "f",
                support = all(x >= 0.00001 &
                                x <= Inf)
              ),
              bound = list(
                df1 = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                },
                df2 = function(x) {
                  all(x >= 0.00001 &
                        x <= Inf)
                },
                ncp = function(x) {
                  all(x >= 0 & x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      gamma = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            assert_engine(min(x) > 0,
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            n <- length(x)
            m <- mean(x)
            v <- (n - 1) / n * var(x)
            c(shape = m^2 / v, rate = m / v)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dgamma(
                x = x,
                shape = par[1],
                rate = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qgamma(
                p = p,
                shape = par[1],
                rate = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::pgamma(
                q = q,
                shape = par[1],
                rate = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dgamma(
                x = x,
                shape = par[1],
                rate = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dgamma(
                x = x,
                shape = par[1],
                rate = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dgamma(
                x = x,
                shape = par[1],
                rate = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Gamma",
                quantile = c("stats", "qgamma"),
                probab = c(
                  "stats",
                  "pgamma"
                ),
                density = c("stats", "dgamma"),
                random = c(
                  "stats",
                  "rgamma"
                ),
                dist = "gamma",
                support = all(x >=
                                0 & x <= Inf)
              ),
              bound = list(
                shape = function(x) {
                  all(x >=
                        0 & x <= Inf)
                },
                rate = function(x) {
                  all(x >= 0 & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      ged = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              mean = 0,
              sd = 1,
              nu = 2
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qged(
                p = p,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pged(
                q = q,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.5
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "GeneralizedError",
                quantile = c("mgalda", "qged"),
                probab = c(
                  "mgalda",
                  "pged"
                ),
                density = c("mgalda", "dged"),
                random = c(
                  "mgalda",
                  "rged"
                ),
                dist = "ged",
                support = all(x >= -Inf &
                                x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sd = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                },
                nu = function(x) {
                  all(x >= 0.5 & x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      gev = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              location = 0,
              scale = 1,
              shape = 0
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgev(
                x = x,
                location = par[1],
                scale = par[2],
                shape = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qgev(
                p = p,
                location = par[1],
                scale = par[2],
                shape = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pgev(
                q = q,
                location = par[1],
                scale = par[2],
                shape = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgev(
                x = x,
                location = par[1],
                scale = par[2],
                shape = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgev(
                x = x,
                location = par[1],
                scale = par[2],
                shape = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgev(
                x = x,
                location = par[1],
                scale = par[2],
                shape = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "GeneralizedExtreme",
                quantile = c("mgalda", "qgev"),
                probab = c(
                  "mgalda",
                  "pgev"
                ),
                density = c("mgalda", "dgev"),
                random = c(
                  "mgalda",
                  "rgev"
                ),
                dist = "gev",
                support = all(x >= -Inf &
                                x <= Inf)
              ),
              bound = list(
                location = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                scale = function(x) {
                  all(x >= 0.00001 &
                        x <= Inf)
                },
                shape = function(x) {
                  all(x >= -Inf & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      gpd = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              location = 0,
              scale = 1,
              shape = 0
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgpd(
                x = x,
                location = par[1],
                scale = par[2],
                shape = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qgpd(
                p = p,
                location = par[1],
                scale = par[2],
                shape = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pgpd(
                q = q,
                location = par[1],
                scale = par[2],
                shape = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgpd(
                x = x,
                location = par[1],
                scale = par[2],
                shape = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgpd(
                x = x,
                location = par[1],
                scale = par[2],
                shape = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgpd(
                x = x,
                location = par[1],
                scale = par[2],
                shape = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  -Inf
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "GeneralizedPareto",
                quantile = c("mgalda", "qgpd"),
                probab = c(
                  "mgalda",
                  "pgpd"
                ),
                density = c("mgalda", "dgpd"),
                random = c(
                  "mgalda",
                  "rgpd"
                ),
                dist = "gpd",
                support = all(x >= -Inf &
                                x <= Inf)
              ),
              bound = list(
                location = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                scale = function(x) {
                  all(x >= 0.00001 &
                        x <= Inf)
                },
                shape = function(x) {
                  all(x >= -Inf & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      gumbel = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(location = 0, scale = 1)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgumbel(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qgumbel(
                p = p,
                location = par[1],
                scale = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pgumbel(
                q = q,
                location = par[1],
                scale = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgumbel(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgumbel(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dgumbel(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Gumbel",
                quantile = c("mgalda", "qgumbel"),
                probab = c(
                  "mgalda",
                  "pgumbel"
                ),
                density = c("mgalda", "dgumbel"),
                random = c("mgalda", "rgumbel"),
                dist = "gumbel",
                support = all(x >= -Inf & x <= Inf)
              ),
              bound = list(
                location = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                scale = function(x) {
                  all(x >= 0.00001 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      halfnorm = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(mean = 0, theta = sqrt(pi / 2))
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dhalfnorm(
                x = x,
                mean = par[1],
                theta = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qhalfnorm(
                p = p,
                mean = par[1],
                theta = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::phalfnorm(
                q = q,
                mean = par[1],
                theta = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dhalfnorm(
                x = x,
                mean = par[1],
                theta = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dhalfnorm(
                x = x,
                mean = par[1],
                theta = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dhalfnorm(
                x = x,
                mean = par[1],
                theta = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "HalfNorm",
                quantile = c("mgalda", "qhalfnorm"),
                probab = c(
                  "mgalda",
                  "phalfnorm"
                ),
                density = c("mgalda", "dhalfnorm"),
                random = c("mgalda", "rhalfnorm"),
                dist = "halfnorm",
                support = all(x >= -Inf & x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                theta = function(x) {
                  all(x >= 0.00001 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      huber = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              k = 0.862,
              mu = 0,
              sigma = 1
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dhuber(
                x = x,
                k = par[1],
                mu = par[2],
                sigma = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  -Inf, 0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qhuber(
                p = p,
                k = par[1],
                mu = par[2],
                sigma = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  -Inf, 0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::phuber(
                q = q,
                k = par[1],
                mu = par[2],
                sigma = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  -Inf, 0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dhuber(
                x = x,
                k = par[1],
                mu = par[2],
                sigma = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  -Inf, 0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dhuber(
                x = x,
                k = par[1],
                mu = par[2],
                sigma = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  -Inf, 0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dhuber(
                x = x,
                k = par[1],
                mu = par[2],
                sigma = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  -Inf, 0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "HubersLeastFavourable",
                quantile = c("mgalda", "qhuber"),
                probab = c(
                  "mgalda",
                  "phuber"
                ),
                density = c("mgalda", "dhuber"),
                random = c(
                  "mgalda",
                  "rhuber"
                ),
                dist = "huber",
                support = all(x >=
                                -Inf & x <= Inf)
              ),
              bound = list(
                k = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                },
                mu = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                },
                sigma = function(x) {
                  all(x >= 0.00001 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      invchisq = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(df = 1)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dinvchisq(
                x = x,
                df = par[1]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qinvchisq(
                p = p,
                df = par[1]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pinvchisq(
                q = q,
                df = par[1]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dinvchisq(
                x = x,
                df = par[1]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dinvchisq(
                x = x,
                df = par[1]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dinvchisq(
                x = x,
                df = par[1]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Invchisq",
                quantile = c("mgalda", "qinvchisq"),
                probab = c(
                  "mgalda",
                  "pinvchisq"
                ),
                density = c("mgalda", "dinvchisq"),
                random = c("mgalda", "rinvchisq"),
                dist = "invchisq",
                support = all(x >= 0.00001 & x <= Inf)
              ),
              bound = list(
                df = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                }
              ),
              optim.method = "Nelder-Mead",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      laplace = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(mu = 0, sigma = 1)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dlaplace(
                x = x,
                mu = par[1],
                sigma = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qlaplace(
                p = p,
                mu = par[1],
                sigma = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::plaplace(
                q = q,
                mu = par[1],
                sigma = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dlaplace(
                x = x,
                mu = par[1],
                sigma = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dlaplace(
                x = x,
                mu = par[1],
                sigma = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dlaplace(
                x = x,
                mu = par[1],
                sigma = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Laplace",
                quantile = c("mgalda", "qlaplace"),
                probab = c(
                  "mgalda",
                  "plaplace"
                ),
                density = c("mgalda", "dlaplace"),
                random = c("mgalda", "rlaplace"),
                dist = "laplace",
                support = all(x >= -Inf & x <= Inf)
              ),
              bound = list(
                mu = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sigma = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      llogis = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            assert_engine(min(x) > 0,
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            p25 <- as.numeric(quantile(x, 0.25))
            p75 <- as.numeric(quantile(x, 0.75))
            shape <- 2 * log(3) / (log(p75) - log(p25))
            scale <- exp(log(p75) + log(p25)) / 2
            c(shape = shape, scale = scale)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dllogis(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qllogis(
                p = p,
                shape = par[1],
                scale = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pllogis(
                q = q,
                shape = par[1],
                scale = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dllogis(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dllogis(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dllogis(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Loglogistic",
                quantile = c("mgalda", "qllogis"),
                probab = c(
                  "mgalda",
                  "pllogis"
                ),
                density = c("mgalda", "dllogis"),
                random = c("mgalda", "rllogis"),
                dist = "llogis",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                shape = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                },
                scale = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      lnorm = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            lx <- log(x)
            n <- length(x)
            sd0 <- sqrt((n - 1) / n) * sd(lx)
            mx <- mean(lx)
            start <- c(mx, sd0)
            names(start) <- c("meanlog", "sdlog")
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dlnorm(
                x = x,
                meanlog = par[1],
                sdlog = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qlnorm(
                p = p,
                meanlog = par[1],
                sdlog = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::plnorm(
                q = q,
                meanlog = par[1],
                sdlog = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dlnorm(
                x = x,
                meanlog = par[1],
                sdlog = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dlnorm(
                x = x,
                meanlog = par[1],
                sdlog = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dlnorm(
                x = x,
                meanlog = par[1],
                sdlog = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Lognormal",
                quantile = c("stats", "qlnorm"),
                probab = c(
                  "stats",
                  "plnorm"
                ),
                density = c("stats", "dlnorm"),
                random = c(
                  "stats",
                  "rlnorm"
                ),
                dist = "lnorm",
                support = all(x >=
                                0 & x <= Inf)
              ),
              bound = list(
                meanlog = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sdlog = function(x) {
                  all(x >= 0 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      logis = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            start <- c(location = median(x), scale = IQR(x) / 2)
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dlogis(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qlogis(
                p = p,
                location = par[1],
                scale = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::plogis(
                q = q,
                location = par[1],
                scale = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dlogis(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dlogis(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dlogis(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Logistic",
                quantile = c("stats", "qlogis"),
                probab = c(
                  "stats",
                  "plogis"
                ),
                density = c("stats", "dlogis"),
                random = c(
                  "stats",
                  "rlogis"
                ),
                dist = "logis",
                support = all(x >=
                                -Inf & x <= Inf)
              ),
              bound = list(
                location = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                scale = function(x) {
                  all(x >= 0 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      loglap = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              location_ald = 0,
              scale_ald = 1,
              tau = 0.5
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dloglap(
                x = x,
                location_ald = par[1],
                scale_ald = par[2],
                tau = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qloglap(
                p = p,
                location_ald = par[1],
                scale_ald = par[2],
                tau = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::ploglap(
                q = q,
                location_ald = par[1],
                scale_ald = par[2],
                tau = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dloglap(
                x = x,
                location_ald = par[1],
                scale_ald = par[2],
                tau = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dloglap(
                x = x,
                location_ald = par[1],
                scale_ald = par[2],
                tau = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dloglap(
                x = x,
                location_ald = par[1],
                scale_ald = par[2],
                tau = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, 0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf, 0.99999),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "LogLaplace",
                quantile = c("mgalda", "qloglap"),
                probab = c(
                  "mgalda",
                  "ploglap"
                ),
                density = c("mgalda", "dloglap"),
                random = c("mgalda", "rloglap"),
                dist = "loglap",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                location_ald = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                scale_ald = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                },
                tau = function(x) {
                  all(x >= 0.00001 &
                        x <= 0.99999)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      norm = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            n <- length(x)
            sd0 <- sqrt((n - 1) / n) * sd(x)
            mx <- mean(x)
            start <- c(mx, sd0)
            names(start) <- c("mean", "sd")
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qnorm(
                p = p,
                mean = par[1],
                sd = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::pnorm(
                q = q,
                mean = par[1],
                sd = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Normal",
                quantile = c("stats", "qnorm"),
                probab = c(
                  "stats",
                  "pnorm"
                ),
                density = c("stats", "dnorm"),
                random = c(
                  "stats",
                  "rnorm"
                ),
                dist = "norm",
                support = all(x >= -Inf &
                                x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sd = function(x) {
                  all(x >= 0 & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      pareto = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            assert_engine(!any(x < 0),
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            m1 <- mean(x)
            m2 <- mean(x^2)
            scale <- (m1 * m2) / (m2 - 2 * m1^2)
            shape <- 2 * (m2 - m1^2) / (m2 - 2 * m1^2)
            start <- c(shape = shape, scale = scale)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dpareto(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qpareto(
                p = p,
                shape = par[1],
                scale = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::ppareto(
                q = q,
                shape = par[1],
                scale = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dpareto(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dpareto(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dpareto(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0.25, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Pareto",
                quantile = c("mgalda", "qpareto"),
                probab = c(
                  "mgalda",
                  "ppareto"
                ),
                density = c("mgalda", "dpareto"),
                random = c("mgalda", "rpareto"),
                dist = "pareto",
                support = all(x >= 0.00001 & x <= Inf)
              ),
              bound = list(
                shape = function(x) {
                  all(x >=
                        0.25 & x <= Inf)
                },
                scale = function(x) {
                  all(x >= 0.00001 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      posnorm = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(mean = 0, sd = 1)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dposnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qposnorm(
                p = p,
                mean = par[1],
                sd = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pposnorm(
                q = q,
                mean = par[1],
                sd = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dposnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dposnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dposnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "PositiveNormalDistribution",
                quantile = c("mgalda", "qposnorm"),
                probab = c(
                  "mgalda",
                  "pposnorm"
                ),
                density = c("mgalda", "dposnorm"),
                random = c("mgalda", "rposnorm"),
                dist = "posnorm",
                support = all(x >= 0.00001 & x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sd = function(x) {
                  all(x >= 0 & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      power = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(alpha = 1, beta = 1)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dpower(
                x = x,
                alpha = par[1],
                beta = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qpower(
                p = p,
                alpha = par[1],
                beta = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::ppower(
                q = q,
                alpha = par[1],
                beta = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dpower(
                x = x,
                alpha = par[1],
                beta = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dpower(
                x = x,
                alpha = par[1],
                beta = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dpower(
                x = x,
                alpha = par[1],
                beta = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "PowerDist",
                quantile = c("mgalda", "qpower"),
                probab = c(
                  "mgalda",
                  "ppower"
                ),
                density = c("mgalda", "dpower"),
                random = c(
                  "mgalda",
                  "rpower"
                ),
                dist = "power",
                support = all(x >=
                                -Inf & x <= Inf)
              ),
              bound = list(
                alpha = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                beta = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      rayleigh = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(scale = 1)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drayleigh(
                x = x,
                scale = par[1]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qrayleigh(
                p = p,
                scale = par[1]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::prayleigh(
                q = q,
                scale = par[1]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drayleigh(
                x = x,
                scale = par[1]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drayleigh(
                x = x,
                scale = par[1]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drayleigh(
                x = x,
                scale = par[1]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Rayleigh",
                quantile = c("mgalda", "qrayleigh"),
                probab = c(
                  "mgalda",
                  "prayleigh"
                ),
                density = c("mgalda", "drayleigh"),
                random = c("mgalda", "rrayleigh"),
                dist = "rayleigh",
                support = all(x >= 0.00001 & x <= Inf)
              ),
              bound = list(
                scale = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                }
              ),
              optim.method = "Nelder-Mead",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      sc_t2 = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(location = 0, scale = 1)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsc_t2(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qsc_t2(
                p = p,
                location = par[1],
                scale = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::psc_t2(
                q = q,
                location = par[1],
                scale = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsc_t2(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsc_t2(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsc_t2(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0.00001),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Studenttdegrees",
                quantile = c("mgalda", "qsc_t2"),
                probab = c(
                  "mgalda",
                  "psc_t2"
                ),
                density = c("mgalda", "dsc_t2"),
                random = c(
                  "mgalda",
                  "rsc_t2"
                ),
                dist = "sc_t2",
                support = all(x >=
                                -Inf & x <= Inf)
              ),
              bound = list(
                location = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                scale = function(x) {
                  all(x >= 0.00001 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      sged = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              mean = 0,
              sd = 1,
              nu = 2,
              xi = 1.5
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qsged(
                p = p,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::psged(
                q = q,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  -Inf, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "SkewGeneralizedError",
                quantile = c("mgalda", "qsged"),
                probab = c(
                  "mgalda",
                  "psged"
                ),
                density = c("mgalda", "dsged"),
                random = c(
                  "mgalda",
                  "rsged"
                ),
                dist = "sged",
                support = all(x >= -Inf &
                                x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sd = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                },
                nu = function(x) {
                  all(x >= -Inf & x <=
                        Inf)
                },
                xi = function(x) {
                  all(x >= -Inf & x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      snorm = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              mean = 0,
              sd = 1,
              xi = 1.5
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsnorm(
                x = x,
                mean = par[1],
                sd = par[2],
                xi = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qsnorm(
                p = p,
                mean = par[1],
                sd = par[2],
                xi = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::psnorm(
                q = q,
                mean = par[1],
                sd = par[2],
                xi = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsnorm(
                x = x,
                mean = par[1],
                sd = par[2],
                xi = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsnorm(
                x = x,
                mean = par[1],
                sd = par[2],
                xi = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsnorm(
                x = x,
                mean = par[1],
                sd = par[2],
                xi = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "SkewNormal",
                quantile = c("mgalda", "qsnorm"),
                probab = c(
                  "mgalda",
                  "psnorm"
                ),
                density = c("mgalda", "dsnorm"),
                random = c(
                  "mgalda",
                  "rsnorm"
                ),
                dist = "snorm",
                support = all(x >=
                                -Inf & x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sd = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                },
                xi = function(x) {
                  all(x >= 0.00001 & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      sstd = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              mean = 0,
              sd = 1,
              nu = 5,
              xi = 1.5
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsstd(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qsstd(
                p = p,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::psstd(
                q = q,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsstd(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsstd(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dsstd(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3],
                xi = par[4]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3, -Inf
                ),
                upper = c(Inf, Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "SkewStudentt",
                quantile = c("mgalda", "qsstd"),
                probab = c(
                  "mgalda",
                  "psstd"
                ),
                density = c("mgalda", "dsstd"),
                random = c(
                  "mgalda",
                  "rsstd"
                ),
                dist = "sstd",
                support = all(x >= -Inf &
                                x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sd = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                },
                nu = function(x) {
                  all(x >= 3 & x <= Inf)
                },
                xi = function(x) {
                  all(x >= -Inf & x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      std = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              mean = 0,
              sd = 1,
              nu = 5
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dstd(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qstd(
                p = p,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pstd(
                q = q,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dstd(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dstd(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dstd(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  3
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Studentt",
                quantile = c("mgalda", "qstd"),
                probab = c(
                  "mgalda",
                  "pstd"
                ),
                density = c("mgalda", "dstd"),
                random = c(
                  "mgalda",
                  "rstd"
                ),
                dist = "std",
                support = all(x >= -Inf &
                                x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sd = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                },
                nu = function(x) {
                  all(x >= 3 & x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      unif = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            l <- bootfn(
              x = x,
              fn = f(min(x, na.rm = TRUE)),
              times = 5,
              rep = 2
            )
            u <- bootfn(
              x = x,
              fn = f(max(x, na.rm = TRUE)),
              times = 5,
              rep = 2
            )
            c(min = l, max = u)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dunif(
                x = x,
                min = par[1],
                max = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qunif(
                p = p,
                min = par[1],
                max = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::punif(
                q = q,
                min = par[1],
                max = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dunif(
                x = x,
                min = par[1],
                max = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dunif(
                x = x,
                min = par[1],
                max = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dunif(
                x = x,
                min = par[1],
                max = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Uniform",
                quantile = c("stats", "qunif"),
                probab = c(
                  "stats",
                  "punif"
                ),
                density = c("stats", "dunif"),
                random = c(
                  "stats",
                  "runif"
                ),
                dist = "unif",
                support = all(x >= -Inf &
                                x <= Inf)
              ),
              bound = list(
                min = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                max = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      weibull = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_engine(!any(x <= 0),
                          env = parent.frame(),
                          msg = "values must be > 0",
                          severity = "stop"
            )
            assert_na_any(x, severity = "stop")
            lx <- log(x)
            m <- mean(lx)
            v <- var(lx)
            shape <- 1.2 / sqrt(v)
            scale <- exp(m + 0.572 / shape)
            start <- c(shape = shape, scale = scale)
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dweibull(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qme_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              stats::qweibull(
                p = p,
                shape = par[1],
                scale = par[2]
              )
            })
            valid <- (function(x) {
              ifelse(is.na(x) | is.nan(x) | is.infinite(x),
                     0, x
              )
            })
            p <- ppoints(7)
            q_theo <- valid(fn(par = par, p = p))
            q_emp <- as.num(quantile(x, probs = p, type = 7))
            sum((q_emp - q_theo)^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          mge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              stats::pweibull(
                q = q,
                shape = par[1],
                scale = par[2]
              )
            })
            sx <- c(-Inf, sort(x), Inf)
            n <- length(sx)
            Di <- fn(par = par, q = sx[-1]) - fn(
              par = par,
              q = sx[-n]
            )
            mean(log(Di)) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dweibull(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dweibull(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          admge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(stats::dweibull(
                x = x,
                shape = par[1],
                scale = par[2]
              ))
            })
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            -n - mean((2 * 1:n - 1) * (log(theop) + log(1 -
                                                          rev(theop)))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  0.00001,
                  0.00001
                ),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(qme_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(mge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(admge_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Weibull",
                quantile = c("stats", "qweibull"),
                probab = c(
                  "stats",
                  "pweibull"
                ),
                density = c("stats", "dweibull"),
                random = c("stats", "rweibull"),
                dist = "weibull",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                shape = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                },
                scale = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "continuous"
            ),
            class = "distrcont"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      dcauchy = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            start <- c(location = median(x), scale = IQR(x) / 2)
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddcauchy(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddcauchy(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddcauchy(
                x = x,
                location = par[1],
                scale = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pdcauchy(
                q = q,
                location = par[1],
                scale = par[2]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qdcauchy(
                p = p,
                location = par[1],
                scale = par[2]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "DiscreteCauchy",
                quantile = c("mgalda", "qdcauchy"),
                probab = c(
                  "mgalda",
                  "pdcauchy"
                ),
                density = c("mgalda", "ddcauchy"),
                random = c("mgalda", "rdcauchy"),
                dist = "dcauchy",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                location = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                scale = function(x) {
                  all(x >= 0 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      dexp = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_engine(!any(x < 0),
                          env = parent.frame(),
                          msg = "values must be >= 0",
                          severity = "stop"
            )
            assert_na_any(x, severity = "stop")
            start <- 1 / mean(x)
            names(start) <- "rate"
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddexp(
                x = x,
                rate = par[1]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddexp(
                x = x,
                rate = par[1]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddexp(
                x = x,
                rate = par[1]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pdexp(
                q = q,
                rate = par[1]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qdexp(
                p = p,
                rate = par[1]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "DiscreteExponential",
                quantile = c("mgalda", "qdexp"),
                probab = c(
                  "mgalda",
                  "pdexp"
                ),
                density = c("mgalda", "ddexp"),
                random = c(
                  "mgalda",
                  "rdexp"
                ),
                dist = "dexp",
                support = all(x >= 0 &
                                x <= Inf)
              ),
              bound = list(
                rate = function(x) {
                  all(x >=
                        0.00001 & x <= Inf)
                }
              ),
              optim.method = "Nelder-Mead",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      dged = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(
              mean = 0,
              sd = 1,
              nu = 2
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddged(
                x = x,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pdged(
                q = q,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qdged(
                p = p,
                mean = par[1],
                sd = par[2],
                nu = par[3]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = c(
                  -Inf, -Inf,
                  0.00001
                ),
                upper = c(Inf, Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "DiscreteGeneralizedError",
                quantile = c("mgalda", "qdged"),
                probab = c(
                  "mgalda",
                  "pdged"
                ),
                density = c("mgalda", "ddged"),
                random = c(
                  "mgalda",
                  "rdged"
                ),
                dist = "dged",
                support = all(x >= 0 &
                                x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sd = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                },
                nu = function(x) {
                  all(x >= 0.00001 & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      dlnorm = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            lx <- log(x)
            n <- length(x)
            sd0 <- sqrt((n - 1) / n) * sd(lx)
            mx <- mean(lx)
            start <- c(mx, sd0)
            names(start) <- c("meanlog", "sdlog")
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddlnorm(
                x = x,
                meanlog = par[1],
                sdlog = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddlnorm(
                x = x,
                meanlog = par[1],
                sdlog = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddlnorm(
                x = x,
                meanlog = par[1],
                sdlog = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pdlnorm(
                q = q,
                meanlog = par[1],
                sdlog = par[2]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qdlnorm(
                p = p,
                meanlog = par[1],
                sdlog = par[2]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "DiscreteLognormal",
                quantile = c("mgalda", "qdlnorm"),
                probab = c(
                  "mgalda",
                  "pdlnorm"
                ),
                density = c("mgalda", "ddlnorm"),
                random = c("mgalda", "rdlnorm"),
                dist = "dlnorm",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                meanlog = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sdlog = function(x) {
                  all(x >= 0 &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      dnorm = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            n <- length(x)
            sd0 <- sqrt((n - 1) / n) * sd(x)
            mx <- mean(x)
            start <- c(mx, sd0)
            names(start) <- c("mean", "sd")
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddnorm(
                x = x,
                mean = par[1],
                sd = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pdnorm(
                q = q,
                mean = par[1],
                sd = par[2]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qdnorm(
                p = p,
                mean = par[1],
                sd = par[2]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, 0),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "DiscreteNormal",
                quantile = c("mgalda", "qdnorm"),
                probab = c(
                  "mgalda",
                  "pdnorm"
                ),
                density = c("mgalda", "ddnorm"),
                random = c(
                  "mgalda",
                  "rdnorm"
                ),
                dist = "dnorm",
                support = all(x >=
                                0 & x <= Inf)
              ),
              bound = list(
                mean = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                sd = function(x) {
                  all(x >= 0 & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      dunif = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            l <- bootfn(
              x = x,
              fn = f(min(x, na.rm = TRUE)),
              times = 5,
              rep = 2
            )
            u <- bootfn(
              x = x,
              fn = f(max(x, na.rm = TRUE)),
              times = 5,
              rep = 2
            )
            c(min = l, max = u)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddunif(
                x = x,
                min = par[1],
                max = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddunif(
                x = x,
                min = par[1],
                max = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::ddunif(
                x = x,
                min = par[1],
                max = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pdunif(
                q = q,
                min = par[1],
                max = par[2]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qdunif(
                p = p,
                min = par[1],
                max = par[2]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = c(-Inf, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Discrete Uniform Distribution",
                quantile = c("mgalda", "qdunif"),
                probab = c(
                  "mgalda",
                  "pdunif"
                ),
                density = c("mgalda", "ddunif"),
                random = c(
                  "mgalda",
                  "rdunif"
                ),
                dist = "dunif",
                support = all(x >=
                                0 & x <= Inf)
              ),
              bound = list(
                min = function(x) {
                  all(x >=
                        -Inf & x <= Inf)
                },
                max = function(x) {
                  all(x >= -Inf &
                        x <= Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      rndbinom = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            c(
              size = mean(x),
              prob = sum(x > mean(x)) / length(x)
            )
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndbinom(
                x = x,
                size = par[1],
                prob = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndbinom(
                x = x,
                size = par[1],
                prob = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndbinom(
                x = x,
                size = par[1],
                prob = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::prndbinom(
                q = q,
                size = par[1],
                prob = par[2]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qrndbinom(
                p = p,
                size = par[1],
                prob = par[2]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = c(-0.5, 0),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Binomial",
                quantile = c("mgalda", "qrndbinom"),
                probab = c(
                  "mgalda",
                  "prndbinom"
                ),
                density = c("mgalda", "drndbinom"),
                random = c("mgalda", "rrndbinom"),
                dist = "rndbinom",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                size = function(x) {
                  all(x >=
                        -0.5 & x <= Inf)
                },
                prob = function(x) {
                  all(x >= 0 &
                        x <= 1)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      rndgeom = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            start <- 1 / (1 + mean(x))
            names(start) <- "prob"
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndgeom(
                x = x,
                prob = par[1]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = 1,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndgeom(
                x = x,
                prob = par[1]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = 1,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndgeom(
                x = x,
                prob = par[1]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = 1,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::prndgeom(
                q = q,
                prob = par[1]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = 1,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qrndgeom(
                p = p,
                prob = par[1]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = 0.00001,
                upper = 1,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Geometric",
                quantile = c("mgalda", "qrndgeom"),
                probab = c(
                  "mgalda",
                  "prndgeom"
                ),
                density = c("mgalda", "drndgeom"),
                random = c("mgalda", "rrndgeom"),
                dist = "rndgeom",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                prob = function(x) {
                  all(x >=
                        0.00001 & x <= 1)
                }
              ),
              optim.method = "Nelder-Mead",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      rndnbinom = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            m <- mean(x)
            v <- var(x)
            size <- if (v > m) {
              m^2 / (v - m)
            } else {
              100
            }
            start <- c(size = size, mu = m)
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndnbinom(
                x = x,
                size = par[1],
                mu = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndnbinom(
                x = x,
                size = par[1],
                mu = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndnbinom(
                x = x,
                size = par[1],
                mu = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::prndnbinom(
                q = q,
                size = par[1],
                mu = par[2]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qrndnbinom(
                p = p,
                size = par[1],
                mu = par[2]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, Inf),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "NegativeBinomial",
                quantile = c("mgalda", "qrndnbinom"),
                probab = c(
                  "mgalda",
                  "prndnbinom"
                ),
                density = c("mgalda", "drndnbinom"),
                random = c("mgalda", "rrndnbinom"),
                dist = "rndnbinom",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                size = function(x) {
                  all(x >=
                        0 & x <= Inf)
                },
                mu = function(x) {
                  all(x >= -Inf & x <=
                        Inf)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      rndpois = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            assert_na_any(x, severity = "stop")
            start <- mean(x)
            names(start) <- "lambda"
            start
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndpois(
                x = x,
                lambda = par[1]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndpois(
                x = x,
                lambda = par[1]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = 0,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::drndpois(
                x = x,
                lambda = par[1]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = 0,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::prndpois(
                q = q,
                lambda = par[1]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = 0,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qrndpois(
                p = p,
                lambda = par[1]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = 0,
                upper = Inf,
                fn = fnobj,
                hessian = T,
                x = x,
                method = "Nelder-Mead"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "Poisson",
                quantile = c("mgalda", "qrndpois"),
                probab = c(
                  "mgalda",
                  "prndpois"
                ),
                density = c("mgalda", "drndpois"),
                random = c("mgalda", "rrndpois"),
                dist = "rndpois",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                lambda = function(x) {
                  all(x >=
                        0 & x <= Inf)
                }
              ),
              optim.method = "Nelder-Mead",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      },
      zipois = function(x) {
        fun_obj <- function(x) {
          dens_valdid <- (function(x) {
            ifelse(!is.finite(x), 0, x)
          })
          init <- (function(x, na.rm = FALSE) {
            c(lambda = 1, pstr0 = 0)
          })
          mle_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dzipois(
                x = x,
                lambda = par[1],
                pstr0 = par[2]
              ))
            })
            -sum(log(fn(par = par, x = x))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          cvmmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dzipois(
                x = x,
                lambda = par[1],
                pstr0 = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            1 / (12 * n) + sum((theop - (2 * 1:n - 1) / (2 *
                                                           n))^2) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          ksmge_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, x) {
              dens_valdid(mgalda::dzipois(
                x = x,
                lambda = par[1],
                pstr0 = par[2]
              ))
            })
            x <- unique(x)
            n <- length(x)
            s <- sort(x)
            theop <- fn(par, s)
            obspu <- seq(1, n) / n
            obspl <- seq(0, n - 1) / n
            max(pmax(abs(theop - obspu), abs(theop -
                                               obspl))) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          chi2_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, q) {
              mgalda::pzipois(
                q = q,
                lambda = par[1],
                pstr0 = par[2]
              )
            })
            freq <- as.vct(table(x))
            count <- as.num(names(table(x)))
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
            sum((freq - expected)^2) / sum(expected) }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          qmediscr_obj <- (function(x) {
            fnobj <- (function(par, x) {{ fn <- (function(par, p) {
              mgalda::qzipois(
                p = p,
                lambda = par[1],
                pstr0 = par[2]
              )
            })
            (fn(par, 1 / 2) - median(x))^2 }})
            do_try(
              optim(
                par = init(x),
                lower = c(0, -Inf),
                upper = c(Inf, 1),
                fn = fnobj,
                hessian = T,
                x = x,
                method = "L-BFGS-B"
              )
            )
          })
          if (length(x) > getOption("mgalda.n_samples", 1500)) {
            x <- sample(x, getOption(
              "mgalda.n_samples",
              1500
            ), F)
          }
          res <- do_try(mle_obj(x))
          if (is_empty(res)) {
            res <- do_try(cvmmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(ksmge_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(chi2_obj(x))
          }
          if (is_empty(res)) {
            res <- do_try(qmediscr_obj(x))
          }
          if (is_empty(res)) {
            cat_null("no result", type = "warn")
          }
          vc <- do_try(solve(res$hessian))
          sds <- do_try(sqrt(diag(vc)))
          list(
            estimate = res$par,
            convergence = res$convergence,
            hessian = res$hessian,
            sd = sds,
            vcov = vc
          )
        }
        res <- function(x) {
          structure(
            list(
              par = NULL,
              call_info = list(
                model = "ZeroInflatedPoisson",
                quantile = c("mgalda", "qzipois"),
                probab = c(
                  "mgalda",
                  "pzipois"
                ),
                density = c("mgalda", "dzipois"),
                random = c("mgalda", "rzipois"),
                dist = "zipois",
                support = all(x >= 0 & x <= Inf)
              ),
              bound = list(
                lambda = function(x) {
                  all(x >=
                        0 & x <= Inf)
                },
                pstr0 = function(x) {
                  all(x >= -Inf &
                        x <= 1)
                }
              ),
              optim.method = "L-BFGS-B",
              type = "discrete"
            ),
            class = "distrdiscr"
          )
        }
        out <- fun_obj(x)
        if (is.null(out)) {
          return(NULL)
        }
        res <- res(x)
        res$par <- out$estimate
        res[["convergence"]] <- out$convergence
        res[["hessian"]] <- out$hessian
        res[["sd"]] <- out$sd
        res[["vcov"]] <- out$vcov
        res[["n"]] <- length(x)
        res$call_info[["call"]] <- match.call()
        res
      }
    )
  ),
  row.names = c(NA, -46L),
  class = c(
    "tbl_df", "tbl",
    "data.frame"
  )
)

