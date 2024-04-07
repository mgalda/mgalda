#' @keywords internal
env_data <- new.env(parent = emptyenv())

env_data$begin_comment <- ".BeGiN_TiDy_IdEnTiFiEr_HaHaHa"
env_data$end_comment <- ".HaHaHa_EnD_TiDy_IdEnTiFiEr"
env_data$line_break <- NULL
env_data$pat_comment <-
  sprintf('invisible\\("\\%s|\\%s"\\)', env_data$begin_comment, env_data$end_comment)
env_data$mat_comment <- sprintf(
  'invisible\\("\\%s([^"]*)\\%s"\\)',
  env_data$begin_comment,
  env_data$end_comment
)
env_data$inline_comment <- ' %\b%[ ]*"([ ]*#[^"]*)"'
env_data$blank_comment <-
  sprintf('invisible("%s%s")', env_data$begin_comment, env_data$end_comment)
env_data$blank_comment2 <-
  paste0("^\\s*", gsub("\\(", "\\\\(", env_data$blank_comment), "\\s*$")

env_data$infix_ops <- "[>$]|T>|<>"

env_data$fonts <- "Helvetica"

#' @export
as_operator <- function(x, ...) {
  UseMethod("as_operator", x)
}
#' @export
as_operator.function <- function(x, ...) {
  if (is_operator(x)) {
    return(x)
  } else {
    stop(x, " cannot be coerced to an operator.")
  }
}
#' @export
as_operator.character <- function(x, ...) {
  if (x %in% operators(...)) {
    eval(as.name(x))
  } else {
    stop(x, " cannot be coerced to an operator.")
  }
}
#' @export
as_operator.name <- function(x, ...) {
  if (deparse(x) %in% operators(...)) {
    eval(x)
  } else {
    stop(x, " cannot be coerced to an operator.")
  }
}

#' @export
can_operator <- function(x, ...) {
  UseMethod("can_operator", x)
}
#' @export
can_operator.default <- function(x, ...) FALSE
#' @export
can_operator.name <- function(x, ...) {
  deparse(x) %in% operators(...)
}
#' @export
can_operator.function <- function(x, ...) {
  deparse(x) %in%
    lapply(operators(...), function(op) deparse(eval(as.symbol(op))))
}
#' @export
can_operator.character <- function(x, ...) {
  x %in% operators(...)
}

#' @export
fun2name <- function(f) {
  nms <- apropos(",*", mode = "function")

  for (nm in nms) {
    if (identical(f, eval(as.name(nm)))) {
      return(nm)
    }
  }

  return(NULL)
}

#' @export
name2fun <- function(x) eval(as.name(x))

.init_ops <- function() {
  set_operator("::", type = "namespace")
  set_operator(":::", type = "namespace")

  set_operator("@", type = "component")
  set_operator("$", type = "component")

  set_operator("[", type = "indexing")
  set_operator("[[", type = "indexing")

  set_operator(":", type = "sequence")

  set_operator("+", type = "arithmetic", inverse = "-")
  set_operator("-", type = "arithmetic", inverse = "+")
  set_operator("*", type = "arithmetic", inverse = "/")
  set_operator("/", type = "arithmetic", inverse = "*")
  set_operator("^", type = "arithmetic") # inverse = as.name('log')
  set_operator("%%", type = "arithmetic")
  set_operator("%/%", type = "arithmetic")


  set_operator("<", type = "relational", inverse = ">=", rel_type = "lt")
  set_operator("<=", type = "relational", inverse = ">", rel_type = "lt")
  set_operator(">", type = "relational", inverse = "<=", rel_type = "gt")
  set_operator(">=", type = "relational", inverse = "<", rel_type = "gt")
  set_operator("==", type = "relational", inverse = "!=", rel_type = "eq")
  set_operator("!=", type = "relational", inverse = "==", rel_type = "ne")
  set_operator("%in%", type = "relational", rel_type = "eq") # %!in%
  set_operator("%in%", type = "relational", inverse = "%!in%", rel_type = "eq")
  set_operator("%!in%", type = "relational", inverse = "%in%", rel_type = "ne")


  set_operator("!", type = "logical") # inverse = identity, !?
  set_operator("&", type = "logical")
  set_operator("&&", type = "logical")
  set_operator("|", type = "logical")
  set_operator("||", type = "logical")

  set_operator("~", type = "tilde")

  set_operator("<-", type = "assignment") # inverse rm
  set_operator("<<-", type = "assignment") #
  set_operator("=", type = "assignment")

  set_operator("?", type = "help")

  set_operator("%*%", type = "matrix")
  set_operator("%x%", type = "matrix")
  set_operator("%o%", type = "matrix")

  if ("package:magrittr" %in% search()) {
    set_operator("%>%", type = "pipe")
    set_operator("%<>%", type = "pipe")
    set_operator("%T>%", type = "pipe")
  }

  if ("package:pipeR" %in% search()) {
    set_operator("%>>%", type = "pipe")
  }

  if ("package:backpipe" %in% search()) {
    set_operator("%<%", type = "pipe")
  }
}

#' @export
inverse_operator <- function(x, ...) {
  UseMethod("inverse_operator")
}
#' @export
inverse_operator.name <- function(x, ...) {
  op <- as.character(x)
  inverses <- sapply(env_data$opts_list, function(x) x$inverse)

  ret <- inverses[[op]]
  if (!is.null(ret)) {
    return(as.name(ret))
  }

  warning("No inverse found for op: ", op, call. = FALSE)

  return(ret)
}
#' @export
inverse_operator.function <- function(x, ...) {
  inverses <- sapply(env_data$opts_list, function(x) x$inverse)

  return(name2fun(as.name(inverses[[fun2name(x)]])))
  warning("No operator matched")
  return(NULL)
}

#' @export
is_operator <- function(x, ...) {
  UseMethod("is_operator", x)
}
#' @export
is_operator.default <- function(x, ...) FALSE
#' @export
is_operator.name <- function(x, ...) {
  x <- as.character(x)

  if (length(list(...)) > 0) {
    x %in% operators(...)
  } else {
    x %in% operators("REG") || x %in% operators("UNREG")
  }
}
#' @export
is_operator.function <- function(x, ...) {
  if (length(list(...)) > 0) {
    any(sapply(
      operators(...),
      function(op) identical(x, name2fun(op))
    ))
  } else {
    any(sapply(
      operators("REG"),
      function(op) identical(x, name2fun(op))
    )) ||
      any(sapply(
        operators("UNREG"),
        function(op) identical(x, name2fun(op))
      ))
  }
}

#' @export
operator_type <- function(op) {
  UseMethod("operator_type", op)
}
#' @export
operator_type.name <- function(op) {
  op <- as.character(deparse(op))

  if (op %in% operators(types = "REG")) {
    return(env_data$opts_list[[op]]$type)
  }

  if (op %in% operators(types = "UNREG")) {
    return("UNREGISTERED")
  }

  return(NULL)
}
#' @export
operator_type.function <- function(op) {
  li.fun <- sapply(operators(types = "REG"), function(x) eval(as.name(x)))

  for (nm in names(li.fun)) {
    if (identical(op, li.fun[[nm]])) {
      return(operator_type(as.name(nm)))
    }
  }


  li.fun <- sapply(operators(types = "UNREG"), function(x) eval(as.name(x)))

  for (nm in names(li.fun)) {
    if (identical(op, li.fun[[nm]])) {
      return("UNREGISTERED")
    }
  }

  return(NULL)
}
#' @export
operators <- function(types = "REGISTERED") {
  core <- names(env_data$opts_list)

  if (is.null(types)) {
    return(unique(c(core, apropos("^%.*%"))))
  }

  if (length(types) == 1 && types %in% c("REG", "REGISTERED")) {
    return(core)
  }

  op.types <- unique(sapply(env_data$opts_list, function(x) x$type))
  ret <- NULL
  for (type in types) {
    if (type == "ALL") {
      ret <- c(ret, core, apropos("^%.*%"))
    } else if (type == "SPECIAL") {
      ret <- c(ret, apropos("^%.*%"))
    } else if (type %in% c("UNREG", "UNREGISTERED")) {
      special <- apropos("^%.*%")
      ret <- c(ret, special[!special %in% core])
    } else if (type %in% op.types) {
      ret <- c(ret, core[sapply(env_data$opts_list, function(x, y) x$type == y, type)])
    } else {
      stop("operator type: ", type, " is unknown")
    }
  }

  return(unique(ret))
}

#' @export
rel_type <- function(x) {
  UseMethod("rel_type", x)
}
#' @export
rel_type.name <- function(x) {
  if (is_operator(x, type = "relational")) {
    env_data$opts_list[[as.character(x)]][["rel_type"]]
  } else {
    NULL
  }
}
#' @export
rel_type.function <- function(x) {
  if (is_operator(x, type = "relational")) {
    env_data$opts_list[[fun2name(x)]][["rel_type"]]
  } else {
    NULL
  }
}
#' @export
rel_type.call <- function(x) {
  if (is.name(x[[1]])) {
    rel_type(as.name(x[[1]]))
  } else {
    NULL
  }
}
#' @export
rel_type.expression <- function(x) {
  sapply(x, rel_type)
}

#' @export
remove_operator <- function(x) {
  x <- as.character(x)
  ops <- env_data$opts_list

  if (x %in% names(ops)) {
    ops[[x]] <- NULL
    options(operators = ops)
  } else {
    warning(x, " is not a REGISTERED operator.  See ?set_operator.")
  }
}
#' @export
set_operator <- function(name, type = "user", ...) {
  if (
    type %in%
    c("ALL", "SPECIAL", "REGISTERED", "REG", "UNREGISTERED", "UNREG")
  ) {
    stop(type, "  is a reserved operator type.")
  }

  opts <- if (!is.null(env_data$opts_list)) env_data$opts_list else list()


  opts[[name]] <- list(name = name, type = type)

  li <- list(...)
  for (nm in names(li)) opts[[name]][[nm]] <- li[[nm]]

  assign(x = "opts_list", value = opts, envir = env_data)
}
#' @export
set_operators <- function(...) {
  ops <- apropos("%.*%")

  for (op in ops) {
    if (!op %in% names(env_data$opts_list)) {
      set_operator(name = op, ...)
    }
  }
}

env_data$crossmetrics <-
  list(
    opsexprs = list(
      mean = quote(mean(x, na.rm = TRUE)),
      median = quote(median(x, na.rm = TRUE)),
      sd = quote(sd(x, na.rm = TRUE)),
      var = quote(var(x, na.rm = TRUE)),
      IQR = quote(IQR(x, na.rm = TRUE)),
      qsr = quote(qsr(x, na.rm = TRUE)),
      amplitude = quote(amplitude(x,na.rm = TRUE)),
      entropy = quote(entropy(x, na.rm = TRUE)),
      skewness = quote(skewness(x, na.rm = TRUE)),
      kurtosis = quote(kurtosis(x, na.rm = TRUE)),
      min = quote(min(x, na.rm = TRUE)),
      max = quote(max(x, na.rm = TRUE))
    ),
    ops_chr = c("addition" = "+", "subtraction" = "-", "division" = "/", "multiplication" = "*")
  )

env_data$init_list <-
  list(
    beta = function(x) {
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
    betanorm = function(x) {
      c(shape1 = 1,shape2 = 1,
        mean = 0,
        sd = 1
      )
    },
    betaprime = function(x) {
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
    burr = function(x) {
      c(
        shape1 = 2,
        shape2 = 1,
        scale = 0.5
      )
    },
    cauchy = function(x) {
      assert_na_any(x, severity = "stop")
      start <- c(location = median(x), scale = IQR(x) / 2)

      start
    },
    chisq = function(x) {
      assert_engine(!any(x <= 0),
                    env = parent.frame(),
                    msg = "values must be > 0",
                    severity = "stop"
      )
      assert_na_any(x, severity = "stop")
      c(df = (mean(x) + sd(x)) / 2, ncp = 0)
    },
    dcauchy = function(x) {
      assert_na_any(x, severity = "stop")
      start <- c(location = median(x), scale = IQR(x) / 2)

      start
    },
    dexp = function(x) {
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
    dged = function(x) {
      c(
        mean = 0, sd = 1,
        nu = 2
      )
    },
    dlnorm = function(x) {
      assert_na_any(x, severity = "stop")
      lx <- log(x)
      n <- length(x)
      sd0 <- sqrt((n - 1) / n) * sd(lx)
      mx <- mean(lx)
      start <- c(mx, sd0)
      names(start) <- c("meanlog", "sdlog")
      start
    },
    dnorm = function(x) {
      assert_na_any(x, severity = "stop")
      n <- length(x)
      sd0 <- sqrt((n - 1) / n) * sd(x)
      mx <- mean(x)
      start <- c(mx, sd0)
      names(start) <- c("mean", "sd")
      start
    },
    dunif = function(x) {
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
    exp = function(x) {
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
    expgeom = function(x) {
      c(scale = 1,shape = 1)
    },
    explog = function(x) {
      c(scale = 1,shape = 1)
    },
    f = function(x) {
      c(df1 = 1,df2 = 1,ncp = 0)
    },
    gamma = function(x) {
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
    ged = function(x) {
      c(
        mean = 0, sd = 1,
        nu = 2
      )
    },
    gev = function(x) {
      c(
        location = 0,
        scale = 1,
        shape = 0
      )
    },
    gpd = function(x) {
      c(
        location = 0,
        scale = 1,
        shape = 0
      )
    },
    gumbel = function(x) {
      c(location = 0, scale = 1)
    },
    halfnorm = function(x) {
      c(mean = 0,theta = sqrt(pi / 2))
    },
    huber = function(x) {
      c(k = 0.862, mu = 0, sigma = 1)
    },
    invchisq = function(x) {
      c(df = 1)
    },
    laplace = function(x) {
      c(mu = 0, sigma = 1)
    },
    llogis = function(x) {
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
    lnorm = function(x) {
      assert_na_any(x, severity = "stop")
      lx <- log(x)
      n <- length(x)
      sd0 <- sqrt((n - 1) / n) * sd(lx)
      mx <- mean(lx)
      start <- c(mx, sd0)
      names(start) <- c("meanlog", "sdlog")
      start
    },
    logis = function(x) {
      assert_na_any(x, severity = "stop")
      start <- c(location = median(x), scale = IQR(x) / 2)

      start
    },
    loglap = function(x) {
      c(
        location_ald = 0,
        scale_ald = 1,
        tau = 0.5
      )
    },
    norm = function(x) {
      assert_na_any(x, severity = "stop")
      n <- length(x)
      sd0 <- sqrt((n - 1) / n) * sd(x)
      mx <- mean(x)
      start <- c(mx, sd0)
      names(start) <- c("mean", "sd")
      start
    },
    pareto = function(x) {
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
    posnorm = function(x) {
      c(mean = 0, sd = 1)
    },
    power = function(x) {
      c(alpha = 1,beta = 1)
    },
    rayleigh = function(x) {
      c(scale = 1)
    },
    rndbinom = function(x) {
      assert_na_any(x, severity = "stop")
      c(size = mean(x),prob = sum(x>mean(x))/length(x))
    },
    rndgeom = function(x) {
      assert_na_any(x, severity = "stop")
      start <- 1 / (1 + mean(x))
      names(start) <- "prob"
      start
    },
    rndnbinom = function(x) {
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
    rndpois = function(x) {
      assert_na_any(x, severity = "stop")
      start <- mean(x)
      names(start) <- "lambda"
      start
    },
    sc_t2 = function(x) {
      c(location = 0, scale = 1)
    },
    sged = function(x) {
      c(
        mean = 0,
        sd = 1,
        nu = 2,
        xi = 1.5
      )
    },
    snorm = function(x) {
      c(mean = 0, sd = 1, xi = 1.5)
    },
    sstd = function(x) {
      c(
        mean = 0,
        sd = 1,
        nu = 5,
        xi = 1.5
      )
    },
    std = function(x) {
      c(
        mean = 0,
        sd = 1, nu = 5
      )
    },
    unif = function(x) {
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
    weibull = function(x) {
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
    zipois = function(x) {
      c(lambda = 1,pstr0 = 0)
    }
  )


env_data$dist_args_list <-
  list(
    beta = c("shape1", "shape2", "ncp"),
    betanorm = c("shape1", "shape2", "mean", "sd"),
    betaprime = c("alpha", "beta"),
    burr = c("shape1", "shape2", "scale"),
    cauchy = c("location", "scale"),
    chisq = c("df", "ncp"),
    dcauchy = c("location", "scale"),
    dexp = "rate",
    dged = c("mean", "sd", "nu"),
    dlnorm = c("meanlog", "sdlog"),
    dnorm = c("mean", "sd"),
    dunif = c("min", "max"),
    exp = "rate",
    expgeom = c("scale","shape"),
    explog = c("scale","shape"),
    f = c("df1", "df2", "ncp"),
    gamma = c("shape", "rate"),
    ged = c("mean", "sd", "nu"),
    gev = c("location", "scale", "shape"),
    gpd = c("location", "scale", "shape"),
    gumbel = c("location", "scale"),
    halfnorm = c("mean", "theta"),
    huber = c("k", "mu", "sigma"),
    invchisq = "df",
    laplace = c("mu", "sigma"),
    llogis = c("shape", "scale"),
    lnorm = c("meanlog", "sdlog"),
    logis = c("location", "scale"),
    loglap = c("location_ald", "scale_ald", "tau"),
    norm = c("mean", "sd"),
    pareto = c("shape", "scale"),
    posnorm = c("mean", "sd"),
    power = c("alpha", "beta"),
    rayleigh = "scale",
    rndbinom = c("size", "prob"),
    rndgeom = "prob",
    rndnbinom = c("size", "mu"),
    rndpois = "lambda",
    sc_t2 = c("location", "scale"),
    sged = c("mean", "sd", "nu", "xi"),
    snorm = c("mean", "sd", "xi"),
    sstd = c("mean", "sd", "nu", "xi"),
    std = c("mean", "sd", "nu"),
    unif = c("min", "max"),
    weibull = c("shape", "scale"),
    zipois = c("lambda", "pstr0")
  )

env_data$hflibs <- FALSE
env_data[["opts_list"]] <- list()

.init_ops()

