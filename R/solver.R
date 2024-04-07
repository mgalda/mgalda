#' optimization_wflow
#'
#'
#' @name optim_wflows
#' @rdname optim_wflows
#' @examples
#' #' # complete pipe
#'
#'
#' optimization_wflow(maximum = TRUE) %>%
#'   add_optim_objective_linear(linear = c(7, 1, 3)) %>%
#'   add_optim_constraint(opconst_linear(
#'     linear = c(6, 4, 5),
#'     directions = "<=",
#'     right_side = 60
#'   )) %>%
#'   add_optim_constraint(opconst_linear(
#'     linear = c(8, 0, 2),
#'     directions = "<=",
#'     right_side = 80
#'   )) %>%
#'   add_optim_constraint(opconst_linear(
#'     linear = c(9, 1, 7),
#'     directions = "<=",
#'     right_side = 70
#'   )) %>%
#'   set_optim_variable_type(var_type = c("I")) %>%
#'   finalize_optimization_wflow() %>%
#'   solve_optim()
#' @export

optimization_wflow <- function(maximum = FALSE, inf = 1e20) {
  ROI_pkgs()
  x <- vector("list", 8)
  names(x) <-
    c(
      "objective",
      "constraints",
      "bounds",
      "types",
      "maximum",
      "n",
      "add_steps",
      "inf"
    )
  x[["maximum"]] <- maximum
  x[["add_steps"]] <-
    list(
      objective = FALSE,
      constraint = FALSE,
      bound = FALSE,
      type = FALSE
    )
  x[["inf"]] <- inf
  class(x) <- "optimization_wflow"
  x
}

#' @export
print.optimization_wflow <- function(x, ...) {
  writeLines("Optimization Problem:\n")
  ## objective
  len <- length(x$objective)
  op_type <- switch(class(x$objective)[2],
    "L_objective" = "linear",
    "Q_objective" = "quadratic",
    "F_objective" = "nonlinear",
    "abstract"
  )
  txt <- sprintf(
    "%s a %s objective function of length %d with",
    ifelse(x$maximum, "Maximize", "Minimize"),
    op_type,
    len
  )
  writeLines(txt)

  available_types <- function() {
    c("C", "I", "B")
  }

  plural_s <- function(condition) {
    if (condition) {
      "s"
    } else {
      ""
    }
  }

  if (any(x$types %in% available_types()[-1L])) {
    tab <- table(x$types)
    nam <-
      setNames(c("continuous", "integer", "binary"), c("C", "I", "B"))
    for (ty in available_types()) {
      if (is_true(ty %in% names(tab))) {
        txt <- sprintf(
          "- %d %s objective variable%s,",
          tab[ty],
          nam[ty],
          plural_s(tab[ty] != 1L)
        )
        writeLines(txt)
      }
    }
  } else {
    txt <- sprintf(
      "- %d continuous objective variable%s,",
      len,
      plural_s(len != 1L)
    )
    writeLines(txt)
  }
  writeLines("\nsubject to")
  ## constraints
  if (length(x$constraints) == 0) {
    writeLines("- 0 constraints")
  } else {
    types <- c(
      L_constraint = "linear",
      Q_constraint = "quadratic",
      C_constraint = "conic",
      F_constraint = "nonlinear"
    )
    len <- length(x$constraints)

    txt <- sprintf(
      "- %d constraint%s of type %s.",
      len,
      plural_s(len != 1L),
      paste(na.omit(types[class(x$constraints)])[1],
        collapse = ", "
      )
    )
    writeLines(txt)
    if (inherits(x$constraints, "C_constraint")) {
      available_cone_types <- function() {
        c(
          "zero",
          "nonneg",
          "soc",
          "psd",
          "expp",
          "expd",
          "powp",
          "powd"
        )
      }

      cones <-
        table(available_cone_types()[x$constraints$cones$cone])
      for (i in seq_along(cones)) {
        txt <- sprintf(
          "  |- %i conic constraint%s of type '%s'",
          cones[i],
          plural_s(cones[i] != 1),
          names(cones)[i]
        )
        writeLines(txt)
      }
    }
  }
  if (!(is.null(x$bounds$lower) & is.null(x$bounds$upper))) {
    len.lo <- length(x$bounds$lower$ind)
    len.up <- length(x$bounds$upper$ind)
    writeLines(
      sprintf(
        "- %d lower and %d upper non-standard variable bound%s.",
        len.lo,
        len.up,
        plural_s(len.up != 1)
      )
    )
  }
}

#' @rdname optim_wflows
#' @export
add_optim_objective_linear <- function(x, linear = NULL) {
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }
  n <- length(linear)

  .obj <-
    ROI::L_objective(L = linear, names = paste0("x", seq_along(linear)))
  x$objective <- .obj
  x$n <- n
  x$add_steps$objective <- TRUE
  x$bounds <-
    ROI::V_bound(
      li = seq_len(n),
      ui = seq_len(n),
      lb = rep(x$inf * -1, n),
      ub = rep(x$inf, n)
    )
  x
}

#' @rdname optim_wflows
#' @export
add_optim_objective_fn <-
  function(x,
           fn,
           n_var,
           gradient = NULL,
           hessian = NULL) {
    if (!check_inherits(x, "optimization_wflow")) {
      cat_stop("'x' debe ser clase 'optimization_wflow'")
    }

    .obj <-
      ROI::F_objective(
        fn,
        n = n_var,
        G = gradient,
        H = hessian,
        names = paste0("x", seq_len(n_var))
      )
    x$objective <- .obj
    x$n <- n
    x$add_steps$objective <- TRUE
    x$bounds <-
      ROI::V_bound(
        li = seq_len(n),
        ui = seq_len(n),
        lb = rep(x$inf * -1, n),
        ub = rep(x$inf, n)
      )
    x
  }

#' @rdname optim_wflows
#' @export
add_optim_objective <- function(x, objv) {
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }
  if (!check_inherits(objv, c(
    "L_objective",
    "Q_objective",
    "F_objective"
  ))) {
    cat_stop("'objv' debe ser clase objective (namespace:ROI)")
  }
  n <- length(objv)
  x$objective <- objv
  x$n <- n
  x$add_steps$objective <- TRUE
  x$bounds <-
    ROI::V_bound(
      li = seq_len(n),
      ui = seq_len(n),
      lb = rep(x$inf * -1, n),
      ub = rep(x$inf, n)
    )
  x
}

#' @rdname optim_wflows
#' @export
add_optim_constraint <- function(x, ...) {
  cnstr <- list(...)
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }
  if (!x$add_steps$objective) {
    cat_stop("el primer paso debe ser definir el objetivo")
    return(NULL)
  }

  inhe_cnstr <- vapply(
    cnstr, function(x) {
      inherits(
        x,
        c(
          "L_constraint",
          "C_constraint",
          "Q_constraint",
          "F_constraint"
        )
      )
    },
    logical(1)
  )
  cnstr <- cnstr[inhe_cnstr]
  if (is_empty(x$constraints)) {
    if (length(cnstr[[1]]) == 1) {
      x$constraints <- cnstr[[1]]
    } else {
      x$constraints <- do.call(rbind, cnstr)
    }
  } else {
    if (length(cnstr[[1]]) == 1) {
      cnstr <- cnstr[[1]]
    } else {
      cnstr <- do.call(rbind, cnstr)
    }
    x$constraints <- rbind(cnstr, x$constraints, use.names = TRUE)
  }
  x$add_steps$constraint <- TRUE
  x
}

#' @rdname optim_wflows
#' @export
add_optim_lowerbound <- function(x, lb) {
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }
  if (!x$add_steps$objective) {
    cat_stop("el primer paso debe ser definir el objetivo")
    return(NULL)
  }
  if (length(lb) != length(x$objective)) {
    if (length(lb) == 1) {
      lb <- rep(lb, length(x$objective))
    } else {
      cat_stop("lb debe ser de largo 1 o { length(x$objective) }")
      return(NULL)
    }
  }

  if (!all(x$bounds$upper$val > lb)) {
    cat_stop("todos los lb deben ser menores que ub")
    return(NULL)
  }

  x$bounds$lower$val <- lb

  x$add_steps$bound <- TRUE
  x
}

#' @rdname optim_wflows
#' @export
add_optim_upperbound <- function(x, ub) {
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }
  if (!x$add_steps$objective) {
    cat_stop("el primer paso debe ser definir el objetivo")
    return(NULL)
  }

  if (length(ub) != length(x$objective)) {
    if (length(ub) == 1) {
      ub <- rep(ub, length(x$objective))
    } else {
      cat_stop("ub debe ser de largo 1 o { length(x$objective) }")
      return(NULL)
    }
  }

  if (!all(x$bounds$lower$val < ub)) {
    cat_stop("todos los ub deben ser mayores que lb")
    return(NULL)
  }

  x$bounds$upper$val <- ub

  x$add_steps$bound <- TRUE
  x
}

#' @rdname optim_wflows
#' @export
update_optimization_direction <- function(x, maximum) {
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }
  x$maximum <- maximum
  x
}

#' @rdname optim_wflows
#' @export
set_optim_variable_type <- function(x, var_type) {
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }
  if (!x$add_steps$objective) {
    cat_stop("el primer paso debe ser definir el objetivo")
    return(NULL)
  }

  if (length(var_type) != length(x$objective)) {
    if (length(var_type) == 1) {
      var_type <- rep(var_type, length(x$objective))
    } else {
      cat_stop("types debe ser de largo 1 o { length(x$objective) }")
      return(NULL)
    }
  }

  check_types <-
    vapply(var_type, function(y) {
      y %in% c("C", "I", "B")
    }, logical(1))
  if (any(!check_types)) {
    cat_stop("types debe ser una de las siguientes opciones c('C', 'I', 'B')")
    return(NULL)
  }
  x$add_steps$type <- TRUE
  x$types <- var_type
  x
}

#' @rdname optim_wflows
#' @export
finalize_optimization_wflow <- function(x) {
  if (!x$add_steps$objective) {
    cat_stop("No se ha agredado ningun objetivo")
    return(NULL)
  } else {
    cat_message("objective - OK")
  }


  if (!x$add_steps$bound) {
    x$bounds <- NULL
  } else {
    cat_message("bounds - OK")
  }

  if (!x$add_steps$constraint) {
    x$constraint <- NULL
  } else {
    cat_message("constraint - OK")
  }

  if (!x$add_steps$type) {
    x$type <- NULL
  } else {
    cat_message("type - OK")
  }
  cat("\n\n")


  x$n <- NULL
  x$inf <- NULL
  x <- compact(x)
  x
  OP(
    objective = x$objective,
    constraints = x$constraints,
    types = x$types,
    bounds = x$bounds,
    maximum = x$maximum
  )
}

#' @rdname optim_wflows
#' @export
solve_optim <- function(x, solver = "auto", control = list(), ..., accuracy = .001) {
  sol <- ROI::ROI_solve(x, solver = solver, control = control, ...)
  list(solution = round_any(sol$solution, accuracy = accuracy), objval = sol$objval)
}


#' optim_constraint
#'
#' @name optim_wflows
#' @rdname optim_wflows
#' @export
opconst_linear <- function(linear, directions, right_side) {
  ROI::L_constraint(
    L = linear,
    dir = directions,
    rhs = right_side,
    names = paste0("x", seq_along(linear))
  )
}

#' @rdname optim_wflows
#' @export
opconst_fn <- function(fn, directions, right_side) {
  ROI::F_constraint(F = fn, dir = directions, rhs = right_side)
}


#' optim_objective
#'
#' @name optim_wflows
#' @rdname optim_wflows
#' @export
opobj_linear <- function(linear) {
  # n <- length(linear)

  ROI::L_objective(L = linear, names = paste0("x", seq_along(linear)))
}

#' @name optim_wflows
#' @rdname optim_wflows
#' @export
opobj_fn <- function(fn,
                     n_var,
                     grad = NULL,
                     hess = NULL) {
  ROI::F_objective(
    fn,
    n = n_var,
    G = grad,
    H = hess,
    names = paste0("x", seq_len(n_var))
  )
}



#'  optim default models
#'
#' @name optim_wflows
#' @rdname optim_wflows
#' @examples
#' # mlogit
#'
#' data("Heating", package = "mlogit")
#' H <- dfidx::dfidx(Heating, choice = "depvar", varying = c(3:12))
#' coef(mlogit::mlogit(
#'   depvar ~ 0 | rooms + region | 0,
#'   data = H,
#'   reflevel = "gc"
#' ))
#'
#' mod <- optimization_wflow() %>%
#'   use_opmods(opmods = "mlogit")
#'
#' mod(data = Heating, var = depvar, mm_formula = ~ rooms + region) %>% solve_opmods()
#'
#'
#' # logisticregression
#'
#' data("plasma", package = "HSAUR")
#' m <-
#'   glm(ESR ~ fibrinogen,
#'     data = plasma,
#'     family = binomial(link = "logit")
#'   )
#' data <- plasma
#' data$ESR <- as_int(data$ESR) - 1L
#' data$globulin <- NULL
#'
#'
#'
#' mod_log <- optimization_wflow() %>%
#'   use_opmods(opmods = "logisticregression")
#' mod_log(data, ESR) %>% solve_opmods()
#'
#' mod_mle <- optimization_wflow() %>%
#'   use_opmods(opmods = "mle")
#' mod_mle(data, ESR) %>% solve_opmods()
#'
#'
#' coef(m)
#'
#'
#' # ordinaryleastsquares
#'
#'
#'
#' mod <- optimization_wflow() %>%
#'   use_opmods(opmods = "ordinaryleastsquares")
#'
#'
#' lm(mpg ~ disp, data = mtcars)
#' mod(mtcars[, c("mpg", "disp")], mpg, intercept = T) %>% solve_opmods()
#'
#'
#'
#' # leastabsolutedeviation --
#'
#' mod <- optimization_wflow() %>%
#'   use_opmods(opmods = "leastabsolutedeviation")
#'
#' mod(data = mtcars, var = mpg) %>% solve_opmods()
#'
#'
#' # lasso
#'
#'
#' mod_qplasso <- optimization_wflow() %>%
#'   use_opmods(opmods = "qplasso")
#'
#' mod_cplasso <- optimization_wflow() %>%
#'   use_opmods(opmods = "cplasso")
#'
#' data(QuickStartExample, package = "glmnet")
#' q_exam <- cbind(data.frame(y = QuickStartExample$y), x = QuickStartExample$x)
#' rm(QuickStartExample)
#'
#' mo <- glmnet::glmnet(
#'   y = q_exam[["y"]],
#'   x = q_exam[, -1],
#'   family = "gaussian",
#'   alpha = 1,
#'   lambda = 1,
#'   intercept = FALSE,
#'   standardize = FALSE
#' )
#' glmnet_beta <- setNames(as_vct(coef(mo)), rownames(coef(mo)))
#'
#' cp0 <- mod_cplasso(data = q_exam, var = y, lambda = 0) %>% solve_opmods()
#' cp1 <- mod_cplasso(data = q_exam, var = y, lambda = nrow(q_exam)) %>% solve_opmods()
#'
#' qp0 <- mod_qplasso(data = q_exam, var = y, lambda = 0) %>% solve_opmods()
#' qp1 <- mod_qplasso(data = q_exam, var = y, lambda = nrow(q_exam)) %>% solve_opmods()
#'
#' cbind(
#'   lm = coef(lm(y ~ ., data = q_exam))[-1], qp = qp0$solution,
#'   cp = cp0$solution
#' )
#'
#' cbind(
#'   lm = round(glmnet_beta, 4), qp = round(c(0, qp1$solution), 4),
#'   cp = round(c(0, cp1$solution), 4)
#' )
#'
#'
#' optimization_wflow() %>%
#'   use_objecmod(objt = "minimax", r_mat = tidyr::drop_na(stocks_data$delta)) %>%
#'   use_constrmod(constr = "budget") %>%
#'   update_optimization_direction(maximum = TRUE) %>%
#'   finalize_optimization_wflow() %>%
#'   solve_optim()
#'
#' data <- stocks_data$delta %>% tidyr::drop_na()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "cvarex2")
#' mod(data, rhs_cardinal = 6, rhs_budget = 1, alpha = 0.99) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "downmadex1")
#' mod(data, rhs_budget = 1) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "downvarex1")
#' mod(data, rhs_budget = 1) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "madex1")
#' mod(data, rhs_budget = 1) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "markex1")
#' mod(data, rhs_budget = 1) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "markex2")
#' mod(data, rhs_budget = 1) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "minimaxex1")
#' mod(data, rhs_budget = 1) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "omegaex1")
#' mod(data, tau = 0) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "quaduex1")
#' mod(data, lambda = 2, rhs_budget = 1) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "rewardex1")
#' mod(data, index = c(3, 17), rhs_group = .5, rhs_budget = 1) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "sharpeex1")
#' mod(data, rf = 0) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "cvarex2ci")
#' mod(data, alpha = c(0.95, 0.99), rhs_budget = 1) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "rewardex2")
#' mod(
#'   data,
#'   index = c(2, 10, 20),
#'   rhs_budget = 1,
#'   rhs_group = 0.5,
#'   rhs_turnover = 0.5,
#'   rhs_cvar = 0.02,
#'   alpha = 0.95
#' ) %>% solve_opmods()
#'
#' mod <- optimization_wflow() %>% use_opmods(opmods = "rewardex3")
#' mod(data, rhs_markowitz = 0.5^2, rhs_budget = 1, dir_markowitz = "<=", times = 10) %>%
#'   solve_opmods()
#' @export
use_opmods <- function(x, opmods) {
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }

  switch(opmods,
    leastabsolutedeviation = opmods_leastabsolutedeviation,
    cplasso = opmods_cplasso,
    logisticregression = opmods_logisticregression,
    mle = opmods_mle,
    mlogit = opmods_mlogit,
    ordinaryleastsquares = opmods_ordinaryleastsquares,
    poisson = opmods_poisson,
    qplasso = opmods_qplasso,
    rewardex1 = opmods_rewardex1,
    markex1 = opmods_markex1,
    markex2 = opmods_markex2,
    madex1 = opmods_madex1,
    downvarex1 = opmods_downvarex1,
    downmadex1 = opmods_downmadex1,
    cvarex2ci = opmods_cvarex2ci,
    minimaxex1 = opmods_minimaxex1,
    quaduex1 = opmods_quaduex1,
    sharpeex1 = opmods_sharpeex1,
    omegaex1 = opmods_omegaex1,
    rewardex2 = opmods_rewardex2,
    rewardex3 = opmods_rewardex3,
    cvarex2 = opmods_cvarex2
  )
}

#' @rdname optim_wflows
#' @export
solve_opmods <- function(x, accuracy = .001) {
  if (!check_inherits(x, "opmods")) {
    cat_stop("'x' debe ser clase 'opmods'")
  }

  if (is_empty(x$custom_sol)) {
    if (!is_empty(x$solver)) {
      solver <- x$solver
    } else {
      solver <- "auto"
    }
    if (!is_empty(x$control)) {
      control <- x$control
    } else {
      control <- list()
    }
    sol <- ROI::ROI_solve(x$op, solver = solver, control = control)

    if (sol$status$msg$symbol == "FAILURE") {
      return(NULL)
    }

    if (!is_empty(x$n_coef)) {
      sol$solution <- sol$solution[1:x$n_coef]
    }

    if (!is_empty(x$res_name)) {
      sol$solution <- sol$solution[seq_along(x$res_name)]
      names(sol$solution) <- x$res_name
    }
    sol$solution <- round_any(x = sol$solution, accuracy = accuracy)
    out <- list(
      solution = sol$solution,
      objval = sol$objval
    )
  } else {
    f <- x$custom_sol$fn
    out <- f(x)
  }
  out$solution <- if (is.list(out$solution)) {
    lapply(out$solution, round_any, accuracy = accuracy)
  } else {
    round_any(out$solution, accuracy = accuracy)
  }

  out
}

#' @rdname optim_wflows
#' @export
modelos_opmods <- function() {
  structure(
    list(
      args = c(
        "cplasso",
        "logisticregression",
        "mle",
        "mlogit",
        "ordinaryleastsquares",
        "poisson",
        "qplasso",
        "rewardex1",
        "markex1",
        "markex2",
        "madex1",
        "downvarex1",
        "downmadex1",
        "cvarex2ci",
        "minimaxex1",
        "quaduex1",
        "sharpeex1",
        "omegaex1",
        "rewardex2",
        "rewardex3",
        "cvarex2"
      ),
      fn = c(
        "opmods_cplasso",
        "opmods_logisticregression",
        "opmods_mle",
        "opmods_mlogit",
        "opmods_ordinaryleastsquares",
        "opmods_poisson",
        "opmods_qplasso",
        "opmods_rewardex1",
        "opmods_markex1",
        "opmods_markex2",
        "opmods_madex1",
        "opmods_downvarex1",
        "opmods_downmadex1",
        "opmods_cvarex2ci",
        "opmods_minimaxex1",
        "opmods_quaduex1",
        "opmods_sharpeex1",
        "opmods_omegaex1",
        "opmods_rewardex2",
        "opmods_rewardex3",
        "opmods_cvarex2"
      ),
      descripcion = c(
        "cp lasso",
        "logistic regression",
        "mle",
        "mlogit",
        "ordinary least squares",
        "poisson",
        "qp lasso",
        "Maximize expected return subject to budget normalization and group constraints",
        "Minimum variance portfolio",
        "Minimum variance portfolio with budget normalization and target return constraint",
        "Minimize mean absolute deviation portfolio",
        "The  minimum lower semi-variance portfolio",
        "Minimum lower semi-absolute deviation portfolio with target return constraint",
        "Minimum [low_porc%] & [low_porc%] conditional value at risk:",
        "The minimax portfolio",
        "Maximize quadratic utility where short selling is allowed but the weights should not be less than -1.",
        "Maximize Sharpe ratio with shortselling",
        "Maximize Omega",
        "Maximize expected returns wrt to cvar, turnover, short selling and group constraints",
        "Maximize expected return wrt variance constraint",
        "Minimize [porc%] cvar wrt cardinality constraints"
      ),
      formals = c(
        "data, var, lambda",
        "data, var, solver",
        "data, var",
        "data, var, mm_formula, solver",
        "data, var, intercept",
        "data, var, mm_formula",
        "data, var, lambda",
        "data, index, rhs_group, rhs_budget",
        "data, rhs_budget",
        "data, rhs_budget",
        "data, rhs_budget",
        "data, rhs_budget",
        "data, rhs_budget",
        "data, alpha, rhs_budget",
        "data, rhs_budget",
        "data, lambda, rhs_budget",
        "data, rf",
        "data, tau",
        "data, rhs_budget, rhs_group, rhs_turnover, rhs_cvar, alpha",
        "data, rhs_markowitz, rhs_budget, dir_markowitz, times",
        "data, rhs_cardinal, rhs_budget, alpha"
      )
    ),
    row.names = c(NA, -21L),
    class = c("tbl_df", "tbl", "data.frame")
  )
}

#' @keywords  internal
opmods_leastabsolutedeviation <- function(data, var) {
  var <- rlang::enexpr(var)
  if (!is.character(var)) {
    var <- rlang::expr_deparse(var)
  }

  x <- data[, c(nmdiff(data, var), var)]

  j <- ncol(x)
  x <- as_mtx(x)

  m <- ncol(x) + 1L
  n <- 2 * nrow(x)
  beta <- double(m + n)
  beta[j + 1] <- 1
  op <- ROI::OP(
    objective = ROI::L_objective(c(rep(0, m), rep(1, n))),
    constraints = rbind(
      ROI::L_constraint(
        L = cbind(1, x, diag(nrow(x)), -diag(nrow(x))),
        dir = eq(nrow(x)),
        rhs = rep(0, nrow(x))
      ),
      ROI::L_constraint(L = beta, dir = "==", rhs = -1)
    ),
    bounds = ROI::V_bound(
      li = seq_len(m),
      lb = rep(-Inf, m),
      nobj = length(beta)
    )
  )
  out <- list(op = op, opmods = "leastabsolutedeviation", n_coef = j)
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_qplasso <- function(data, var, lambda) {
  dbind <- function(...) {
    .dbind <- function(x, y) {
      A <- slam::simple_triplet_zero_matrix(NROW(x), NCOL(y))
      B <- slam::simple_triplet_zero_matrix(NROW(y), NCOL(x))
      rbind(cbind(x, A), cbind(B, y))
    }
    Reduce(.dbind, list(...))
  }

  var <- rlang::enexpr(var)
  if (!is.character(var)) {
    var <- rlang::expr_deparse(var)
  }
  y <- matrix(data[[var]], ncol = 1)
  x <- as_mtx(datadiff(data = data, x = var))
  stzm <- slam::simple_triplet_zero_matrix
  stdm <- slam::simple_triplet_diag_matrix
  m <- NROW(x)
  n <- NCOL(x)
  Q0 <- dbind(stzm(n), stdm(1, m), stzm(n))
  a0 <- c(
    b = double(n),
    g = double(m),
    t = lambda * rep(1, n)
  )
  op <- ROI::OP(objective = ROI::Q_objective(Q = Q0, L = a0))
  ## y - X %*% beta = gamma  <=>  X %*% beta + gamma = y
  A1 <- cbind(x, stdm(1, m), stzm(m, n))
  LC1 <- ROI::L_constraint(A1, rep.int("==", m), y)
  ##  -t <= beta  <=>  0 <= beta + t
  A2 <- cbind(stdm(1, n), stzm(n, m), stdm(1, n))
  LC2 <- ROI::L_constraint(A2, rep.int(">=", n), double(n))
  ##   beta <= t  <=>  beta - t <= 0
  A3 <- cbind(stdm(1, n), stzm(n, m), stdm(-1, n))
  LC3 <- ROI::L_constraint(A3, rep.int("<=", n), double(n))
  ROI::constraints(op) <- rbind(LC1, LC2, LC3)
  ROI::bounds(op) <- ROI::V_bound(ld = -Inf, nobj = ncol(Q0))
  out <- list(op = op, opmods = "qplasso", n_coef = n, solver = "qpoases")
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_cplasso <- function(data, var, lambda) {
  stm <- slam::simple_triplet_matrix
  stzm <- slam::simple_triplet_zero_matrix
  stdm <- slam::simple_triplet_diag_matrix

  var <- rlang::enexpr(var)
  if (!is.character(var)) {
    var <- rlang::expr_deparse(var)
  }
  y <- matrix(data[[var]], ncol = 1)
  x <- as_mtx(datadiff(data = data, x = var))

  m <- NROW(x)
  n <- NCOL(x)
  nobj <- 2 * n + 1
  op <-
    ROI::OP(c(
      beta = double(n),
      t = lambda * rep(1, n),
      u = 0.5
    ))
  ## ||(1 - u, 2 y - 2 X %*% beta)||_2 <= 1 + u
  A1 <-
    rbind(stm(1, nobj, -1), stm(1, nobj, 1), cbind(2 * x, stzm(m, n + 1)))
  LC1 <- ROI::C_constraint(A1, ROI::K_soc(m + 2), rhs = c(1, 1, 2 * y))
  ## -t <= z  <=>  0 <= z + t
  A2 <- cbind(stdm(1, n), stdm(1, n), stzm(n, 1))
  ##  z <= t  <=>  z - t <= 0
  A3 <- cbind(stdm(1, n), stdm(-1, n), stzm(n, 1))
  LC2 <-
    ROI::L_constraint(rbind(-A2, A3), leq(2 * n), double(2 * n))
  ROI::constraints(op) <- rbind(LC2, LC1)
  ROI::bounds(op) <- ROI::V_bound(ld = -Inf, nobj = nobj)
  out <- list(op = op, opmods = "ordinaryleastsquares", n_coef = n, solver = "ecos")
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_ordinaryleastsquares <- function(data, var, intercept = TRUE) {
  var <- rlang::enexpr(var)
  if (!is.character(var)) {
    var <- rlang::expr_deparse(var)
  }
  y <- matrix(data[[var]], ncol = 1)
  x <- as_mtx(datadiff(data = data, x = var))
  if (intercept) {
    x <- cbind(int = 1, x)
  }
  Q <- 2 * t(x) %*% x
  L <- -2 * t(y) %*% x
  op <- ROI::OP(
    objective = ROI::Q_objective(Q = Q, L = L),
    bounds = ROI::V_bound(ld = -Inf, nobj = ncol(x))
  )
  out <- list(op = op, opmods = "ordinaryleastsquares")
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_logisticregression <- function(data, var, solver = "ecos") {
  var <- rlang::enexpr(var)
  if (!is.character(var)) {
    var <- rlang::expr_deparse(var)
  }
  y <- data[[var]]
  X <- as_mtx(datadiff(data = data, x = var))
  X <- cbind(int = 1, X)

  stm <- slam::simple_triplet_matrix
  stzm <- slam::simple_triplet_zero_matrix
  stopifnot(is.vector(y), length(y) == nrow(X))
  m <- nrow(X)
  n <- ncol(X)
  i <- 3 * seq_len(m) - 2
  op <-
    ROI::OP(c(-(y %*% X), rep.int(1, m), double(m)), maximum = FALSE)
  C11 <-
    stm(rep(i, n), rep(seq_len(n), each = m), -drop(X), 3 * m, n)
  C12 <- stm(i, seq_len(m), rep.int(1, m), 3 * m, m)
  C13 <- stm(i + 2, seq_len(m), rep.int(-1, m), 3 * m, m)
  C1 <- cbind(C11, C12, C13)
  C2 <- cbind(stzm(3 * m, n), C12, -C13)
  C <- rbind(C1, C2)
  cones <- ROI::K_expp(2 * m)
  rhs <- c(rep(c(0, 1, 1), m), rep(c(0, 1, 0), m))
  ROI::constraints(op) <- ROI::C_constraint(C, cones, rhs)
  ROI::bounds(op) <- ROI::V_bound(ld = -Inf, nobj = ncol(C))
  out <-
    list(
      op = op,
      opmods = "logisticregression",
      n_coef = n,
      solver = solver
    )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_mle <- function(data, var) {
  var <- rlang::enexpr(var)
  if (!is.character(var)) {
    var <- rlang::expr_deparse(var)
  }
  y <- data[[var]]
  X <- as_mtx(datadiff(data = data, x = var))
  X <- cbind(int = 1, X)


  mle <- function(beta) {
    drop(y %*% X %*% beta - sum(log(1 + exp(X %*% beta))))
  }

  op <- ROI::OP(
    ROI::F_objective(mle, n = ncol(X)),
    maximum = TRUE,
    bounds = ROI::V_bound(ld = -Inf, nobj = ncol(X))
  )

  out <-
    list(
      op = op,
      opmods = "mle",
      solver = "optimx",
      control = list(start = double(ncol(X)))
    )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_mlogit <- function(data, var, mm_formula, solver = "auto") {
  var <- rlang::enexpr(var)
  if (!is.character(var)) {
    var <- rlang::expr_deparse(var)
  }

  y <- data[[var]]

  if (!is.factor(y)) {
    return(NULL)
  }
  X <- model.matrix(object = mm_formula, data = data)
  res_name <- apply(
    expand.grid(levels(y)[-1], colnames(X)), 1,
    function(x) {
      paste0(x[2], ":", x[1])
    }
  )
  stm <- slam::simple_triplet_matrix
  stzm <- slam::simple_triplet_zero_matrix
  y <- as_num(y)
  stopifnot(is.vector(y), length(y) == nrow(X))
  ymat <- model.matrix(~ as_fct(y))[, -1]
  xtilde <- model.matrix(~ 0 + ymat:X)
  ytilde <-
    (y != min(y)) + 0 # indicator taking zero for category to be excluded
  n <- nrow(X)
  p <- ncol(X)
  J <- max(y)
  ptilde <- ncol(xtilde)
  i <- 3 * seq_len(n) - 2 ## triplets for cones
  ## Variables: beta_2, .., beta_J, t_i, u^1,..., u^J
  op <-
    ROI::OP(c(-(ytilde %*% xtilde), rep.int(1, n), double(n * J)), maximum = FALSE)
  Ct <- stm(i, seq_len(n), rep.int(1, n), 3 * n, n)
  Cu <- stm(i + 2, seq_len(n), rep.int(-1, n), 3 * n, n)
  Clist <- lapply(seq_len(J), function(j) {
    Cx <- if (j == 1) {
      stzm(3 * n, ptilde)
    } else {
      stm(
        rep(i, p), rep((seq_len(p) - 1) * (J - 1) + j - 1, each = n),
        -drop(X), 3 * n, ptilde
      )
    }
    CC <-
      cbind(Cx, Ct, stzm(3 * n, n * (j - 1)), Cu, stzm(3 * n, n * (J - j)))
  })
  C <- do.call("rbind", Clist)
  cones <- ROI::K_expp(J * n)
  rhs <- rep(c(0, 1, 0), n * J)
  CL <- cbind(
    stzm(n, ptilde + n),
    stm(rep(seq_len(n), J), seq_len(n * J), rep.int(1, n * J), n, n * J)
  )
  rhs <- rep(c(0, 1, 0), n * J)
  ROI::constraints(op) <- rbind(
    ROI::C_constraint(C, cones, rhs),
    ROI::L_constraint(CL,
                      dir = rep("<=", nrow(CL)),
                      rhs = rep(1, nrow(CL))
    )
  )
  ROI::bounds(op) <- ROI::V_bound(ld = -Inf, nobj = ncol(C))
  out <- list(op = op, opmods = "mlogit", solver = solver, res_name = res_name)
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_poisson <- function(data, var, mm_formula) {
  var <- rlang::enexpr(var)
  if (!is.character(var)) {
    var <- rlang::expr_deparse(var)
  }

  y <- data[[var]]

  if (!is_dblint(y)) {
    return(NULL)
  }
  X <- model.matrix(object = mm_formula, data = data)
  m <- nrow(X)
  n <- ncol(X)
  i <- 3 * seq_len(m) - 2
  op <- ROI::OP(c(-(y %*% X), rep.int(1, m)))
  stm <- slam::simple_triplet_matrix
  A <-
    cbind(
      stm(rep(i, n), rep(seq_len(n), each = m), -drop(X), 3 * m, n),
      stm(i + 2, seq_len(m), rep.int(-1, m), 3 * m, m)
    )
  rhs <- rep(c(0, 1, 0), m)
  cones <- ROI::K_expp(m)
  ROI::constraints(op) <- ROI::C_constraint(A, cones, rhs)
  ROI::bounds(op) <- ROI::V_bound(ld = -Inf, nobj = ncol(A))
  out <-
    list(
      op = op,
      opmods = "poisson",
      n_coef = n,
      solver = "ecos"
    )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_rewardex1 <- function(data, index, rhs_group, rhs_budget = 1) {
  op <- ROI::OP(
    objective = solvobjective_max_return(r_mat = data)$objective,
    constraints = rbind(
      constraint_budget(r_mat = data, rhs = rhs_budget),
      constraint_group(
        r_mat = data,
        index = index,
        dir = "==",
        rhs = rhs_group
      )
    ),
    maximum = T
  )

  out <- list(op = op, solver = "glpk")
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_markex1 <- function(data, rhs_budget = 1) {
  op <- ROI::OP(
    objective = solvobjective_min_variance(r_mat = data)$objective,
    constraints = rbind(constraint_budget(r_mat = data, rhs = rhs_budget))
  )

  out <- list(op = op, solver = "quadprog")
  class(out) <- "opmods"

  out
}
#' @keywords  internal
opmods_markex2 <- function(data, rhs_budget = 1) {
  op <- ROI::OP(
    objective = solvobjective_min_variance(r_mat = data)$objective,
    constraints = rbind(
      constraint_budget(r_mat = data, rhs = rhs_budget),
      constraint_reward(r_mat = data, rhs = 0.001)
    )
  )
  out <- list(op = op, solver = "quadprog")
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_madex1 <- function(data, rhs_budget = 1) {
  op <- ROI::OP(
    objective = solvobjective_min_mean_absdeviation(r_mat = data)$objective,
    constraints = rbind(
      solvobjective_min_mean_absdeviation(r_mat = data)$constraint,
      constraint_budget(r_mat = data, rhs = rhs_budget),
      use.names = TRUE
    )
  )

  out <- list(
    op = op,
    solver = "glpk",
    n_coef = NCOL(data)
  )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_downvarex1 <- function(data, rhs_budget = 1) {
  tmp <- solvobjective_min_lower_semivariance(r_mat = data)
  op <- ROI::OP(
    objective = tmp$objective,
    constraints = rbind(
      tmp$constraint,
      constraint_budget(r_mat = data, rhs = rhs_budget),
      use.names = TRUE
    )
  )

  out <- list(
    op = op,
    solver = "quadprog",
    n_coef = NCOL(data)
  )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_downmadex1 <- function(data, rhs_budget = 1) {
  tmp <- solvobjective_min_lower_semiabsdeviation(r_mat = data)
  op <- ROI::OP(
    objective = tmp$objective,
    constraints = rbind(
      tmp$constraint,
      constraint_budget(r_mat = data, rhs = rhs_budget),
      constraint_reward(r_mat = data, rhs = 0.001),
      use.names = TRUE
    )
  )

  out <- list(
    op = op,
    solver = "glpk",
    n_coef = NCOL(data)
  )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_cvarex2ci <- function(data,
                             alpha = c(.95, .99),
                             rhs_budget = 1) {
  if (length(alpha) != 2) {
    cat_stop("alpha debe ser de largo 2")
    return(NULL)
  }
  if (any(!is_prob(alpha))) {
    cat_stop("alpha debe ser [0,1]")
    return(NULL)
  }

  alpha <- sort(alpha)

  tmp_low <-
    solvobjective_min_conditional_valuerisk(r_mat = data, alpha = alpha[1])

  op_low <- ROI::OP(
    objective = tmp_low$objective,
    constraints = rbind(
      tmp_low$constraint,
      constraint_budget(r_mat = data, rhs = rhs_budget),
      use.names = TRUE
    ),
    bounds = tmp_low$bounds
  )

  tmp_up <-
    solvobjective_min_conditional_valuerisk(r_mat = data, alpha = alpha[2])

  op_up <- ROI::OP(
    objective = tmp_up$objective,
    constraints = rbind(
      tmp_up$constraint,
      constraint_budget(r_mat = data, rhs = rhs_budget),
      use.names = TRUE
    ),
    bounds = tmp_up$bounds
  )

  custom_sol <- function(x) {
    sol_low <- ROI::ROI_solve(x$op$low, solver = x$solver)
    sol_up <- ROI::ROI_solve(x$op$up, solver = x$solver)

    list(
      solution = list(
        low = sol_low$solution[seq_len(x$n_coef)],
        up = sol_up$solution[seq_len(x$n_coef)]
      ),
      objval = list(
        low = sol_low$objval,
        up = sol_up$objval
      ),
      var = list(low = sol_low$solution["gamma"], up = sol_up$solution["gamma"]),
      range = list(
        low = x$custom_sol$range[1],
        up = x$custom_sol$range[2]
      )
    )
  }
  out <-
    list(
      op = list(low = op_low, up = op_up),
      solver = "glpk",
      n_coef = NCOL(data),
      custom_sol = list(fn = custom_sol, range = alpha)
    )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_minimaxex1 <- function(data, rhs_budget = 1) {
  tmp <- solvobjective_minimax(r_mat = data)
  op <- ROI::OP(
    objective = tmp$objective,
    constraints = rbind(
      tmp$constraint,
      constraint_budget(r_mat = data, rhs = rhs_budget),
      use.names = TRUE
    ),
    bounds = tmp$bounds,
    maximum = T
  )

  out <- list(
    op = op,
    solver = "glpk",
    n_coef = NCOL(data)
  )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_quaduex1 <- function(data,
                            lambda = 2,
                            rhs_budget = 1) {
  tmp <- solvobjective_quadratic_utility(r_mat = data, lambda = lambda)
  op <- ROI::OP(
    objective = tmp$objective,
    constraints = constraint_budget(r_mat = data, rhs = rhs_budget),
    bounds = ROI::V_bound(
      li = seq_len(NCOL(data)),
      lb = rep(-1, NCOL(data))
    ),
    maximum = T
  )
  out <- list(
    op = op,
    solver = "quadprog",
    n_coef = NCOL(data)
  )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_sharpeex1 <- function(data, rf = 0) {
  N <- NCOL(data)
  tmp <- solvobjective_max_sharpe_ratio(r_mat = data, rf = rf)

  op <- ROI::OP(maximum = T)
  ROI::objective(op) <- tmp$objective
  mat <- cbind(diag(N), 1)
  shortsell_constraint <- ROI::L_constraint(mat,
                                            dir = rep(">=", N),
                                            rhs = rep(0, N)
  )

  ROI::constraints(op) <- rbind(
    tmp$constraint,
    shortsell_constraint
  )
  ROI::bounds(op) <- ROI::V_bound(
    li = seq_len(N + 1),
    lb = c(rep(-Inf, N), 0)
  )
  custom_sol <- function(x) {
    sol <- ROI::ROI_solve(x = x$op, solver = x$solve)
    if (sol$status$msg$symbol == "FAILURE") {
      return(NULL)
    }
    sol_sharpe <- sol$solution
    x_opt <-
      round(sol_sharpe[seq_len(x$n_coef)] / sol_sharpe["kappa_sharpe"], 3)
    names(x_opt) <- x$custos_sol$mat_names
    list(solution = x_opt, objval = sol$objval)
  }
  out <-
    list(
      op = op,
      solver = "quadprog",
      n_coef = NCOL(data),
      custom_sol = list(fn = custom_sol, mat_names = colnames(data))
    )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_omegaex1 <- function(data, tau = 0) {
  tmp <- solvobjective_max_omega(r_mat = data, tau = tau)
  op <- ROI::OP(
    objective = tmp$objective,
    constraints = tmp$constraint,
    maximum = T
  )
  custom_sol <- function(x) {
    sol <- ROI::ROI_solve(x = x$op, solver = x$solve)
    if (sol$status$msg$symbol == "FAILURE") {
      return(NULL)
    }
    sol_omega <- ROI::solution(sol)
    x_opt <-
      round(sol_omega[seq_len(x$n_coef)] / sol_omega["z_omega"], 3)
    names(x_opt) <- x$custos_sol$mat_names
    list(solution = x_opt, objval = sol$objval)
  }

  out <- list(
    op = op,
    solver = "glpk",
    n_coef = NCOL(data),
    custom_sol = list(fn = custom_sol, mat_names = colnames(data))
  )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_rewardex2 <- function(data,
                             index,
                             rhs_budget = 1,
                             rhs_group = 0.5,
                             rhs_turnover = 0.5,
                             rhs_cvar = 0.02,
                             alpha = 0.95) {
  op <- ROI::OP(maximum = T)
  ROI::constraints(op) <- rbind(
    constraint_budget(r_mat = data, rhs = rhs_budget),
    constraint_group(
      r_mat = data,
      index = index,
      dir = "<=",
      rhs = rhs_group
    ),
    constraint_turnover(
      r_mat = data,
      dir = "<=",
      rhs = rhs_turnover
    ),
    constraint_cvar(
      r_mat = data,
      alpha = alpha,
      rhs = rhs_cvar
    ),
    use.names = TRUE
  )
  obj <-
    c(terms(solvobjective_max_return(r_mat = data)$objective)$L)
  ROI::objective(op) <-
    c(obj, double(ncol(ROI::constraints(op)) - length(obj)))
  out <- list(
    op = op,
    solver = "glpk",
    n_coef = NCOL(data)
  )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_rewardex3 <- function(data,
                             rhs_markowitz = 0.5^2,
                             rhs_budget = 1,
                             dir_markowitz = "<=",
                             times = 10) {
  op <- ROI::OP(maximum = T)
  ROI::constraints(op) <- rbind(
    constraint_markowitz(
      r_mat = data,
      dir = dir_markowitz,
      rhs = rhs_markowitz
    ),
    constraint_budget(r_mat = data, rhs = rhs_budget),
    use.names = TRUE
  )
  ROI::objective(op) <-
    solvobjective_max_return(r_mat = data)$objective

  custom_sol <- function(x) {
    mstart <- lapply(1:x$custom_sol$times, function(i) {
      runif(length(ROI::objective(x$op)))
    })
    solus <-
      lapply(mstart, function(s) {
        rs <- ROI::ROI_solve(x$op, solver = x$solver, start = s)
        if (rs$status$msg$symbol == "FAILURE") {
          return(NULL)
        }
        rs
      })

    solus <- compact(solus)
    if (is_empty(solus)) {
      return(NULL)
    }

    best_solution <-
      which.max(sapply(solus, solution, type = "objval"))

    list(
      solution = solus[[best_solution]]$solution,
      objval = solus[[best_solution]]$objval
    )
  }
  out <- list(
    op = op,
    solver = "alabama",
    n_coef = NCOL(data),
    custom_sol = list(fn = custom_sol, times = times)
  )
  class(out) <- "opmods"
  out
}
#' @keywords  internal
opmods_cvarex2 <- function(data,
                           rhs_cardinal,
                           rhs_budget = 1,
                           alpha = 0.99) {
  tmp <-
    solvobjective_min_conditional_valuerisk(r_mat = data, alpha = alpha)
  op <- ROI::OP()

  ROI::constraints(op) <- rbind(
    tmp$constraint,
    constraint_budget(r_mat = data, rhs = rhs_budget),
    constraint_cardinality(
      r_mat = data,
      dir = "<=",
      rhs = rhs_cardinal
    ),
    use.names = TRUE
  )

  obj <- c((tmp$objective)$L)
  ROI::objective(op) <-
    c(obj, double(NCOL(ROI::constraints(op)) - length(obj)))

  ROI::types(op) <- rep("C", NCOL(ROI::constraints(op)))
  ROI::types(op)[grep("z_card_aux", ROI::constraints(op)$names)] <- "B"

  out <- list(
    op = op,
    solver = "glpk",
    n_coef = NCOL(data)
  )
  class(out) <- "opmods"
  out
}


#' objectivos pre establecidos
#'
#'
#' @name optim_wflows
#' @rdname optim_wflows
#' @export
use_objecmod <- function(x, objt, r_mat, ...) {
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }

  fn <- switch(objt,
               max_omega = solvobjective_max_omega,
               max_return = solvobjective_max_return,
               max_sharpe_ratio = solvobjective_max_sharpe_ratio,
               min_conditional_valuerisk = solvobjective_min_conditional_valuerisk,
               min_lower_semiabsdeviation = solvobjective_min_lower_semiabsdeviation,
               min_lower_semivariance = solvobjective_min_lower_semivariance,
               min_mean_absdeviation = solvobjective_min_mean_absdeviation,
               min_variance = solvobjective_min_variance,
               minimax = solvobjective_minimax,
               quadratic_utility = solvobjective_quadratic_utility,
               NULL
  )

  obj_e <- fn(r_mat = r_mat, ...)
  x[["add_info"]] <- list(r_mat = r_mat, ...)
  x$objective <- obj_e$objective
  x$add_steps$objective <- TRUE
  x$n <- length(x$objective)
  if (!is_empty(obj_e$constraint)) {
    x <- add_optim_constraint(x = x, obj_e$constraint)
    x$add_steps$constraint <- TRUE
  }

  if (!is_empty(obj_e$bounds)) {
    x$bounds <- obj_e$bounds
    x$add_steps$bound <- TRUE
  } else {
    x$bounds <-
      ROI::V_bound(
        li = seq_len(x$n),
        ui = seq_len(x$n),
        lb = rep(x$inf * -1, x$n),
        ub = rep(x$inf, x$n)
      )
  }
  x
}


#' @keywords  internal
solvobjective_max_return <- function(r_mat) {
  objective <- ROI::L_objective(colMeans(r_mat))
  list(objective = objective)
}
#' @keywords  internal
solvobjective_min_variance <- function(r_mat) {
  objective <- ROI::Q_objective(
    Q = 2 * cov(r_mat),
    L = rep(0, NCOL(r_mat))
  )
  list(objective = objective)
}
#' @keywords  internal
solvobjective_min_mean_absdeviation <- function(r_mat) {
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  S <- nrow(r_mat)
  mu <- colMeans(r_mat)
  Amat <-
    cbind(sweep(as_mtx(r_mat), 2, mu), -diag(S), diag(S))
  var.names <- c(
    x.names,
    paste0("y_mad_aux", seq_len(S)),
    paste0("z_mad_aux", seq_len(S))
  )

  constraint <- ROI::L_constraint(
    L = Amat,
    dir = rep("==", S),
    rhs = rep(0, S),
    names = var.names
  )

  objective <- ROI::L_objective(L = c(rep(0, N), rep(1 / S, 2 * S)))

  list(objective = objective, constraint = constraint)
}
#' @keywords  internal
solvobjective_min_lower_semivariance <- function(r_mat, x.names = NULL) {
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  S <- NROW(r_mat)
  mu <- colMeans(r_mat)
  Amat <- cbind(sweep(as_mtx(r_mat), 2, mu), diag(S))
  var.names <- c(x.names, paste0("z_dvar_aux", seq_len(S)))

  constraint <- ROI::L_constraint(
    L = Amat,
    dir = rep(">=", S),
    rhs = rep(0, S),
    names = var.names
  )
  objective <-
    ROI::Q_objective(
      Q = 2 * diag(c(rep(1e-05, N), rep(1 / S, S))),
      L = rep(0, N + S)
    )

  list(objective = objective, constraint = constraint)
}
#' @keywords  internal
solvobjective_min_lower_semiabsdeviation <- function(r_mat) {
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  S <- NROW(r_mat)
  mu <- colMeans(r_mat)
  Amat <- cbind(sweep(as_mtx(r_mat), 2, mu), diag(S))
  var.names <- c(x.names, paste0("z_dmad_aux", seq_len(S)))

  constraint <- ROI::L_constraint(
    L = Amat,
    dir = rep(">=", S),
    rhs = rep(0, S),
    names = var.names
  )

  objective <- ROI::L_objective(L = c(rep(0, N), rep(1 / S, S)))

  list(objective = objective, constraint = constraint)
}
#' @keywords  internal
solvobjective_min_conditional_valuerisk <- function(r_mat, alpha, probs = NULL) {
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  S <- NROW(r_mat)
  # mu <- colMeans(r_mat)
  if (is.null(probs)) {
    probs <- rep(1 / S, S)
  }
  if (alpha < 0.5) {
    alpha <- 1 - alpha
  }

  Amat <- cbind(as_mtx(r_mat), diag(S), 1)
  var.names <-
    c(x.names, paste0("z_cvar_aux", seq_len(S)), "gamma")

  ## set bounds for gamma (-Inf, Inf)
  bnds <- ROI::V_bound(
    li = c(N + S + 1),
    lb = c(-Inf),
    ui = c(N + S + 1),
    ub = c(Inf)
  )

  constraint <- ROI::L_constraint(
    L = Amat,
    dir = rep(">=", S),
    rhs = rep(0, S),
    names = var.names
  )

  objective <- ROI::L_objective(c(rep(0, N), probs / (1 - alpha), 1))

  list(
    objective = objective,
    constraint = constraint,
    bounds = bnds
  )
}
#' @keywords  internal
solvobjective_minimax <- function(r_mat) {
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  S <- NROW(r_mat)
  Amat <- cbind(as_mtx(r_mat), -1)

  bnds <- ROI::V_bound(
    li = c(N + 1),
    lb = c(-Inf),
    ui = c(N + 1),
    ub = c(Inf)
  )
  constraint <-
    ROI::L_constraint(
      L = Amat,
      dir = rep(">=", S),
      rhs = rep(0, S),
      names = c(x.names, "mp_aux")
    )

  objective <- ROI::L_objective(c(rep(0, N), 1))

  list(
    objective = objective,
    constraint = constraint,
    bounds = bnds
  )
}
#' @keywords  internal
solvobjective_quadratic_utility <- function(r_mat, lambda = 2) {
  objective <- ROI::Q_objective(
    Q = -lambda * cov(r_mat),
    L = colMeans(r_mat)
  )
  list(objective = objective)
}
#' @keywords  internal
solvobjective_max_sharpe_ratio <- function(r_mat, rf = 0) {
  N <- NCOL(r_mat)
  # S <- NROW(r_mat)
  mu <- colMeans(r_mat)
  Amat <- rbind(
    c(mu - rf, 0),
    c(rep(0, N), 1),
    c(rep(1, N), -1)
  )
  var.names <-
    c(paste0("y_sharpe_aux", seq_len(N)), "kappa_sharpe")

  constraint <- ROI::L_constraint(
    L = Amat,
    dir = c("==", ">", "=="),
    rhs = c(1, 0, 0),
    names = var.names
  )

  mat <- matrix(0, ncol = N + 1, nrow = N + 1)
  mat[1:N, 1:N] <- 2 * cov(r_mat)
  mat[N + 1, N + 1] <- 1e-04

  objective <- ROI::Q_objective(Q = -mat, L = c(rep(0, N), 0))

  list(objective = objective, constraint = constraint)
}
#' @keywords  internal
solvobjective_max_omega <- function(r_mat, tau = 0) {
  ## variables y_1, ... y_N, u_1, .., u_S, z
  N <- NCOL(r_mat)
  S <- NROW(r_mat)
  mu <- colMeans(r_mat)

  Amat <-
    rbind(
      cbind(as_mtx(r_mat), diag(S), 0),
      # u_s >= tau - r_s'y
      c(rep(0, N), rep(1, S), 0),
      # sum(u) = 1
      c(rep(1, N), rep(0, S), -1),
      # sum(y) = z
      c(mu, rep(0, S), -tau),
      c(rep(0, N), rep(0, S), 1)
    ) # mu'y  >= tau * z
  var.names <- c(
    paste0("y_omega_aux", seq_len(N)),
    paste0("u_omega_aux", seq_len(S)),
    "z_omega"
  )
  constraint <- ROI::L_constraint(
    L = Amat,
    dir = c(rep(">=", S), "==", "==", ">=", ">"),
    rhs = c(rep(tau, S), 1, 0, 0, 1e-05),
    names = var.names
  )
  objective <- ROI::L_objective(L = c(mu, rep(0, S), -tau))

  list(objective = objective, constraint = constraint)
  ## x* <- y*/z*
}

#' constraint pre definidos
#'
#' @name optim_wflows
#' @rdname optim_wflows
#' @export
use_constrmod <- function(x, constr, r_mat = NULL, ...) {
  if (!check_inherits(x, "optimization_wflow")) {
    cat_stop("'x' debe ser clase 'optimization_wflow'")
  }
  if (!x$add_steps$objective) {
    cat_stop("el primer paso debe ser definir el objetivo")
    return(NULL)
  }
  fn <- switch(constr,
               budget = constraint_budget,
               cardinality = constraint_cardinality,
               cvar = constraint_cvar,
               group = constraint_group,
               markowitz = constraint_markowitz,
               reward = constraint_reward,
               turnover = constraint_turnover,
               NULL
  )

  if (!is_empty(x$add_info$r_mat)) {
    r_mat <- x$add_info$r_mat
  }
  constr <- fn(r_mat = r_mat, ...)
  x <- add_optim_constraint(x = x, constr)
  x
}

#' @keywords  internal
constraint_budget <- function(r_mat, dir = "==", rhs = 1) {
  x.names <- colnames(r_mat)
  ROI::L_constraint(
    L = rep(1, NCOL(r_mat)),
    dir = dir,
    rhs = rhs,
    names = x.names
  )
}
#' @keywords  internal
constraint_group <- function(r_mat, index, coef.index = 1, dir = "==", rhs) {
  ## index = (i, j)
  ## coef.index = c(a,b)
  ## rhs = c
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  L <- rep(0, N)
  L[index] <- coef.index
  ROI::L_constraint(
    L = L,
    dir = dir,
    rhs = rhs,
    names = x.names
  )
}
#' @keywords  internal
constraint_turnover <- function(r_mat, x0 = NULL, dir = "<=", rhs = 100) {
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  S <- NROW(r_mat)
  if (is.null(x0)) {
    x0 <- rep(1 / N, N)
  }
  Amat <- cbind(diag(N), -diag(N), diag(N))
  var.names <- c(
    x.names,
    paste0("y_plus_aux", seq_len(N)),
    paste0("y_minus_aux", seq_len(N))
  )

  rbind(
    ROI::L_constraint(
      L = Amat,
      dir = rep("==", N),
      rhs = x0,
      names = var.names
    ),
    ROI::L_constraint(
      c(rep(0, N), rep(1, N), rep(1, N)),
      dir = dir,
      rhs = rhs,
      names = var.names
    )
  )
}
#' @keywords  internal
constraint_cardinality <- function(r_mat, dir = "<=", rhs = 100) {
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  Amat <- cbind(diag(N), -diag(N))
  var.names <- c(x.names, paste0("z_card_aux", seq_len(N)))
  cat("Variable types for z_card_aux must be set to binary.\n")
  rbind(
    ROI::L_constraint(
      L = Amat,
      dir = rep("<=", N),
      rhs = rep(0, N),
      names = var.names
    ),
    ROI::L_constraint(
      L = c(rep(0, N), rep(1, N)),
      dir = dir,
      rhs = rhs,
      names = var.names
    )
  )
}
#' @keywords  internal
constraint_reward <- function(r_mat, dir = ">=", rhs = 0) {
  x.names <- colnames(r_mat)
  ROI::L_constraint(
    L = colMeans(r_mat),
    dir = dir,
    rhs = rhs,
    names = x.names
  )
}
#' @keywords  internal
constraint_markowitz <- function(r_mat, dir = "<", rhs = 1000) {
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  ROI::Q_constraint(
    Q = 2 * cov(r_mat),
    L = rep(0, N),
    dir = dir,
    rhs = rhs,
    names = x.names
  )
}
#' @keywords  internal
constraint_cvar <- function(r_mat, alpha, probs = NULL, dir = "<=", rhs = 100) {
  x.names <- colnames(r_mat)
  N <- NCOL(r_mat)
  S <- NROW(r_mat)
  if (alpha < 0.5) {
    alpha <- 1 - alpha
  }
  if (is.null(probs)) {
    probs <- rep(1 / S, S)
  }

  Amat <- cbind(as_mtx(r_mat), diag(S), 1)
  var.names <-
    c(x.names, paste0("z_cvar_aux", seq_len(S)), "gamma")
  # set bounds for gama
  bnds <- ROI::V_bound(
    li = c(N + S + 1),
    lb = c(-Inf),
    ui = c(N + S + 1),
    ub = c(Inf)
  )
  rbind(
    ROI::L_constraint(
      L = Amat,
      dir = rep(">=", S),
      rhs = rep(0, S),
      names = var.names
    ),
    ROI::L_constraint(
      c(rep(0, N), rep(1 / ((
        1 - alpha
      ) * S), S), 1),
      dir = dir,
      rhs = rhs,
      names = var.names
    )
  )
}


#' facility_location_problem
#'
#' @name optim_wflows
#' @rdname optim_wflows
#'
#' @examples
#' \dontrun{
#' set.seed(1234)
#' grid_size <- 1000
#' n <- 25
#' customer_locations <- data.frame(
#'   id = 1:n,
#'   x = round(runif(n) * grid_size),
#'   y = round(runif(n) * grid_size)
#' )
#' m <- 5
#' warehouse_locations <- data.frame(
#'   id = 1:m,
#'   x = round(runif(m) * grid_size),
#'   y = round(runif(m) * grid_size),
#'   fixedcost = round(rnorm(m, mean = prod(grid_size, 100), sd = grid_size))
#' )
#'
#'
#' facility_location_problem() %>%
#'   flp_add_facility_data(
#'     long = warehouse_locations$x,
#'     lat = warehouse_locations$y,
#'     fixedcost = warehouse_locations$fixedcost,
#'     id = warehouse_locations$id
#'   ) %>%
#'   flp_add_demand_data(
#'     long = customer_locations$x,
#'     lat = customer_locations$y,
#'     id = customer_locations$id
#'   ) %>%
#'   flp_update_cost(
#'     fn = function(x) {
#'       x
#'     }
#'   ) %>%
#'   flp_finalize_wflow() %>%
#'   flp_solve()
#' }
#'
#' @export

facility_location_problem <- function(...) {
  ROI_pkgs()
  structure(
    list(
      structure(
        list(integer(0), numeric(0), numeric(0)),
        .Names = c(
          "id",
          "x", "y"
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = integer(0)
      ),
      structure(
        list(integer(0), numeric(0), numeric(0), numeric(0)),
        .Names = c(
          "id",
          "x", "y", "fixedcost"
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = integer(0)
      ), structure(list(FALSE, FALSE),
                   .Names = c("demand", "facility")
      ), function(x) {
        identity(x)
      },
      structure(list())
    ),
    .Names = c(
      "demand_coords",
      "facility_coords",
      "step_check",
      "cost",
      "new_constraint"
    ),
    class = "facility_location_problem",
    status = "pre"
  )
}

#' @rdname optim_wflows
#' @export
flp_add_facility_data <- function(x, long, lat, fixedcost, id = NULL) {
  if (!check_inherits(x, "facility_location_problem")) {
    cat_stop("'x' debe ser clase 'facility_location_problem'")
  }

  if (length(long) != length(lat)) {
    cat_stop("'x' e 'y' deben ser del mismo tamano")
    return(NULL)
  }

  if (!is_empty(id)) {
    if (length(id) != length(long)) {
      cat_stop("'id' debe ser del mismo tamano de 'x' e 'y'")
      return(NULL)
    }
  } else {
    id <- seq_along(long)
  }
  facility_dat <-
    data.frame(
      id = id,
      x = long,
      y = lat,
      fixedcost = fixedcost
    )

  x$facility_coords <- rbind(x$facility_coords, facility_dat)
  x$step_check$facility <- TRUE
  x
}

#' @rdname optim_wflows
#' @export
flp_add_demand_data <- function(x, long, lat, id = NULL) {
  if (!check_inherits(x, "facility_location_problem")) {
    cat_stop("'x' debe ser clase 'facility_location_problem'")
  }

  if (length(long) != length(lat)) {
    cat_stop("'x' e 'y' deben ser del mismo tamano")
    return(NULL)
  }

  if (!is_empty(id)) {
    if (length(id) != length(long)) {
      cat_stop("'id' debe ser del mismo tamano de 'x' e 'y'")
      return(NULL)
    }
  } else {
    id <- seq_along(x)
  }

  demand_dat <-
    data.frame(
      id = id,
      x = long,
      y = lat
    )

  x$demand_coords <- rbind(x$demand_coords, demand_dat)
  x$step_check$demand <- TRUE
  x
}

#' @rdname optim_wflows
#' @export
flp_update_cost <- function(x, fn) {
  if (!check_inherits(x, "facility_location_problem")) {
    cat_stop("'x' debe ser clase 'facility_location_problem'")
  }
  x$cost <- fn
  x
}

#' @rdname optim_wflows
#' @export
flp_finalize_wflow <- function(x) {
  cost_fn <- function(cost, demand_coords, facility_coords) {
    demand_coords <- demand_coords[, c("x", "y")]
    facility_coords <- facility_coords[, c("x", "y")]

    rlang::new_function(alist(i = , j = ), body = substitute(
      {
        fn <- function(x) {
          COST_FN
        }
        demand_coords <- DEMAND_COORDS
        facility_coords <- FACILITY_COORDS
        to <- demand_coords[i, ]
        from <- facility_coords[j, ]
        round(fn(sqrt((to$x - from$x)^2 + (to$y - from$y)^2)), 2)
      },
      list(
        DEMAND_COORDS = rlang::expr(!!demand_coords),
        FACILITY_COORDS = rlang::expr(!!facility_coords),
        COST_FN = body(cost)
      )
    ))
  }

  if (!x$step_check$demand) {
    cat_stop("La informacion de la ubicacion de la demanda no ha sido ingresada")
    return(NULL)
  }
  if (!x$step_check$facility) {
    cat_stop("La informacion de las ubicaciones a analizar no ha sido ingresada")
    return(NULL)
  }

  m <- nrow(x$facility_coords)
  n <- nrow(x$demand_coords)

  transport_cost <-
    cost_fn(
      cost = x$cost,
      demand_coords = x$demand_coords,
      facility_coords = x$facility_coords
    )

  fixedcost <- x$facility_coords$fixedcost

  e1 <- rec_expr(transport_cost(i, j) * x[i, j], i = 1:n, j = 1:m)
  e2 <- rec_expr(fixedcost[j] * y[j], j = 1:m)

  obj_exprs <- exprs_ops(ops = "+", expr1 = e1, expr2 = e2)

  model <- ompr::MIPModel() %>%
    ompr::add_variable(x[i, j],
                       i = 1:n,
                       j = 1:m,
                       type = "binary"
    ) %>%
    ompr::add_variable(y[j], j = 1:m, type = "binary")


  args <- list(
    model = model,
    expression = obj_exprs,
    sense = "min"
  )

  model <- do.call("set_objective", args)

  e1 <- rec_expr(x[i, j], j = 1:m)
  e2 <- 1

  contr_exprs <- exprs_ops(ops = "==", expr1 = e1, expr2 = 1)

  args <- list(
    .model = model,
    .constraint_expr = contr_exprs,
    i = 1:n
  )

  model <- do.call("add_constraint", args)
  model <-
    ompr::add_constraint(model, x[i, j] <= y[j], i = 1:n, j = 1:m)
  structure(list(data = x, model = model),
            class = "facility_location_problem",
            status = "build"
  )
}

#' @rdname optim_wflows
#' @export
flp_solve <- function(x) {
  if (!check_inherits(x, "facility_location_problem")) {
    cat_stop("'x' debe ser clase 'facility_location_problem'")
  }

  if (attr(x, "status") != "build") {
    cat_stop("el modelo no esta construido")
  }
  sol <-
    ompr::solve_model(x$model, ompr.roi::with_ROI(solver = "glpk", verbose = TRUE))
  matching <- sol %>%
    ompr::get_solution(x[i, j]) %>%
    filter(value > .9) %>%
    dplyr::select(i, j)
  plot_assignment <- matching %>%
    inner_join(x$data$demand_coords, by = c("i" = "id")) %>%
    inner_join(x$data$facility_coords, by = c("j" = "id"))
  customer_count <-
    matching %>%
    group_by(j) %>%
    summarise(n = dplyr::n()) %>%
    rename(id = j)
  plot_facility <- x$data$facility_coords %>%
    rename(costs = fixedcost) %>%
    inner_join(customer_count, by = "id") %>%
    filter(id %in% unique(matching$j))


  x_range <-
    range(c(x$data$demand_coords$x, x$data$facility_coords$x))
  y_range <-
    range(c(x$data$demand_coords$y, x$data$facility_coords$y))

  p <-
    ggplot(x$data$demand_coords, aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(
      data = x$data$facility_coords,
      color = "red",
      alpha = 0.5,
      shape = 17
    ) +
    ggplot2::scale_x_continuous(limits = c(x_range)) +
    ggplot2::scale_y_continuous(limits = c(y_range)) +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()
    )

  p +
    ggplot2::geom_segment(data = plot_assignment, aes(
      x = x.y,
      y = y.y,
      xend = x.x,
      yend = y.x
    )) +
    ggplot2::geom_point(
      data = plot_facility,
      color = "red",
      size = 3,
      shape = 17
    ) +
    ggrepel::geom_label_repel(
      data  = plot_facility,
      aes(label = paste0("fixed costs:", costs, "; customers: ", n)),
      size = 2,
      nudge_y = 20
    ) +
    ggplot2::ggtitle(
      paste0("Cost optimal locations and demand assignment"),
      "Big red triangles show warehouses that will be built, light red are unused warehouse locations.
   Dots represent demand served by the respective location."
    )

  print(p)
  x[["solution"]] <- sol
  x[["matching"]] <- matching
  x[["facilitys"]] <- unique(matching$j)
  x[["plot"]] <- p
  attr(x, "status") <- "solve"
  x
}



#' @keywords  internal
ROI_pkgs <- function() {
  current_session <- sessionInfo()

  current_packs <-
    names(current_session$otherPkgs)

  p <- getOption("mgalda.roi_pkgs")
  p <- p[!p %in% current_packs]
  if (length(p) > 0) {
    invisible(require_library(p))
  }
}
