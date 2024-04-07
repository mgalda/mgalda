#' nonstandard_eval_formula
#'
#' @name nonstandard_eval_formula
#' @rdname nonstandard_eval_formula
#' @keywords internal
#' @examples
#'
#' # frm_build returns a formula from two strings and an environment
#' make_formula("y", "a", environment())
#'
#' # frm_build uses the calling environment if none is given
#' make_formula("y", "a")
#'
#' # frm_build returns an empty model when no terms are given
#' make_formula("y", character(0))
#'
#' # righthand
#' frm_modify(action = "righthand", y ~ a + b)
#'
#' # frm_expand expands a formula with implicit terms into an explicit formula
#' frm_modify(action = "expand", y ~ a * b)
#'
#' # frm_remove_term removes the given term from the formula
#' frm_modify(action = "remove_term", y ~ a * b, "a")
#'
#' # frm_remove_term returns empty model if all right-hand terms are deleted
#' frm_modify(action = "remove_term", y ~ a, "a")
#'
#' # frm_remove_var is okay with terms that already dont exist
#' frm_modify(action = "remove_var", y ~ a, "b")
#'
#' # frm_remove_var removes every term on the
#' # right-hand side containing the variable
#' frm_modify(action = "remove_var", y ~ a + a:b + c, "a")
#'
#' # frm_remove_var properly handles name overlap
#' frm_modify(action = "remove_var", y ~ a + aa, "a")
#'
#' # frm_remove_ functions return the empty model
#' # when all right-hand terms are deleted
#' frm_modify(action = "remove_term", y ~ a, "a")
#' frm_modify(action = "remove_var", y ~ a, "a")
#'
#' # frm_outcome extracts the term from the left-hand side of the formula
#' frm_modify(action = "outcome", y ~ a * b)
#'
#' # frm_terms extracts the term from the right-hand side of the formula
#' frm_modify(action = "terms", y ~ a)
#'
#' # frm_terms expands the formula before extracting
#' frm_modify(action = "terms", y ~ a * b)
#'
#' # frm_interaction_terms extracts the interactive terms from the formula
#' frm_modify(action = "interaction_terms", y ~ a * b)
#'
#' # frm_fixed_terms only extracts non-random terms from the formula
#' frm_modify(action = "fixed_terms", y ~ a + (1 | group))
#'
#' # frm_random_terms only extracts random terms from the formula
#' frm_modify(action = "random_terms", y ~ a + (1 | group))
#'
#' # frm_vars extracts all variables from the right-hand side
#' frm_modify(action = "vars", y ~ a + a:b + b:c + (1 | group))
#'
#' # frm_fixed_vars extracts only the variables used in fixed terms
#' frm_modify(action = "fixed_vars", y ~ a + a:b + b:c + (1 | group))
#'
#' # frm_random_vars extracts only the variables used in random terms
#' frm_modify(action = "random_vars", y ~ a + a:b + b:c + (1 | group))
#'
#' get_vars(Species ~ ., iris)
#' get_vars(quote(Sepal.Length * Sepal.Width), iris)
#'
#'
#' invert(quote(A > 5))
#' invert(quote(A >= 5))
#' invert(quote(A < 5))
#' invert(quote(A <= 5))
#' invert(quote(A == 5))
#' invert(quote(A != 5))
#' invert(quote(A %in% lettters[1:5]))
#' invert(quote(A %!in% letters[1:5]))
#'
#'
#'
#' form <- y ~ x
#'
#' is_onesided_frm(form)
#' is_twosided_frm(form)
#'
#' # FORMULA
#' f <- A + B ~ C + D
#' frm_lhs(f)
#' frm_lhs(f) <- quote(E / F)
#' f
#' frm_rhs(f)
#' frm_rhs(f) <- quote(G + H)
#' op(f)
#' op(frm_rhs(f))
#' op(quote(A)) # NULL:
#' op_type(f)
#'
#' # ONE-SIDED FORMULA
#' f <- ~A #
#' frm_lhs(f) # NULL
#' frm_rhs(f) # A
#'
#'
#' # EXPRESSION
#' e <- expression(A + B == C + D)
#' frm_lhs(e)
#' frm_rhs(e)
#' op(e)
#' op_type(e)
#'
#'
#' # CALL
#' c <- quote(A + B > C + D)
#' frm_lhs(c)
#' frm_lhs(c) <- quote(E)
#' frm_rhs(c)
#'
#' op(c)
#' op_type(c)
#'
#'
#' split_terms(1) # 1
#' split_terms(quote(a)) # a
#' split_terms(quote(-a)) # -a
#' split_terms(quote(a + 1)) # a, 1
#' split_terms(quote(1 + a)) # 1, a
#' split_terms(quote(-1 + a)) # -1, a
#' split_terms(quote(-1 - a))
#'
#' split_terms(quote(a + b + c)) # a,b,c
#' split_terms(quote((a + b) + 1)) # (a+b),1
#' split_terms(quote((a + b) + 1), recursive = TRUE) # a,b,1
#' split_terms(quote((a - b) + 1), recursive = TRUE) # a,-b,1
#' split_terms(quote(-a)) # -a
#'
#' split_terms(quote(a - 1)) # a, -1
#' split_terms(quote(-a - 1)) # -a, -1
#' split_terms(quote(-(a + 1))) # -(a+1)
#' split_terms(quote(-(a + 1)), recursive = TRUE) # -a,-1
#'
#' split_terms(quote(---a))
#' split_terms(quote(-(a + (b - (c + d)))), recursive = TRUE)
#'
#'
#'
#' terms(quote(A + B))
#'
#' data(iris)
#' x <- terms(quote(. - Species), data = iris)
#'
#'
#'
#' toggle_sign(1:3)
#' toggle_sign(quote(a))
#' toggle_sign(quote(-a))
#'
#' exp <- expression(a, -b, -(a - b))
#' toggle_sign(exp)
#'
#'
#'
NULL


#' make formula
#'
#' @name make_formula
#' @rdname nonstandard_eval_formula
#' @param action action to apply in funtion:
#'         "expand","fixed_terms","fixed_vars",
#'         "interaction_terms","outcome","random_terms",
#'         "random_vars","remove_term",
#'         "remove_var","terms","vars"
#' @param frm formula
#' @param lhs lefthand side of formula
#' @param rhs righthand side of formula
#' @param env environment
#'
#' @export
make_formula <- function(lhs, rhs, env = parent.frame()) {
  if (is.character(rhs) | is.null(rhs)) {
    if(!is.character(lhs)){
      lhs <- lang2str(lhs)
    }

    rhs <- if (length(rhs) == 0) {
      "NULL"
    } else {
      rhs
    }
    if (!is_chr(rhs)) {
      rhs <- deparse_all(rhs)
    }
    string <- paste(lhs, "~", paste(rhs, collapse = "+"))
    return(as_frm(string, env = env))
  } else {
    if (is.null(lhs)) {
      f <- make_call("~", rhs)
    } else {
      if(is.character(lhs)){
        lhs <- str2lang(lhs)
      }
      f <- make_call("~", lhs, rhs)
    }

    return(structure(f,
                     class = "formula",
                     .Environment = env
    ))
  }
}


#' @export
create_frm_modify <-
  function(action = "expand",
           outcome = c("language", "character")) {
    .frm_outcome <- function(frm) {
      if (!is_operator(frm[[1]])) {
        cat_stop(frm[[1]], " does not appear to be an operator.")
      }

      if (is_twosided_frm(frm)) {
        frm[[2]]
      } else
        if (is_onesided_frm(frm)) {
          NULL
        }
    }
    .frm_random_terms <- function(frm) {
      frm <- as_frm(frm)
      possible_bars <- stringr::str_detect(
        .frm_terms(frm),
        stringr::fixed("|")
      )
      random_terms <- if (any(possible_bars)) {
        lme4::findbars(frm)
      } else {
        list()
      }
      map_chr(random_terms, rlang::expr_deparse)
    }
    .frm_terms <- function(frm) {
      frm <- as_frm(frm)
      labels(stats::terms(frm))
    }
    .frm_vars <- function(frm) {
      frm <- as_frm(frm)
      all.vars(.frm_righthand(frm))
    }
    .frm_righthand <- function(frm) {
      if (!is_operator(frm[[1]])) {
        cat_stop(frm[[1]], " does not appear to be an operator.")
      }

      if (is_twosided_frm(frm)) {
        frm[[3]]
      } else
        if (is_onesided_frm(frm)) {
          frm[[2]]
        }
    }

    action_opts <-
      c(
        expand = TRUE,
        fixed_terms = FALSE,
        fixed_vars = FALSE,
        interaction_terms = FALSE,
        outcome = FALSE,
        random_terms = FALSE,
        remove_term = TRUE,
        remove_var = TRUE,
        righthand = FALSE,
        terms = FALSE,
        vars = FALSE,
        random_vars = FALSE
      )
    outcome <- match.arg(outcome)
    action <-
      match.arg(action, names(action_opts))
    f_frm <- switch(action,
                    expand = function(frm) {
                      make_formula(.frm_outcome(frm), .frm_terms(frm), environment(frm))
                    },
                    fixed_terms = function(frm) {
                      frm <- as_frm(frm)
                      setdiff(.frm_terms(frm), .frm_random_terms(frm))
                    },
                    fixed_vars = function(frm) {
                      frm <- as_frm(frm)
                      possible_bars <- stringr::str_detect(
                        .frm_terms(frm),
                        stringr::fixed("|")
                      )
                      frm <- if (any(possible_bars)) {
                        lme4::nobars(frm)
                      } else {
                        frm
                      }
                      .frm_vars(frm)
                    },
                    interaction_terms = function(frm) {
                      frm <- as_frm(frm)
                      factors <- attr(stats::terms(frm), "factors")
                      multi_var_terms <- colSums(factors) > 1
                      colnames(factors)[multi_var_terms]
                    },
                    outcome = function(frm) {
                      deparse(.frm_outcome(frm))
                    },
                    random_terms = .frm_random_terms,
                    random_vars = function(frm) {
                      frm <- as_frm(frm)
                      possible_bars <- stringr::str_detect(
                        .frm_terms(frm),
                        stringr::fixed("|")
                      )
                      random_terms <- if (any(possible_bars)) {
                        lme4::findbars(frm)
                      } else {
                        list()
                      }
                      map(random_terms, all.vars) %>%
                        purrr::flatten_chr() %>%
                        unique()
                    },
                    remove_term = function(frm, term) {
                      rhs <- setdiff(.frm_terms(frm), term)
                      if (length(rhs) < 1) {
                        rhs <- "NULL"
                      }
                      make_formula(.frm_outcome(frm), rhs, environment(frm))
                    },
                    remove_var = function(frm, var) {
                      frm <- as_frm(frm)
                      factors <- attr(terms(frm), "factors")
                      if (var %in% rownames(factors)) {
                        var_in_term <- factors[var, ] > 0
                        to_remove <- colnames(factors)[var_in_term]
                      } else {
                        return(frm)
                      }
                      remaining_terms <-
                        setdiff(.frm_terms(frm), to_remove)
                      make_formula(.frm_outcome(frm), remaining_terms, environment(frm))
                    },
                    terms = .frm_terms,
                    vars = .frm_vars,
                    righthand = function(frm) {
                      deparse(.frm_righthand(frm))
                    },
                    cat_stop("'{action}' no es una accion de esta funcion")
    )
    if (action_opts[names(action_opts) %in% action]) {
      fun <- if (outcome == "character") {
        function(frm, ...) {
          frm <- f_frm(frm, ...)
          as_chr(frm)
        }
      } else {
        f_frm
      }
    } else {
      fun <- if (outcome == "character") {
        f_frm
      } else {
        function(frm, ...) {
          frm <- f_frm(frm, ...)
          if (length(frm) == 1) {
            str2lang(frm)
          } else {
            map(frm, str2lang)
          }
        }
      }
    }
    fun
  }

#' @export
frm_modify <-
  function(action = "expand",
           frm,
           ...,
           outcome = c("language", "character")) {
    outcome <- match.arg(outcome)
    action <-
      match.arg(
        action,
        c(
          "expand",
          "fixed_terms",
          "fixed_vars",
          "interaction_terms",
          "outcome",
          "random_terms",
          "random_vars",
          "remove_term",
          "remove_var",
          "terms",
          "vars",
          "righthand"
        )
      )
    fun <-create_frm_modify(action = action, outcome = outcome)
    fun(frm,...)
  }

#' @keywords internal
frm_string <- function(frm) {
  rlang::expr_deparse(as_frm(frm))
}
#' @export
frm_vars <-create_frm_modify(action = "vars",outcome = "language")

#' @export
frm_rhs_lenght <- function(frm){
  length(frm_modify(action = "terms",frm = frm))
}

#' @export
frm_rhs <- function(x, ...) {
  frm_rhs_expression <- function(x, ...) {
    ret <- vector("expression", length(x))
    for (i in 1:length(x)) {
      rh <- frm_rhs(x[[i]])
      if (!is.null(rh)) {
        ret[[i]] <- rh
      }
    }
    ret
  }

  if(is_l(x)){
    return(lapply(x, frm_rhs, ...))
  } else if(is_expr(x)){
    return(frm_rhs_expression(x))
  }

  rlang::f_rhs(x)
}
#' @export
`frm_rhs<-` <- function(x, value) {
  if (is_quosure(x)) {
    return(quo_set_expr(x, value))
  } else if (is_frm(x)) {
    x[[length(x)]] <- value
  } else if (is_call(x)) {
    x[[3]] <- value
  } else if (is_expr(x) | is_l(x)) {
    if (length(value) == 1) {
      for (i in 1:length(x)) {
        frm_rhs(x[[i]]) <- value
      }
    } else if (length(x) == length(value)) {
      for (i in 1:length(x)) {
        frm_rhs(x[[i]]) <- value[[i]]
      }
    } else {
      cat_warn("length of object != length of rhs replacement")
    }
  } else {
    cat_stop("no method defined")
  }
  x
}

#' @export
frm_lhs <- function(x, ...) {
  frm_lhs_expression <- function(x, ...) {
    ret <- vector("expression", length(x))
    for (i in 1:length(x)) {
      lh <- frm_lhs(x[[i]])
      if (!is.null(lh)) {
        ret[[i]] <- lh
      }
    }
    ret
  }

  if(is_l(x)){
    return(lapply(x, frm_lhs, ...))
  } else if(is_expr(x)){
    return(frm_lhs_expression(x))
  }

  rlang::f_lhs(x)
}
#' @export
`frm_lhs<-` <- function(x, value) {
  if (is_quosure(x)) {
    abort("Can't set the LHS of a quosure.")
  } else if (is_frm(x)) {
    if (length(x) < 3) {
      x <- duplicate(x)
      node_poke_cdr(x, pairlist(value, x[[2]]))
    } else {
      x[[2]] <- value
    }
  } else if (is_call(x)) {
    if (is_twosided_frm(x)) {
      x[[2]] <- value
    } else {
      x[[3]] <- x[[2]]
      x[[2]] <- value
    }
  } else if (is_expr(x) | is_l(x)) {
    if (length(value) == 1) {
      for (i in 1:length(x)) {
        frm_lhs(x[[i]]) <- value
      }
    } else if (length(x) == length(value)) {
      for (i in 1:length(x)) {
        frm_lhs(x[[i]]) <- value[[i]]
      }
    } else {
      cat_warn("length of object != length of lhs replacement")
    }
  } else {
    cat_stop("no method defined")
  }
  x
}


#' @export
get_vars <- function(x, ...) {
  UseMethod("get_vars")
}
#' @export
get_vars.default <- function(x) {
  generic_default()
}
#' @export
get_vars.formula <- function(x, data = NULL, ...) {
  vars.lhs <- get_vars(frm_lhs(x), data = data, ...)

  term.rhs <- terms.formula(x, data = data, ...)
  labels <- attr(term.rhs, "term.labels")
  order <- attr(term.rhs, "order")
  vars.rhs <- labels[order == 1]

  unique(c(vars.lhs, vars.rhs))
}
#' @export
get_vars.call <- function(x, data = NULL, ...) {
  term <- terms(x, data = data, ...)
  return(term)
}
#' @export
get_vars.expression <- function(x, ...) {
  all.vars(x, ...)
}
#' @export
get_vars.name <- function(x, ...) {
  lang2str(x)
}
#' @export
get_vars.NULL <- function(x, ...) {
  NULL
}


#' @export
invert <- function(x, ...) {
  UseMethod("invert")
}
#' @export
invert.default <- function(x) {
  generic_default()
}
#' @export
invert.call <- function(x) {
  o <- as.character(op(x))

  if (o %in% operators(types = "relational")) {
    op(x) <- as.name(env_data$opts_list[[o]][["inverse"]])
  } else {
    warning("No inverse found for op:", op(x))
  }

  return(x)
}
#' @export
invert.expression <- function(x) {
  for (i in 1:length(x)) {
    x[[i]] <- invert(x[[i]])
  }

  return(x)
}


.is_onesided_frm <- function(x, ...) {
  is.name(x[[1]]) &&
    deparse(x[[1]]) %in% c("~", "!") &&
    length(x) == 2
}
.is_onesided_frm.plural <- function(x, ...) {
  sapply(x, is_onesided_frm)
}
#' @export
is_onesided_frm <- function(x, ...) {
  UseMethod("is_onesided_frm")
}
#' @export
is_onesided_frm.default <- function(x) {
  generic_default()
}
#' @export
is_onesided_frm.formula <- .is_onesided_frm
#' @export
is_onesided_frm.call <- .is_onesided_frm
#' @export
is_onesided_frm.expression <- .is_onesided_frm.plural
#' @export
is_onesided_frm.list <- .is_onesided_frm.plural


#' @export
lhs_vars <- function(x, ...) {
  UseMethod("lhs_vars")
}
.lhs_vars <- function(x, ..., data = NULL) {
  if (class(x[[1]]) == "name" &&
      deparse(x[[1]]) %in% operators()) {
    get_vars(frm_lhs(x), ..., data = data)
  } else {
    cat_warn(paste("There is no relational operator defined for ", deparse(x)))
  }
}
#' @export
lhs_vars.default <- function(x) {
  generic_default()
}
#' @export
lhs_vars.formula <- .lhs_vars
#' @export
lhs_vars.call <- .lhs_vars
#' @export
lhs_vars.expression <- function(x, ...) {
  lapply(x, .lhs_vars, ...)
}

#' @export
rhs_vars <- function(x, ...) {
  UseMethod("rhs_vars")
}
.rhs_vars <- function(x, ..., data = NULL) {
  if (class(x[[1]]) == "name" &&
      deparse(x[[1]]) %in% operators()) {
    term.rhs <- terms(x, data = data, ...)
    labels <- attr(term.rhs, "term.labels")
    order <- attr(term.rhs, "order")
    vars.rhs <- labels[order == 1]

    vars.rhs
  } else {
    warning("There is no relational operator defined for ", deparse(x))
  }
}
#' @export
rhs_vars.default <- function(x) {
  generic_default()
}
#' @export
rhs_vars.formula <- .rhs_vars
#' @export
rhs_vars.call <- .rhs_vars
#' @export
rhs_vars.expression <- function(x, ...) {
  lapply(x, .rhs_vars, ...)
}

.is_twosided_frm.plural <- function(x, ...) {
  sapply(x, .is_twosided_frm)
}
.is_twosided_frm <- function(x, ...) {
  is.name(x[[1]]) &&
    deparse(x[[1]]) %in% operators() &&
    length(x) == 3
}
#' @export
is_twosided_frm <- function(x, ...) {
  UseMethod("is_twosided_frm")
}
#' @export
is_twosided_frm.default <- function(x) {
  generic_default()
}
#' @export
is_twosided_frm.formula <- .is_twosided_frm
#' @export
is_twosided_frm.call <- .is_twosided_frm
#' @export
is_twosided_frm.expression <- .is_twosided_frm.plural
#' @export
is_twosided_frm.list <- .is_twosided_frm.plural


#' @export
toggle_sign <- function(x) {
  UseMethod("toggle_sign")
}
#' @export
toggle_sign.default <- function(x) {
  -x
}
#' @export
`toggle_sign.(` <- function(x) {
  toggle_sign(x[[2]])
}
#' @export
toggle_sign.call <- function(x) {
  elem.deparsed <- deparse(x)
  if (substr(elem.deparsed, 1, 1) == "-") {
    substr(elem.deparsed, 1, 1) <- " "
  } else {
    elem.deparsed <- paste0("-", elem.deparsed)
  }

  parse(text = elem.deparsed)[[1]]
}
#' @export
toggle_sign.name <- function(x) {
  elem.deparsed <- deparse(x)
  if (substr(elem.deparsed, 1, 1) == "-") {
    substr(elem.deparsed, 1, 1) <- " "
  } else {
    elem.deparsed <- paste0("-", elem.deparsed)
  }

  parse(text = elem.deparsed)[[1]]
}
#' @export
toggle_sign.expression <- function(x) {
  for (i in 1:length(x)) {
    elem.deparsed <- deparse(x[[i]])

    # Alrea
    if (substr(elem.deparsed, 1, 1) == "-") {
      substr(elem.deparsed, 1, 1) <- " "
    } else {
      elem.deparsed <- paste0("-", elem.deparsed)
    }

    x[[i]] <- parse(text = elem.deparsed)[[1]]
  }

  x
}


#' @export
op <- function(x) {
  UseMethod("op")
}
#' @export
op.default <- function(x) {
  generic_default()
}
#' @export
op.formula <- function(x) {
  x[[1]]
}
#' @export
op.call <- function(x) {
  x[[1]]
}
#' @export
op.name <- function(x) {
  if (as.character(x) %in% operators("ALL")) {
    return(x)
  }
}
#' @export
op.expression <- function(x) {
  ret <- vector("expression", length(x))
  for (i in 1:length(x)) {
    o <- op(x[[i]])
    if (!is.null(op)) {
      ret[[i]] <- o
    }
  }
  ret
}
#' @export
op.list <- function(x) {
  lapply(x, op)
}
#' @export
`op<-` <- function(x, value) {
  op_call <- function(x, value) {
    x[[1]] <- as.name(value)
    x
  }
  op_formula <- function(x, value) {
    new.op <- as.name(value)

    if (new.op == op(x)) {
      return(x)
    }

    if (as.character(value) %in% operators("ALL")) {
      c <- quote(x == y) # generic call object
      frm_lhs(c) <- frm_lhs(x)
      op(c) <- new.op
      frm_rhs(c) <- frm_rhs(x)
    } else {
      stop(value, " was not found as an operator.")
    }

    c
  }
  .replace.op.plural <- function(x, value) {
    if (length(value) == 1) {
      for (i in 1:length(x)) {
        op(x[[i]]) <- as.name(value)
      }
    } else if (length(x) == length(value)) {
      for (i in 1:length(x)) {
        op(x[[i]]) <- as.name(value[[i]])
      }
    } else {
      warning("length of object != length of op replacement")
    }

    x
  }

  switch(class(x),
         call = op_call(x, value),
         formula = op_formula(x, value),
         expression = .replace.op.plural(x, value),
         list = .replace.op.plural(x, value)
  )
}

#' @export
op_type <- function(x) {
  UseMethod("op_type")
}
#' @export
op_type.default <- function(x) {
  operator_type(op(x))
}
#' @export
op_type.expression <- function(x) {
  lapply(x, function(x) {
    operator_type(op(x))
  })
}
#' @export
op_type.list <- function(x) {
  lapply(x, function(x) {
    operator_type(op(x))
  })
}


#' @export
split_terms <- function(x, recursive = FALSE) {
  UseMethod("split_terms")
}
#' @export
split_terms.default <- function(x, recursive = FALSE) {
  return(as.expression(x))
}
#' @export
split_terms.name <- function(x, recursive = FALSE) {
  return(as.expression(x))
}
#' @export
`split_terms.(` <- function(x, recursive = FALSE) {
  if (recursive) {
    ret <- split_terms(x[[2]], recursive)
  } else {
    ret <- as.expression(x)
  }

  ret
}
#' @export
split_terms.call <- function(x, recursive = FALSE) {
  if (length(x) == 1) {
    return(as.expression(x))
  }

  if (!as.character(op(x)) %in% c("+", "-", "(")) {
    return(as.expression(x))
  }

  if (length(x) == 2 & as.character(x[[1]]) %in% c("+", "-")) {
    e <- split_terms(x[[2]], recursive = recursive)
    if (op(x) == "-") {
      e <- toggle_sign(e)
    }
    return(e)
  }


  expr <- expression()
  for (i in 2:length(x)) {
    if (recursive == TRUE &&
        length(x[[i]]) > 1 &&
        x[[i]][[1]] == "(") {
      e <- split_terms(x[[i]][[2]], recursive = recursive)
      if (x[[1]] == "-") {
        e <- toggle_sign(e)
      }
      expr <- append(expr, e)
    } else {
      e <- x[[i]]
      if (i == 3 && op(x) == "-") {
        e <- toggle_sign(e)
      }
      if (recursive == TRUE ||
          x[[i]] != "(") {
        e <- split_terms(e, recursive = recursive)
      }
      expr <- append(expr, e)
    }
  }
  expr
}

#' @method terms expression
#' @export
terms.expression <- function(x, ...) {
  lapply(x, terms, ...)
}
#' @export
terms.call <- function(x, ...) {
  if (deparse(x[[1]]) %in% c(operators())) {
    all.vars(x)
  } else {
    form <- stats::formula(paste("~", as.expression(x)))
    stats::terms(form, ...)
  }
}


#' Create a list of formulas
#'
#' @rdname nonstandard_eval_formula
#'
#' @param .response A one-sided formula used as the left-hand side of
#'   all resulting formulas.
#' @param ... List of formulas whose right-hand sides will be merged
#'   to `.response`.
#' @export
#' @examples
#' # Provide named arguments to create a named list of formulas:
#' data <- iris
#' names(data) <- c("var1", "var2", "var3", "var4", "lhs")
#' models <- formulas(~lhs,
#'   additive = ~ var1 + var2,
#'   all = ~.,
#'   interaction = ~ var1 * var2,
#'   .data = data
#' )
#' models
#'
#' # The formulas are created sequentially, so that you can refer to
#' # previously created formulas:
#' formulas(~lhs,
#'   linear = ~ var1 + var2,
#'   hierarchical = add_predictors(linear, ~ (1 | group))
#' )
formulas <- function(.response, ..., .data = NULL) {
  find_dot_formula <- function(f_list) {
    f_list <- purrr::map(f_list, rlang::f_rhs)
    purrr::map_lgl(f_list, ~ all(as.character(.x) == "."))
  }
  f_list <- tibble::lst(...)

  .response <- as_lazy(.response)
  validate_formulas(.response, f_list)

  if (!identical(.response$env, emptyenv())) {
    if (!is_empty(.data)) {
      .response$env <- to_env(.data)
    }
  }
  out <- purrr::map(f_list, set_lhs, .response)
  dot_f <- find_dot_formula(out)
  if (any(dot_f)) {
    if (is_empty(.data)) {
      cat_stop("cuando existen funciones '~ .' se debe agregar la data")
    }

    out[dot_f] <- purrr::map(out[dot_f], function(w, .data) {

      w <- rlang::new_formula(rlang::f_lhs(w), quote(.), env = to_env(.data))
      as_frm(model.frame(formula = w, .data))
    }, .data = .data)
  }
  out
}

#' Add predictors to a formula
#'
#' This merges a one- or two-sided formula `f` with the
#' right-hand sides of all formulas su pplied in dots
#'
#' @rdname nonstandard_eval_formula
#'
#' @param f A formula.
#' @param ... Formulas whose right-hand sides will be merged to
#'   `f`.
#' @param fun A function name indicating how to merge the right-hand
#'   sides.
#' @export
#' @examples
#' f <- lhs ~ rhs
#' add_predictors(f, ~var1, ~var2)
#'
#' # Left-hand sides are ignored:
#' add_predictors(f, lhs1 ~ var1, lhs2 ~ var2)
#'
#' # fun can also be set to a function like "*":
#' add_predictors(f, ~var1, ~var2, fun = "*")
add_predictors <- function(f, ..., fun = "+") {
  rhss <- purrr::map(list(f, ...), f_zap_lhs)
  rhs <- purrr::reduce(.x = rhss,.f =  merge_formulas, fun = fun)
  env <- merge_envs(f, rhs)
  rlang::new_formula(rlang::f_lhs(f), rlang::f_rhs(rhs), env)
}

merge_formulas <- function(f1, f2, fun = "+") {
  rhs <- call(fun, rlang::f_rhs(f1), rlang::f_rhs(f2))
  lhss <- cmp_map(list(f1, f2), rlang::f_lhs)

  if (length(lhss) == 0) {
    lhs <- NULL
  } else {
    lhs <- reduce_common(lhss, "Left-hand sides must be identical")
  }

  env <- merge_envs(f1, f2)
  rlang::new_formula(lhs, rhs, env)
}
merge_envs <- function(f1, f2) {
  symbols_f1 <- find_symbols(f1)
  symbols_f2 <- find_symbols(f2)
  conflicts <- intersect(symbols_f1, symbols_f2)
  conflicts_envs <- cmp_map(
    conflicts, find_env_conflicts,
    f1, f2
  )
  all_symbols <- union(symbols_f1, symbols_f2)
  nonconflicts <- setdiff(all_symbols, conflicts)
  nonconflicts_envs <- purrr::compact(purrr::map(nonconflicts, find_env_nonconflicts, f1, f2))
  nonconflicts_envs <-

    all_envs <- c(conflicts_envs, nonconflicts_envs)
  if (length(all_envs) == 0) {
    environment(f1)
  } else {
    reduce_common(
      all_envs,
      "Cannot merge formulas as their scopes conflict across symbols"
    )
  }
}
find_env_conflicts <- function(symbol, f1, f2) {
  env1 <- find_binding_env(symbol, environment(f1))
  env2 <- find_binding_env(symbol, environment(f2))
  if (is.null(env1) || is.null(env2)) {
    return(env1 %||% env2)
  }
  if (!identical(env1, env2)) {
    cat_stop("Cannot merge formulas as their scopes conflict for the symbol '{symbol}'")
  }
  env1
}
find_env_nonconflicts <- function(symbol, f1, f2) {
  env1 <- find_binding_env(symbol, environment(f1))
  env2 <- find_binding_env(symbol, environment(f2))
  env1 %||% env2
}
find_binding_env <- function(symbol, env) {
  if (exists(symbol, envir = env)) {
    env
  } else if (!identical(env, emptyenv())) {
    find_binding_env(symbol, parent.env(env))
  } else {
    NULL
  }
}
find_symbols <- function(lang) {
  if (is.call(lang)) {
    find_symbols_call(lang)
  } else if (is.symbol(lang)) {
    as_chr(lang)
  } else {
    character(0)
  }
}
find_symbols_call <- function(lang) {
  fun_name <- as_chr(lang[[1]])
  if (fun_name %in% c("$", "@")) {
    as_chr(lang[[2]])
  } else if (fun_name %in% c("::", ":::")) {
    character(0)
  } else {
    res <- map(as_l(lang[-1]), find_symbols)
    purrr::flatten_chr(res)
  }
}
f_zap_lhs <- function(f) {
  make_formula(NULL, rlang::f_rhs(f), env = env_frm(f))
}
reduce_common <-
  function(x, msg = "Objects must be identical", operator = identical) {
    purrr::reduce(x, function(.x, .y) {
      if (!operator(.x, .y)) {
        cat_stop(msg)
      }
      .y
    })
  }
set_lhs <- function(f, lhs) {
  if (is_lazy(lhs)) {
    env_frm(lhs$expr) <- lhs$env
    lhs <- lhs$expr
  }
  env <- merge_envs(lhs, f)
  rlang::new_formula(rlang::f_rhs(lhs), rlang::f_rhs(f), env)
}
validate_formulas <- function(response, formulas) {
  if (is_lazy(response)) {
    response <- response$expr
  }

  if (!is_formula(response) || length(response) != 2) {
    cat_stop(".response must be a one-sided formula")
  }
  if (!length(formulas)) {
    cat_stop("No formula provided")
  }
  map(formulas, function(f) {
    if (!is_formula(f)) {
      cat_stop("'...' must contain only formulas")
    }
  })
  invisible()
}


#' @rdname nonstandard_eval_formula
#' @export
frm2fn <- function(x,env = parent.frame()){
  body <- rlang::f_rhs(x)
  args_sym <-  frm_modify(action = "vars",frm = x)
  args_chr <-  frm_modify(action = "vars",frm = x,outcome = "character")
  args <- as_prls(setNames(as.list(args_sym),args_chr))
  make_function(args = args,body = body,env = env)
}
