# lazyquo


#' Convert an object to a lazy expression or lazy dots.
#'
#' @name datatools
#' @rdname datatools
#'
#' @param x An R object.
#' @param env Environment to use for objects that don't already have
#'   associated environment.
#' @export
#' @examples
#'
#' library(rlang)
#' #'
#' env <- child_env(current_env())
#' lazy <-
#'   structure(list(expr = quote(foo(bar)), env = env), class = "lazy")
#' lazyquo(lazy)
#' lazy_str <- "foo(bar)"
#' lazyquo(lazy_str)
#' lazy_lang <- quote(foo(bar))
#' lazyquo(lazy_lang)
#' lazy_sym <- quote(foo)
#' lazyquo(lazy_sym)
#'
#'
#' env <- child_env(current_env())
#' lazy_dots <-
#'   structure(class = "lazy_dots", list(
#'     lazy = structure(list(expr = quote(foo(
#'       bar
#'     )), env = env), class = "lazy"),
#'     lazy_lang = quote(foo(bar))
#'   ))
#' expected <-
#'   list(
#'     lazy = new_quosure(quote(foo(bar)), env),
#'     lazy_lang = quo(foo(bar)),
#'     quo(foo(bar))
#'   )
#' lazyquos(lazy_dots, current_env(), "foo(bar)")
#'
#'
#' lazy_dots <-
#'   structure(class = "lazy_dots", list("foo(baz)", quote(foo(bar))))
#' expected <-
#'   list(
#'     `foo(baz)` = quo(foo(baz)),
#'     `foo(bar)` = quo(foo(bar)),
#'     foobarbaz = quo(foo(barbaz))
#'   )
#' dots <-
#'   lazyquos(lazy_dots,
#'     current_env(),
#'     foobarbaz = "foo(barbaz)",
#'     .named = TRUE
#'   )
#' dots
#'
#'
#' lazyquo()
#'
#'
#' lazy <-
#'   structure(list(expr = quote(foo), env = empty_env()), class = "lazy")
#' lazyquos(lazy)
#'
#'
#' lazyquos(quote(foo), empty_env())
#' lazyquos(quote(foo(bar)), empty_env())
#'
#'
#' lazyquos(NULL, current_env(), c("foo", "bar"))
#'
#'
#' lazyquos(NULL, current_env(), NA_real_)
#' lazyquos(NULL, current_env(), 1:3)
#'
#'
#' lazyquo(~foo)
#' lazyquos(~foo)
#'
#' @export
lazyquo <- function(x, env = env_call(), warn = FALSE) {
  if (missing(x)) {
    return(quo())
  }
  if (is_quosure(x)) {
    return(x)
  }
  if (is_formula(x)) {
    return(as_quosure(x, env))
  }

  out <- switch(typeof(x),
    symbol = ,
    language = new_quosure(x, env),
    character = {
      if (warn) {
        warn_text_se()
      }
      parse_quo(x[[1]], env)
    },
    logical = ,
    integer = ,
    double = {
      if (length(x) > 1) {
        warn("Truncating vector to length 1")
        x <- x[[1]]
      }
      new_quosure(x, env)
    },
    list =
      if (inherits(x, "lazy")) {
        new_quosure(x$expr, x$env)
      } else {
        new_quosure(x,env)
      }
  )

  if (is_null(out)) {
    new_quosure(x,env)
  } else {
    out
  }
}
#' @export
lazyquos <- function(dots, env = env_call(), ..., .named = FALSE) {
  if (missing(dots)) {
    dots <- list()
  }
  dots <- if (is_langatom(dots)) {
    dots <- list(dots)
  } else {
    dots <- unclass(dots)
  }
  dots <- c(dots, list(...))

  warn <- TRUE
  for (i in seq_along(dots)) {
    dots[[i]] <- lazyquo(x = dots[[i]],env =  env,warn =  warn)
    warn <- FALSE
  }

  named <- have_name(dots)
  if (.named && any(!named)) {
    nms <-
      vapply(dots[!named], function(x)
        expr_text(get_expr(x)), character(1))
    names(dots)[!named] <- nms
  }

  names(dots) <- names2(dots)
  new_quosures(dots)
}

warn_text_se <- function() {
  return(NULL)
  warn("Text parsing is deprecated, please supply an expression or formula")
}

# quoted

#' @export
. <- function(..., .env = parent.frame()) {
  dots <- as_l(match.call())
  dots[[1]] <- NULL
  dots$.env <- NULL
  structure(as.list(dots), env = .env, class = "quoted")
}

#' @export
as_quoted <- function(x, env = parent.frame()) {
  UseMethod("as_quoted")
}
#' @export
as_quoted.call <- function(x, env = parent.frame()) {
  structure(as.list(x)[-1], env = env, class = "quoted")
}
#' @export
as_quoted.character <- function(x, env = parent.frame()) {
  structure(lapply(x, function(x) {
    parse(text = x)[[1]]
  }),
  env = env,
  class = "quoted"
  )
}
#' @export
as_quoted.numeric <- function(x, env = parent.frame()) {
  structure(x, env = env, class = c("quoted", "numeric"))
}
#' @export
as_quoted.formula <- function(x, env = parent.frame()) {
  simplify <- function(x) {
    if (length(x) == 2 && x[[1]] == as.name("~")) {
      return(simplify(x[[2]]))
    }
    if (length(x) < 3) {
      return(list(x))
    }
    op <- x[[1]]
    a <- x[[2]]
    b <- x[[3]]

    if (op == as.name("+") ||
      op == as.name("*") || op == as.name("~")) {
      c(simplify(a), simplify(b))
    } else if (op == as.name("-")) {
      c(simplify(a), bquote(-.(x), list(x = simplify(b))))
    } else {
      list(x)
    }
  }

  structure(simplify(x), env = env, class = "quoted")
}
#' @export
as_quoted.quoted <- function(x, env = parent.frame()) {
  x
}
#' @export
as_quoted.NULL <- function(x, env = parent.frame()) {
  structure(list(), env = env, class = "quoted")
}
#' @export
as_quoted.name <- function(x, env = parent.frame()) {
  structure(list(x), env = env, class = "quoted")
}
#' @export
as_quoted.factor <- function(x, env = parent.frame()) {
  as_quoted(as.character(x), env)
}
#' @export
c.quoted <- function(..., recursive = FALSE) {
  structure(NextMethod("c"),
    class = "quoted",
    env = attr(list(...)[[1]], "env")
  )
}
#' @export
`[.quoted` <- function(x, i, ...) {
  structure(NextMethod("["), env = attr(x, "env"), class = "quoted")
}
#' @export
print.quoted <- function(x, ...) {
  utils::str(x)
}
#' @export
names.quoted <- function(x) {
  deparse2 <- function(x) {
    paste(deparse(x), collapse = "")
  }
  part_names <- unlist(lapply(x, deparse2))
  user_names <- names(unclass(x))

  if (!is.null(user_names)) {
    part_names[user_names != ""] <- user_names[user_names != ""]
  }

  unname(part_names)
}

# lazy

#' Convert an object to a lazy expression or lazy dots.
#'
#' @name datatools
#' @rdname datatools
#'
#' @param x An R object.
#' @param env Environment to use for objects that don't already have
#'   associated environment.
#' @examples
#'
#'
#' # lazy_dots(x = 1)
#' # lazy_dots(a, b, c * 4)
#'
#' # f <- function(x = a + b, ...) {
#' #  lazy_dots(x = x, y = a + b, ...)
#' # }
#' # f(z = a + b)
#' lazy_dots(letters)
#'
#' # You can also modify a dots like a list. Anything on the RHS will
#' # be coerced to a lazy.
#' l <- lazy_dots(x = 1)
#' l$y <- quote(f)
#' l[c("y", "x")]
#' l["z"] <- list(~g)
#' # c(lazy_dots(x = 1), lazy_dots(f))
#'
#' lz_expr <- as_call("mean(x)")
#' lz <- as_lazy(lz_expr)
#' lz
#' x <- runif(10)
#' eval_lazy(expr = lz, data = list(x = x))
#'
#' lz <- as_lazy(letters[1:4])
#' x <- letters[1:4]
#' eval_lazy(expr = lz, data = list(x = x))
#'
#' lz <- as_lazy(~ y + x * 2)
#' y <- runif(min = -1, max = 2, n = 10)
#' x <- runif(10)
#' eval_lazy(expr = lz, data = list(x = x, y = y))
#'
#' lz <- as_lazy(lz)
#' eval_lazy(expr = lz, data = list(x = x, y = y))
#'
#' lz <- as_lazy(TRUE)
#' eval_lazy(expr = lz)
#'
#' lz <- as_lazy(x)
#' eval_lazy(expr = lz)
#'
#' lz <- as_lazy(NULL)
#' eval_lazy(expr = lz)
#'
#' lz <- as_lazy(2)
#' eval_lazy(expr = lz)
#'
#' lz <- quo(x + y)
#' lz <- as_lazy(lz)
#' eval_lazy(expr = lz)
#'
#' lz <- .(x, y)
#' lz <- as_lazy(lz)
#' eval_lazy(expr = lz)
#'
#' lazy(quote(a + x), globalenv())
#' f <- function(x = b - a) {
#'   x <- substitute(x)
#'   lazy(x)
#' }
#' f()
#' f(a + b / c)
#' lazy(quote(a + b / c))
#' g <- function(y) f(y)
#' h <- function(z) g(z)
#' f(quote(a + b))
#' g(quote(a + b))
#' h(quote(a + b))
#'
#' @keywords internal
NULL

#' @export
lazy <- function(lazy, env = env_call()) {
  as_lazy(x = lazy, env = env)
}
#' @export
as_lazy <- function(x, env = env_call()) {
  UseMethod("as_lazy")
}
#' @export
as_lazy.default <- function(x, env = env_call()) {
  x <- enexpr(x)
  structure(list(expr = x, env = env), class = "lazy")
}
#' @export
as_lazy.lazy <- function(x, env = env_call()) {
  x
}
#' @export
as_lazy.numeric <- function(x, env = env_call()) {
  expr <- as_quosure(enquo(x), env = env)
  expr <- get_expr(expr)
  structure(list(expr = expr, env = env), class = "lazy")
}
#' @export
as_lazy.formula <- function(x, env = env_call()) {
  env <- env_get_safe(x, env)
  structure(list(expr = get_expr(rlang::new_quosure(x, env)), env = env), class = "lazy")
}
#' @export
as_lazy.function <- function(x, env = env_call()) {
  env <- env_get_safe(x, env)
  structure(list(expr = x, env = env), class = "lazy")
}
#' @export
as_lazy.call <- function(x, env = env_call()) {
  env <- env_get_safe(x, env)
  structure(list(expr = x, env = env), class = "lazy")
}
#' @export
as_lazy.character <- as_lazy.numeric
#' @export
as_lazy.name <- function(x, env = env_call()) {
  env <- env_get_safe(x, env)
  structure(list(expr = x, env = env), class = "lazy")
}
#' @export
as_lazy.quoted <- function(x, env = attr(x, "env")) {
  env <- attr(x, "env")
  x <- expr_get_quotable(x)
  as_lazy(x = x, env = env)
}
#' @export
as_lazy.list <- function(x, env = env_call()) {
  x <- eval(substitute_w(x),envir = env)
  structure(map(x, ~ as_lazy(x = .x, env = env)), class = "lazy_dots")
}
#' @export
as_lazy.quosure <- function(x, env = env_call()) {
  env <- attr(x, ".Environment")

  x <- expr_get_quotable(x)
  structure(list(expr = x, env = env), class = "lazy")
}
#' @export
as_lazy.quosures <- function(x, env = env_call()) {
  lzclean(map(x, as_lazy, env = env))
}
#' @export
as_lazy.NULL <- function(x, env = emptyenv()) {
  quote(NULL)
}
#' @export
as_lazy.logical <- as_lazy.numeric

lzclean <- function(x) {
  lgl <-
    map_lgl(x, ~ inherits(.x, "lazy_dots") |
      all(names(.x) != c("expr", "env")))

  if (sum(lgl) == 0) {
    return(x)
  }

  if (sum(lgl) == 1) {
    xl <- unclass(x[[which(lgl)]])
  } else {
    xl <- map(x[which(lgl)], unclass)

    xl <- Reduce(f = "c", x = xl)
  }

  if (sum(!lgl) == 1) {
    xq <- x[[which(!lgl)]]
  } else {
    xq <- x[which(!lgl)]
  }

  c(xl, xq)
}

#' @export
print.lazy <- function(x, ...) {
  sty_bold <- function(x) {
    cat_cumstyle(
      text = x,
      style = "bold",
      color = "silver"
    )
  }
  code <- deparse(x$expr)
  if (length(code) > 1) {
    code <- paste(code[[1]], "...")
  }

  cat_title_head("<lazy>\n")
  cat(cat_cumstyle(
    text = paste(sty_bold("  expr: "), code, "\n", sep = ""),
    color = "none"
  ))
  cat(cat_cumstyle(
    text = paste(sty_bold("  env:  "), format(x$env), "\n", sep = ""),
    color = "none"
  ))
}

#' @export
as_lazy_dots <- function(..., env) UseMethod("as_lazy_dots")
#' @export
lazy_dots <- function(...,env = env_call(), .named = FALSE) {
  x <- enexprs(...,.named = .named)
  structure(map(x, ~ as_lazy(x = .x, env = env)), class = "lazy_dots")
}
#' @export
as_lazy_dots.NULL <- function(..., env = env_call()) {
  structure(list(), class = "lazy_dots")
}
#' @export
as_lazy_dots.default <- function(..., env = env_call()) {
  x <- enexprs(...,.named = TRUE)
  structure(map(x, ~ as_lazy(x = .x, env = env)), class = "lazy_dots")
}
#' @export
as_lazy_dots.list <- function(..., env = env_call()) {
  structure(map(c(...), ~ as_lazy(x = .x, env = env)), class = "lazy_dots")
}
#' @export
as_lazy_dots.lazy_dots <- function(..., env = env_call()) {
  c(...)
}
#' @export
as_lazy_dots.lazy <- function(x, env = env_call()) {
  structure(list(x),class = "lazy_dots")
}
#' @export
as_lazy_dots.numeric <- function(..., env = env_call()) {
  expr <- as_quosure(enquo(...), env = env)
  expr <- get_expr(expr)
  expr <- structure(list(expr = expr, env = env), class = "lazy")
  structure(list(expr),class = "lazy_dots")
}
#' @export
as_lazy_dots.logical <- as_lazy_dots.numeric
#' @export
as_lazy_dots.character <- as_lazy_dots.numeric
#' @export
as_lazy_dots.formula <- function(..., env = env_call()) {
  env <- env_get_safe(..., env)
  expr <-  structure(list(expr = get_expr(rlang::new_quosure(..., env)), env = env), class = "lazy")
  structure(list(expr),class = "lazy_dots")
}
#' @export
as_lazy_dots.function <- function(x, env = env_call()) {
  env <- env_get_safe(..., env)
  expr <- structure(list(expr = get_expr(rlang::new_quosure(..., env)), env = env), class = "lazy")
  structure(list(expr),class = "lazy_dots")
}
#' @export
as_lazy_dots.call <- function(..., env = env_call()) {
  env <- env_get_safe(..., env)
  expr <- structure(list(expr = get_expr(rlang::new_quosure(..., env)), env = env), class = "lazy")
  structure(list(expr),class = "lazy_dots")
}
#' @export
as_lazy_dots.name <- function(..., env = env_call()) {
  env <- env_get_safe(..., env)
  expr <- structure(list(expr = get_expr(rlang::new_quosure(..., env)), env = env), class = "lazy")
  structure(list(expr),class = "lazy_dots")
}
#' @export
as_lazy_dots.quoted <- function(..., env = attr(x, "env")) {
  env <- attr(..., "env")
  x <- expr_get_quotable(...)
  expr <- as_lazy(x = x, env = env)
  structure(list(expr),class = "lazy_dots")
}


#' @export
`[.lazy_dots` <- function(x, i) {
  structure(NextMethod(), class = "lazy_dots")
}
#' @export
`$<-.lazy_dots` <- function(x, i, value) {
  value <- as_lazy(value, parent.frame())
  x <- unclass(x)
  x[[i]] <- value
  structure(x, class = "lazy_dots")
}
#' @export
`[<-.lazy_dots` <- function(x, i, value) {
  value <- lapply(value, as_lazy, env = parent.frame())
  NextMethod()
}
#' @export
c.lazy_dots <- function(..., recursive = FALSE) {
  structure(NextMethod(), class = "lazy_dots")
}
#' @export
print.lazy_dots <- function(x, ...) {
  .print_lazy <- function(x, ...) {
    sty_bold <- function(x) {
      cat_cumstyle(
        text = x,
        style = "bold",
        color = "silver"
      )
    }
    code <- deparse(x$expr)
    if (length(code) > 1) {
      code <- paste(code[[1]], "...")
    }

    cat_subtitle1("<lazy>\n")
    cat(cat_cumstyle(
      text = paste(sty_bold("  expr: "), code, "\n", sep = ""),
      color = "none"
    ))
    cat(cat_cumstyle(
      text = paste(sty_bold("  env:  "), format(x$env), "\n", sep = ""),
      color = "none"
    ))
  }
  cat_title_head("'<lazy_dots>\n")

  for (i in seq_along(x)) {
    cat("\n")
    .print_lazy(x[[i]])
    cat("\n")
  }
}

# rec_expr

#' Construct a sum expression
#'
#' @name  datatools
#' @rdname datatools
#'
#' @param expr an expression that can be expanded to a sum
#' @param ... bind variables in expr using dots. See examples.
#'
#' @examples
#' # create a sum from x_1 to x_10
#' rec_expr(x[i], i = 1:10)
#' # create a sum from x_2 to x_10 with even indexes
#' rec_expr(x[i], i = 1:10, i %% 2 == 0)
#' @export
rec_expr <- function(expr, ...) {
  classify_quantifiers <- function(dots) {
    assignments <- names(dots) != ""
    list(
      quantifiers = dots[assignments],
      filters = dots[!assignments]
    )
  }
  build_quantifier_candidates <-
    function(subs, subs_names, filter_dots) {
      candidates <- expand.grid(subs, stringsAsFactors = FALSE)
      colnames(candidates) <- subs_names
      if (length(filter_dots) > 0) {
        filter <- Reduce(function(acc, x) {
          rlang::quo(!!(acc) & (!!rlang::as_quosure(x$expr, x$env)))
        }, filter_dots, init = TRUE)
        filtered_candidates <-
          rlang::quo(candidates[(!!filter), , drop = FALSE])
        candidates <-
          rlang::eval_tidy(filtered_candidates, data = candidates)
      }
      rownames(candidates) <- NULL
      candidates
    }

  ast <- substitute(expr)
  stopifnot(is_call(ast))
  dots <- lapply(dots(...), as_lazy)

  class_quan <- classify_quantifiers(dots)
  class_quan

  bound_subscripts <- lapply(class_quan$quantifiers, eval_lazy)
  subs_comb <- build_quantifier_candidates(
    bound_subscripts,
    names(bound_subscripts),
    class_quan$filters
  )

  if (nrow(subs_comb) == 0) {
    return(substitute(0))
  }
  list_of_eval_exps <- apply(subs_comb, 1, function(row) {
    binding_env <- as.environment(as_l(row))
    try_eval_exp_rec(ast, binding_env)
  })
  Reduce(f = function(acc, el) {
    if (is.null(acc)) {
      return(el)
    }
    substitute(x + y, list(x = acc, y = el))
  }, x = list_of_eval_exps, init = NULL)
}

#' @keywords internal
try_eval_exp <- function(ast, envir = parent.frame()) {
  return_null <- function(x) {
    NULL
  }
  result <- tryCatch(
    eval(ast, envir = envir),
    error = return_null
  )
  if (!is.numeric(result)) {
    ast
  } else {
    result
  }
}
#' @keywords internal
try_eval_exp_rec <- function(base_ast, envir = parent.frame()) {
  on_element <-
    function(push,
             inplace_update_ast,
             get_ast_value,
             element) {
      path <- element$path
      ast <-
        if (is.null(element$ast)) {
          get_ast_value(path)
        } else {
          element$ast
        }
      stop_traversal <- element$stop_traversal
      is_final_sum_expr_call <- element$is_final_sum_expr_call
      exclude_vars <- element$exclude_vars
      if (is.null(stop_traversal)) {
        stop_traversal <- FALSE
      }
      if (is.null(is_final_sum_expr_call)) {
        is_final_sum_expr_call <- FALSE
      }
      if (is.null(exclude_vars)) {
        exclude_vars <- ""
      }
      if (!is.call(ast)) {
        if (is_name(ast) &&
          !as_chr(ast) %in% exclude_vars) {
          new_ast_eval <- try_eval_exp(ast, envir)
          if (is.numeric(new_ast_eval)) {
            inplace_update_ast(path, new_ast_eval)
          }
        }
      } else if (is.call(ast)) {
        if (as_chr(ast[[1]]) == "sum_expr") {
          if (!is_final_sum_expr_call) {
            push(list(
              path = path,
              is_final_sum_expr_call = TRUE
            ))
            free_vars <- free_indexes_rec(ast[[2]])
            free_vars <- free_vars[names(envir)]
            for (i in 3:length(ast)) {
              new_element <- list(
                ast = ast[[i]],
                path = c(path, i),
                exclude_vars = free_vars
              )
              push(new_element)
            }
          } else {
            # this expands the sum_expr expression
            # and triggers a reevaluation
            expanded_ast <- eval(ast)
            inplace_update_ast(path, expanded_ast)
            push(list(ast = expanded_ast, path = path))
          }
        } else if (!stop_traversal) {
          # we need to revisit the same node after the updates
          push(list(
            ast = ast,
            path = path,
            stop_traversal = TRUE
          ))
          for (i in 2:length(ast)) {
            new_element <- list(
              ast = ast[[i]],
              path = c(path, i),
              exclude_vars = exclude_vars
            )
            push(new_element)
          }
        } else {
          new_ast_eval <- try_eval_exp(ast, envir)
          if (is.numeric(new_ast_eval)) {
            inplace_update_ast(path, new_ast_eval)
          }
        }
      } else {
        cat_stop("Does not compute.")
      }
    }
  ast_walker(base_ast, on_element)
}
#' @keywords internal
free_indexes <- function(expr) {
  if (expr[[1]] == "[" && length(expr) >= 3) {
    vars <- vapply(3:length(expr), function(i) {
      x <- expr[[i]]
      if (is.name(x)) {
        as_chr(x)
      } else {
        NA_character_
      }
    }, character(1))
    return(vars[!is.na(vars)])
  }
  character()
}
#' @keywords internal
free_indexes_rec <- function(expr) {
  free_vars <- character()
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    path <- element$path
    ast <- if (is.null(element$ast)) get_ast_value(path) else element$ast
    if (is_call(ast)) {
      if (ast[[1]] == "[") {
        free_vars <<- c(free_vars, free_indexes(ast))
      } else {
        for (i in seq_len(length(ast))) {
          if (i > 1) {
            push(list(ast = ast[[i]], path = c(path, i)))
          }
        }
      }
    }
  }
  ast_walker(expr, on_element)
  unique(free_vars)
}
#' @keywords internal
ast_walker <- function(ast, on_element) {

  push <- function(x) {
    stack_data <<- list(x, stack_data)
  }
  get_ast_value <- function(path) {
    if (length(path) > 0) {
      ast[[path]]
    } else {
      ast
    }
  }
  inplace_update_ast <- function(path, value) {
    if (length(path) > 0) {
      ast[[path]] <<- value
    } else {
      ast <<- value
    }
  }

  stack_data <- list()
  push(list(ast = ast, path = c(), multiplier = NULL))
  while (length(stack_data) > 0) {
    element <- stack_data[[1]]
    stack_data <- stack_data[[2]]
    on_element(push, inplace_update_ast, get_ast_value, element)
  }
  ast
}


# interp

#' Interpolate values into an expression.
#'
#' @name datatools
#' @rdname datatools
#' @param _obj An object to modify: can be a call, name, formula,
#'   \code{\link{lazy}}, or a string.
#' @param ...,.values Either individual name-value pairs, or a list
#'   (or environment) of values.
#' @export
#' @examples
#' # Interp works with formulas, lazy objects, quoted calls and strings
#' interp(~ x + y, x = 10)
#' interp(quote(x + y), x = 10)
#' interp("x + y", x = 10)
#'
#' # Use as.name if you have a character string that gives a
#' # variable name
#' interp(~ mean(var), var = as.name("mpg"))
#' # or supply the quoted name directly
#' interp(~ mean(var), var = quote(mpg))
#'
#' # Or a function!
#' interp(~ f(a, b), f = as.name("+"))
#'
#' # If you've built up a list of values through some other
#' # mechanism, use .values
#' interp(~ x + y, .values = list(x = 10))
#'
#' # You can also interpolate variables defined in the current
#' # environment, but this is a little risky.
#' y <- 10
#' interp(~ x + y, .values = environment())
interp <- function(obj_, ..., .values) {
  UseMethod("interp")
}

#' @export
interp.call <- function(obj_, ..., .values) {
  values <- all_values(.values, ...)

  substitute_q(obj_, values)
}

#' @export
interp.name <- function(obj_, ..., .values) {
  values <- all_values(.values, ...)

  substitute_q(obj_, values)
}

#' @export
interp.formula <- function(obj_, ..., .values) {
  if (length(obj_) != 2) {
    cat_stop("Must use one-sided formula.")
  }

  values <- all_values(.values, ...)

  obj_[[2]] <- substitute_q(obj_[[2]], values)
  obj_
}

#' @export
interp.lazy <- function(obj_, ..., .values) {
  values <- all_values(.values, ...)

  obj_$expr <- substitute_q(obj_$expr, values)
  obj_
}

#' @export
interp.character <- function(obj_, ..., .values) {
  values <- all_values(.values, ...)

  expr1 <- parse(text = obj_)[[1]]
  expr2 <- substitute_q(expr1, values)
  paste(deparse(expr2), collapse = "\n")
}

#' @keywords internal
all_values <- function(.values, ...) {
  is_lazy <- function(x) inherits(x, "lazy")
  if (missing(.values)) {
    values <- list(...)
  } else if (identical(.values, globalenv())) {
    # substitute doesn't want to replace in globalenv
    values <- as_l(globalenv())
  } else {
    values <- .values
  }

  if (is.list(values)) {
    # Replace lazy objects with their expressions
    is_lazy <- vapply(values, is_lazy, logical(1))
    values[is_lazy] <- lapply(values[is_lazy], `[[`, "expr")
  }
  values
}
#' @export
interp_deparse <- function(call, old, new, env = parent.frame()) {
  call <- eval(substitute(call), env)

  if (!is_chr(call) && is_symname(call)) {
    call <- deparse(call)
  }

  dep_call <-
    paste0("interp(quote(", call, "),", old, " = quote(", new, "))")

  eval(parse(text = dep_call), envir = env)
}
#' @export
expr_substitute <- function(expr, old, new) {
  expr <- duplicate(expr)
  switch(typeof(expr),
         language = node_walk_replace(rlang::node_cdr(expr), old, new),
         symbol = if (identical(expr, old)) {
           return(new)
         }
  )
  expr
}
node_walk_replace <- function(node, old, new) {
  while (!is_null(node)) {
    switch(typeof(rlang::node_car(node)),
           language = if (!rlang::is_call(
             rlang::node_car(node),
             c("~", "function")
           ) || is_call(rlang::node_car(node), "~",
                        n = 2
           )) {
             node_walk_replace(rlang::node_cdar(node), old, new)
           },
           symbol = if (identical(rlang::node_car(node), old)) {
             rlang::node_poke_car(
               node,
               new
             )
           }
    )
    node <- rlang::node_cdr(node)
  }
}


# FUN LIST

#' @rdname datatools
#' @param ... functions to combine. each function should produce a single
#' @export
#' @examples
#' # Call min() and max() on the vector 1:10
#' fn_each(min, max)(1:10)
#' # This syntax looks a little different.  It is shorthand for the
#' # the following:
#' f <- fn_each(min, max)
#' f(1:10)
#' # Three equivalent ways to call min() and max() on the vector 1:10
#' fn_each("min", "max")(1:10)
#' fn_each(c("min", "max"))(1:10)
#' fn_each(c(min, max))(1:10)
#' # Call length(), min() and max() on a random normal vector
#' fn_each(length, mean, var)(rnorm(100))
#'
fn_each <- function(...) {
  fnames <-  names(auto_name(as.list(match.call()[-1])))

  fs <- list(...)
  if (length(fs[[1]]) > 1) {
    fs <- fs[[1]]

    # Jump through hoops to work out names
    snames <- as.list(match.call()[2])[[1]]
    fnames <- unlist(lapply(as.list(snames)[-1], deparse))
  }

  # Find function names and replace with function objects
  char <- map_lgl(fs, is.character)
  char
  fnames[char] <- fs[char]
  fs[char] <- lapply(fs[char], match.fun)

  unames <- names(fs)
  if (is.null(unames)) {
    unames <- fnames
  }
  unames[unames == ""] <- fnames[unames == ""]

  n <- length(fs)

  if (n == 1) {
    # If there is only one function, things are simple.  We just
    # need to name the output, if appropriate.
    structure(function(x, ...) {
      res <- fs[[1]](x, ...) # nolint
      if (length(res) == 1) {
        names(res) <- unames
      }
      res
    }, class = c("fn_each", "function"),
    call = match.call())
  } else {
    # nolint start
    proto <- NULL
    result <- NULL
    # nolint end

    structure(function(x, ...,reset = TRUE) {
      # For n > 1 things are a little tricky
      # Construct protoype for output on first call
      if (is.null(proto)) {
        result <<- vector("list", length = n)
        names(result) <- unames

        for (i in 1:n) {
          result[[i]] <- fs[[i]](x, ...)
        } # nolint
        proto <<- list2vector(result)
      } else {
        if(reset){
          proto <<- vector("list", length = n)
          names(proto) <- unames
        }

        for (i in 1:n) {
          proto[[i]] <- fs[[i]](x, ...)
        }
        if(reset){
          proto <<- list2vector(proto)
        }
      }
      proto
    }, class = c("fn_each", "function"),call = match.call())
  }
}

#' @export
as.list.fn_each <- function(x) {
  names <- list2vector(get("fnames", envir = env_fn(x = x)))
  xfn <- get("fs", envir = env_fn(x = x))
  setNames(xfn, names)
}
#' @export
`[.fn_each` <- function(x, i) {
  as.list.fn_each(x)[i]
}
#' @export
`[[.fn_each` <- function(x, i) {
  x[i][[1]]
}
#' @export
`$.fn_each`<-`[[.fn_each`
#' @export
update.fn_each<-function(x,nm,value){
  stopifnot(is_fn(value))
  sym_value<-substitute(value)
  stopifnot(inherits(x,"fn_each"))
  stopifnot(inherits(nm,"character"))
  xcall <- attr(x,"call")
  env_l<-map(as.list(x),env_fn)
  env_l[[nm]] <- env_fn(value)
  map(c(names(x),nm),function(w) {
    wcall <-eval(substitute_w(xcall[[w]]))
    if(is.null(wcall)){
      wcall <- sym_value
    }
    xcall[[w]] <<- eval(substitute_w2(x = wcall,caller2 = env_l[[w]]))

  })
  eval(xcall)
}
#' @export
`[<-.fn_each` <- function(x,i,value){
  xcall<-match.call()
  names(xcall)[3] <- "nm"
  xcall[[1]] <- quote(update)
  eval(xcall)
}
#' @export
`$<-.fn_each` <- function(x,i,value){
  xcall<-match.call()
  names(xcall)[3] <- "nm"
  xcall$nm <- lang2str(xcall$nm)
  xcall[[1]] <- quote(update)
  eval(xcall)
}
#' @export
`[[<-.fn_each` <- function(x,i,value){
  xcall<-match.call()
  names(xcall)[3] <- "nm"
  xcall[[1]] <- quote(update)
  eval(xcall)
}
#' @export
names.fn_each <- function(x){
  names(as.list.fn_each(x))
}
#' @export
length.fn_each <- function(x){
  length(as.list(x))
}
#' @export
print.fn_each <- function(x, ..., width = getOption("width")) {
  cat_class("fn_each")
  names <- list2vector(get("fnames",envir = env_fn(x = x)))
  xfn <- get("fs",envir = env_fn(x = x))
  code <- c()
  for (i in seq_along(xfn)) code[[i]] <- formals_names(xfn[[i]])
  code <- map_if(.x = code, .p = ~ length(.x) == 0, ~ c("..."),.else = commas)
  names <- strjustify(x = names,justify = "left")
  cat(paste0("$ ", names, ": ", code, collapse = "\n"))
}
#' @export
random_fn <- function(...){
  fncall<-match.call()
  fncall[[1]] <- quote(fn_each)
  f<-eval(fncall,env_call())
  f[[sample(length(f),1)]]
}
list2vector<- function (res) {
  n <- length(res)
  if (n == 0)
    return(vector())
  if (n == 1)
    return(res[[1]])
  atomic <- sapply(res, is.atomic)
  if (all(atomic)) {
    numeric <- all(unlist(lapply(res, is.numeric)))
    classes <- unique(lapply(res, class))
    if (numeric || length(classes) == 1) {
      res <- unlist(res)
    }
  }
  res
}



#' @rdname datatools
#' @export
#' @examples
#' compact1 <- function(x) Filter(Negate(is.null), x)
#'
#' # we can write:
#' compact2 <- partial_fn(Filter, Negate(is.null))
#'
#' # and the generated source code is very similar to what we made by hand
#' compact1
#' compact2
#'
#' # Note that the evaluation occurs "lazily" so that arguments will be
#' # repeatedly evaluated
#' f <- partial_fn(runif, n = rpois(1, 5))
#' f
#' f()
#' f()
#'
#' # You can override this by saying .lazy = FALSE
#' f <- partial_fn(runif, n = rpois(1, 5), .lazy = FALSE)
#' f
#' f()
#' f()
#'
#' # # This also means that partial works fine with functions that do
#' # # non-standard evaluation
#' # my_long_variable <- 1:10
#' # plot2 <- partial_fn(plot, my_long_variable)
#' # plot2()
#' # plot2(runif(10), type = "l")
partial_fn <- function(.fn, ..., .env = parent.frame(), .lazy = TRUE) {
  stopifnot(is.function(.fn))

  if (.lazy) {
    fcall <- substitute(.fn(...))
  } else {
    fcall <- make_call(substitute(.fn), .args = list(...))
  }
  # Pass on ... from parent function
  fcall[[length(fcall) + 1]] <- quote(...)

  args <- list("..." = quote(expr = ))
  make_function(args, fcall, .env)
}

## envirioment

#' @rdname datatools
#' @export
env_get <- rlang::get_env
#' @rdname datatools
#' @export
env_get_safe <- function(x, env = env_call(), lock = TRUE) {
  newenv <- list()
  if (inherits(x, "quoted")) {
    newenv <- attr(x, "env")
  } else if (inherits(x, "lazy")) {
    newenv <- x$env
  } else if (inherits(x, "quosure")) {
    newenv <- attr(x, ".Environment")
  } else if (inherits(x, "lazy")) {
    newenv <- x$env
  } else if (is.list(x) & newenv %==% list()) {
    newenv <- map(x, ~ env_get_safe(x = .x, env = env))
  } else if (is_frm(x) | is_fn(x) & newenv %==% list()) {
    newenv <- do_try(env_get(x))
  } else {
    newenv <- env
  }
  if (is_emptyenv(newenv) || is_empty(newenv)) {
    newenv <- env
  }

  if (is.list(newenv)) {
    newenv <- reduce(.x = newenv, .f = env_join, .init = baseenv())
  }

  out <- do_try(env_clone(newenv))
  if (lock) {
    lockEnvironment(out)
  }
  out
}

#' @rdname datatools
#' @export
expr_get_quotable <- function(x) {
  if (is_lazy(x)) {
    return(get_expr(x$expr))
  }

  if (is_lazy_dots(x)) {
    return(map(x, ~ get_expr(.x$expr)))
  }

  if (is_quosures(x) || is.list(x)) {
    x <- map(x, ~ get_expr(.x))
  } else {
    x <- get_expr(x)
  }
  x
}

#' Given an environment or object, return an \code{envlist} of its
#' parent environments.
#'
#' If \code{e} is not specified, it will start with environment from which
#' the function was called.
#' @rdname datatools
#'
#' @param e An environment or other object.
#' @param all If \code{FALSE} (the default), stop at the global
#'   environment or the empty environment. If \code{TRUE}, print all
#'   parents, stopping only at the empty environment (which is the
#'   top-level environment).
#' @examples
#' # Print the current environment and its parents
#' envs_prnts()
#'
#' # Print the parent environments of the load_all function
#' e <- envs_prnts(envs_prnts)
#' e
#'
#' # Get all parent environments, going all the way to empty env
#' e <- envs_prnts(envs_prnts, TRUE)
#' e
#'
#' # Print e with paths
#' print(e, path = TRUE)
#'
#' # Print the first 6 environments in the envlist
#' e[1:6]
#'
#' # Print just the parent environment of load_all.
#' # This is an envlist with one element.
#' e[1]
#'
#' # Pull that environment out of the envlist and see what's in it.
#' e[[1]]
#' ls(e[[1]], all.names = TRUE)
#' @export
envs_prnts <- function(e = parent.frame(), all = FALSE) {
  if (!is.environment(e)) {
    e <- environment(e)
  }
  if (is.null(e)) {
    return(NULL)
  }

  envs <- list(e)
  while (TRUE) {
    if (identical(e, emptyenv())) {
      break
    }
    if (!all && identical(e, globalenv())) {
      break
    }

    e <- parent.env(e)
    envs <- c(envs, e)
  }
  as.envlist(envs)
}

#' Convert a list of environments to an \code{envlist} object.
#'
#' @rdname datatools
#' @param x A list of environments.
#' @export
as.envlist <- function(x) {
  if (!is.list(x) || !all(vapply(x, is.environment, logical(1)))) {
    cat_stop("Cannot convert to envlist: input is not a list of environments.")
  }
  structure(x, class = "envlist")
}

#' @export
`[.envlist` <- function(x, i) {
  as.envlist(.subset(x, i))
}

#' @export
print.envlist <- function(x, name = TRUE, path = FALSE, ...) {
  labels <- vapply(x, format, FUN.VALUE = character(1))
  dat <- data.frame(label = labels, stringsAsFactors = FALSE)

  if (name) {
    names <- vapply(x,
                    FUN.VALUE = character(1),
                    function(e) {
                      paste('"', attr(e, "name"), '"', sep = "")
                    }
    )
    dat <- cbind(dat, name = names, stringsAsFactors = FALSE)
  }

  if (path) {
    paths <- vapply(x,
                    FUN.VALUE = character(1),
                    function(e) {
                      paste('"', attr(e, "path"), '"', sep = "")
                    }
    )
    dat <- cbind(dat, path = paths, stringsAsFactors = FALSE)
  }

  print(dat, ..., right = FALSE)

  invisible(x)
}


#' Get parent/ancestor environment
#'
#' @rdname datatools
#' @param env an environment
#' @param n number of parents to go up
#' @export
#' @examples
#' adder <- function(x) function(y) x + y
#' add2 <- adder(2)
#' env_prnt(add2)
env_prnt <- function(env = parent.frame(), n = 1) {
  env <- to_env(env)
  for (i in seq_len(n)) {
    env <- parent.env(env)
  }
  env
}

#' @rdname datatools
#' @export
env_call <- function(n = 1) {
  parent.frame(n + 1)
}
#' @rdname datatools
#' @export
env_file <- function(file) {
  .env_file_parent <-
    function(file) {
      env <- new.env(parent = parent.env(globalenv()))
      methods::setPackageName("roxygen_devtest", env)
      .sys.source(file, envir = env)
      env
    }
  .env_file_global <-
    function(file) {
      env <- env_clone(globalenv())
      methods::setPackageName("roxygen_devtest", env)
      .sys.source(file, envir = env)
      env
    }
  do_try(.env_file_parent(file), .env_file_global(file))
}

#' @rdname datatools
#' @export
env_curr <- function() {
  parent.frame()
}
#' @rdname datatools
#' @export
env_ns <- rlang::ns_env
#' @rdname datatools
#' @export
env_fn <- function(x, default_env = env_prnt()) {
  assert_function(x, severity = "stop")
  env <- environment(x)
  if (rlang::is_primitive(x)) {
    return(env_ns("base"))
  } else if (is_null(env) || identical(env, emptyenv())) {
    return(default_env)
  } else {
    env
  }
}
#' @export
`env_fn<-` <- function(x, value) {
  assert_function(x, severity = "stop")
  environment(x) <- value
  x
}

#' @export
env_frm <- rlang::f_env
#' @export
`env_frm<-` <- rlang::`f_env<-`

#' @export
env_new <- rlang::new_environment

#' @export
env_join <- function(env1, env2) {
  new_env <- env_new(data = list(), parent = emptyenv())

  if (is_empty(env2) | !is_env(env2)) {
    return(env1)
  }
  names_1 <- ls(env1)
  l_env1 <- mget(names_1, env1)

  names_2 <- ls(env2)
  names_2 <- names_2[names_2 %!in% names_1]

  if (length(names_2) == 0) {
    return(env1)
  }
  l_env2 <- mget(names_2, env2)

  list2env(x = c(l_env1, l_env2), parent = new_env)
}

#' @export
env_lock <- rlang::env_lock
#' @export
env_is_locked <- rlang::env_is_locked
#' @export
env_unlock <- rlang::env_unlock

#' @export
df2env <-
  function(x,
           envir = NULL,
           parent = parent.frame(),
           hash = (ncol(x) > 100),
           size = max(29L, ncol(x))) {
    list2env(x = as.list(x), envir = envir, parent = parent, hash = hash, size = size)
  }
#' @export
env_poke2 <- function(env, name, value) {
  if (env_has(env, name)) {
    cat_warn(paste0("\"", name, "\" is already assigned to a value."))
    return(invisible(env))
  }

  env_poke(env, name, value)
  invisible(env)
}
#' @export
env_rebind <- function(name, value, env = env_call()) {
  if (identical(env, empty_env())) {
    cat_warn("Can't find `", name, "`")
    ret_invis(env)
  } else if (env_has(env, name)) {
    ret_invis(env_poke(env, name, value))
  } else {
    env_rebind(name, value, env_parent(env))
  }
}
#' @export
env_has <- rlang::env_has
#' @export
env_where <- function(name, env = env_call()) {
  if (identical(env, emptyenv())) {
    cat_stop("Can't find `", name, "`.")
  } else if (env_has(env, name)) {
    env
  } else {
    env_where(name, env_parent(env))
  }
}
#' @export
env_where2 <- function(name, env = env_call(), results = list()) {
  if (identical(env, emptyenv())) {
    results
  } else {
    if (env_has(env, name)) {
      results <- c(results, env)
    }
    env_where2(name, env_parent(env), results)
  }
}

#' @export
env_search <- rlang::search_env

#' @export
to_env <- function(x, quiet = FALSE) {
  if (is.environment(x)) {
    x
  } else if (is_df(x)) {
    df2env(x)
  } else if (is.list(x)) {
    list2env(x)
  } else if (is.function(x)) {
    environment(x)
  } else if (length(x) == 1 && is.character(x)) {
    if (!quiet) {
      cat_message("Using environment ", x)
    }
    as.environment(x)
  } else if (length(x) == 1 && is.numeric(x) && x > 0) {
    if (!quiet) {
      cat_message("Using environment ", search()[x])
    }
    as.environment(x)
  } else {
    cat_stop("Input can not be coerced to an environment")
  }
}

#' @export
env2list <- function(envir = env_call(), nms = NULL) {
  if (is_empty(nms)) {
    nms <- names2(envir)
    nms <- nms[nms != ""]
  }
  rlang::env_get_list(env = envir, nms = nms)
}

## eval

#' eval
#'
#' @rdname datatools
#' @examples
#' data <- rep(1:3, 10)
#' data <- dplyr::tibble(a = data, b = data, c = data) %>% dplyr::arrange(a)
#'
#' expr_e <- quote(expr = kmeans(x = data, centers = k))
#' expr_e
#' k <- 3
#' k <- kmeans(x = data, centers = k)
#' k <- k$centers
#' data
#' k
#' res <- kmeans(x = data, centers = k)
#'
#' eval_lazy(expr = as_lazy(expr_e)) %==% res
#' eval_quosures(expr = as_quosure(expr_e)) %==% res
#' eval_quosures(expr = lazyquo(expr_e)) %==% res
#' eval_parse(expr = deparse(expr_e)) %==% res
#'
#' # eval_lazy(expr = as_lazy_dots(expr_e))
#' # eval_quosures(expr = as_quosures(expr_e,env = env_call()))
#' # eval_quosures(expr = lazyquos(expr_e,env = env_call()))
#'
#'
#' expr_e <- list(a = expr_e, b = expr_e)
#'
#' res <- list(a = res, b = res)
#'
#' eval_lazy(expr = as_lazy(expr_e)) %==% res
#' eval_quosures(expr = as_quosure(expr_e)) %==% unname(res)
#' eval_quosures(expr = lazyquo(expr_e)) %==% res
#' eval_parse(expr = deparse(expr_e)) %==% res
#'
#' eval_lazy(expr = as_lazy_dots(expr_e)) %==% res
#' eval_quosures(expr = as_quosures(expr_e, env = env_call())) %==% res
#' eval_quosures(expr = lazyquos(expr_e, env = env_call())) %==% res
#'
#' rm_ls()
#' a <- sample(1:5)
#' b <- sample(1:5)
#' c <- sample(1:5)
#'
#' abc_env <- a + b - c
#' data <- dplyr::tibble(a = runif(5), b = runif(5), c = runif(5))
#' abc_data <- data$a + data$b - data$c
#' expr <- quote(a + b - c)
#'
#' eval_safe(expr = as_lazy(expr)) %==% abc_env
#' eval_safe(expr = as_quosure(expr)) %==% abc_env
#' eval_safe(expr = lazyquo(expr)) %==% abc_env
#' eval_safe(expr = expr) %==% abc_env
#' eval_safe(expr = deparse(expr)) %==% abc_env
#' eval_safe(expr = as_lazy_dots(expr), follow = TRUE) %==% abc_env
#' eval_safe(expr = lazyquos(expr), follow = TRUE) %==% abc_env
#'
#' eval_safe(expr = as_lazy(expr), envir = env_call()) %==% abc_env
#' eval_safe(expr = as_quosure(expr), envir = env_call()) %==% abc_env
#' eval_safe(expr = lazyquo(expr), envir = env_call()) %==% abc_env
#' eval_safe(expr = expr, envir = env_call()) %==% abc_env
#' eval_safe(expr = deparse(expr), envir = env_call()) %==% abc_env
#' #eval_safe(expr = as_lazy_dots(expr), envir = env_call(), follow = TRUE) %==% abc_env
#' eval_safe(expr = lazyquos(expr), envir = env_call(), follow = TRUE) %==% abc_env
#'
#' eval_safe(expr = as_lazy(expr), data = data) %==% abc_data
#' eval_safe(expr = as_quosure(expr), data = data) %==% abc_data
#' eval_safe(expr = lazyquo(expr), data = data) %==% abc_data
#' eval_safe(expr = expr, data = data) %==% abc_data
# eval_safe(expr = deparse(expr), data = data) %==% abc_data
#' eval_safe(expr = as_lazy_dots(expr), data = data, follow = TRUE) %==% abc_data
#'
#' eval_safe(expr = as_lazy(expr), data = data, envir = env_call()) %==% abc_data
#' eval_safe(expr = as_quosure(expr), data = data, envir = env_call()) %==% abc_data
#' eval_safe(expr = lazyquo(expr), data = data, envir = env_call()) %==% abc_data
#' eval_safe(expr = expr, data = data, envir = env_call()) %==% abc_data
#' eval_safe(expr = deparse(expr), data = data, envir = env_call()) %==% abc_data
#' eval_safe(expr = as_lazy_dots(expr), data = data, envir = env_call(), follow = TRUE) %==% abc_data
#'
#'
#'
#' q <- quote(c)
#' expr <- quote(a + b - q)
#'
#'
#'
#' eval_safe(expr = as_lazy(expr), follow = TRUE) %==% abc_env
#' eval_safe(expr = as_quosure(expr), follow = TRUE) %==% abc_env
#' eval_safe(expr = lazyquo(expr), follow = TRUE) %==% abc_env
#' eval_safe(expr = expr, follow = TRUE) %==% abc_env
#' eval_safe(expr = deparse(expr), follow = TRUE) %==% abc_env
#' eval_safe(expr = as_lazy_dots(expr), follow = TRUE) %==% abc_env
#' eval_safe(expr = lazyquos(expr), follow = TRUE) %==% abc_env
#'
#' eval_safe(expr = as_lazy(expr, env = env_call()), envir = env_call(), follow = TRUE) %==% abc_env
#' eval_safe(expr = as_quosure(expr), envir = env_call(), follow = TRUE) %==% abc_env
#' eval_safe(expr = lazyquo(expr), envir = env_call(), follow = TRUE) %==% abc_env
#' eval_safe(expr = expr, envir = env_call(), follow = TRUE) %==% abc_env
#' eval_safe(expr = deparse(expr), envir = env_call(), follow = TRUE) %==% abc_env
#' #eval_safe(expr = as_lazy_dots(expr), envir = env_call(), follow = TRUE) %==% abc_env
#' #eval_safe(expr = lazyquos(expr), envir = env_call(), follow = TRUE) %==% abc_env
#'
#' eval_safe(expr = as_lazy(expr), data = data, follow = TRUE) %==% abc_data
#' eval_safe(expr = as_quosure(expr), data = data, follow = TRUE) %==% abc_data
#' eval_safe(expr = lazyquo(expr), data = data, follow = TRUE) %==% abc_data
#' eval_safe(expr = expr, data = data, follow = TRUE) %==% abc_data
#' eval_safe(expr = deparse(expr), data = data, follow = TRUE) %==% abc_data
#' #eval_safe(expr = as_lazy_dots(expr), data = data, follow = TRUE) %==% abc_data
#' #eval_safe(expr = lazyquos(expr), data = data, follow = TRUE) %==% abc_data
#'
#' # eval_safe(
#' #   expr = as_lazy(expr),
#' #   data = data,
#' #   envir = env_call(),
#' #   follow = TRUE
#' # ) %==% abc_data
#' #
#' # eval_safe(
#' #   expr = as_quosure(expr),
#' #   data = data,
#' #   envir = env_call(),
#' #   follow = TRUE
#' # ) %==% abc_data
#' #
#' # eval_safe(
#' #   expr = lazyquo(expr),
#' #   data = data,
#' #   envir = env_call(),
#' #   follow = TRUE
#' # ) %==% abc_data
#' #
#' # eval_safe(
#' #   expr = expr,
#' #   data = data,
#' #   envir = env_call(),
#' #   follow = TRUE
#' # ) %==% abc_data
#' #
#' #
#' # eval_safe(
#' #   expr = deparse(expr),
#' #   data = data,
#' #   envir = env_call(),
#' #   follow = TRUE
#' # ) %==% abc_data
#' #
#' # eval_safe(
#' #   expr = as_lazy_dots(expr),
#' #   data = data,
#' #   envir = env_call(),
#' #   follow = TRUE
#' # ) %==% abc_data
#' #
#' # eval_safe(
#' #   expr = lazyquos(expr),
#' #   data = data,
#' #   envir = env_call(),
#' #   follow = TRUE
#' # ) %==% abc_data
#' @export
eval_quoted <- function(exprs, envir = NULL, enclos = NULL, try = FALSE) {
  if (is.numeric(exprs)) {
    return(envir[exprs])
  }

  if (!is.null(envir) &&
      !is.list(envir) && !is.environment(envir)) {
    cat_stop("envir must be either NULL, a list, or an environment.")
  }

  qenv <-
    if (is_quoted(exprs)) {
      attr(exprs, "env")
    } else {
      parent.frame()
    }
  if (is.null(envir)) {
    envir <- qenv
  }
  if (is.data.frame(envir) && is.null(enclos)) {
    enclos <- qenv
  }

  if (try) {
    results <- lapply(
      exprs,
      purrr::possibly(NULL, eval, quiet = TRUE),
      envir = envir,
      enclos = enclos
    )
  } else {
    results <- lapply(exprs, eval, envir = envir, enclos = enclos)
  }
  names(results) <- names(exprs)

  results
}
#' @export
eval_tidyish <- function(expr) {
  env <- new.env(parent = parent.frame())
  env[["~"]] <- function(...) {
    call <- sys.call()
    env <- environment(call)
    eval(call[[2]], env)
  }
  eval(expr, env)
}
#' @export
eval_lazy <- function(expr, data = NULL) {
  if (is_lazy_dots(expr)) {
    if(length(expr) == 1){
      expr <- expr[[1]]
    } else {
      return(lapply(expr, eval_lazy, data = data))
    }
  }
  expr <- as_lazy(expr)
  #expr$expr <- expr_get_quotable(expr)
  if (!is.null(data)) {
    expr <- eval(expr$expr, data, expr$env)
  } else {
    expr <- eval(expr$expr, expr$env, emptyenv())
  }
  expr
}
#' @export
eval_quosures <- function(expr, data = NULL, envir = env_call()) {
  if (inherits(expr, "quosures")) {
    return(lapply(expr, eval_quosures))
  }
  expr_x <- expr_get_quotable(expr)
  env_x <- env_get_safe(expr, env = envir)
  if (is.list(expr_x)) {
    out <- vector("list", length(expr_x))
    for (i in seq_along(expr_x)) {
      tmp_out <- NULL
      if (!is_empty(data)) {
        tmp_out <- do_try(eval(expr_x[[i]], data, env_x))
      } else {
        tmp_out <- do_try(eval(expr_x[[i]], env_x, emptyenv()))
      }
      if (is.null(tmp_out)) {
        out[[i]] <- expr_x[[i]]
      } else {
        out[[i]] <- tmp_out
      }
    }
  } else {
    if (!is_empty(data)) {
      out <- do_try(eval(expr_x, data, env_x))
    } else {
      out <- do_try(eval(expr_x, env_x, emptyenv()))
    }
    if (is.null(out)) {
      out <- expr_x
    }
  }

  out
}
#' @keywords internal
eval_callname <- function(expr, envir = env_call(), data = NULL) {
  expr <- eval(bquote_w(expr), envir = envir)

  if (inherits(expr, "list")) {
    return(map(
      seq_along(expr),
      ~ eval_callname(
        expr = expr[[.x]],
        envir = envir,
        data = data
      )
    ))
  }

  if (!(is_call(expr) || is_name(expr))) {
    return(expr)
  }

  if (!is.null(data)) {
    out <- eval(expr, data, envir)
  } else {
    out <- eval(expr, envir, emptyenv())
  }
  out
}
#' @export
eval_parse <- function(expr, envir = env_call(), data = NULL) {
  file <- tempfile("eval_parsed_", fileext = ".R")
  on.exit(if (file.exists(file)) file.remove(file))

  writeLines(expr, file)
  exprs <- parse(file, keep.source = TRUE)

  out <- list()
  for (e in exprs) {
    if (!is.null(data)) {
      out <- c(out, list(eval(e, data, envir)))
    } else {
      out <- c(out, list(eval(e, envir, emptyenv())))
    }
  }

  if (length(out) == 1) {
    out[[1]]
  } else {
    out
  }
}

lang_get_names <- function(x) {
  if (is_call(x)) {
    tmp <- call_args_nms(x)
    res <- character(0)
    for (j in tmp) {
      j_tmp <- do_try(str2lang(j))
      if (is_call(j_tmp)) {
        res <- c(res, lang_get_names(j_tmp))
      } else {
        res <- c(res, j)
      }
    }
  } else if (is_name(x)) {
    res <- lang2str(x)
  }
  res
}
eval_follow <- function(x, envir = env_call(), data = NULL) {
  x <- eval(substitute_w(x), envir = env_call())

  if (is_lazy(x) | is_lazy_dots(x)) {
    call_x <- expr_get_quotable(x)
  } else if (rlang::is_quosure(x) | rlang::is_quosures(x)) {
    call_x <- expr_get_quotable(x)
  } else if (is_chr(x)) {
    call_x <- do_try(str2lang(x))
  } else if (is_call(x) | is_name(x)) {
    call_x <- x
  } else {
    return(x)
  }


  arg_chr <- lang_get_names(call_x)
  has_arg_env <- env_has(env = envir, nms = arg_chr)

  has_arg_data <- arg_chr %in% names(data)
  names(has_arg_data) <- arg_chr
  if (!(any(has_arg_env | has_arg_data))) {
    return(x)
  }
  l_envir <- as.list(envir)
  l_data <- df2list(data)
  res <- call_x

  for (i in arg_chr) {
    if (has_arg_data[i]) {
      res <- do_try(interp(obj_ = res, .values = as_prls(l_data[i])), res)
    } else if (has_arg_env[i]) {
      res <-
        do_try(interp(obj_ = res, .values = as_prls(l_envir[i])), res)
    }
  }

  res
}
#' @export
df2list <- function(df) {
  map(df, ~.x)
}
#' @export
eval_safe <- function(expr, envir = env_call(), data = NULL, follow = FALSE) {
  expr <- eval(substitute_w(expr), envir = envir)
  if (is_true(is.list(expr) & length(expr) == 1)) {
    expr <- expr[[1]]
  }
  if (!is.null(data)) {
    data <- eval(substitute_w(data), envir = envir)
  } else {
    data <- do_try(expr = dplyr::cur_data())
  }
  if (is_quoted(data)) {
    data <- eval_quoted(exprs = data, envir = envir)
  } else if (is_quosure(data)) {
    data <- eval_quosures(expr = data, envir = envir)
  } else if (is_lazy(data) | is_lazy_dots(data)) {
    data <- eval_lazy(expr = data)
  }
  out <- NULL
  if (is_lazy(expr) | is_lazy_dots(expr)) {
    out <- do_try_ier(eval_lazy(expr, data))
    if (is_ier(out)) {
      newenv <- env_get_safe(x = expr, env = envir)
      envir <- env_join(newenv, envir)
      expr <- expr_get_quotable(expr)
      return(eval_safe(
        expr = expr,
        envir = envir,
        data = data,
        follow = follow
      ))
    }
  } else if (rlang::is_quosure(expr) | rlang::is_quosures(expr)) {
    out <- eval_quosures(expr, data, envir)
  } else if (is_quoted(expr)) {
    out <- do_try_ier(eval_quoted(expr, attr(expr, "env")))
    if (is_ier(out)) {
      newenv <- env_get_safe(x = expr, env = envir)
      envir <- env_join(newenv, envir)
      expr <- expr_get_quotable(expr)
      return(eval_safe(
        expr = expr,
        envir = envir,
        data = data,
        follow = follow
      ))
    }
  } else if (is_character(expr)) {
    out <- do_try_ier(eval_parse(expr, envir, data))
    if (is_ier(out)) {
      expr <- str2lang(expr)
      return(eval_safe(
        expr = expr,
        envir = envir,
        data = data,
        follow = follow
      ))
    }
  } else if (is.list(expr) & is_empty(out)) {
    return(map(
      .x = expr,
      .f = ~ eval_safe(.x, envir, data,
                       follow = TRUE
      )
    ))
  } else {
    if (!is.null(data)) {
      out <- do_try(eval(expr, data, envir), expr)
    } else {
      out <- do_try(eval(expr, envir, emptyenv()), expr)
    }
  }
  if (follow) {
    repeat {
      out_rep <- eval_follow(out, envir, data)
      if (is_true(!identical(out_rep, out))) {
        out_rep <- do_try(
          eval_safe(
            expr = out_rep,
            envir = envir,
            data = data
          ),
          out_rep
        )
        if (!is_empty(out_rep)) {
          out <- out_rep
        }
      } else {
        out <- do_try(eval(out), out)
        break
      }
    }
  }
  out
}
#' @export
eval_expr <- function(expr, envir = caller_env()) {
  expr <- rlang::enexpr(expr)
  eval(expr, env = envir)
}

### gets

#' @export
fn_get <- function(name, env = env_call(), inherits = TRUE) {
  if (env_has(env, name)) {
    obj <- env_get(env, name)

    if (is.function(obj)) {
      return(obj)
    }
  }

  if (identical(env, emptyenv()) || !inherits) {
    cat_stop("Could not find a function called \"", name, "\".",
             call. = FALSE
    )
  }

  fn_get(name, env_parent(env))
}
#' @export
fn_get2 <- function(name, env = env_call()) {
  if (env_has(env, name)) {
    obj <- env_get(env, name)

    if (is.function(obj)) {
      return(list(fun = obj, env = env))
    }
  }

  if (identical(env, emptyenv())) {
    cat_stop("Could not find a function called \"", name, "\"",
             call. = FALSE
    )
  }

  fn_get2(name, env_parent(env))
}
#' @export
fn_str <- function(fun_name, env = env_call()) {
  if (!is.character(fun_name) && length(fun_name) == 1) {
    cat_stop("`fun_name` must be a string.")
  }
  fun_env <- fn_get2(fun_name, env)

  list(
    where = fun_env$env,
    enclosing = fn_env(fun_env$fun)
  )
}
#' @export
get2 <- function(name, env = env_call()) {
  name_sym <- sym(name)
  eval(name_sym, env)
}
#' @export
assign2 <- function(name, value, env = env_call()) {
  name_sym <- ensym(name)
  assign_expr <- expr(!!name_sym <- !!value)
  eval(assign_expr, env)
}

#try return

ier <- function() {
  `ier!`
}
is_ier <- function(x) {
  inherits(x, "mg_ier")
}
`ier!` <- structure(list(), class = "mg_ier")
#' @export
print.mg_ier <- function(x, ...) {
  cat_class("<ier>")
}

ierand <- function() {
  `ierand!`
}
is_ierand <- function(x) {
  inherits(x, "mg_ierand")
}
`ierand!` <- structure(list(), class = "mg_ierand")
#' @export
print.mg_ierand <- function(x, ...) {
  cat_class("<ierand>")
}

do_try_ier <- function(expr) {
  .doTryCatch <- function(expr, error = NULL, env = parent.frame()) {
    .Internal(.addCondHands(
      "error", list(error), env, environment(),
      FALSE
    ))
    expr
  }

  value <- suppressall(.doTryCatch(return(expr),
                                   error = ier(),
                                   env = parent.frame()
  ))
  if (is_ier(value[[3]])) {
    return(ier())
  }
  value
}
do_try_ierand <- function(expr, .p = .05) {
  make_iterator <- function(f, n = 1) {
    fun_call <- function(f, ...) {
      f(...)
    }
    function(x) {
      Reduce(fun_call, rep.int(list(f), n), x, right = TRUE)
    }
  }
  .doTryCatch <- function(expr,
                          error = NULL,
                          env = parent.frame()) {
    .Internal(.addCondHands("error", list(error), env, environment(), FALSE))
    expr
  }
  if (runif(1, 0, 1) <= .p) {
    return(ierand())
  }

  value <-
    suppressall(.doTryCatch(return(expr), error = ier(), env = parent.frame()))
  if (is_ier(value[[3]])) {
    return(ier())
  }
  value
}

#' @export
ret_invis <- function(x = NULL) {
  ret_inv <- rlang::expr(invisible(return(!!x)))
  rlang::eval_bare(ret_inv, env = parent.frame())
}

#parse

#' @export
parse_all <- function(x, filename = NULL, allow_error = FALSE) {
  UseMethod("parse_all")
}
#' @export
parse_all.character <-
  function(x, filename = NULL, allow_error = FALSE) {
    # Do not convert strings to factors by desfault in data.frame()
    op <- options(stringsAsFactors = FALSE)
    on.exit(options(op), add = TRUE)
    file
    if (length(grep("\n", x))) {
      # strsplit('a\n', '\n') needs to return c('a', '') instead of c('a')
      x <- gsub("\n$", "\n\n", x)
      x[x == ""] <- "\n"
      x <-
        unlist(strsplit(x, "\n"),
               recursive = FALSE,
               use.names = FALSE
        )
    }
    n <- length(x)

    if (is.null(filename)) {
      filename <- "<text>"
    }
    src <- srcfilecopy(filename, x)
    if (allow_error) {
      exprs <- tryCatch(
        parse(text = x, srcfile = src),
        error = identity
      )
      if (inherits(exprs, "error")) {
        return(structure(data.frame(
          src = paste(x, collapse = "\n"),
          expr = I(list(expression()))
        ),
        PARSE_ERROR = exprs
        ))
      }
    } else {
      exprs <- parse(text = x, srcfile = src)
    }

    # No code, only comments and/or empty lines
    ne <- length(exprs)
    if (ne == 0) {
      return(data.frame(src = append_break(x), expr = I(rep(list(
        NULL
      ), n))))
    }

    srcref <- attr(exprs, "srcref", exact = TRUE)

    # Stard/End line numbers of expressions
    pos <-
      do.call(rbind, lapply(srcref, unclass))[, c(1, 3), drop = FALSE]
    l1 <- pos[, 1]
    l2 <- pos[, 2]
    # Add a third column i to store the indices of expressions
    pos <- cbind(pos, i = seq_len(nrow(pos)))
    pos <- as.data.frame(pos) # split() does not work on matrices

    # Split line number pairs into groups: if the next start line is the same as
    # the last end line, the two expressions must belong to the same group
    spl <- cumsum(c(TRUE, l1[-1] != l2[-ne]))
    # Extract src lines and expressions for each group; also record the start line
    # number of this group so we can re-order src/expr later
    res <- lapply(split(pos, spl), function(p) {
      n <- nrow(p)
      data.frame(
        src = paste(x[p[1, 1]:p[n, 2]], collapse = "\n"),
        expr = I(list(exprs[p[, 3]])),
        line = p[1, 1]
      )
    })

    # Now process empty expressions (comments/blank lines); see if there is a
    # "gap" between the last end number + 1 and the next start number - 1
    pos <- cbind(c(1, l2 + 1), c(l1 - 1, n))
    pos <- pos[pos[, 1] <= pos[, 2], , drop = FALSE]

    # Extract src lines from the gaps, and assign empty expressions to them
    res <- c(res, lapply(seq_len(nrow(pos)), function(i) {
      p <- pos[i, ]
      r <- p[1]:p[2]
      data.frame(
        src = x[r],
        expr = I(rep(list(NULL), p[2] - p[1] + 1)),
        line = r - 1
      )
    }))

    # Bind everything into a data frame, order it by line numbers, append \n to
    # all src lines except the last one, and remove the line numbers
    res <- do.call(rbind, res)
    res <- res[order(res$line), ]
    res$src <- append_break(res$src)
    res$line <- NULL

    # For compatibility with evaluate (<= 0.5.7): remove the last empty line (YX:
    # I think this is a bug)
    n <- nrow(res)
    if (res$src[n] == "") {
      res <- res[-n, ]
    }

    rownames(res) <- NULL
    res
  }
append_break <- function(x) {
  n <- length(x)
  if (n <= 1) {
    x
  } else {
    paste(x, rep(c("\n", ""), c(n - 1, 1)), sep = "")
  }
}
#' @export
parse_all.connection <- function(x, filename = NULL, ...) {
  if (!isOpen(x, "r")) {
    open(x, "r")
    on.exit(close(x))
  }
  text <- readLines(x)
  if (is.null(filename)) {
    filename <- summary(x)$description
  }
  parse_all(text, filename, ...)
}
#' @export
parse_all.function <- function(x, filename = NULL, ...) {
  src <- attr(x, "srcref", exact = TRUE)
  if (is.null(src)) {
    src <- deparse(body(x))
    # Remove { and }
    n <- length(src)
    if (n >= 2) {
      src <- src[-c(1, n)]
    }
    if (is.null(filename)) {
      filename <- "<function>"
    }
    parse_all(src, filename, ...)
  } else {
    src2 <- attr(body(x), "srcref", exact = TRUE)
    n <- length(src2)
    if (n > 0) {
      if (is.null(filename)) {
        filename <- attr(src, "srcfile")$filename
      }
      if (n >= 2) {
        parse_all(unlist(lapply(src2[-1], as.character)), filename, ...)
      } else {
        # f <- function(...) {}
        parse_all(character(0), filename, ...)
      }
    } else {
      if (is.null(filename)) {
        filename <- "<function>"
      }
      parse_all(deparse(body(x)), filename, ...)
    }
  }
}
#' @export
parse_all.default <- function(x, filename = NULL, ...) {
  if (is.null(filename)) {
    filename <- "<expression>"
  }
  parse_all(deparse(x), filename, ...)
}
#' @export
parse_all.call <- function(x, filename = NULL, ...) {
  out <- parse_all.default(x, filename = filename, ...)
  out$expr <- list(as.expression(x))
  out
}

#' @export
deparse_all <- function(expr) {
  function_sym <- quote(`function`)
  brace_sym <- quote(`{`)
  str <- deparse(expr, 60L)
  if (length(str) > 1) {
    if (is_call(expr, function_sym)) {
      expr[[3]] <- quote(...)
      str <- deparse(expr, 60L)
    } else if (is_call(expr, brace_sym)) {
      str <- "{ ... }"
    } else if (is_call(expr)) {
      str <- deparse(rlang::call2(expr[[1]], quote(...)), 60L)
    }
    str <- paste(str, collapse = "\n")
  }
  str
}

# hooks

set_hooks <- function(hooks, action = "append") {
  old <- list()
  for (hook_name in names(hooks)) {
    old[[hook_name]] <- getHook(hook_name)
    setHook(hook_name, hooks[[hook_name]], action = action)
  }
  invisible(old)
}
remove_hooks <- function(hooks) {
  for (hook_name in names(hooks)) {
    hook <- getHook(hook_name)
    for (fun in unlist(hooks[hook_name])) {
      hook[sapply(hook, identical, fun)] <- NULL
    }
    setHook(hook_name, hook, "replace")
  }
}
