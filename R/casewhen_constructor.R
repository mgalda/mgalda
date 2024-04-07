#' Crear expreciones case_when
#'
#'
#' @name casewhen_constructor
#' @rdname casewhen_constructor
#' @keywords internal
NULL

# export ----------------------------------------------------------------------------


#' Crear expreciones case_when
#'
#' @rdname casewhen_constructor
#' @param vars argumento a evaluar
#' @export

casewhen_constructor <- function(..., vars = "x") {
  fn_cw <- rlang::dots_list(...)
  structure(
    .casewhen_constructor(!!!fn_cw, vars = vars),
    class = c("casewhen_constructor", "function")
  )
}


#' @name casewhen_constructor
#' @aliases variable.names.casewhen_constructor
#' @rdname casewhen_constructor
#' @method variable.names casewhen_constructor
#' @export

variable.names.casewhen_constructor <-
  function(object, ...) {
    get("vars", envir = environment(object))
  }

#' @name casewhen_constructor
#' @aliases print.casewhen_constructor
#' @rdname casewhen_constructor
#' @method print casewhen_constructor
#' @export

print.casewhen_constructor <- function(x, ...) {
  fn_cw <- fn_cw(x)
  var_names <- stats::variable.names(x)
  n_forms <- length(fn_cw)
  n_vars <- length(var_names)
  out <-
    utils::capture.output(purrr::walk(fn_cw, print, showEnv = FALSE))
  out <- c(
    crayon::cyan("<CASE WHEN>"),
    crayon::magenta(
      n_vars,
      paste0("variable", plural(var_names), ":"),
      paste(var_names, collapse = ", ")
    ),
    crayon::magenta(n_forms, paste0("condition", plural(fn_cw), ":")),
    crayon::green(paste("->", out)),
    ""
  )
  cat(paste0(out, collapse = "\n"))
  invisible(x)
}


# internal --------------------------------------------------------------------------

#' @rdname casewhen_constructor
#' @keywords internal
fn_cw <- function(x, ...) {
  UseMethod("fn_cw")
}

#' @rdname casewhen_constructor
#' @keywords internal
fn_cw.casewhen_constructor <-
  function(x, ...) {
    get("fn_cw", envir = environment(x))
  }

#' @rdname casewhen_constructor
#' @keywords  internal

.casewhen_constructor <-
  function(..., vars, fn = dplyr::case_when) {
    assertthat::assert_that(is.character(vars))

    fun_fmls <-
      purrr::map(rlang::set_names(vars), ~ rlang::missing_arg())

    fun_body <- substitute({
      match_call <- match.call()
      args_call <- as.list(match_call[-1])
      modify_vars <- function(x) {
        if (is.name(x)) {
          if (as.character(x) %in% names(args_call)) {
            return(args_call[[as.character(x)]])
          }
        }
        x
      }
      n <- length(fn_cw)
      new_fn_cw <- vector("list", n)
      for (i in seq_len(n)) {
        lhs <- rlang::f_lhs(fn_cw[[i]])
        rhs <- rlang::f_rhs(fn_cw[[i]])
        new_lhs <- pryr::modify_lang(lhs, modify_vars)
        new_rhs <- pryr::modify_lang(rhs, modify_vars)
        new_fn_cw[[i]] <-
          rlang::new_formula(new_lhs, new_rhs, env = base::parent.frame())
      }
      do.call(fn, new_fn_cw)
    })
    fn_cw <- rlang::dots_list(...)
    purrr::walk(
      fn_cw,
      ~ assertthat::assert_that(rlang::is_formula(.x),
        msg = "Argumento fuera de la formula"
      )
    )
    rlang::new_function(fun_fmls, fun_body)
  }

#' @rdname casewhen_constructor
#' @keywords  internal

plural <- function(x, plural = "s") {
  if (length(x) > 1) {
    return(plural)
  } else {
    ""
  }
}
