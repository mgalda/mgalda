#' util
#'
#'
#' @name pipes_utils
#' @rdname pipes_utils
#' @examples
#'
#' \dontrun{
#' cons_res <- function(r1, r2, r3, r4) {
#'   list(
#'     "%>%" = r1,
#'     "%|>%" = r2,
#'     "%!>%" = r3,
#'     "%?>%" = r4
#'   )
#' }
#'
#' ## ej1
#'
#' foo4 <- foo3 <- foo2 <- foo1 <- FALSE
#' TRUE %>% assign("foo1", .)
#' TRUE %|>% assign("foo2", .)
#' TRUE %!>% assign("foo3", .)
#' TRUE %?>% assign("foo4", .)
#'
#' cons_res(foo1,foo2,foo3,foo4)
#'
#' ## ej2
#'
#' foo1 <- function() {
#'   TRUE %>% return()
#'   FALSE
#' }
#' foo2 <- function() {
#'   TRUE %|>% return()
#'   FALSE
#' }
#' foo3 <- function() {
#'   TRUE %!>% return()
#'   FALSE
#' }
#' foo4 <- function() {
#'   TRUE %?>% return()
#'   FALSE
#' }
#'
#' cons_res(foo1(),foo2(),foo3(),foo4())
#'
#' ## ej3
#'
#' foobar <- function(x) x %>% quux()
#' quux <- function(x) x %>% stop()
#'
#' "tilt" %>% foobar()
#' foo1<-rlang::last_trace()
#'
#' foobar <- function(x) x %|>% quux()
#' quux <- function(x) x %|>% stop()
#'
#' "tilt" %|>% foobar()
#' foo2<-rlang::last_trace()
#'
#' foobar <- function(x) x %!>% quux()
#' quux <- function(x) x %!>% stop()
#'
#' "tilt" %!>% foobar()
#' foo3<-rlang::last_trace()
#'
#' foobar <- function(x) x %?>% quux()
#' quux <- function(x) x %?>% stop()
#'
#' "tilt" %?>% foobar()
#' foo4<-rlang::last_trace()
#'
#' cons_res(foo1,foo2,foo3,foo4)
#'
#' ## ej4
#'
#' foo1 <- function() {
#'   TRUE %>% return()
#'   FALSE
#' }
#' foo2 <- function() {
#'   TRUE %|>% return()
#'   FALSE
#' }
#' foo3 <- function() {
#'   TRUE %!>% return()
#'   FALSE
#' }
#' foo4 <- function() {
#'   TRUE %?>% return()
#'   FALSE
#' }
#'
#' cons_res(
#'   foo1(),
#'   foo2(),
#'   foo3(),
#'   foo4()
#' )
#'
#' ## ej5
#'
#' cons_res(
#'   try("foo" %>% list(., .)),
#'   try("foo" %|>% list(., .)),
#'   try("foo" %!>% list(., .)),
#'   try("foo" %?>% list(., .))
#' )
#'
#' ## ej6
#'
#' cons_res(
#'   try({
#'     stop("oh no") %>% try(silent = TRUE)
#'     "success"
#'   }),
#'   try({
#'     stop("oh no") %|>% try(silent = TRUE)
#'     "success"
#'   }),
#'   try({
#'     stop("oh no") %!>% try(silent = TRUE)
#'     "success"
#'   }),
#'   try({
#'     stop("oh no") %?>% try(silent = TRUE)
#'     "success"
#'   })
#' )
#'
#' factory <- function(x) function() x
#' foo1 <- TRUE %>% factory()
#' foo2 <- TRUE %|>% factory()
#' foo3 <- TRUE %!>% factory()
#' foo4 <- TRUE %?>% factory()
#'
#' cons_res(foo1(),foo2(),try(foo3()),foo4())
#'
#' ## ej7
#'
#' faulty <- function() stop("tilt")
#' f <- function(x) x + 1
#' g <- function(x) x + 2
#' h <- function(x) x + 3
#'
#' faulty() %>% f() %>% g() %>% h()
#' foo1<-capture.output(traceback())
#' faulty() %|>% f() %|>% g() %|>% h()
#' foo2<-capture.output(traceback())
#' faulty() %!>% f() %!>% g() %!>% h()
#' foo3<-capture.output(traceback())
#' faulty() %?>% f() %?>% g() %?>% h()
#' foo4<-capture.output(traceback())
#'
#' cons_res(foo1,foo2,foo3,foo4)
#'
#' dput_source(cat(opps,sep = "\n"))
#'
#' has_attr(has_attr, "fail")
#' x <- 10
#' x %has_attr% "a"
#'
#' y <- list(a = 1, b = 2)
#' y %has_name% "c"
#' y %has_name% c("a", "g", "f")
#'
#' par(mar = c(1,1,1,1))
#'
#' rnorm(200) %>%
#'   matrix(ncol = 2) %T>%
#'   plot %>%
#'   colSums
#'
#'
#' y <- set_names(sample(1:4,10,T),sample(letters[1:10]))
#'
#' y %where_name% c("a", "b", "c")
#'
#' foo<-function(a,b,c) sum(a*b/c)
#' foo %edit_args% alist(a=1,b = 2,c=3)
#'
#' y %~in_nm% c("a", "b", "c")
#' y %~in% c(2,3)
#'
#' #%insd%
#' x<-runif(20,0,10)
#' x %insd% c(3,7)
#' x %class% "try-error" %attr% list(length = length(x))
#'
#' x %<a-% runif(1)
#' x
#' x %<a-% runif(10)
#' x
#' rm(x)
#'
#' x %<c-% 10
#' y %<c-% 1 + 2
#' y
#' z %<c-% (1 + 2)
#' z
#'
#' rm(list = ls())
#' x %<d-% (a + b)
#' a <- 10
#' b <- 100
#' x
#'
#' par(mar=c(1,1,1,1))
#'
#' rnorm(200) %>%
#'   matrix(ncol = 2) %T>%
#'   plot %>%
#'   colSums
#'
#' x <- c("a", "b", "d")
#' y <- c("a", "c", "d")
#'
#' x %u% y
#' x %n% y
#' x %diff% y
#'
#' M <- matrix(1:6, ncol = 2)
#' 4 %x% M
#' diag(1, 3) %x% M
#'
#'
#' x <- 1:4
#' (z <- x %*% x)
#' drop(z)
#'
#' y <- diag(x)
#' z <- matrix(1:12, ncol = 3, nrow = 4)
#' y %*% z
#' y %*% x
#' x %*% z
#'
#' x <- -1:12
#' x %% 2
#' x %/% 5
#' x %% Inf
#'
#' }
#'
 #'
#' @keywords internal
NULL

#' @importFrom magrittr %>%
#' @export
`%>%` <- magrittr::`%>%`
#' @importFrom magrittr pipe_nested
#' @export
`%|>%` <- magrittr::pipe_nested
#' @importFrom magrittr pipe_eager_lexical
#' @export
`%!>%` <- magrittr::pipe_eager_lexical
#' @importFrom magrittr pipe_lazy_masking
#' @export
`%?>%` <- magrittr::pipe_lazy_masking
#' @importFrom magrittr %$%
#' @export
`%$%` <- magrittr::`%$%`
#' @importFrom magrittr %T>%
#' @export
`%T>%` <- magrittr::`%T>%`
#' @importFrom magrittr %<>%
#' @export
`%<>%` <- magrittr::`%<>%`

#' @importFrom ggplot2 %+replace%
#' @export
`%+replace%` <- ggplot2::`%+replace%`

#' @export
`%n%` <- function(a, b) {
  intersect(a, b)
}

#' @export
`%u%` <- function(a, b) {
  union(a, b)
}

#' @export
`%diff%` <- function(a, b) {
  setdiff(a, b)
}

#' @export
`%xor%` <- function(a, b) {
  xor(a, b)
}

#' @export
`%||%` <- function(x, y) {
  if (is_true(is_empty(x))) {
    y
  } else {
    x
  }
}

#' @export
`%|0|%` <- function(x, y) {
  if (!length(x)) y else x
}

#' @export
`%!in%` <- function(x, y) {
  !x %in% y
}

#' @export
`%out%` <- function(x, y) {
  inherits(x, "numeric")
  inherits(y, "numeric")
  w <- x[x > max(y)]
  z <- x[x < min(y)]
  c(w, z)
}

#' @export
`%~in_nm%` <- function(x, y) {
  x[names(x) %in% y]
}
#' @export
`%~in%` <- function(x, y) {
  x[x %in% y]
}


#' @export
`%is%` <- function(x, y) {
  invisible(suppressall(testthat::expect_equal(
    object = x, expected = y
  )))
}

#' @export
`%insd%` <- function(x, y) {
  check_inherits(x, "numeric")
  check_inherits(y, "numeric")
  x <- x[x < max(y)]
  x <- x[x > min(y)]
  x
}

#' @export
`%pdput%` <- function(x, .f = NULL) {
  if (is_true(.f == "output")) {
    capture_output(x)
  } else if (is_true(.f == "dput")) {
    dput_source(x)
  } else {
    .fx <- do_try_ier(match.fun(.f))
    if (is_ier(.fx)) {
      .fx <- match.call()
      .fx <- .fx$.f
      if (is.call(.fx) | is.name(.fx)) {
        if (is_fn(eval(.fx)) & is_name(.fx)) {
          .fx <- call(deparse(.fx))
        }
        .fx$x <- x
        return(rstudioapi::insertText(do_try(
          eval(dput_source(.fx)), dput_source(x)
        )))
      } else {
        return(eval_bare(rstudioapi::insertText(deparse(x)), env = env_call()))
      }
    } else {
      dput_source(.fx(x))
    }
  }

  invisible()
}

#' @export
has_attr <- function(x, which) {
  !is.null(attr(x, which, exact = TRUE))
}
#' @export
`%has_attr%` <- has_attr

#' @export
where_name<-function(x, which) {
  seq_along(x)[names(x) %in% which]
}
#' @export
`%where_name%` <- where_name

#' @export
has_name <- function(x, which) {
  all(which %in% names(x))
}
#' @export
`%has_name%` <- has_name

#' @export
formals_modify <- function(.f, args) {
  inherits(x = .f, what = c("call", "function"))
  inherits(x = args, what = c("pairlist", "list"))

  forms <- as_l(formals(.f))

  if (length(forms) == 0) {
    forms <- args
  } else {
    forms <- modifyList(x = forms, val = as_l(args))
    act <-names2(forms)
    ags_nm <- names(args)
    ags_nm <- base::intersect(ags_nm,act)
    act[act%in% ags_nm] <- ags_nm
    forms <- forms[act]
  }
  forms = as_prls(forms)
  body <- body(.f)
  stopifnot(
    all_named(forms),
    is.language(body)
  )
  env <- to_env(env)

  eval(call("function", forms, body), parent.frame())

}
#' @export
`%edit_args%` <- formals_modify
#' @export
`%rm_args%` <- function(.f, args) {
  inherits(x = .f, what = c("call", "function"))
  inherits(
    x = args,
    what = c("pairlist", "list", "name", "character")
  )

  if (is.name(args)) {
    args <- lang2str(args)
  } else if (is.list(args)) {
    args <- names(args)
  } else if (is_prls(args)) {
    args <- prls_name(args)
    args <- names(args)
  }

  formals(.f) <- formals(.f)[formals_names(.f) %!in% args]
  .f
}

#' @export
`%==%` <- function(x, y) identical(x, y)
#' @export
`%!=%` <- function(x, y) !identical(x, y)

#' @export
`%<a-%` <- function(x, value) {
  x <- substitute(x)
  value <- substitute(value)

  if (!is.name(x)) cat_stop("Left-hand side must be a name")

  env <- parent.frame()
  f <- make_function(alist(value = ), value, env)

  # Mimic regular assignment operation which overrides existing bindings
  if (exists(deparse(x), envir = env, inherits = FALSE)) {
    rm(list = deparse(x), envir = env)
  }

  makeActiveBinding(deparse(x), f, env)
}

#' @export
`%<c-%` <- function(x, value) {
  name <- substitute(x)
  if (!is.name(name)) cat_stop("Left-hand side must be a name")

  env <- parent.frame()
  assign(as.character(name), value, env)
  lockBinding(name, env)

  invisible(value)
}

#' @export
`%<d-%` <- function(x, value) {
  name <- substitute(x)
  value <- substitute(value)

  if (!is.name(name)) cat_stop("Left-hand side must be a name")

  env <- parent.frame()
  call <- substitute(delayedAssign(deparse(name), value,
                                   eval.env = env, assign.env = env), list(value = value))
  eval(call)

  invisible()
}

# impute & extract ---

#' @export
extract <- `[`

#' @export
extract2 <- `[[`

#' @export
use_series <- `$`

#' @export
not <- `!`

#' @export
set_colnames <- `colnames<-`
#' @export
`%colnm%`<- set_colnames

#' @export
set_rownames <- `rownames<-`
#' @export
`%rownm%`<- set_rownames

#' @export
set_names <- function(x, names = x) {
  names(x) <- names
  x
}

#' @export
set_class <- `class<-`
#' @export
`%class%` <- function(x, y) {
  class(x) <- y
  x
}

#' @export
inset <- `[<-`

#' @export
inset2 <- `[[<-`

#' @export
set_attr <- `attr<-`

#' @export
set_attributes <- `attributes<-`
#' @export
`%attr%` <- function(x, y) {
  attributes(x) <- y
  x
}

#' @export
`%@%` <- rlang::`%@%`

#' @export
`%@%<-` <- rlang::`%@%<-`

# coerce ---
#' @export
as_error <- function(...) {
  e<-list()
  class(e) <- c("simpleError", "error", "condition")
  .expr<-rlang::expr(invisible(return(!!e)))
  if(env_call() %==% .GlobalEnv){
    return(e)
  }
  rlang::eval_bare(expr = .expr, env = parent.frame())
}
#' @export
as_prls <- function(x){
  as.pairlist(x)
}

#' @export
as_l <- as.list

#' @export
as_vct <- function(x, mode = "any") {
  as.vector(x = x, mode = mode)
}

#' @export
as_dbl <- function(x) {
  as.double(x)
}

#' @export
as_num <- function(x) {
  as.numeric(x)
}

#' @export
as_int <- function(x) {
  as.integer(x)
}

#' @export
as_fct <- function(x) {
  as.factor(x)
}

#' @export
as_chr <- function(x) {
  as.character(x)
}

#' @export
as_lgl <- function(x) {
  as.logical(x)
}

#' @importFrom rlang as_function
#' @export
as_fn <- rlang::as_function

#' @export
as_df <- as.data.frame

#' @export
as_mtx <- as.matrix

#' @importFrom tibble as_tibble
#' @export
as_tbl <- tibble::as_tibble

#' @importFrom rlang as_string
#' @export
as_str <-   rlang::as_string

#' @export
as_null <- function(...) {
  do_try(expr = list(...), error = NULL)
  NULL
}

#' @export
as_true<- function(...){
  do_try(...)
  TRUE
}

#' @export
as_false<- function(...){
  do_try(...)
  FALSE
}

#' @export
as_na<- function(...){
  do_try(...)
  NA
}

#' @export
as_name <- function(x) {
  UseMethod("as_name")
}

#' @export
as_name.name <- function(x) {
  x
}

#' @export
as_name.character <- function(x) {
  if (length(x) > 1) {
    cat_stop("Can not coerce character vector of length > 1 to name")
  }

  as.name(x)
}

#' @export
as_name.call <- function(x) {
  x[[1]]
}

#' @export
as_name.formula <- function(x) {
  as_name(f_rhs(x))
}

#' @export
as_call <- function(x) {
  UseMethod("as_call")
}

#' @export
as_call.name <- function(x) {
  make_call(x)
}

#' @export
as_call.call <- function(x) {
  x
}

#' @export
as_call.character <- function(x) {
  if (length(x) > 1) {
    cat_stop("Can not coerce character vector of length > 1 to name")
  }

  parse(text = x)[[1]]
}

#' @export
as_call.formula <- function(x) {
  as_call(f_rhs(x))
}


#' @export
as_frm <- function(x, ...) {
  UseMethod("as_frm", x)
}
#' @export
as_frm.default <- function(x, env = parent.frame()) {
  if (inherits(x, "formula")) {
    x
  } else {
    rval <- formula(x, env = baseenv())
    if (identical(environment(rval), baseenv()) || !missing(env)) {
      environment(rval) <- env
    }
    rval
  }
}
#' @export
as_frm.formula <- function(x, ...) {
  x
}
#' @export
as_frm.call <- function(x, ...) {
  res <- ~x
  res[[2]] <- x[[2]]
  res
}
#' @export
as_frm.name <- function(x, env = parent.frame(), ...) {
  res <- ~x
  environment(res) <- env
  res[[2]] <- x
  res
}


