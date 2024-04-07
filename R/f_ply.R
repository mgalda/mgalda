#' Apply func
#'
#' @name  f_ply
#' @rdname f_ply
#' @keywords internal
NULL

#' @rdname f_ply
#' @export
lgl_all <- function(.x,.f,...){
  all(map_lgl(.x,.f,...))
}

#' @rdname f_ply
#' @export
lgl_any <- function(.x,.f,...){
  any(map_lgl(.x,.f,...))
}

#' @rdname f_ply
#' @export
chr_cllp <- function(.x, .f, ..., .collapse = "") {
  paste(map_chr(.x = .x, .f = .f, ...), collapse = .collapse)
}

#' @rdname f_ply
#' @export
cmp_map <- function(.x, .f, ...) {
  compact(map(.x = .x, .f = .f, ...))
}

#' @rdname f_ply
#' @export
cmp_map2 <- function(.x, .y, .f, ...) {
  compact(map2(.x = .x, .y = .y, .f = .f, ...))
}

#' @rdname f_ply
#' @export
cmp_pmap <- function(.l, .f, ...) {
  compact(pmap(.l = .l, .f = .f, ...))
}

#' @rdname f_ply
#' @export
map_maxdepth <- function(.x, .f, ...) {
  if (is.list(.x)) {
    x_depth <- map(.x, purrr::vec_depth)
    .is_depth <- map_lgl(x_depth, ~ .x > 0)
    if (any(.is_depth)) {
      x_depth[.is_depth] <-
        map(.x[.is_depth], ~ map_maxdepth(.x = .x, .f = .f, ...))
    }
    if (!all(.is_depth)) {
      x_depth[!.is_depth] <-
        map(.x[!.is_depth], ~ .f(.x=.x,...))
    }
    .x <- x_depth
  } else {
    .x <- .f(.x=.x, ...)
  }
  .x
}

#' @export
pluck <- purrr::pluck

#' @rdname f_ply
#' @export
flatten <- purrr::flatten

#' @rdname f_ply
#' @export
cmp_flatten <- function(.x) {
  compact(flatten(.x))
}

#' @rdname f_ply
#' @export
cmp_ftn_map <- function(.x, .f, ...) {
  purrr::compact(flatten(purrr::map(.x, .f, ...)))
}

#' @rdname f_ply
#' @export
cmp_ftn_map_if <- function(.x,.p, .f, ...,.else = NULL) {
  purrr::compact(flatten(purrr::map_if(.x, .p, .f, ...,.else =  .else)))
}

#' @export
as_mapper <- purrr::as_mapper

#' @export
walk <- purrr::walk

#' @rdname f_ply
#' @param x vector of values to aggregate
#' @param .group grouping vector
#' @param .fn aggregation function
#' @param ... other arguments passed on to \code{.fun}
#' @param .default default value used for missing groups.  This argument is
#'   also used as the template for function output.
#' @export
#' @examples
#' # Some examples of use borrowed from ?tapply
#' n <- 17; fac <- factor(rep(1:3, length.out = n), levels = 1:5)
#' table(fac)
#' vaggr(1:n, fac, sum)
#' vaggr(1:n, fac, sum, .default = NA_integer_)
#' vaggr(1:n, fac, range)
#' vaggr(1:n, fac, range, .default = c(NA, NA) + 0)
#' vaggr(1:n, fac, quantile)
#'
#' # But it is about 10x faster
#' x <- rnorm(1e6)
#' y1 <- sample.int(10, 1e6, replace = TRUE)
#' system.time(tapply(x, y1, mean))
#' system.time(vaggr(x, y1, mean))
vaggr <- function(x, .group, .fn, ..., .default = NULL) {
  if (is.null(.default)) {
    .default <- suppressall(.fn(x[0], ...))
  }

  fun <- function(w) {
    if (length(w) == 0) return(.default)
    .fn(w, ...)
  }

  .value <- split(x, .group)
  vapply(.value, fun, .default)
}
#' @rdname f_ply
#' @export
vaggr_mean <- function(x, .group) {
  vaggr(x = x,.group = .group,.fn = mean)
}

#' @export
rollfn <- function(x, n, fn, fill = F, ...) {
  out <- rep(NA, length(x))

  offset <- trunc(n / 2)
  if (fill) {
    for (i in 1:length(x)) {
      xf <- (i - offset):(i + offset - 1)
      xf <- xf[xf > 0]
      xf <- xf[xf <= length(x)]
      out[i] <- fn(x[xf], ...)
    }
  } else {
    for (i in (offset + 1):(length(x) - n + offset + 1)) {
      out[i] <- fn(x[(i - offset):(i + offset - 1)], ...)
    }
  }
  out
}

#' @rdname f_ply
#' @export
slide_dbl <- function(.x,
                      .fn,
                      ...,
                      .size = 1,
                      .partial = c("none", "left", "right", "center"),
                      .fill = TRUE) {
  .partial <- match.arg(.partial)
  .partial <- switch(.partial,
                     left = .size,
                     right = 1,
                     center = ceiling(.size / 2),
                     none = FALSE
  )

  out <- numeric(if (.partial) {
    length(.x)
  } else {
    length(.x) - .size + 1
  })
  for (i in seq_along(out)) {
    idx <-
      seq.int(i + .size * (-1L + !.partial) + .partial,
              i + (.size * !.partial) - 1 + .partial,
              by = 1L
      )
    if (.fill) {
      idx <- idx[idx > 0 & idx <= length(.x)]
    } else {
      idx[idx <= 0] <- NA_integer_
    }

    out[i] <- .fn(.x[idx], ...)
  }
  out
}

#' @rdname f_ply
#' @export
simplify <- function(.x, .type = NULL) {
  can_simplify <- function(x, type = NULL, n = 10) {
    can_coerce <- function(x, type) {
      .is_mold <- function(type) {
        modes <- c(
          "numeric",
          "logical",
          "integer",
          "double",
          "complex",
          "character",
          "raw"
        )
        length(type) > 1 || (!type %in% modes)
      }
      actual <- typeof(x[[1]])
      if (.is_mold(type)) {
        lengths <- unique(map_int(x, length))
        if (length(lengths) > 1 || !(lengths == length(type))) {
          return(FALSE)
        } else {
          type <- typeof(type)
        }
      }
      if (actual == "integer" && type %in% c(
        "integer", "double",
        "numeric"
      )) {
        return(TRUE)
      }
      if (actual %in% c("integer", "double") &&
          type == "numeric") {
        return(TRUE)
      }
      actual == type
    }
    x <- sample(x, n, length(x) <= n)
    is_atomic <- vapply(x, is.atomic, logical(1))
    if (!all(is_atomic)) {
      return(FALSE)
    }
    mode <- unique(vapply(x, typeof, character(1)))
    if (length(mode) > 1 && !all(c("double", "integer") %in%
                                 mode)) {
      return(FALSE)
    }
    is.null(type) || can_coerce(x, type)
  }

  if (can_simplify(.x, .type)) {
    unlist(.x)
  } else {
    .x
  }
}

#' @rdname f_ply
#' @export
.un <- function(x) {
  unname(unlist(x))
}


#' @rdname f_ply
#' @export
probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_predicate(.p, ..., .mapper = TRUE)
    map_lgl(.x, .p, ...)
  }
}
keep <- function(.x, .f, ...) {
  .x[probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
list_along <- function (x) {
  vector("list", length(x))
}

transpose <- function(.l) {
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
  }

  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}
every <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
negate <- function(.p) {
  function(...) !.p(...)
}

detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}

map <- function(.x, .f, ...) {
  .f <- as_mapper(.f, ...)
  lapply(.x, .f, ...)
}
map_mold <- function(.x, .f, .mold, ...) {
  .f <- as_mapper(.f, ...)
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}
map2 <- function(.x, .y, .f, ...) {
  .f <- as_mapper(.f, ...)
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}
args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}
pmap <- function(.l, .f, ...) {
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  args <- args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}


map_lgl <- function(.x, .f, ...) {
  map_mold(.x, .f, logical(1), ...)
}
map_int <- function(.x, .f, ...) {
  map_mold(.x, .f, integer(1), ...)
}
map_dbl <- function(.x, .f, ...) {
  map_mold(.x, .f, double(1), ...)
}
map_chr <- function(.x, .f, ...) {
  map_mold(.x, .f, character(1), ...)
}
map_cpl <- function(.x, .f, ...) {
  map_mold(.x, .f, complex(1), ...)
}

map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
map2_cpl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "complex")
}

map_if <- function (.x, .p, .f, ..., .else = NULL) {
  sel <- probe(.x, .p)
  out <- list_along(.x)
  out[sel] <- map(.x[sel], .f, ...)
  if (is_null(.else)) {
    out[!sel] <- .x[!sel]
  }
  else {
    out[!sel] <- map(.x[!sel], .else, ...)
  }
  set_names(out, names(.x))
}
imap <- function(.x, .f, ...) {
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}

#' @rdname f_ply
#' @export
map2_if <- function(.x, .y, .p, .f, ...) {
  matches <- probe(.x, .p)
  .x[matches] <- map2(.x[matches], .y[matches], .f, ...)
  .x
}

#' @rdname f_ply
#' @export
imap_if <- function(.x, .p, .f, ...) {
  matches <- probe(.x, .p)
  .x[matches] <- imap(.x[matches], .f, ...)
  .x
}

invoke <- function (.f, .x = NULL, ..., .env = NULL) {
  .env <- .env %||% parent.frame()
  args <- c(as_l(.x), list(...))
  do.call(.f, args, envir = .env)
}
compact <- function(x) {
  if (all(lengths(x) > 0)) {
    return(x)
  }
  x <- do_try(Filter(function(elt) {
    !suppressall(is_empty(elt))
  }, x), x)
  x <- do_try(Filter(function(elt) {
    !inherits(elt, "try-error")
  }, x), x)
  x <- do_try(Filter(length, x), x)
  x
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

as_friendly_type <- function(type) {
  switch(type,
         logical = "a logical vector",
         integer = "an integer vector",
         numeric = ,
         double = "a double vector",
         complex = "a complex vector",
         character = "a character vector",
         raw = "a raw vector",
         string = "a string",
         list = "a list",
         `NULL` = "NULL",
         environment = "an environment",
         externalptr = "a pointer",
         weakref = "a weak reference",
         S4 = "an S4 object",
         name = ,
         symbol = "a symbol",
         language = "a call",
         pairlist = "a pairlist node",
         expression = "an expression vector",
         quosure = "a quosure",
         formula = "a formula",
         char = "an internal string",
         promise = "an internal promise",
         ... = "an internal dots object",
         any = "an internal `any` object",
         bytecode = "an internal bytecode object",
         primitive = ,
         builtin = ,
         special = "a primitive function",
         closure = "a function",
         type
  )
}
friendly_type_of <- function(x, length = FALSE) {
  paste_classes <- function(x) {
    paste(class(x), collapse = "/")
  }
  if (is.object(x)) {
    return(sprintf("a `%s` object", paste_classes(x)))
  }
  friendly <- as_friendly_type(typeof(x))
  if (length && is_vector(x)) {
    friendly <- paste0(friendly, sprintf(" of length %s", length(x)))
  }
  friendly
}
as_predicate <- function(.fn, ..., .mapper) {
  as_predicate_friendly_type_of <- function(x) {
    if (is_na(x)) {
      "a missing value"
    } else {
      friendly_type_of(x, length = TRUE)
    }
  }
  if (.mapper) {
    .fn <- as_mapper(.fn, ...)
  }
  function(...) {
    out <- .fn(...)
    if (!is_bool(out)) {
      msg <-
        sprintf(
          "Predicate functions must return a single `TRUE` or `FALSE`, not %s",
          as_predicate_friendly_type_of(out)
        )
      cat_stop(msg)
    }
    out
  }
}
