#' @include util_base.R
#' @include util_env.R
#' @include utils.R
#' @include util_print.R
NULL

#' test inheritance
#'
#' @name inheritance_tools
#' @rdname inheritance_tools
#' @keywords internal
NULL

is_true_try <- function (expr) {

  .doTryCatch <- function(expr, env = parent.frame()) {
    .Internal(.addCondHands("error", list(FALSE), env, environment(),FALSE))
    expr
  }
  v <- .doTryCatch(return(expr), env = parent.frame())
  ifelse(is.logical(v),expr,FALSE)
}
#' @rdname inheritance_tools
#' @export
is_unique_scalar <- function(x) {
  v <- rep_along(FALSE, x)
  id <- names(table(x) == 1)
  v[x %in% id] <- TRUE
  v
}
#' @rdname inheritance_tools
#' @export
is_bool <- function(x) {
  if(length(x) > 1){
    return(FALSE)
  }
  is_true_try(is.logical(x) && !is.na(x))
}

#' @rdname inheritance_tools
#' @export
is_prls <- function(x) {
  is_true(is.pairlist(x))
}

#' @rdname inheritance_tools
#' @export

is_l <- function(x) {
  is_true(is.list(x))
}

#' @rdname inheritance_tools
#' @export
is_vct <- function(x) {
  is_true(is.vector(x))
}

#' @rdname inheritance_tools
#' @export

is_frm <- function(x) {
  is_true(inherits(x, "formula"))
}

#' @rdname inheritance_tools
#' @export

is_fn <- function(x) {
  is_true(is.function(x))
}

#' @rdname inheritance_tools
#' @export

is_mtx <- function(x) {
  is_true(is.matrix(x))
}

#' @rdname inheritance_tools
#' @export

is_tbl <- function(x) {
  is_true(tibble::is_tibble(x))
}

#' @rdname inheritance_tools
#' @export
is_str <- function(x, string = NULL) {
    is_true(rlang::is_string(x, string = string))
  }


#' @rdname inheritance_tools
#' @export
is_atomic <- function(x, .len = NULL) {
  if (!is_empty(.len)) {
    if (is_true(length(x) != .len)) {
      return(FALSE)
    }
  }
  is_true(typeof(x) %in% c("logical", "integer", "double", "complex", "character", "raw"))
}

#' @rdname inheritance_tools
#' @export
is_vector <-
  function(x,
           .len = NULL,
           .min_len = NULL,
           .max_len = NULL) {
    if (!is.vector(x)) {
      return(msg_false("'x' no es un vector"))
    }
    if (!is_null(.len)) {
      if (length(x) != .len) {
        return(msg_false(
          "x es de largo %s y se requiere %s",
          length(x),
          .len
        ))
      }
    }

    if (!is_null(.min_len)) {
      if (length(x) < .min_len) {
        return(msg_false(
          "x es de largo %s y se requiere minimo %s",
          length(x),
          .min_len
        ))
      }
    }

    if (!is_null(.max_len)) {
      if (length(x) > .max_len) {
        return(msg_false(
          "x es de largo %s y se requiere maximo %s",
          length(x),
          .max_len
        ))
      }
    }
    return(TRUE)
  }


#' @rdname inheritance_tools
#' @export
is_sf <- function(x) {
  if (is_true(inherits(x, "sf"))) {
    if (is.na(st_crs(x)$wkt)) {
      cat_warn("crs is NA")
    }
    TRUE
  } else {
    FALSE
  }
}


#' @rdname inheritance_tools
#' @export
is_num <- function(x, .int = FALSE, .pos = FALSE, .finite = TRUE) {
  if (!is_dblint(x)) {
    return(FALSE)
  }
  if (length(x) != 1) {
    return(FALSE)
  }

  if (.finite) {
    if (!any(is_numvalid(x))) {
      return(FALSE)
    }
  }

  if (.pos) {
    if (min(x, na.rm = TRUE) < 0) {
      return(FALSE)
    }
  }

  if (.int) {
    if (!is_true(all(x == round(x)))) {
      return(FALSE)
    }
  }

  TRUE
}

#' @rdname inheritance_tools
#' @export
is_sorted <- function(x) {
  if (!is_true(is.vector(x))) {
    return(FALSE)
  }
  is_true(!is.unsorted(x))
}


#' @rdname inheritance_tools
#' @export
is_increasing <- function(x) {
  if (!is_dblint(x)) {
    return(FALSE)
  }
  c(TRUE,  x[-1] - x[-length(x)] > 0)
}


#' @rdname inheritance_tools
#' @export
is_df <- function(x) {
  is_true(inherits(x, "data.frame"))
}


#' @rdname inheritance_tools
#' @export
is_nonnum <- function(x) {

  if (!is_dblint(x)) {
    return(rep(TRUE, length(x)))
  }
  !is.finite(x)
}


#' @rdname inheritance_tools
#' @export
is_numvalid <- function(x) {
  is.finite(x) & is_dblint(x)
}


#' @rdname inheritance_tools
#' @export
is_even <- function(x) {
  if (!is_dblint(x)) {
    return(rep(FALSE, length(x)))
  }

  x %% 2 == 0
}


#' @rdname inheritance_tools
#' @export
is_fctchr <- function(x) {
  is_true(is.factor(x) | is.character(x))
}


#' @rdname inheritance_tools
#' @export
is_dblint <- function(x) {
  is_true(is.integer(x) | is.double(x))
}


#' @rdname inheritance_tools
#' @export
is_odd <- function(x) {
  if (!is_dblint(x)) {
    return(rep(FALSE, length(x)))
  }
  x %% 2 != 0
}


#' @rdname inheritance_tools
#' @export
is_true <- function(x) {
  do_try(expr = identical(x, TRUE), error = FALSE)
}

#' @rdname inheritance_tools
#' @export
is_nottrue <- function(x) {
  not(do_try(expr = identical(x, TRUE), error = FALSE))
}

#' @rdname inheritance_tools
#' @export
is_false <- function(x) {
  do_try(expr = identical(x, FALSE), error = FALSE)
}

#' @rdname inheritance_tools
#' @export
is_nonempty <- function(x) {
  is_true(length(x) > 0) || is_true(is.object(x))
}

#' @rdname inheritance_tools
#' @export
is_empty <- function(x) {
  if (is_true(missing(x))) {
    return(TRUE)
  }
  if (is_true(is.null(x))) {
    return(TRUE)
  }
  if (is_true(length(x) == 0)) {
    return(TRUE)
  }
  if (is_true(x == "")) {
    return(TRUE)
  }
  if (is_true(rlang::is_reference(x, quote(expr = )))) {
    return(TRUE)
  }
  if (is_langlist(x)) {
    if (is_true(is.null(expr_get_quotable(x)))) {
      return(TRUE)
    }
  }
  FALSE
}

#' @rdname inheritance_tools
#' @export
is_emptyna <- function(x) {
  if (is_empty(x)) {
    return(TRUE)
  }
  if (length(x) == 1) {
    if (is.na(x)) {
      return(TRUE)
    }
  }

  FALSE
}


#' @rdname inheritance_tools
#' @export
is_valid <- function(x) {
  if (is_empty(x)) {
    return(FALSE)
  }
  if(is.atomic(x)){
    if(is_dblint(x)){
      return(is.finite(x))
    } else {
      return(!is.na(x))
    }
  }
  is.object(x)

}


#' @rdname inheritance_tools
#' @export
is_named <- function(x) {
  all(are_named(x))
}


#' @export
is_lang <- function(x) {
  is_call(x) || is_pairlist(x) || is_atomic(x) || is_name(x) || is_true(is.null(x))
}


#' @rdname inheritance_tools
#' @export
is_name <- function(x) {
  is_true(typeof(x) == "symbol")
}

#' @rdname inheritance_tools
#' @export
is_prob <- function(x) {
  is_dblint(x) & x >= 0 & x <= 1
}


#' @rdname inheritance_tools
#' @export
is_try <- function(x) {
  is_true(inherits(x, "try-error"))
}


#' @rdname inheritance_tools
#' @export
is_call <- function(x,name = NULL,n = NULL,ns = NULL) {
  .is_call <- function(x,
                       name = NULL,
                       n = NULL,
                       ns = NULL) {
    is_namespaced_symbol <- function(x,
                                     ns = NULL,
                                     private = NULL) {
      namespace_sym <- quote(`::`)
      namespace2_sym <- quote(`:::`)

      if (typeof(x) != "language") {
        return(FALSE)
      }
      if (!is_null(ns) &&
          !identical(rlang::node_cadr(x), rlang::sym(ns))) {
        return(FALSE)
      }
      head <- rlang::node_car(x)
      if (is_null(private)) {
        identical(head, namespace_sym) || identical(head, namespace2_sym)
      } else if (private) {
        identical(head, namespace2_sym)
      } else {
        identical(head, namespace_sym)
      }
    }
    is_namespaced_call <- function(x,
                                   ns = NULL,
                                   private = NULL) {
      if (typeof(x) != "language") {
        return(FALSE)
      }
      if (!is_namespaced_symbol(rlang::node_car(x), ns, private)) {
        return(FALSE)
      }
      TRUE
    }
    if (typeof(x) != "language") {
      return(FALSE)
    }

    call_unnamespace <- function(x) {
      if (is_namespaced_call(x)) {
        call <- rlang::call2(rlang::node_cadr(rlang::node_cdar(x)))
        rlang::node_poke_cdr(call, rlang::node_cdr(x))
      } else {
        x
      }
    }

    if (!is_null(ns)) {
      good_ns <- FALSE

      for (elt in ns) {
        if (identical(elt, "") && !is_namespaced_call(x, private = FALSE)) {
          good_ns <- TRUE
          break
        } else if (is_namespaced_call(x, elt, private = FALSE)) {
          good_ns <- TRUE
          break
        }
      }

      if (!good_ns) {
        return(FALSE)
      }
    }

    x <- call_unnamespace(x)

    if (!is_null(name)) {
      if (!is_vector(name)) {
        name <- list(name)
      }

      unmatched <- TRUE
      for (elt in name) {
        if (identical(rlang::node_car(x), rlang::sym(elt))) {
          unmatched <- FALSE
          break
        }
      }

      if (unmatched) {
        return(FALSE)
      }
    }

    if (!is_null(n) && !has_length(x, n + 1L)) {
      return(FALSE)
    }

    TRUE
  }

  is_true(.is_call(
    x = x,
    name = name,
    n = n,
    ns = ns
  ))
}

#' @rdname inheritance_tools
#' @export
is_symbol <- function(x, name = NULL) {
  if (!is_true(typeof(x) == "symbol")) {
    return(FALSE)
  }
  if (is_empty(name)) {
    return(TRUE)
  }
  as_str(x) %in% name
}


#' @rdname inheritance_tools
#' @export
is_same_type <- function(x, y) {
  is_true(identical(typeof(x), typeof(y)) ||
            (is.numeric(x) && is.numeric(y)))
}


#' @rdname inheritance_tools
#' @export
is_integerish <- rlang::is_integerish

#' @rdname inheritance_tools
#' @export
is_na_value <- function(x) {
  is_true(is.vector(x) && is.na(x) && length(x) == 1)
}


#' @rdname inheritance_tools
#' @export
is_dbl <- function(x, n = NULL, finite = FALSE) {
  if (!is_true(is.double(x))) {
    return(FALSE)
  }
  if (!all(is.finite(x)) & !finite) {
    return(FALSE)
  }
  if (!is_empty(n)) {
    if (length(x) != n) {
      return(FALSE)
    }
  }

  TRUE
}

#' @rdname inheritance_tools
#' @export
is_int <- function(x, n = NULL, finite = FALSE) {
  .int <- f(is_true(is.integer(x) | identical(trunc(x), x)))

  if (!(.int(x) & is_atomic(x))) {
    return(FALSE)
  }

  if (!all(is.finite(x)) & !finite) {
    return(FALSE)
  }

  if (!is_empty(n)) {
    if (length(x) != n) {
      return(FALSE)
    }
  }

  TRUE
}


#' @rdname inheritance_tools
#' @export
is_chr <- function(x) {
  is_true(is.atomic(x) & inherits(x, "character"))
}


#' @rdname inheritance_tools
#' @export
is_fct <- function(x) {
  is_true(is.atomic(x) & inherits(x, "factor"))
}


#' @rdname inheritance_tools
#' @export
is_symname <- function(x) {
  is_true(typeof(x) %in% c("language", "symbol"))
}


#' @rdname inheritance_tools
#' @export
is_expr <- function(x) {
  is_true(is.expression(x))
}

#' @rdname inheritance_tools
#' @export
is_lazy_dots <- function(x) {
  is_true(inherits(x, "lazy_dots"))
}

#' @rdname inheritance_tools
#' @export
is_quosures <- function (x) {
  is_true(inherits(x, "quosures"))
  }


#' @rdname inheritance_tools
#' @export
is_lazy <- function(x) {
  inherits(x, c("lazy", "lazys"))
}

#' @export
is_quoted <- function(x) {
  is_true(inherits(x, "quoted"))
}

#' @export
is_quotable <- function(x) {
  is_langlist(x) | is_langatom(x)
}
#' @export
is_langatom <- function(x) {
  is_true(is_lazy(x) | is_quosure(x))
}
#' @export
is_langlist <- function(x) {
  is_true(is_lazy_dots(x) | is_quosures(x) | is_quoted(x))
}


#' @rdname inheritance_tools
#' @export
is_valid_name <- function(x) {
  is_short_enough <- is_true(nchar(x) <= 10000L)
  is_valid_name <- is_true(make.names(x) == x)

  is_short_enough && is_valid_name
}


#' @rdname inheritance_tools
#' @export
is_installed <- function(pkg) {
  all(map_lgl(pkg, function(x) {
    is_true(requireNamespace(x, quietly = TRUE))
  }))
}


#' @rdname inheritance_tools
#' @export
is_null <- function(x) {
  is_true(is.null(x))
}


#' @rdname inheritance_tools
#' @export
is_existing <- function(x,
                        envir = parent.frame(),
                        inherits = TRUE,
                        .xname = names_in_parent(x)) {
  x <- suppressall(as_chr(x))
  if (is_empty(x)) {
    return(logical(0))
  }
  if (length(x) > 1L) {
    return(map_lgl(x,
                   is_existing,
                   envir    = envir,
                   inherits = inherits
    ))
  }
  if (!exists(x,
              envir    = envir,
              inherits = inherits
  )) {
    return(msg_false("%s does not exist.", .xname))
  }
  TRUE
}


#' @rdname inheritance_tools
#' @export
is_formula <- rlang::is_formula

#' @rdname inheritance_tools
#' @export
is_equal <- function(x, y, ...) {
  UseMethod("is_equal")
}

#' @rdname inheritance_tools
#' @export
is_equal.default <- function(x, y, ..., by_element = TRUE) {
  if (is_true(length(x) != length(y))) {
    return(FALSE)
  }
  if (is.atomic(x)) {
    if (by_element & length(x) > 1) {
      return(sapply(seq_along(x), function(w) {
        mgalda::is_true(all.equal(
          target = x[w],
          current = y[w]
        ))
      }))
    } else {
      return(mgalda::is_true(all.equal(
        target = x, current = y
      )))
    }
  }
  is_true(identical(deparse(x), deparse(y)))
}

#' @rdname inheritance_tools
#' @export
is_equal.numeric <- function(x,
                             y,
                             .tol = 1e-4,
                             by_element = TRUE) {
  if (is_true(length(x) != length(y))) {
    return(FALSE)
  }
  if (by_element & length(x) > 1) {
    sapply(seq_along(x), function(w) {
      mgalda::is_true(all.equal.numeric(
        target = x[w],
        current = y[w],
        tolerance = .tol
      ))
    })
  } else {
    mgalda::is_true(all.equal.numeric(
      target = x,
      current = y,
      tolerance = .tol
    ))
  }
}
#' @rdname inheritance_tools
#' @export
is_equal.integer <- function(x,
                             y,
                             .tol = 0,
                             by_element = TRUE) {
  if (is_true(length(x) != length(y))) {
    return(FALSE)
  }
  if (by_element & length(x) > 1) {
    sapply(seq_along(x), function(w) {
      mgalda::is_true(all.equal.numeric(
        target = x[w],
        current = y[w],
        tolerance = .tol
      ))
    })
  } else {
    mgalda::is_true(all.equal.numeric(
      target = x,
      current = y,
      tolerance = .tol
    ))
  }
}

#' @rdname inheritance_tools
#' @export
is_equal.double <- function(x,
                            y,
                            .tol = 1e-4,
                            by_element = TRUE) {
  if (is_true(length(x) != length(y))) {
    return(FALSE)
  }
  if (by_element & length(x) > 1) {
    sapply(seq_along(x), function(w) {
      mgalda::is_true(all.equal.numeric(
        target = x[w],
        current = y[w],
        tolerance = .tol
      ))
    })
  } else {
    mgalda::is_true(all.equal.numeric(
      target = x,
      current = y,
      tolerance = .tol
    ))
  }
}



#' @rdname inheritance_tools
#' @export
is_colour <- function(x) {
  UseMethod("is_colour", x)
}
#' @rdname inheritance_tools
#' @export
is_colour.character <- function(x) {
  grepl("#[a-f0-9]{6}", x, TRUE) |
    x %in% grDevices::colors() | is.na(x)
}
#' @rdname inheritance_tools
#' @export
is_colour.numeric <- function(x) {
  x %in% seq_along(grDevices::palette())
}
#' @rdname inheritance_tools
#' @export
is_colour.logical <- function(x) {
  is.na(x)
}
#' @rdname inheritance_tools
#' @export
is_colour.factor <- function(x) {
  is_colour.character(as_chr(x))
}

is_operator <- function(x, ...) {
  UseMethod("is_operator", x)
}
#' @export
is_operator.default <- function(x, ...) {
  FALSE
}
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
      function(op) {
        identical(x, name2fun(op))
      }
    ))
  } else {
    any(sapply(
      operators("REG"),
      function(op) {
        identical(x, name2fun(op))
      }
    )) ||
      any(sapply(
        operators("UNREG"),
        function(op) {
          identical(x, name2fun(op))
        }
      ))
  }
}

#' @rdname inheritance_tools
#' @export
is_onesided_frm <- function(x, ...) {
  is.name(x[[1]]) &&
    deparse(x[[1]]) %in% c("~", "!") &&
    length(x) == 2
}

#' @rdname inheritance_tools
#' @export
is_twosided_frm <- function(x, ...) {
  is.name(x[[1]]) &&
    deparse(x[[1]]) %in% operators() &&
    length(x) == 3
}

#' @rdname inheritance_tools
#' @export
is_monotonic_increasing <-
  function(x,strictly = FALSE,.xname = names_in_parent(x)) {
    if (anyNA(x)) {
      return(na(
        "There are missing values, so monotonicity cannot be determined."
      ))
    }
    res <- which(if (strictly) {
      diff(x) <= 0
    } else {
      diff(x) < 0
    })
    is_empty(res)
  }

#' @rdname inheritance_tools
#' @export
is_monotonic_decreasing <-
  function(x, strictly = FALSE) {
    if (anyNA(x)) {
      return(NA)
    }
    res <- which(if (strictly) {
      diff(x) >= 0
    } else {
      diff(x) > 0
    })
    is_empty(res)
  }

#' @rdname inheritance_tools
#' @export
is_inrange <- function(x, xmin = 0, xmax = 1) {
  if (is_nottrue(is_dblint(x))) {
    return(FALSE)
  }
  if (is_nonempty(xmin) && any(x < xmin)) {
    return(FALSE)
  }

  if (is_nonempty(xmax) && any(x > xmax)) {
    return(FALSE)
  }
  TRUE
}

#' @rdname inheritance_tools
#' @export
is_string <- function(x) {
  is.character(x) && length(x) == 1
}

#' @rdname inheritance_tools
#' @export
is_number <- function(x) {
  is_true(is.numeric(x) && length(x) == 1)
}

#' @rdname inheritance_tools
#' @export
is_dir <- function(path) {
  is_string(path) &
    is_true(file.exists(path)) & is_true(file.info(path)[["isdir"]])
}

#' @rdname inheritance_tools
#' @export
is_syntactic_literal <- function(x) {
  switch(typeof(x),
         `NULL` = {
           TRUE
         },
         logical = ,
         integer = ,
         double = ,
         character = {
           length(x) == 1
         },
         complex = {
           if (length(x) != 1) {
             return(FALSE)
           }
           is_na(x) || Re(x) == 0
         },
         FALSE
  )
}

#' @rdname inheritance_tools
#' @export
is_pairlist <- function(x) {
  typeof(x) == "pairlist"
}

#' @keywords internal
#' @rdname inheritance_tools
#' @export
all_named <- function(x) {
  .is_df <- function(x) {
    is_true(inherits(x, "data.frame"))
  }
  if (.is_df(x) | is.matrix(x)) {
    nm <- colnames(x)
  } else {
    nm <- names(x)
  }
  !is.null(nm) && all(!is.na(nm) & nm != "")
}

#' @rdname inheritance_tools
#' @export
is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

#' @rdname inheritance_tools
#' @export
is_env <- function (x) {
  is_true(typeof(x) == "environment")
}

#' @rdname inheritance_tools
#' @export
is_emptyenv <- function(x){
  is_true(length(x)==0) & is_env(x)
}

#' @rdname inheritance_tools
#' @export
are_named <- function(x) {
  if (is.call(x)) {
    x <- rlang::get_expr(x)
  } else if (is.function(x)) {
    x <- formals(x)
  }

  map_lgl(.x = seq_along(x), function(i) {
    is_true(any(is_valid(names2(x)[i]), names2(x)[i] != ""))
  })
}

#' @rdname inheritance_tools
#' @export
have_name <- rlang::have_name

#' @rdname inheritance_tools
#' @export
are_double <- function(x) {
  map_lgl(x, is_dbl)
}


#' @rdname inheritance_tools
#' @export
are_chr <- function(x) {
  map_lgl(x, is_chr)
}


#' @rdname inheritance_tools
#' @export
are_dblint <- function(x) {
  map_lgl(x, is_dblint)
}


#' @rdname inheritance_tools
#' @export
are_fctchr <- function(x) {
  map_lgl(x, is_fctchr)
}


#' @rdname inheritance_tools
#' @export
are_prob <- function(x) {
  map_lgl(x, is_prob)
}


#' @rdname inheritance_tools
#' @export
are_increasing <- function(x) {
  map_lgl(x, is_increasing)
}


#' @rdname inheritance_tools
#' @export
are_sorted <- function(x) {
  map_lgl(x, is_sorted)
}


#' @rdname inheritance_tools
#' @export
any_nonnum <- function(x) {
  map_lgl(x, is_nonnum)
}

#' @rdname inheritance_tools
#' @export
have_same_length <- function(x, y) {
  is_true(identical(length(x), length(y)))
}

#' @rdname inheritance_tools
#' @export
have_same_dims <- function(x, y) {
  is_true(identical(dim(x), dim(y)))
}

#' @rdname inheritance_tools
#' @export
has_vectors_same_length <- function(v1, v2) {
  if (!(is.vector(v1) & is.vector(v2))) {
    return(FALSE)
  }
  is_trained(length(v1) == length(v2))
}

#' @rdname inheritance_tools
#' @export
has_vectors_disjoint <- function(v1, v2) {
  if (!(is_vector(v1) && is_vector(v2))) {
    return(FALSE)
  }

  if (typeof(v1) != typeof(v2)) {
    return(TRUE)
  }

  return(length(intersect(v1, v2)) == 0)
}

#' @export
has_singles_phylo <- function(tree) {
  fun <- function(x) {
    tab <- tabulate(x$edge[, 1])
    if (any(tab == 1L)) {
      return(TRUE)
    }
    FALSE
  }
  if (inherits(tree, "phylo")) {
    return(fun(tree))
  }
  if (inherits(tree, "multiPhylo")) {
    return(sapply(tree, fun))
  }
}
#' @rdname inheritance_tools
#' @export
are_na <- function(x) {
  map_lgl(data, ~ is_true(any(is.na(.x))))
}

#' @rdname inheritance_tools
#' @export
are_null <- function(x) {
  map_lgl(data, ~ is_true(is.null(.x)))
}

#' @rdname inheritance_tools
#' @export
are_integerish <- function(x) {
  map_lgl(x, is_integerish)
}

#' @rdname inheritance_tools
#' @export
has_length <- function(x, n = NULL) {
  len <- do_try(length(x), FALSE)
  if (is_null(n)) {
    as_logical(len)
  } else {
    len == n
  }
}

#' @rdname inheritance_tools
#' @export
has_seed <- function() {
  exists(".Random.seed",
         globalenv(),
         mode = "integer",
         inherits = FALSE
  )
}

# assert ---

#' assert_tools
#'
#' @name assert_tools
#' @rdname assert_tools
#' @keywords internal
#' @examples
#'
#' x <- c("a", "b", "c")
#'
#' assert_engine(is.character(x), severity = "warning") %is% TRUE
#' assert_engine(length(x) == 3, severity = "warning") %is% TRUE
#' assert_engine(5 < 3, msg = "Five is not smaller than three", severity = "warning") %is% FALSE
#' is_odd <- function(x) {
#'   assert_engine(is.numeric(x), length(x) == 1)
#'   x %% 2 == 1
#' }
#' assert_engine(is_odd(2), severity = "warning") %is% FALSE
#' on_failure(is_odd) <- function(call, env) {
#'   paste0(deparse(call$x), " is even")
#' }
#' assert_engine(is_odd(2), severity = "warning") %is% FALSE
#'
#'
#' data <- mgalda::datalearn$iris
#'
#' assert_data.frame(data, severity = "warning") %is% TRUE
#' assert_data.frame(data, .exact_ncol = ncol(data), severity = "warning") %is% FALSE
#' assert_data.frame(data, .exact_ncol = nrow(data), severity = "warning") %is% TRUE
#' assert_data.frame(data, .exact_nrow = nrow(data), severity = "warning") %is% FALSE
#' assert_data.frame(data, .required_colnames = "species", severity = "warning") %is% TRUE
#' assert_data.frame(data, .exact_colnames = "species", severity = "warning") %is% FALSE
#' assert_data.frame(data, .exact_colnames = names(data), severity = "warning") %is% TRUE
#' assert_data.frame(x = 1L) %is% FALSE
#'
#' set.seed(7)
#' x_vec <- runif(10)
#'
#' x_df <- data.frame(x = runif(10), y = runif(10))
#'
#' x_matrix <- matrix(data = 0, nrow = 10, ncol = 10)
#'
#' assert_have_same_dims(x = x_df, y = x_df, severity = "warning") %is% TRUE
#' assert_have_same_dims(x = x_vec, y = x_vec, severity = "warning") %is% TRUE
#' assert_have_same_dims(x = x_matrix, y = x_matrix, severity = "warning") %is% TRUE
#' assert_have_same_dims(x = x_matrix, y = x_df, severity = "warning") %is% FALSE
#' assert_have_same_dims(x = x_vec, y = x_df, severity = "warning") %is% FALSE
#' assert_have_same_dims(x = x_df, y = x_matrix, severity = "warning") %is% FALSE
#' assert_have_same_dims(x = x_vec, y = x_matrix, severity = "warning") %is% FALSE
#' assert_have_same_dims(x = x_df, y = x_vec, severity = "warning") %is% FALSE
#' assert_have_same_dims(x = x_matrix, y = x_vec, severity = "warning") %is% FALSE
#'
#'
#' x <- runif(10)
#' w <-sample(1:10, size = 100, replace = T)
#' assert_vector(x = x, .class = "numeric", severity = "warning") %is% TRUE
#' assert_vector(x = x, .class = "numeric", .len = 10, severity = "warning") %is% TRUE
#' assert_vector(x = x, .class = "numeric", .len = 20, severity = "warning") %is% FALSE
#' assert_vector(x = x, .class = "numeric", .min_len = 20, severity = "warning") %is% FALSE
#' assert_vector(x = x, .class = "numeric", .max_len = 2, severity = "warning") %is% FALSE
#' assert_vector(x = x, .class = "numeric", .sorted = T, severity = "warning") %is% FALSE
#' assert_vector(x = x, .class = "numeric", .names = T, severity = "warning") %is% FALSE
#' assert_vector(x = w, .class = "numeric", .unique = TRUE) %is% FALSE
#' assert_vector(x = w, .class = "numeric", .names = TRUE) %is% FALSE
#' assert_vector(x = w, .class = "numeric", .sorted = TRUE) %is% FALSE
#'
#'
#' x <- letters[1:5]
#' assert_vector(x = x, .class = "numeric", .len = 18, severity = "warning") %is% FALSE
#' assert_vector(x = x, .class = "character", severity = "warning") %is% TRUE
#'
#' assert_list(x = runif(10), severity = "warning") %is% FALSE
#' assert_list(x = list(x = runif(10)), severity = "warning") %is% TRUE
#' assert_list(x = list(x = runif(10)), exact_length = 1, severity = "warning") %is% TRUE
#' assert_list(x = list(x = runif(10)), exact_length = 2, severity = "warning") %is% FALSE
#' assert_list(x = list(x = runif(10)), required_names = "x") %is% TRUE
#' assert_list(x = list(x = runif(10)), required_names = "a") %is% FALSE
#'
#' y <- x <- runif(10)
#' y[2] <- NA
#'
#' assert_na_any(x, severity = "warning") %is% TRUE
#' assert_na_any(y, severity = "warning") %is% FALSE
#' assert_na_all(x, severity = "warning") %is% TRUE
#' assert_na_all(y, severity = "warning") %is% TRUE
#' assert_na_all(rep(NA, 5), severity = "warning") %is% FALSE
#'
#' assert_function(mean, severity = "warning") %is% TRUE
#' assert_function(1, severity = "warning") %is% FALSE
#'
#' assert_dblint(x = 1L) %is% TRUE
#' assert_chr(runif(1)) %is% FALSE
#' assert_chr("d") %is% TRUE
#' assert_lgl(runif(1)) %is% FALSE
#' assert_dbl(1L) %is% FALSE
#' assert_int(1L) %is% TRUE
#' assert_int(1.101) %is% FALSE
#' assert_fct(as.factor(stringr::fruit)) %is% TRUE
#'
NULL

#' @rdname assert_tools
#' @export
assert_engine <-
  function(...,
           env = parent.frame(),
           msg = NULL,
           severity = c("stop", "warning", "message", "none")) {
    asserts <- eval(substitute(alist(...)))
    severity <- match.arg(severity)
    for (assertion in asserts) {
      res <- do_try(eval(assertion, env), FALSE)

      check_assert(res)

      res <- is_true(res)

      # Failed, so figure out message to produce
      if (!res) {
        if (is.null(msg)) {
          msg <- get_message(res, assertion, env)
        }

        if (identical(env, globalenv())) {
          return(msg_false(msg, severity = severity))
        } else {
          false_call <-
            rlang::expr(return(msg_false(!!msg, severity = !!severity)))
          rlang::eval_bare(false_call, env = parent.frame())
        }
      }
    }

    res
  }

#' @rdname assert_tools
#' @export
assert_engine_any <-
  function(...,
           env = parent.frame(),
           msg = NULL,
           severity = c("stop", "warning", "message", "none")) {
    asserts <- eval(substitute(alist(...)))
    severity <- match.arg(severity)

    res_msg <- res <- list()
    for (assertion in asserts) {


      res_tmp<- do_try(eval(assertion, env), FALSE)

      check_assert(res_tmp)

      res_tmp<- is_true(res_tmp)

      if (is.null(msg)) {
        res_msg[[length(res_msg)+1]] <-
          get_message(res_tmp, assertion, env)
      }

      res[[length(res)+1]] <- res_tmp
    }

    if (lgl_all(.un(res), ~is_false(.x))) {
      if (is.null(msg)) {
        msg <- chr_cllp(.x = res_msg, ~.x, .collapse = ", ")
      }

      if (identical(env, globalenv())) {
        return(msg_false(msg, severity = severity))
      } else {
        false_call <-
          rlang::expr(return(msg_false(!!msg, severity = !!severity)))
        rlang::eval_bare(false_call, env = parent.frame())
      }
    }
    TRUE
  }



#' @keywords internal
check_assert <- function(x) {
  if (!is.logical(x)) {
    cat_stop("assertion must return a logical value")
  }
  if (any(is.na(x))) {
    cat_stop("missing values present in assertion")
  }
  if (length(x) != 1) {
    cat_stop("length of assertion is not 1")
  }
  TRUE
}

#' @keywords internal
give_feedback <- function(severity, msg) {
  switch(severity,
         stop = cat_stop(msg),
         warning = cat_warn(msg),
         message = cat_message(msg)
  )
  FALSE
}

#' @keywords internal
get_message <- function(res, call, env = parent.frame()) {
  stopifnot(is.call(call), length(call) >= 1)

  if (has_attr(res, "msg")) {
    return(attr(res, "msg"))
  }

  f <- eval(call[[1]], env)
  if (!is.primitive(f)) {
    call <- match.call(f, call)
  }
  fname <- deparse(call[[1]])

  fail <- on_failure(f) %||% msg_env[[fname]] %||% fail_default
  fail(call, env)
}
fail_default <- function(call, env) {
  call_string <- deparse(call, width.cutoff = 60L)
  if (length(call_string) > 1L) {
    call_string <- paste0(call_string[1L], "...")
  }

  paste0(call_string, " is not TRUE")
}

#' @rdname assert_tools
#' @export
on_failure <- function(x) {
  attr(x, "fail")
}

#' @export
#' @usage on_failure(x) <- value
`on_failure<-` <- function(x, value) {
  stopifnot(is.function(x), identical(names(formals(value)), c("call", "env")))
  attr(x, "fail") <- value
  x
}

#' @export
msg_false <-
  function(...,
           severity = getOption("mgalda.severity", "warning")) {
    msg <- paste(..., collapse = "\n")
    give_feedback(severity = severity, msg = msg)
    call <- rlang::expr(return(FALSE))
    rlang::eval_bare(call, env = parent.frame())
  }


#' @rdname assert_tools
#' @export
assert_function <-
  function(x,
           severity = getOption("mgalda.severity", "warning")) {

    assert_engine(is_fn(x), msg = "objeto no es funcion", severity = severity)
  }

#' @rdname assert_tools
#' @export
assert_list <- function(x,
                        required_names = NULL,
                        exact_length = NULL,
                        severity = getOption("mgalda.severity", "warning")) {
  check_inherits(
    x = x,
    what = "list",
    severity = severity
  )

  if (!is.null(required_names)) {
    assert_engine(
      has_name(x, required_names),
      severity = severity,
      msg = paste(
        "'x' no contiene los nombres ",
        paste0(required_names[!required_names %in% names(x)], collapse = ", "),
        sep = ": "
      )
    )
  }

  if (!is.null(exact_length)) {
    assert_engine(has_length(x, exact_length), severity = severity)
  }
  TRUE
}
#' @rdname assert_tools
#' @export

assert_vector <-
  function(x,
           .class,
           .len = NULL,
           .min_len = NULL,
           .max_len = NULL,
           .unique = FALSE,
           .sorted = FALSE,
           .names = FALSE,
           severity = getOption("mgalda.severity", "warning")) {

    check_inherits(x = x, what = .class,severity = severity)

    assert_engine(is.vector(x),severity = severity)

    if(!is_empty(.len)){
      assert_engine(has_length(x,.len),severity = severity)
    }

    if(!is_empty(.min_len)){
      assert_engine(length(x) >= .min_len,severity = severity)
    }

    if(!is_empty(.max_len)){
      assert_engine(length(x) <= .max_len,severity = severity)
    }

    if (is_true(.unique)) {
      assert_engine(n_unique(x) == length(x), severity = severity)
    }
    if (is_true(.names)) {
      assert_engine(all_named(x), severity = severity)
    }
    if (is_true(.sorted)) {
      assert_engine_any(is_monotonic_increasing(x),is_monotonic_decreasing(x),severity ="warning")
    }
    TRUE
  }

#' @rdname assert_tools
#' @export
assert_have_same_dims <-
  function(x,
           y,
           severity = getOption("mgalda.severity", "warning"),
           msg = NULL) {
    get_dim_string <- function(x) {
      if (is.null(dim(x))) {
        toString(length(x))
      } else {
        toString(dim(x))
      }
    }

    compare_dim <- function(x, y) {
      dx <- if (is_empty(dim(x))) {
        length(x)
      } else {
        dim(x)
      }
      dy <- if (is_empty(dim(y))) {
        length(y)
      } else {
        dim(y)
      }
      if (!is_true(all.equal(dx, dy))) {
        return(msg_false(
          gettext(
            "'y' and 'x' are not identical dimensions.'x' has dim %s but 'y' has dim %s."
          ),
          get_dim_string(x),
          get_dim_string(y)
        ))
      }
      TRUE
    }

    res <-
      assert_engine(compare_dim(x, y), severity = severity, msg = msg)
    res
  }




#' @rdname assert_tools
#' @export
assert_data.frame <- function(x,
                              .exact_ncol = NULL,
                              .min_ncol = NULL,
                              .max_ncol = NULL,
                              .exact_nrow = NULL,
                              .min_nrow = NULL,
                              .max_nrow = NULL,
                              .exact_rownames = NULL,
                              .exact_colnames = NULL,
                              .required_rownames = NULL,
                              .required_colnames = NULL,
                              msg = NULL,
                              severity = getOption("mgalda.severity", "warning")) {

  check_inherits(x = x, what = "data.frame",severity = severity)

  if (!is_empty(.max_ncol)) {
    assert_engine(is_true(ncol(x) > .max_ncol),msg = sprintf("ncol de 'x' es mayor que %s", .max_ncol),
                  severity = severity
    )

  }

  if (!is_empty(.min_ncol)) {
    assert_engine(
      is_true(ncol(x) < .min_ncol),
      msg = sprintf("ncol de 'x' es menor que %s", .min_ncol),
      severity = severity
    )

  }
  if (!is_empty(.exact_ncol)) {
    assert_engine(
      is_true(ncol(x) != .exact_ncol),
      msg = sprintf("ncol de 'x' es diferente que que %s", .exact_ncol),
      severity = severity
    )

  }
  if (!is_empty(.max_nrow)) {
    assert_engine(
      is_true(nrow(x) > .max_nrow),
      msg = sprintf("nrow de 'x' es mayor que %s", .max_nrow),
      severity = severity
    )

  }

  if (!is_empty(.min_nrow)) {
    assert_engine(
      is_true(nrow(x) < .min_nrow),
      msg = sprintf("nrow de 'x' es menor que %s", .min_nrow),
      severity = severity
    )

  }
  if (!is_empty(.exact_nrow)) {
    assert_engine(
      is_true(nrow(x) != .exact_nrow),
      msg = sprintf("nrow de 'x' es diferente que que %s", .exact_nrow),
      severity = severity
    )

  }

  colrow_names <- function(x,
                           exact_rownames,
                           exact_colnames,
                           required_rownames,
                           required_colnames) {
    if (!is.null(exact_rownames)) {
      if (!all(sort(exact_rownames) == sort(rownames(x)))) {
        for (rowname in rownames(x)) {
          if (!(rowname %in% exact_rownames)) {
            return(msg_false("'x' no existe in exact_rownames", rowname))
          }
        }
      }
    } else if (!is.null(required_rownames)) {
      for (rowname in required_rownames) {
        if (!(rowname %in% rownames(x))) {
          return(msg_false("'x'no existe in rownames", rowname))
        }
      }
    }


    if (!is.null(exact_colnames)) {
      if (!all(sort(exact_colnames) == sort(colnames(x)))) {
        for (colname in colnames(x)) {
          if (!(colname %in% exact_colnames)) {
            return(msg_false("%s no existe in exact_colnames", colname))
          }
        }
      }
    } else if (!is.null(required_colnames)) {
      for (colname in required_colnames) {
        if (!(colname %in% colnames(x))) {
          return(msg_false("'x' no existe in colnames", colname))
        }
      }
    }

    TRUE
  }
  assert_engine(
    colrow_names(
      x,
      .exact_rownames,
      .exact_colnames,
      .required_rownames,
      .required_colnames
    ),
    severity = severity
  )
}

#' @rdname assert_tools
#' @export
assert_dblint <-
  function(x,severity = getOption("mgalda.severity", "warning")) {
    env <- env_curr()

    check_inherits(
      x = x,
      what = c("numeric","integer"),
      severity = severity,
      .envir = env
    )
  }

#' @rdname assert_tools
#' @export
assert_int <-
  function(x,severity = getOption("mgalda.severity", "warning")) {
    env <- env_curr()

    check_inherits(
      x = x,
      what = "integer",
      severity = severity,
      .envir = env
    )
  }

#' @rdname assert_tools
#' @export
assert_dbl <-
  function(x,severity = getOption("mgalda.severity", "warning")) {
    env <- env_curr()

    check_inherits(
      x = x,
      what = "double",
      severity = severity,
      .envir = env
    )
  }

#' @rdname assert_tools
#' @export
assert_chr <-
  function(x,severity = getOption("mgalda.severity", "warning")) {
    env <- env_curr()

    check_inherits(
      x = x,
      what = "character",
      severity = severity,
      .envir = env
    )
  }

#' @rdname assert_tools
#' @export
assert_lgl <-
  function(x,severity = getOption("mgalda.severity", "warning")) {
    env <- env_curr()

    check_inherits(
      x = x,
      what = "logical",
      severity = severity,
      .envir = env
    )
  }

#' @rdname assert_tools
#' @export
assert_fct <-
  function(x,severity = getOption("mgalda.severity", "warning")) {
    env <- env_curr()

    check_inherits(
      x = x,
      what = "factor",
      severity = severity,
      .envir = env
    )
  }

#' @rdname assert_tools
#' @export
assert_fctchr <-
  function(x,severity = getOption("mgalda.severity", "warning")) {
    env <- env_curr()

    check_inherits(
      x = x,
      what = c("factor","character"),
      severity = severity,
      .envir = env
    )
  }

#' @rdname assert_tools
#' @export
assert_na_any <-
  function(x, severity = getOption("mgalda.severity")) {
    assert_engine(!na_any(x), severity = severity, msg = "any value is na")
  }

#' @rdname assert_tools
#' @export
assert_na_all <-
  function(x, severity = getOption("mgalda.severity")) {
    assert_engine(!na_all(x), severity = severity, msg = "all value are na")
  }

#' @rdname assert_tools
#' @export
assert_notempty <-
  function(x,severity = getOption("mgalda.severity", "warning")) {
    env <- env_curr()

    assert_engine(is_nonempty(x), env = env, severity = severity)
  }

#' @rdname assert_tools
#' @export
assert_numeric_all <-
  function(x,
           severity = getOption("mgalda.severity", "warning")) {
    assert_engine(lgl_all(.x = x, .f = ~ is_dblint(.x)),
                  severity = severity,
                  msg = "not all are numeric"
    )
  }

#' @rdname assert_tools
#' @export
assert_cols_exist <-
  function(data,
           ...,
           severity = getOption(
             "mgalda.severity",
             "warning"
           )) {
    x <- dots(...)
    x <- lang2str(x)
    assert_engine(data %has_name% x, severity = severity)
  }


#' @rdname assert_tools
#' @export
assert_value_order <- function(low, high, target = NULL) {
  assert_dblint(x = low, severity = "stop")
  assert_dblint(x = high, severity = "stop")

  assert_engine(length(low) == 1, severity = "stop")
  assert_engine(length(high) == 1, severity = "stop")

  assert_na_any(x = low, severity = "stop")
  assert_na_any(x = high, severity = "stop")


  if (is_nonempty(target)) {
    assert_engine(length(target) == 1, severity = "stop")
    assert_dblint(x = target, severity = "stop")
    assert_na_any(x = target, severity = "stop")

    assert_engine(low < target, target < high, severity = "stop")
  } else {
    assert_engine(low < high, severity = "stop")
  }

  invisible(TRUE)
}

#' @rdname assert_tools
#' @export
assert_unit_range <-
  function(x,
           xmin = 0,
           xmax = 1,
           severity = getOption("mgalda.severity", "warning")) {
    assert_dblint(x, severity = severity)
    assrt_rng <-
      quote(assert_engine(min(x,na.rm = TRUE) >= xmin, max(x,na.rm = TRUE) <= xmax, severity = severity))
    eval(interp(assrt_rng, xmin = xmin, xmax = xmax))
  }

#' @rdname assert_tools
#' @export
assert_vectors_nums <-
  function(x,
           y,
           severity = getOption("mgalda.severity", "warning")) {
    assert_engine(is_vct(x),
                  is_dblint(x),
                  severity = severity,
                  env = env_curr()
    )
    assert_engine(is_vct(y),
                  is_dblint(y),
                  severity = severity,
                  env = env_curr()
    )
    assert_engine(length(x) == length(y),
                  severity = severity,
                  env = env_curr()
    )
  }

#' @rdname assert_tools
#' @export
assert_across_df <-
  function(.data,
           ...,
           predicate,
           severity = getOption("mgalda.severity", "warning"),
           msg = NULL,
           envir = new.env()) {
    predicate <- eval(substitute(quote(predicate)), envir)
    cdot <- dots(...)

    assert_engine(
      "x" == call_args_nms(predicate),
      msg = "call args must be 'x'",
      severity = "stop",
      env = env_curr()
    )

    data_assrt <- dplyr::select(.data, !!!cdot)
    .envdata <- df2env(x = data_assrt)
    call_assrt <-
      map(
        names(data_assrt),
        ~ interp_deparse(
          call = deparse(predicate),
          old = "x",
          new = .x
        )
      )

    assrt <-
      call("assert_engine",
           quote(call_assrt),
           severity = severity,
           env = .envdata
      )
    assrt$severity <- severity
    assrt$env <- .envdata

    if(is_nonempty(msg)){
      assrt$msg <- msg
    }

    for (i in seq_along(call_assrt)) {
      tmp_call <- interp(obj_ = assrt, call_assrt = call_assrt[[i]])

      eval(tmp_call, envir = envir)
    }
    invisible(TRUE)
  }



#' @rdname assert_tools
#' @keywords internal
#' @export
check_inherits <-
  function(x,
           what,
           severity = c("none", "stop", "warning", "message"),
           .envir = parent.frame()) {
    check_inherits_msg <- function(x, res, what, lang_is) {
      if (!res) {
        if (lang_is) {
          return(sprintf("Function %s is FALSE", what))
        } else if (is.character(what)) {
          return(
            sprintf(
              "Element '%s' needs to inherit from '%s', but its class is '%s'.",
              names_in_parent(x),
              commas(what),
              commas(class(x))
            )
          )
        } else {
          return(sprintf(
            "args '%s' debe ser clase 'function' o 'character'",
            what
          ))
        }
      }
      res
    }

    .inherits <- f(is_true(inherits(x = .x, what = what)))

    stopifnot(is.character(what) | is.function(what))

    if (is.character(what)) {
      res <- .inherits(x, what)
    } else {
      res <- is_true(what(x))
    }

    if (!res) {
      msg <- do_try(get_message(
        res = res,
        call = call(paste0(
          "is.",
          what
        ), quote(x)),
        env = .envir
      ))
      if (is_empty(msg)) {
        if (is.function(what)) {
          what <- names_in_parent(what)
          lang_is <- TRUE
        } else {
          lang_is <- FALSE
        }
        msg <- check_inherits_msg(x, res, what, lang_is)
      }
      res <- structure(res, msg = msg)
      severity <- match.arg(severity)
      if (severity != "none") {
        false_call <- rlang::expr(cat_return_false(msg = !!msg, type = !!severity))
        rlang::eval_bare(false_call, env = parent.frame())
      }
    }
    res
  }



#' @rdname assert_tools
#' @export
check_rango_muestras <-
  function(rango) {
    if (!rlang::is_bare_numeric(rango)) {
      cat_stop("rango debe ser numerico")
    }
    if (length(rango) != 2) {
      cat_stop("rango debe tener 2 elementos numericos")
    }
    if (any(rango < 0)) {
      cat_stop("rango fuera de los limites [0,1]")
    }
    if (any(rango > 1)) {
      cat_stop("rango fuera de los limites [0,1]")
    }
  }

#' @rdname assert_tools
#' @export
na_any <- function(x) {
  UseMethod("na_any")
}
#' @export
na_any.default <- function(x) {
  anyNA(x)
}
#' @export
na_any.data.frame <- function(x) {
  any(sapply(x, na_any))
}

#' @rdname assert_tools
#' @export
na_all <- function(x) {
  UseMethod("na_all")
}
#' @export
na_all.default <- function(x) {
  all(is.na(x))
}
#' @export
na_all.data.frame <- function(x) {
  all(sapply(x, na_all))
}

#' @export
names_in_parent <- function(x, escape_percent = TRUE) {
  safe_deparse <- function(expr, ...) {
    deparse1(expr,
             ...,
             collapse = "",
             width.cutoff = getOption("width")
    )
  }

  xname <-
    safe_deparse(do.call(
      what = substitute,
      args = list(substitute(x), parent.frame())
    ))
  if (escape_percent) {
    xname <- gsub("%", "%%", xname)
  }
  xname
}

###  msg_env
msg_env <- new.env(parent = emptyenv())

logical_is_not <- function(failed) {
  function(call, env) {
    lhs <- paste(deparse(call[[2]]), collapse = "")
    rhs <- paste(deparse(call[[3]]), collapse = "")
    paste0(lhs, " not ", failed, " ", rhs)
  }
}
is_not <- function(thing) {
  function(call, env) {
    paste0(deparse(call[[2]]), " is not ", thing)
  }
}
path_is_not <- function(thing, var = "x") {
  function(call, env) {
    paste0("Path '", eval(call[[var]], env), "' is not ", thing)
  }
}


on_failure(is_number) <- function(call, env) {
  paste0(deparse(call$x), " is not a number (a length one numeric vector).")
}
on_failure(check_inherits) <- function(call, env) {
  eval(expr = call,)
}
on_failure(is_dblint) <- function(call, env) {
  paste0(deparse(call$x), " is not a numeric or integer vector.")
}
on_failure(is_chr) <- function(call, env) {
  paste0(deparse(call$x), " is not a character vector.")
}
on_failure(has_name) <- function(call, env) {
  out_names <-
    paste0("'", paste0(eval(call$which, env), collapse = "', '"), "'")
  paste0(
    deparse(call$x),
    " does not have all of these name(s): ",
    out_names
  )
}
on_failure(all_named) <- function(call, env) {
  paste0("Not all elements of ", deparse(call$x), " have names.")
}
on_failure(has_attr) <- function(call, env) {
  paste0(
    deparse(call$x),
    " does not have attribute ",
    eval(call$which, env)
  )
}
on_failure(has_length) <- function(call, env) {
  paste0(
    deparse(call$x),
    " length is not ",
    eval(call$n, env)
  )
}
on_failure(is_dir) <- path_is_not("a directory", "path")
on_failure(is_string) <- function(call, env) {
  paste0(
    deparse(call$x),
    " is not a string (a length one character vector)."
  )
}
on_failure(is_odd) <- function(call, env) {
  paste0(deparse(call$x), " is even")
}
on_failure(is_monotonic_increasing) <- function(call, env) {
  strictly<-ifelse(call$strictly," strictly","")
  paste0(deparse(call$x), " is not",strictly," increasing")
}
on_failure(is_monotonic_decreasing) <- function(call, env) {
  strictly<-ifelse(call$strictly," strictly","")
  paste0(deparse(call$x), " is not",strictly," decreasing")
}
on_failure(is_fn) <- is_not("a function")
on_failure(is_nonempty) <- function(call, env) {
  paste0(deparse(call$x), " is empty")
}

msg_env$"==" <- logical_is_not("equal to")
msg_env$"<" <- logical_is_not("less than")
msg_env$">" <- logical_is_not("greater than")
msg_env$">=" <- logical_is_not("greater than or equal to")
msg_env$"<=" <- logical_is_not("less than or equal to")
msg_env$"!=" <- logical_is_not("not equal to")


# Vectors
msg_env$is.atomic <- is_not("an atomic vector")
msg_env$is_atomic <- is_not("an atomic vector")
msg_env$is.character <- is_not("a character vector")
msg_env$is.logical <- is_not("a logical vector")
msg_env$is_lgl <- is_not("a logical vector")
msg_env$is_chr <- is_not("a character vector")
msg_env$is.complex <- is_not("a complex vector")
msg_env$is.double <- is_not("a double vector")
msg_env$is_dbl <- is_not("a double vector")
msg_env$is.integer <- is_not("an integer vector")
msg_env$is_int <- is_not("an integer vector")
msg_env$is.numeric <- is_not("a numeric or integer vector")
msg_env$is.raw <- is_not("a raw vector")
msg_env$is.vector <- is_not("an atomic vector without attributes")
msg_env$is_vct <- is_not("an atomic vector without attributes")

# Factors
msg_env$is.factor <- is_not("a factor")
msg_env$is_fct <- is_not("a factor")
msg_env$is.ordered <- is_not("an ordered factor")

# More complicated data structures
msg_env$is_tbl <- is_not( "a 'tbl_df' class ('tibble')")
msg_env$is.array <- is_not("an array")
msg_env$is.data.frame <- is_not("a data frame")
msg_env$is_df <- is_not("a data frame")
msg_env$is.list <- is_not("a list")
msg_env$is_l <- is_not("a list")
msg_env$is.matrix <- is_not("a matrix")
msg_env$is_mtx <- is_not("a matrix")
msg_env$is.null <- is_not("NULL")

# Functions and environments
msg_env$is.environment <- is_not("an environment")
msg_env$is.function <- is_not("a function")
msg_env$is.primitive <- is_not("a primitive function")
msg_env$is_formula <- is_not("a formula")
msg_env$is_frm <- is_not("a formula")

# Computing on the language
msg_env$is.call <- is_not("a quoted call")
msg_env$is_call <- is_not("a quoted call")
msg_env$is.expression <- is_not("an expression object")
msg_env$is_expr <- is_not("an expression object")
msg_env$is.name <- is_not("a name")
msg_env$is.pairlist <- is_not("a pairlist")
msg_env$is_pairlist <- is_not("a pairlist")
msg_env$is_prls <- is_not("a pairlist")
msg_env$is.recursive <- is_not("a recursive object")
msg_env$is.symbol <- is_not("a name")
msg_env$is_symbol <- is_not("a name")


# other class o type
msg_env$is_null <- is_not("NULL")
msg_env$is_sf <- is_not("simple features (encode spatial vector data)")

# especial conditions
msg_env$is_colour <- is_not("colour")
msg_env$is_density <- is_not("density function with integrate equal 1")
msg_env$is_discrete_distribution <- is_not("discrete vector")
msg_env$is_equal <- is_not("both object are not equal")
msg_env$is_longlat <- is_not("longlat")
msg_env$is_prob <- is_not("a probability numerical vector")
msg_env$is_dblint <- is_not("a numeric or integer vector")
msg_env$is_fctchr <- is_not("a factor o character")
msg_env$is_empty <- is_not("empty")
msg_env$is_true <- is_not("TRUE")

msg_env$is_dir <- function(call, env) {
  paste0(call$x, " is not a directory path")
}
msg_env$is_existing <- function(call, env) {
  paste0(call$x, "object not exist")
}
msg_env$is_false <- function(call, env) {
  paste0(call$x, " is not a FALSE result")
}
msg_env$is_installed <- function(call, env) {
  paste0(call$x, "package not instaled")
}
msg_env$is_integerish <- function(call, env) {
  paste0(call$x, " is not a integerish")
}
msg_env$is_named <- function(call, env) {
  paste0(call$x, " is not named")
}
msg_env$is_str <- function(call, env) {
  paste0(call$x, " is not a charac")
}
msg_env$is_string <- function(call, env) {
  paste0(call$x, " is not a scalar character")
}
msg_env$is_number <- function(call, env) {
  paste0(call$x, " is not a scalar numeric")
}
msg_env$is_bool <- function(call, env) {
  paste0(call$x, " is not a scalar logical")
}

msg_env$is_same_type <- function(call, env) {
  paste0(call$x, "'x' and 'y' have different type")
}
msg_env$is_distrobj <- function(call, env) {
  paste0(call$x, "is not a fit distribution object")
}
msg_env$is_symname <- function(call, env) {
  paste0(call$x, "is not a 'language' or 'symbol' type")
}
msg_env$is_valid_name <- function(call, env) {
  paste0(call$x, "is not a syntactically valid names character vectors")
}
msg_env$is_inrange<- function(call, env) {
  paste0(
    "'",
    deparse(call$x),
    "' values should be numeric and complete in the range [",
    if (is_empty(call$xmin)) {
      "-Inf"
    } else {
      deparse(call$xmin)
    },
    ", ",
    if (is_empty(call$xmax)) {
      "Inf"
    } else {
      deparse(call$xmax)
    },
    "]."
  )
}

msg_env$inherits <- function(call, env) {
  force(call)
  class <- eval(call$what, env)
  paste0(deparse(call$x), " does not inherit from class ", class)
}
msg_env$"&&" <- function(call, env) {
  lhs <- eval(call[[2]], env)
  if (!lhs) {
    get_message(lhs, call[[2]], env)
  } else {
    rhs <- eval(call[[3]], env)
    get_message(rhs, call[[3]], env)
  }
}

msg_env$"||" <- function(call, env) {
  lhs <- eval(call[[2]], env)
  l_msg <- get_message(lhs, call[[2]], env)

  rhs <- eval(call[[3]], env)
  r_msg <- get_message(rhs, call[[3]], env)

  paste0(l_msg, " or ", r_msg)
}

msg_env$any <- function(call, env) {
  paste0("No elements of ", deparse(call[[2]]), " are true")
}
msg_env$all <- function(call, env) {
  res <- eval(call[[2]], env)
  i <- which(!res)
  if (length(i) > 10) {
    i <- c(i[1:5], "...")
  }

  paste0(
    "Elements ",
    paste(i, collapse = ", "),
    " of ",
    deparse(call[[2]]),
    " are not true"
  )
}

msg_env$file.exists <- function(call, env) {
  path <- eval(call[[2]], env)
  paste0("Path '", path, "' does not exist")
}
msg_env$anyDuplicated <- function(call, env) {
  paste0(call$x, " is not unique")
}
msg_env$identical <- function(call, env) {
  paste0(deparse(call$x), " not identical to ", deparse(call$y))
}

