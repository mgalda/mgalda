#' herramientas
#'
#'
#' @name nonstandard_eval
#' @rdname nonstandard_eval
#' @keywords internal
#'
#' @examples
#'
#' d <- quote(c)
#' c <- quote(b)
#' b <- quote(a)
#' a <- 2
#' x <- 1
#' y <- quote(x)
#' z <- quote(y)
#' w <- "z"
#'
#' sym_inputs(x, y, z, "z", w)
#'
#' gg <- function(...) {
#'   sym_inputs(...)
#' }
#' gg(x, y, z, "z", w)
#'
#' hh <- function(...) {
#'   gg(...)
#' }
#'
#' hh(x, y, z, "z", w)
#'
#' jj <- function(...) {
#'   hh(...)
#' }
#' jj(x, y, z, "z", w)
#'
#' kk <- function(...) {
#'   jj(...)
#' }
#' kk(x, y, z, "z", w)
#'
#'
#' sym_inputs(d, "d")
#' gg(d, "d")
#' hh(d, "d")
#' jj(d, "d")
#' kk(d, "d")
#'
#' rm(list = ls())
#'
#' call <- quote(f(a, b))
#' str(call_args(call))
#'
#' e1 <- quote(x + 1)
#' e2 <- quote(y + 2)
#'
#' exprs_ops(ops = "+", expr1 = e1, expr2 = e2)
#' exprs_ops(ops = "/", expr1 = e1, expr2 = e2)
#'
#' x <- runif(5, 2, 10)
#'
#' glue_eval("sqrt({{x}})", envir = env_curr())
#'
#' formalArgs(def = runif)
#'
#' formals2chr(rnorm)
#'
#' eval_expr(x + 2, envir = env_curr())
#'
#' eval_parse("x + 2")
#'
#' chr2pairlist(x = letters[1:3], y = runif(3))
NULL


#' @rdname nonstandard_eval
#' @export
sym_inputs <- function(...,
                       evaluated = TRUE,
                       follow = TRUE) {
  expr <- ensyms(...)

  if (is_langlist(expr)) {
    expr <- unclass(expr)
  }

  if (is_langatom(expr)) {
    expr <- list(expr_get_quotable(expr))
  }

  expr <- auto_name(expr)

  .expr <-
    lapply(expr,
           eval_safe_name,
           evaluated = evaluated,
           follow = follow
    )

  if (length(.expr) == 1) {
    .expr <- .expr[[1]]
  }
  .expr
}
eval_safe_name <- function(expr,evaluated = TRUE,follow = FALSE) {
  n <- 0

  expr <- enexpr(expr)

  while (n <= max(sys.parent())) {
    if (is.name(expr)) {
      expr_chr <- lang2str(expr, evaluated = F)
    } else if (is.character(expr)) {
      expr_chr <- expr
      expr <- str2lang(expr)
    } else {
      break
    }

    if ((evaluated || follow)) {
      sfrm <- sys.frames()
      for (i in seq_along(sfrm)) {
        ev_expr <- do_try(eval(expr = expr, envir = sfrm[[i]]))
        if(is_null(ev_expr)){
          tmp_env <- do_try(env2list(envir = sfrm[[i]]))
          ev_expr <- do_try(enexprs(tmp_env[[expr_chr]]))
        }

        if (is_str(ev_expr)) {
          ev_expr <- do_try(eval(expr = str2lang(ev_expr), envir = sfrm[[i]]))
        }

        if (!is.null(ev_expr)) {
          if (!(!is.name(ev_expr) && !evaluated)) {
            expr <- ev_expr
          }
          if (is_str(ev_expr)) {
            for (j in seq_along(sfrm) - 1) {
              if (!identical(ev_expr, expr_chr)) {
                if (is_true(env_has(env = sfrm[[j]], nms = ev_expr))) {
                  expr <- str2lang(ev_expr)
                  expr_chr <- ev_expr
                }
              }
            }
          }

          if (!identical(expr_chr, lang2str(expr)) & !follow) {
            break
          }
          expr_chr <- lang2str(expr)
        }
      }
    }

    s_call_n <-names(as.list(sys.call(n))[-1])

    if (!is_empty(s_call_n)) {
      s_call <-
        unlist(map_if(sys.call(n), ~ is_chr(.x) ||
                        is.name(.x), lang2str))

      s_call <- do_try(s_call[s_call == expr_chr])

      if (is_nonempty(s_call)) {
        expr_chr <- unname(unique(s_call))
        expr <- lang2str(expr_chr)

        if (!follow) {
          n <- n + 1
          break
        }
      }
    }


    s_frame <- sys.frame(which = n)

    if (!is_true(identical(expr_chr, lang2str(expr, evaluated = F)))) {
      if (!evaluated) {
        expr <- lang2str(expr_chr)
      }

      if (!follow) {
        break
      }
    }

    n <- n + 1
  }
  if (is_str(expr) & !evaluated) {
    expr <- str2lang(expr)
  }


  expr
}

auto_name <- function(x, max_width = 40) {
  names(x) <- auto_names(x, max_width = max_width)
  x
}
auto_names <- function(x, max_width = 40) {
  x <- map(x,as_lazy)

  nms <- names(x) %||% rep("", length(x))

  missing <- nms == ""
  expr <- lapply(x[missing], `[[`, "expr")
  nms[missing] <- vapply(expr, deparse_trunc,
                         width = max_width,
                         FUN.VALUE = character(1), USE.NAMES = FALSE
  )

  nms
}
deparse_trunc <- function(x, width = getOption("width")) {
  if (is.symbol(x) | is.character(x)) {
    return(as.character(x))
  }

  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width) {
    return(text)
  }

  paste0(substr(text[1], 1, width - 3), "...")
}
is_basens <- function(x){
  is_true(identical(x, .BaseNamespaceEnv))
}

#' @rdname nonstandard_eval
#' @export
call_args <- function(call) {
  .call_args <- function(call) {

      if (!is_true(is_symname(call))) {
        return(NULL)
      }
      call <- .step_call_args(call)
      call <- call[map_lgl(call, is_symname)]
      if (!all(lgcall <-map_lgl(call, ~ is_symbol(.x)))) {
        return(cmp_ftn_map_if(call, !lgcall, ~ .call_args(.x)))
      }
      call
    }
  .step_call_args <- function(call) {
    call <- rlang::get_expr(call)
    if (!is_call(call)) {
      cat_stop(sprintf("`%s` must be a quoted call", call))
    }
    args <- as_l(call[-1])
    setNames((args), rlang::names2(args))
  }
  call <- do_try(.call_args(call),.step_call_args(call))
  set_names(call, lang2str(call))
}

#' @rdname nonstandard_eval
#' @export
call_args_nms<- function (call) {
  .call_args <- function(call) {
    if (!is_true(is_symname(call))) {
      return(NULL)
    }
    call <- .step_call_args(call)
    call <- call[map_lgl(call, is_symname)]
    if (!all(lgcall <- map_lgl(call, ~typeof(.x) == "symbol"))) {
      return(cmp_ftn_map_if(call, !lgcall, ~.call_args(.x)))
    }
    call
  }
  .step_call_args <- function(call) {
    call <- rlang::get_expr(call)
    if (!is_call(call)) {
      cat_stop(sprintf("`%s` must be a quoted call"),
                   call)
    }
    args <- as_l(call[-1])
    setNames((args), rlang::names2(args))
  }
  call <- do_try(.call_args(call), .step_call_args(call))
  unname(lang2str(call))
}

#' @rdname nonstandard_eval
#' @export
active_source_path <- function() {
  rstudioapi::getSourceEditorContext()$path
}

#' @rdname nonstandard_eval
#' @export
exprs_ops <- function(ops, expr1, expr2, envir = env_curr()) {
  expr1 <- do_try(eval.parent(substitute(expr1)), substitute(expr1))
  expr2 <-
    do_try(eval.parent(substitute(expr2)), substitute(expr2))
  expr_base <- switch(ops,
                      `+` = rlang::expr(expr1 + expr2),
                      `-` = rlang::expr(expr1 + -1 * expr2),
                      `/` = rlang::expr(expr1 * (1 / expr2)),
                      `*` = rlang::expr(expr1 * expr2),
                      `==` = rlang::expr(expr1 == expr2),
                      `!=` = rlang::expr(expr1 != expr2),
                      `>` = rlang::expr(expr1 > expr2),
                      `>=` = rlang::expr(expr1 >= expr2),
                      `<` = rlang::expr(expr1 < expr2),
                      `<=` = rlang::expr(expr1 <= expr2),
                      `~` = rlang::expr(expr1 ~ expr2)
  )

  do.call("substitute", list(expr = expr_base, env = as_l(envir)))
}

#' @rdname nonstandard_eval
#' @export
fns_ops <- function(ops, f1, f2, envir = env_curr()) {
  form_chr <-
    function(f, nm_f) {
      fms_f <- formals(formals_default(formals_empty(f)))
      gsub(
        pattern = "list",
        replacement = nm_f,
        formals2chr(x = fms_f)
      )
    }
  meged_formals <- function(f1, f2) {
    f1 <- formals(f1)
    f2 <- formals(f2)
    f2 <- f2[names(f2) %!in% names(f1)]
    as_prls(c(f1, f2))
  }
  f1_chr <- form_chr(f = f1, nm_f = "f1")
  f2_chr <- form_chr(f = f2, nm_f = "f2")

  fms <- meged_formals(f1, f2)

  f1_dep <-
    paste("f1 <- ", paste(deparse(f1), collapse = "\n"), collapse = "")
  f2_dep <-
    paste("f2 <- ", paste(deparse(f2), collapse = "\n"), collapse = "")

  f_chr <- switch(ops,
                  `+` = paste(f1_chr, " + ", f2_chr),
                  `-` = paste(f1_chr, " + -1 * ", f2_chr),
                  `/` = paste(f1_chr, " * (1 / ", f2_chr, ")"),
                  `*` = paste(f1_chr, " * ", f2_chr),
                  `==` = paste(f1_chr, " == ", f2_chr),
                  `!=` = paste(f1_chr, " != ", f2_chr),
                  `>` = paste(f1_chr, " >", f2_chr),
                  `>=` = paste(f1_chr, " >= ", f2_chr),
                  `<` = paste(f1_chr, " < ", f2_chr),
                  `<=` = paste(f1_chr, " <= ", f2_chr)
  )
  f_chr <- paste(f_chr, "\n", sep = "")
  make_function(args = fms,
                body = rlang::parse_expr(paste("{", f1_dep, f2_dep, f_chr, "}", sep = "\n")))
}


#' @rdname nonstandard_eval
#' @export
glue_eval <- function(..., envir = env_curr()) {
  pexpr <- glue::glue(..., .envir = envir, .sep = "")
  pexpr <- rlang::parse_expr(x = pexpr)
  rlang::eval_bare(pexpr, env = envir)
}

#' @rdname nonstandard_eval
#' @export
dots <- function(...) {
  eval(substitute(alist(...)))
}

#' @rdname nonstandard_eval
#' @export
dots_named <- function(...) {
  args <- eval(substitute(alist(...)))
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  missing <- nms == ""
  if (all(!missing)) {
    return(args)
  }
  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(args[missing], deparse2, character(1),
                     USE.NAMES = FALSE
  )
  names(args)[missing] <- defaults
  args
}

#' @rdname nonstandard_eval
#' @export
formals_names <- function(x) {
  if (inherits(x, "function")) {
    x <- formals(x)
  }

  check_inherits(x, c("list", "pairlist"))

  nm <- names(x)

  if (any(nm == "")) {
    nm <- c(nm[nm != ""], "...")
  }
  unique(nm)
}

#' @rdname nonstandard_eval
#' @export
formals2chr <- function(x) {
  if (inherits(x, "function")) {
    x <- formals(x)
  }
  nms <- formals_names(x)
  args <-
    sapply(x, function(x) {
      deparse1(x, collapse = "", width.cutoff = 500)
    })
  args <- ifelse(args == "", "quote(expr = )", args)
  nm <- paste0("list(", commas(paste(nms, " = ", args)), ")")
  gsub(
    pattern = '\"',
    replacement = "'",
    x = nm,
    fixed = T
  )
}

#' @rdname nonstandard_eval
#' @export
formals_default <- function(fn) {
  form <- formals(fun = fn)
  form$... <- NULL
  form <- as_prls(imap_if(as_l(form), is_empty, ~ str2lang(.y)))
  fn %edit_args% form
}

#' @rdname nonstandard_eval
#' @export
formals_empty <- function(fn) {
  form <- formals(fun = fn)
  form$... <- NULL
  form <- as_prls(map(as_l(form),~quote(expr = )))
  fn %edit_args% form
}

#' @rdname nonstandard_eval
#' @export
chr_pair2call <- function(call_chr, pairlist, default = FALSE) {
  check_inherits(pairlist, c("list", "pairlist"))
  check_inherits(call_chr, "character")
  args <- formals_names(pairlist)
  if (default) {
    d_args <-
      map_chr(pairlist, ~ paste0(deparse(.x, width.cutoff = 500), collapse = ""))
    args <- ifelse(d_args != "", paste(args, "=", d_args), args)
  }

  str2lang(paste0(call_chr, "(", commas(args), ")"))
}

#' @rdname nonstandard_eval
#' @export
capture_dots <-
  function(envir = parent.frame(3L),
           call = match.call(
             definition = sys.function(sys.parent()),
             call = sys.call(sys.parent()),
             expand.dots = FALSE,
             envir = parent.frame(2L)
           ),
           as_expr = TRUE) {
    call <- call$...
    if (as_expr) {
      if (length(call) > 1) {
        for (i in seq_along(call)) {
          if (!is_empty(names(call)[i])) {
            call[[i]] <- paste0(
              names(call)[i], " = ",
              deparse(call[[i]])
            )
          } else {
            call[[i]] <- deparse(call[[i]])
          }
        }
        call <-
          paste0("c(", paste0(unlist(call), collapse = ", "),
                 ")",
                 sep = ""
          )
        call <- str2lang(call)
      } else {
        call <- call[[1]]
        call <- str2lang(deparse(call))
      }
    }
    call
  }


#' @rdname nonstandard_eval
#' @export
complementar_args <-
  function(call = sys.call(sys.parent()),
           envir = parent.frame(2L)) {
    list_args <- rlang::call_args(call)
    call <- call[[1]]
    eval_expr(modify_args(x = !!call, !!list_args), envir = envir)
  }

#' @rdname nonstandard_eval
#' @export
modify_args <- function(x, forms, removals = NULL) {
  if (missing(removals)) {
    removals <- NULL
  }

  sub_args <- function(x, forms, removals = NULL) {
    args <- formals(x)

    for (i in intersect(names(forms), names(args))) {
      args[[i]] <- forms[[i]]
    }
    if (!is.null(removals)) {
      args[removals] <- NULL
    }
    args
  }

  mod_call_args <- function(x, forms, removals = NULL) {
    if (!is.null(removals)) {
      for (i in removals) {
        x[[i]] <- NULL
      }
    }
    arg_names <- names(forms)
    for (i in arg_names) {
      x[[i]] <- forms[[i]]
    }
    x
  }

  if (is.function(x)) {
    args_new <- sub_args(
      x = x,
      forms = forms,
      removals = removals
    )
  } else if (is.symbol(x) | is.call(x)) {
    args_new <- mod_call_args(
      x = x,
      forms = forms,
      removals = removals
    )
  }
  args_new
}

#' @export

get_list_fn <-
  function(package, character.only = FALSE) {
    if (!character.only) {
      package <- as_chr(substitute(package))
    }
    pkgNm <- paste("package", package, sep = ":")
    if (!is.na(match(pkgNm, search())) ||
        require(package, character.only = TRUE, quietly = TRUE)) {
      nm <- ls(env <- as.environment(pkgNm), all.names = TRUE)
      nm[unlist(lapply(nm, function(n) {
        exists(n,
               where = env,
               mode = "function",
               inherits = FALSE
        )
      }))]
    } else {
      character(0)
    }
  }

#' @export
get_list_fn_body <-
  function(package,
           character.only = FALSE,
           paste = FALSE) {
    if (!character.only) {
      package <- as_chr(substitute(package))
    }
    fn_nm <-
      eval_expr(get_list_fn(
        package = !!package,
        character.only = !!character.only
      ))
    fn_body <- lapply(fn_nm, get)

    names(fn_body) <- fn_nm

    if (paste) {
      datapasta::vector_paste(fn_body)
      datapasta::vector_paste(fn_nm)
    }

    fn_body
  }


#' @rdname nonstandard_eval
#' @description crear un pairlist desde un vector de caracteres
#'
#' @param x vector con el nombre de las variables
#'
#' @export
chr2pairlist <- function(x, y = NULL) {
  chr <- commas(paste0(x, " =", y))
  chr <- paste0("alist(", chr, ")")
  eval(parse(text = chr))
}

#' @rdname nonstandard_eval
#' @description enlistar funciones desde un archivo .R
#'
#' @param file ubicacion del archivo. se recomienda completa
#'
#' @export

get_fns_ns <-  function(file) {
    code <- parse(file)
    tokens <- as_l(code)
    tokens <- compact(tokens)
    fn_tokens <- map(tokens, as.list)
    fn_tokens <- map(fn_tokens, function(x) {
      out <-
        tryCatch(
          expr = list(
            name = x[[2]],
            fmls = rlang::call_args(x[[3]])[[1]],
            body = rlang::call_args(x[[3]])[[2]]
          ),
          error = function(e) {
            NULL
          }
        )
      out
    })
    fn_tokens <- compact(fn_tokens)
    nm <- map(fn_tokens, ~ as_chr(.x$name))
    indx <- lengths(nm) > 1

    fn_tokens <- fn_tokens[!indx]
    nm <- map_chr(nm[!indx], ~.x)

    names(fn_tokens) <- nm
    fn_tokens
  }

#' @rdname nonstandard_eval
#' @description simplifica operaciones aritmeticas
#'
#' @param x exprecion a simplificar
#'
#' @export
simplificar_expr <- function(x) {
  ori <- rlang::expr(!!x)
  if (is.numeric(x)) {
    return(x)
  }
  if (is.name(x)) {
    return(x)
  }
  if (!(is.call(x) | is.character(x))) {
    cat_stop("debe ser call o character")
  }
  if (is.call(x)) {
    x <- rlang::expr_deparse(x)
  }

  yacas_sol <- function(x) {
    x <- Ryacas::simplify(Ryacas::simplify(Ryacas::yac_symbol(x)))

    rlang::parse_expr(Ryacas::yac_str(x))
  }
  x <- try(yacas_sol(x), silent = T)
  if (inherits(x, "try-error")) {
    x <- ori
  }
  x
}

#' @rdname nonstandard_eval
#' @description  sustituir varibles em exprecion no evaluadas
#'
#' @export
modify_body <- function(expr, ...) {
  subst_args <- eval(substitute_w(alist(...)),envir = env_call())
  expr <- do_try(eval.parent(substitute(expr)), substitute(expr))

  subst_args <- do_try(as_prls(subst_args), subst_args)
  if (!is.expression(expr)) {
    if (is.call(expr)) {
      expr <- as.expression(expr)
    } else {
      cat_stop("debo ser tipo call o expression")
    }
  }
  do.call(
    substitute,
    list(as_l(expr)[[1]], env = env_new(subst_args))
  )


}

#' @rdname nonstandard_eval
#' @description consolidar n r script en uno solo
#'
#' @export
merge_rscrips <- function(filepath) {
  .read_tidylines <- function(x) {
    x <- readLines(x)

    ltype <- ifelse(substr(x, 1, 2) == "#'", "block", "code")
    ltype_n <-
      map_dbl(
        seq_fromalong(from = 2, along.with = ltype),
        ~ ifelse(ltype[.x] == ltype[.x - 1], 0, 1)
      )

    ltype_n <- cumsum(c(1, ltype_n))
    ltype_n <- padstr(x = ltype_n, pad = "0")

    x <- split(x, paste0(ltype_n, ltype))

    ncodes <- grep(
      pattern = "code",
      x = names(x),
      value = T
    )

    x[ncodes] <-
      map(x[ncodes], ~ capture.output(cat(
        do_try(
          expr = formatR:::tidy_source(
            text = .x,
            comment = FALSE,
            output = F
          )$text.tidy,
          error = .x
        ),
        sep = "\n"
      )))

    x <- map(x, ~ c(.x, "", ""))
    .un(x)
  }

  .read_rscrips <- function(filepath) {
    nfiles <-
      list.files(
        path = filepath,
        full.names = T,
        include.dirs = F,
        recursive = TRUE
      )
    nfiles <-
      nfiles[sapply(nfiles, tools::file_ext) %in% c(
        "R",
        "r"
      )]
    if (length(nfiles) == 0) {
      cat_stop("no existen archivos .R en la caperta ")
    }
    # if (tools::file_ext(output_file) != "R") {
    #    cat_stop("output_file debe ser extension .R")
    # }
    # if (file.exists(output_file)) {
    #    cat_stop("ya existe un archivo con ese nombre")
    # }
    .ttl <- map(nfiles, .title_rscrips)
    map2(nfiles, .ttl, ~ c(.y, "", "", .read_tidylines(.x)))
  }
  .title_rscrips <- function(x) {
    sep_name <-
      stringr::str_split(
        string = x,
        pattern = stringr::fixed("/"),
        simplify = T
      )
    sep_name <- sep_name[length(sep_name)]
    sep_name <- paste("nombre archivo : ", sep_name, sep = "")
    nnn <- stringr::str_length(sep_name) + 3
    paste("# ", sep_name, paste(rep("-", 83 - nnn), collapse = ""), sep = "")
  }

  out <- .read_rscrips(filepath)
  output_file <- paste(filepath, ".R", sep = "")


  sink(output_file)
  if (is.list(out)) {
    walk(out, ~ cat(c(.x, "\n"), sep = "\n"))
  } else {
    cat(c(out, "\n"), sep = "\n")
  }
  sink()
  file.edit(output_file)
  invisible()
}

#' Modify the arguments of a call.
#'
#' @rdname nonstandard_eval
#' @param call A call
#' @param env Environment in which to look up call value.
#' @param new_args A named list of expressions
#' (constants, names or calls).
#' Use \code{NULL} to remove arguments.
#' @export
#' @examples
#' call <- quote(mean(x, na.rm = TRUE))
#' call_standardise(call)
#'
#' # Modify an existing argument
#' call_modify(call, list(na.rm = FALSE))
#' call_modify(call, list(x = quote(y)))
#'
#' # Remove an argument
#' call_modify(call, list(na.rm = NULL))
#'
#' # Add a new argument
#' call_modify(call, list(trim = 0.1))
#'
#' # Add an explicit missing argument
#' call_modify(call, list(na.rm = quote(expr = )))
call_modify <- function(call, new_args, env = parent.frame()) {
  stopifnot(is.call(call), is.list(new_args))

  call <- call_standardise(call, env)

  if (!all_named(new_args)) {
    cat_stop("All new arguments must be named")
  }

  for (nm in names(new_args)) {
    call[[nm]] <- new_args[[nm]]
  }
  call
}

#' @rdname nonstandard_eval
#' @export
call_standardise <- function(call, env = parent.frame()) {
  stopifnot(is_call(call))

  f <- eval(call[[1]], env)
  if (is.primitive(f)) {
    return(call)
  }

  match.call(f, call)
}

#' Make a function from its components.
#'
#' This constructs a new function given it's three components:
#' list of arguments, body code and parent environment.
#'
#' @rdname nonstandard_eval
#' @param args A named list of default arguments.
#'   Note that if you want
#'  arguments that don't have defaults, you'll
#'  need to use the special function
#'  \code{\link{alist}}, e.g. \code{alist(a = , b = 1)}
#' @param body A language object representing the
#' code inside the function.
#'   Usually this will be most easily generated
#'   with \code{\link{quote}}
#' @param env The parent environment of the function,
#' defaults to the calling
#'  environment of \code{make_function}
#' @export
#' @examples
#' f <- function(x) x + 3
#' g <- make_function(alist(x = ), quote(x + 3))
#'
#' # The components of the functions are identical
#' identical(formals(f), formals(g))
#' identical(body(f), body(g))
#' identical(environment(f), environment(g))
#'
#' # But the functions are not identical because f has src code reference
#' identical(f, g)
#'
#' attr(f, "srcref") <- NULL
#' # Now they are:
#' stopifnot(identical(f, g))
make_function <- function(args, body, env = parent.frame()) {
  args <- as_prls(args)
  stopifnot(
    all_named(args),
    is.language(body)
  )
  env <- to_env(env)

  eval(call("function", args, body), env)
}

#' Make and evaluate calls.
#'
#' @rdname nonstandard_eval
#' @param f Function to call. For \code{make_call}, either a string, a symbol
#'   or a quoted call. For \code{do_call}, a bare function name or call.
#' @param ...,.args Arguments to the call either in or out of a list
#' @param .env Environment in which to evaluate call. Defaults to parent frame.
#' @export
#' @examples
#' # f can either be a string, a symbol or a call
#' make_call("f", a = 1)
#' make_call(quote(f), a = 1)
#' make_call(quote(f()), a = 1)
#'
#' #' Can supply arguments individual or in a list
#' make_call(quote(f), a = 1, b = 2)
#' make_call(quote(f), list(a = 1, b = 2))
make_call <- function(f, ..., .args = list()) {
  if (is.character(f)) {
    f <- as.name(f)
  }
  as.call(c(f, ..., .args))
}

#' @rdname nonstandard_eval
#' @export
do_call <- function(f,
                    ...,
                    .args = list(),
                    .env = parent.frame()) {
  f <- substitute(f)

  call <- make_call(f, ..., .args)
  eval(call, .env)
}

#' @rdname nonstandard_eval
#' @export
call2fn <- function(call, env = env_call()) {
  if (inherits(call, "frame")) {
    return(call$fn)
  }
  expr <- rlang::get_expr(call)
  env <- rlang::get_env(call, env)
  if (!is_call(expr)) {
    cat_stop("`{call}` must be a quoted call")
  }
  switch(call_type(expr),
         recursive = cat_stop("`call` does not call a named or inlined function"),
         inlined = rlang::node_car(expr),
         named = ,
         namespaced = ,
         rlang::eval_bare(
           rlang::node_car(expr),
           env
         )
  )
}
call_type <- function(x) {
  is_namespaced_symbol <- function(x,
                                   ns = NULL,
                                   private = NULL) {
    namespace_sym <- quote(`::`)
    namespace2_sym <- quote(`:::`)
    if (typeof(x) != "language") {
      return(FALSE)
    }
    if (!is_empty(ns) &&
        !identical(rlang::node_cadr(x), rlang::sym(ns))) {
      return(FALSE)
    }
    head <- rlang::node_car(x)
    if (is_empty(private)) {
      identical(head, namespace_sym) || identical(head, namespace2_sym)
    } else if (private) {
      identical(head, namespace2_sym)
    } else {
      identical(head, namespace_sym)
    }
  }

  x <- rlang::get_expr(x)
  stopifnot(typeof(x) == "language")
  type <- typeof(rlang::node_car(x))
  if (type == "symbol") {
    "named"
  } else if (is_namespaced_symbol(rlang::node_car(x))) {
    "namespaced"
  } else if (type == "language") {
    "recursive"
  } else if (type %in% c("closure", "builtin", "special")) {
    "inlined"
  } else {
    cat_stop("corrupt language object")
  }
}

#' Make a function from its components.
#'
#' @rdname nonstandard_eval
#' @param args A named list of default arguments.  Note that if you want
#'  arguments that don't have defaults, you'll need to use the special function
#'
#' @param body A language object representing the code inside the function.
#'   Usually this will be most easily generated with
#' @param env The parent environment of the function, defaults to the calling
#'  environment
#' @export
#' @examples
#' f <- function(x) x + 3
#' g <- make_function(alist(x = ), quote(x + 3))
#'
#' # The components of the functions are identical
#' identical(formals(f), formals(g))
#' identical(body(f), body(g))
#' identical(environment(f), environment(g))
#'
#' # But the functions are not identical because f has src code reference
#' identical(f, g)
#'
#' attr(f, "srcref") <- NULL
#' # Now they are:
#' stopifnot(identical(f, g))
make_function <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  stopifnot(
    all_named(args),
    is.language(body)
  )
  env <- to_env(env)

  eval(call("function", args, body), env)
}

#' Unenclose a closure.
#'
#' @rdname nonstandard_eval
#' @param f a closure
#' @export
#' @examples
#' power <- function(exp) {
#'   function(x) x^exp
#' }
#' square <- power(2)
#' cube <- power(3)
#'
#' square
#' cube
#' unenclose(square)
#' unenclose(cube)
unenclose <- function(f) {
  stopifnot(is.function(f))

  env <- environment(f)
  make_function(formals(f), substitute_q(body(f), env), parent.env(env))
}
#' @export
substitute_q <- function(x, env) {
  stopifnot(is.language(x))
  env <- to_env(env)

  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}
#' @export
substitute_w <- function(x) {
  caller <- parent.frame()
  caller2 <- sys.frame(sys.parent(2))

  expr <- substitute(x)
  expr <- bquote(.(substitute)(.(expr)))
  expr <- eval(expr, caller)

  expr <- bquote(.(bquote)(.(expr)))
  expr <- eval(expr, caller2)
  bquote(with(.(caller2), .(expr)))
}
substitute_w2<-function(x,caller2 = env_call()) {
  caller <- parent.frame()


  expr <- substitute(x)
  expr <- bquote(.(substitute)(.(expr)))
  expr <- eval(expr, caller)

  expr <- bquote(.(bquote)(.(expr)))
  expr <- eval(expr, caller2)
  bquote(with(.(caller2), .(expr)))
}
#' @export
bquote_w <- function(x) {
  caller <- parent.frame()

  expr <- substitute(x)
  expr <- bquote(.(bquote)(.(expr)))

  expr <- eval(expr, caller) # expr == a + 2
  bquote(with(.(caller), .(expr))) # with(<caller>, a + 2)
}
#' @export
subs <- function(x, env = parent.frame()) {
  if (identical(env, globalenv())) {
    env <- as.list(env)
  }

  substitute_q(substitute(x), env)
}

#' A compact syntax for anonymous functions.
#'
#' @rdname nonstandard_eval
#' @param ... The last argument is the body of
#' the function, all others are
#'   arguments to the function.  If there is
#'   only one argument, the formals
#'   are guessed from the code.
#' @param .env parent environment of the created function
#' @export
#' @examples
#' \dontrun{
#'
#' f(x + y)
#' f(x + y)(1, 10)
#' f(x, y = 2, x + y)
#'
#' f({
#'   y <- runif(1)
#'   x + y
#' })
#' }
#'
f <- function(..., .env = parent.frame()) {
  dots <- match.call(expand.dots = FALSE)$`...`
  n <- length(dots)

  if (n == 1) {
    fun <- make_function(alist(... = ), dots[[1]], .env)

    names <- find_globals(fun, merge = FALSE)$variables
    args <-
      stats::setNames(rep(list(substitute()), length(names)), names)
    formals(fun) <- args

    fun
  } else {
    body <- dots[[n]]
    args <- dots[-n]

    # translate unnamed args into named empty symbols
    bare <- (names(args) %||% rep("", length(args))) == ""
    bare_names <- vapply(args[bare], as_chr, character(1))
    bare_names[bare_names == ".dots"] <- "..."

    args[bare] <- rep(list(substitute()), sum(bare))
    names(args)[bare] <- bare_names

    make_function(args, body, .env)
  }
}


#' Random seed
#'
#' @rdname nonstandard_eval
#'
#' @param seed `[integer(1)]`\cr The random seed to use to evaluate the code.
#' @param code code to evaluate
#' @param .kind `[character(1)]`\cr Kind of (uniform) RNG to use.
#' @param .normal_kind `[character(1)]`\cr Kind of normal RNG to use.
#' @param .sample_kind `[character(1)]`\cr Kind of RNG to use for sampling.
#'
#' @export
#'
#' @examples
#'
#' # Same random values:
#' with_seed(seed = 7, code = runif(5))
#' with_seed(seed = 10, code = runif(5))
#'
#' # Use a pseudorandom value as seed to advance the RNG and pick a different
#' # value for the next call:
#' with_seed(seed <- sample.int(.Machine$integer.max, 1L), runif(5))
#' with_seed(seed, runif(5))
#' with_seed(seed <- sample.int(.Machine$integer.max, 1L), runif(5))
with_seed <-
  function(seed,
           code,
           .kind = "default",
           .normal_kind = "default",
           .sample_kind = "default") {
    .set_seed <- function(seed) {
      do.call(RNGkind, args = as_l(seed$kind))
      if (is.null(seed$seed)) {
        assign(".Random.seed", seed$random_seed, globalenv())
      } else {
        set.seed(seed$seed)
      }
    }

    .with_seed <- function(code) {
      old_seed <- get_seed()
      if (is.null(old_seed)) {
        on.exit(.rm_seed(), add = TRUE)
      } else {
        on.exit(.set_seed(old_seed), add = TRUE)
      }
      code
    }

    .rm_seed <- function() {
      if (!has_seed()) {
        return(NULL)
      }
      set.seed(seed = NULL)
      rm(".Random.seed", envir = globalenv())
    }

    if (is_empty(seed)) {
      seed <- sample.int(10^5, 1)
    }

    if (is_empty(.kind)) {
      .kind <- c(
        "Wichmann-Hill",
        "Marsaglia-Multicarry",
        "Super-Duper",
        "Mersenne-Twister",
        "Knuth-TAOCP",
        "user-supplied",
        "Knuth-TAOCP-2002",
        "L'Ecuyer-CMRG",
        "default"
      )
      .kind <- sample(.kind, 1)
    }
    if (is_empty(.normal_kind)) {
      .normal_kind <- c(
        "Buggy Kinderman-Ramage",
        "Ahrens-Dieter",
        "Box-Muller",
        "user-supplied",
        "Inversion",
        "Kinderman-Ramage",
        "default"
      )
      .normal_kind <- sample(.normal_kind, 1)
    }

    if (is_empty(.sample_kind)) {
      .sample_kind <- c("Rounding", "Rejection", "default")
      .sample_kind <- sample(.sample_kind, 1)
    }

    force(seed)
    force(.kind)
    force(.normal_kind)
    force(.sample_kind)
    out <- .with_seed({
      .set_seed(list(
        seed = seed,
        kind = c(
          .kind, .normal_kind,
          .sample_kind
        )
      ))
      code
    })
    attr(out, "used_seed") <-
      structure(
        list(
          seed = seed,
          kind = .kind,
          normal.kind = .normal_kind,
          sample.kind = .sample_kind
        ),
        class = "used_seed"
      )
    out
  }

#' @export
print.used_seed <- function(x) {
  cat("<used_seed>\n")
}

#' @export
get_seed <- function() {
  if (!has_seed()) {
    return(NULL)
  }
  list(
    random_seed = get(
      ".Random.seed",
      globalenv(),
      mode = "integer",
      inherits = FALSE
    ),
    kind = RNGkind()
  )
}

#' @export
with_fnoncode <- function(code, .fn, .ext = ".R") {
  .temp <- tempfile(
    pattern = "with_fnoncode__",
    tmpdir = tempdir(),
    fileext = .ext
  )

  writeLines(text = code, con = .temp)

  on.exit(file.remove(.temp))
  .temp_fn <- f({
    on_error(~ return(invisible()))
    .fn(.temp)
  })
  .temp_fn(.temp)
}
#' Fit a list of formulas
#'
#' @rdname nonstandard_eval
#'
#' @param data A dataset used to fit the models.
#' @param .f A fitting function
#' @param .formulas A list of formulas specifying a model.
#' @param ... Additional arguments passed on to `.f`
#'
#' @export
#'
#' @examples
#'
#' # fit_with_frm() is typically used with formulas().
#' disp_fits <- mtcars %>% fit_with_frm(lm, formulas(~disp,
#'   additive = ~ drat + cyl,
#'   interaction = ~ drat * cyl,
#'   full = add_predictors(interaction, ~am, ~vs)
#' ))
#'
#' # The list of fitted models is named after the names of the list of
#' # formulas:
#' disp_fits$full
#'
#' # Additional arguments are passed on to .f
#' mtcars %>% fit_with_frm(glm, list(am ~ disp, am ~ gear + mpg), family = binomial)
#'
#' x <- runif(100,0,100)
#' y <- 1.5*roll_noise(x,random_percent = .01)^2
#'
#' data  <- data.frame(x = x, y = y)
#'
#' rm(x,y)
#'
#' fit_with_boots(
#'   .data = data,
#'   .fn = nls,
#'   times = 10,
#'   lhs = y ,
#'   rhs =  a*x^b,
#'   start = list(a = 1,b = 1)
#' )
#'
#' fit_with_boots(
#'   .data = mtcars,
#'   .fn = glm,
#'   times = 10,
#'   lhs = am,
#'   rhs = gear + mpg,
#'   family = binomial
#' )
#'
fit_with_frm <- function(data, .f, .formulas, ..., .seeds = NULL) {
  if (is_empty(.seeds)) {
    .seeds <- sample.int(10^5, length(.formulas))
  }

  args <- list(...)
  args$data <- quote(data)
  purrr::map2(.formulas, .seeds, function(formula, seed) {
    with_seed(
      seed = seed,
      code = invoke(".f", args, formula = formula, .env = environment())
    )
  })
}
#' @export
fit_with_boots <-
  function(.data,
           .fn,
           lhs,
           rhs,
           times = 10,
           ...,
           .seeds = NULL) {
    lhs <- enexpr(lhs)
    rhs <- enexpr(rhs)
    .formula <- make_formula(lhs = lhs, rhs = rhs)
    args <- dots(...)
    boots <-
      do_call(
        "bootsplits",
        list(
          dataset = .data,
          times = times,
          outcome = lhs
        )
      )
    .fn <- force(.fn)
    boots <- map(boots$splits, rsample::analysis)
    purrr::map(boots, function(x) {
      do_call(
        f = .fn,
        .args = c(formula = .formula, list(data = x), args)
      )
    })
  }

#' Create a new "with" or "local" function
#'
#'
#' @name nonstandard_eval
#' @param set `[function(...)]`\cr Function used to set the state.
#'   The function can have arbitrarily many arguments, they will be replicated
#'   in the formals of the returned function.
#' @param reset `[function(x)]`\cr Function used to reset the state.
#'   The first argument can be named arbitrarily, further arguments with default
#'   values, or a "dots" argument, are supported but not used: The function will
#'   be called as `reset(old)`.
#' @param envir `[environment]`\cr Environment of the returned function.
#' @param new `[logical(1)]`\cr Replace the first argument of the `set` function
#'  by `new`? Set to `FALSE` if the `set` function only has optional arguments.
#'
#' @examples
#' getwd()
#'
#' with_dir <- with_newfn(setwd)
#' with_dir(tempdir(), getwd())
#'
#' global_stack <- list()
#' set_global_state <- function(state, msg = "Changing global state.") {
#'   global_stack <- c(list(state), global_stack)
#'  message(msg)
#'   state
#' }
#' reset_global_state <- function(state) {
#'   old_state <- global_stack[[1]]
#'   global_stack <- global_stack[-1]
#'   stopifnot(identical(state, old_state))
#' }
#' with_newfn(set_global_state, reset_global_state)
#'
#' #' # number of significant digits to print
#'
#' set_options <- function(new_options) {
#'    do.call(options, as_l(new_options))
#' }
#'
#' reset_options <- function(old_options) {
#'    options(old_options)
#' }
#'
#' set_locale <- function(cats) {
#'    cats <- as_chr(cats)
#'    stopifnot(is_named(cats))
#'
#'    if ("LC_ALL" %in% names(cats)) {
#'       stop("Setting LC_ALL category not implemented.")
#'    }
#'
#'    old <- vapply(names(cats), Sys.getlocale, character(1))
#'
#'    mapply(Sys.setlocale, names(cats), cats)
#'    invisible(old)
#' }
#'
#' with_options <- with_newfn(set_options, reset_options)
#' local_options <- local_newfn(set_locale, dots = TRUE)
#' # modify temporarily the number of significant digits to print
#' with_options(list(digits = 3), getOption("digits"))
#' with_options(list(digits = 3), print(pi))
#'
#' # modify temporarily the character to be used as the decimal point
#' getOption("digits")
#' with_options(list(OutDec = ","), print(pi))
#'
#' # modify temporarily multiple options
#' with_options(list(OutDec = ",", digits = 3), print(pi))
#'
#' @export
with_newfn <-
  function(set,
           reset = set,
           envir = parent.frame(),
           new = TRUE) {
    fmls <- formals(set)

    if (length(fmls) > 0L) {
      # called pass all extra formals on
      called_fmls <-
        stats::setNames(lapply(names(fmls), as.symbol), names(fmls))

      if (new) {
        # rename first formal to new
        called_fmls[[1]] <- as.symbol("new")

        fun_args <- c(alist(new = , code = ), fmls[-1L])
      } else {
        fun_args <- c(alist(code = ), fmls)
      }
    } else {
      # no formals -- only have code
      called_fmls <- NULL

      fun_args <- alist(code = )
    }

    set_call <- as.call(c(substitute(set), called_fmls))

    reset <-
      if (missing(reset)) {
        substitute(set)
      } else {
        substitute(reset)
      }

    fun <- eval(bquote(function(args) {
      old <- .(set_call)
      on.exit(.(reset)(old))
      force(code)
    }))

    # substitute does not work on arguments, so we need to fix them manually
    formals(fun) <- fun_args

    environment(fun) <- envir

    fun
  }
#' @export
local_newfn <-
  function(set,
           reset = set,
           envir = parent.frame(),
           new = TRUE,
           dots = FALSE) {
    fmls <- formals(set)

    if (length(fmls) > 0L) {
      # called pass all extra formals on
      called_fmls <-
        stats::setNames(lapply(names(fmls), as.symbol), names(fmls))

      if (new) {
        if (dots) {
          called_fmls[[1]] <- as.symbol(".new")
          fun_args <- c(alist(.new = list(), ... = ), fmls[-1L])
        } else {
          called_fmls[[1]] <- as.symbol("new")
          fun_args <- c(alist(new = list()), fmls[-1L])
        }
      } else {
        fun_args <- fmls
      }
    } else {
      # no formals
      called_fmls <- NULL

      fun_args <- alist()
    }

    set_call <- as.call(c(substitute(set), called_fmls))

    reset <-
      if (missing(reset)) {
        substitute(set)
      } else {
        substitute(reset)
      }

    if (dots) {
      modify_call <- quote(.new <- list_combine(as_l(.new), list(...)))

      fun <- eval(bquote(function(args) {
        list_combine <- function(rhs, lhs) {
          for (nme in names(lhs)) {
            rhs[nme] <- lhs[nme]
          }
          rhs
        }
        .(modify_call)
        old <- .(set_call)
        defer(.(reset)(old), envir = .local_envir)
        invisible(old)
      }))
    } else {
      fun <- eval(bquote(function(args) {
        old <- .(set_call)
        defer(.(reset)(old), envir = .local_envir)
        invisible(old)
      }))
    }

    # substitute does not work on arguments, so we need to fix them manually
    formals(fun) <-
      c(fun_args, alist(.local_envir = parent.frame()))

    environment(fun) <- envir

    fun
  }

#' @keywords internal
set_options <- function(new_options) {
  do.call(options, as_l(new_options))
}
#' @keywords internal
reset_options <- function(old_options) {
  options(old_options)
}
#' @keywords internal
set_locale <- function(cats) {
  cats <- as_chr(cats)
  stopifnot(is_named(cats))

  if ("LC_ALL" %in% names(cats)) {
    cat_stop("Setting LC_ALL category not implemented.")
  }

  old <- vapply(names(cats), Sys.getlocale, character(1))

  mapply(Sys.setlocale, names(cats), cats)
  invisible(old)
}

#' @export
with_options <- with_newfn(set_options, reset_options)
#' @export
local_options <- local_newfn(set_locale, dots = TRUE)
#' @export
with_sink <-
  with_newfn(
    set = f({
      on_error(~ sink())
      sink(file = file)
    }),
    reset = f({
      as_null(old)
      sink()
    })
  )


#' @export
lang2str <- function(x, ...) {
  UseMethod("lang2str")
}
#' @export
lang2str.call <- function(x) {
  x <- rlang::quo_squash(x)
  if (rlang::is_missing(x)) {
    return("<empty>")
  }
  switch(typeof(x),
         `NULL` = "NULL",
         symbol = as_str(x),
         language = {
           if (is_data_pronoun(x)) {
             data_pronoun_name(x) %||% "<unknown>"
           } else {
             name <- deparse_all(x)
             name <- gsub("\n.*$", "...", name)
             name
           }
         },
         if (is_bare_a < tomic(x, n = 1)) {
           name <- expr_text(x)
           name <- gsub("\n.*$", "...", name)
           name
         } else {
           paste0("<", pillar:::type_sum(x), ">")
         }
  )
}
#' @export
lang2str.default <- function(x, ...) {
  as_label(x,...)
}
#' @export
lang2str.formula <- function(x, ...) {
  rlang::f_text(x = x)
}
#' @export
lang2str.function <- function(x, ...) {
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
      args = list(
        substitute(x),
        parent.frame(2)
      )
    ))
  if (length(xname) > 1) {
    xname <- deparse(eval(expr, environment(expr)))
  }
  xname <- gsub("\"", "", xname)
  xname
}
#' @export
lang2str.name <- function(x,...) {
  x <- rlang::quo_squash(x)
  if (rlang::is_missing(x)) {
    return("<empty>")
  }
  switch(typeof(x),
         `NULL` = "NULL",
         symbol = as_str(x),
         language = {
           if (is_data_pronoun(x)) {
             data_pronoun_name(x) %||% "<unknown>"
           } else {
             name <- deparse_all(x)
             name <- gsub("\n.*$", "...", name)
             name
           }
         },
         if (rlang::is_bare_atomic(x, n = 1)) {
           name <- rlang::expr_text(x)
           name <- gsub("\n.*$", "...", name)
           name
         } else {
           paste0("<", pillar:::type_sum(x), ">")
         }
  )
}
#' @export
lang2str.character <- function(x,evaluated = TRUE) {

  if(evaluated){
    return(rlang::expr_text(enexpr(x)))
  }
  call_x <- match.call()$x
  if (is_atomic(call_x)) {
    return(rlang::expr_text(enexpr(call_x)))
  } else {
    return(rlang::as_label(call_x))
  }
}
#' @export
lang2str.numeric <- lang2str.character
#' @export
lang2str.logical <- lang2str.character
#' @export
lang2str.factor <- lang2str.character
#' @export
lang2str.NULL <- function(x,...) {
  quote(NULL)
}
#' @export
lang2str.list <- function(x,...) {
  x <- as.list(x)
  nm <- names2(x)

  set_names(map(x,rlang::as_label),nm)
}
#' @export
lang2str.pairlist <- function(x,...) {
  map(match.call(expand.dots = FALSE)$x,~ as.character(.x))
}
#' @export
lang2str.lazy <- function(x,...) {
  rlang::as_label(x$expr)
}
#' @export
lang2str.lazy_dots <- function(x,...) {
  map(x, lang2str)
}
#' @export
lang2str.quosure <- function(x,...) {
  rlang::as_label(expr_get_quotable(x))
}
#' @export
lang2str.quosures <- function(x,...) {
  map(x, lang2str)
}

#' @export
prls_name <- function(x) {
  ids <- !are_named(x)
  str_x <- map_chr(unlist(x), lang2str)
  nms <- names2(x)
  names(x) <- ifelse(nms == "", str_x, nms)
  names(x) <- gsub(pattern = '\"',"",names(x),fixed = T)
  x
}
#' @keywords internal
data_pronoun_name <- function(x) {
  if (is_call(x, "$")) {
    arg <- rlang::node_cadr(rlang::node_cdr(x))
    if (is_symbol(arg)) {
      return(as_str(arg))
    } else {
      return(NULL)
    }
  }
  if (is_call(x, "[[")) {
    arg <- rlang::node_cadr(rlang::node_cdr(x))
    if (rlang::is_string(arg)) {
      return(arg)
    } else {
      return(NULL)
    }
  }
}
#' @keywords internal
is_data_pronoun <- function(x) {
  dot_data_sym <- .data
  is_call(x, c("[[", "$"), n = 2L) &&
    identical(
      rlang::node_cadr(x),
      dot_data_sym
    )
}
#' @keywords internal
is_bare_atomic <- function(x, n = NULL) {
  !is.object(x) && is_atomic(x, n)
}
#' @keywords internal
expr_text <- function(expr,width = 60L,nlines = Inf) {
  sym_text <- function(sym) {
    needs_backticks <- function(str) {
      if (!rlang::is_string(str)) {
        str <- as_str(str)
      }
      n <- nchar(str)
      if (!n) {
        return(FALSE)
      }
      if (str %in% reserved_words) {
        return(TRUE)
      }
      start <- substr(str, 1, 1)
      if (!grepl("[[:alpha:].]", start)) {
        return(TRUE)
      }
      if (n == 1) {
        return(FALSE)
      }
      remaining <- substr(str, 2, n)
      if (start == "." && grepl("^[[:digit:]]", remaining)) {
        return(TRUE)
      }
      grepl("[^[:alnum:]_.]", remaining)
    }
    text <- as_str(sym)
    if (needs_backticks(text)) {
      text <- sprintf("`%s`", text)
    }
    text
  }

  if (is_symbol(expr)) {
    return(sym_text(expr))
  }
  str <- deparse(expr, width.cutoff = width, backtick = TRUE)
  if (length(str) > nlines) {
    str <- c(str[seq_len(nlines - 1)], "...")
  }
  paste0(str, collapse = "\n")
}
#' @export
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

#' @export
req_code <- function(x) {
  x <- enexpr(x)
  unique(find_pkgs_rec(x))
}
req_text <- function(text) {
  tryCatch(
    error = function(err) {
      character()
    },
    {
      code <- parse(text = text)
      req_code(!!code)
    }
  )
}
find_pkgs_rec <- function(x) {
  flat_map_chr <- function(x, f, ...) {
    if (length(x) == 0) {
      character()
    } else {
      unlist(lapply(x, f, ...))
    }
  }
  if (is_syntactic_literal(x) || is_symbol(x)) {
    char_or_sym <- function(x) {
      if (is.character(x)) {
        x
      } else if (is.symbol(x)) {
        as.character(x)
      } else {
        character()
      }
    }
    if (is_chr(x)) {
      return(req_text(x))
    } else {
      return(character())
    }
  }

  if (is_pairlist(x) || is.expression(x)) {
    return(flat_map_chr(as.list(x), find_pkgs_rec))
  }

  if (is_call(x, c("::", ":::"))) {
    char_or_sym(x[[2]])
  } else if (is_call(x, c("library", "require"))) {
    x <- call_standardise(x, env = baseenv())
    if (isTRUE(x$character.only) ||
        identical(x$character.only, quote(T))) {
      if (is.character(x$package)) {
        x$package
      } else {
        character()
      }
    } else {
      char_or_sym(x$package)
    }
  } else if (is_call(x, c("requireNamespace", "loadNamespace"))) {
    x <- call_standardise(x, env = baseenv())
    char_or_sym(x$package)
  } else {
    flat_map_chr(as.list(x), find_pkgs_rec)
  }
}

#' @export
auto_name_frm <- function(funs) {
  where <-
    !rlang::have_name(funs) &
    map_lgl(funs, function(x) {
      rlang::is_bare_formula(x) &&
        is_call(rlang::f_rhs(x))
    })
  names(funs)[where] <-
    map_chr(funs[where], function(x) {
      lang2str(rlang::f_rhs(x)[[1]])
    })
  funs
}
#' @export
capture_output <-
  function(code,.env = env_prnt(),.print = TRUE,.ext = ".R") {
    .temp <- tempfile(fileext = .ext)

    env <- new.env(parent = .env)

    code <- substitute(code)

    with_sink(
      new = .temp,
      code = if (.print) {
        print(do_try(eval(code, envir = env)))
      } else {
        dput(do_try(eval(code, envir = env)))
      }
    )

    out <- readLines(.temp)

    file.remove(.temp)

    if (.print) {
      rstudioapi::insertText(deparse_all(out))
    } else {
      rstudioapi::insertText(capture.output(cat(out, sep = "\n")))
    }

    invisible()
  }
#' @export
unstructure <- function (x) {
  out <- x
  attributes(out) <- NULL
  dim(out) <- dim(x)
  names(out) <- names(x)
  out
}
#' @export
dput_source<-function (x) {
  invisible(rstudioapi::insertText(capture.output(dput(x))))
}
