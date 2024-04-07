collect_usage_handlers <- new.env(hash = TRUE, parent = emptyenv())

collect_locals_assign_handler <- function(e, w) {
  w$collect(get_assigned_var(e), e, w)
  walk_code(e[[2]], w)
  walk_code(e[[3]], w)
}
collect_locals_for_handler <- function(e, w) {
  signal <- function(msg)
    w$exit(e, msg, w)
  w$collect(as.character(is_symstr(e[[2]], signal)), e, w)
  walk_code(e[[3]], w)
  walk_code(e[[4]], w)
}

collect_usage <- function(fun, name = "<anonymous>", ...) {
  w <- make_usage_collector(fun, ...)
  collect_usage_fun(name, formals(fun), body(fun), w)
}
collect_usage_fun <- function(name, formals, body, w) {
  w$name <- c(w$name, name)
  parnames <- names(formals)
  locals <- find_func_locals(formals, body)
  w$env <- new.env(hash = TRUE, parent = w$env)
  for (n in c(parnames, locals)) {
    assign(n, TRUE, w$env)
  }
  w$start_collect_locals(parnames, locals, w)
  for (a in drop_missings(formals)) {
    walk_code(a, w)
  }
  walk_code(body, w)
  w$finish_collect_locals(w)
}
collect_usage_call <- function(e, w) {
  if (typeof(e[[1]]) %in% c("symbol", "character")) {
    fn <- as.character(e[[1]])
    if (w$is_local(fn, w)) {
      w$enter_local("function", fn, e, w)
    } else {
      w$enter_global("function", fn, e, w)
    }
  } else {
    walk_code(e[[1]], w)
  }
  collect_usage_args(e, w)
}
collect_usage_leaf <- function(v, w) {
  if (typeof(v) == "symbol") {
    vn <- as.character(v)
    if (v == "...") {
      w$signal("... may be used in an incorrect context",
               w)
    } else if (is_dd_sym(v)) {
      if (w$is_local("...", w)) {
        w$enter_local("variable", "...", v, w)
      } else {
        w$signal(paste(v, "may be used in an incorrect context"),
                 w)
      }
    } else if (w$is_local(vn, w)) {
      w$enter_local("variable", vn, v, w)
    } else if (!vn %in% c("*tmp*", "*tmpv*")) {
      w$enter_global("variable", vn, v, w)
    }
  }
}
collect_usage_is_local <- function (v, w) {
  if (is_dd_sym(v)) {
    v <- "..."
  }
  ! is_baseenv(find_env_owner(v, w$env, stop = w$globalenv, default = baseenv()))
}
collect_usage_args <- function (e, w) {
  for (a in drop_missings(e[-1]))
    if (typeof(a) == "symbol" &&
        a == "...") {
      if (w$is_local("...", w))
        w$enter_local("variable", "...", a, w)
      else
        w$signal(paste(a, "may be used in an incorrect context:",
                       paste_expr(e)),
                 w)
    }
  else
    walk_code(a, w)
}

make_constant_folder <-
  function(...,
           leaf = fold_leaf,
           handler = function(v, w) {
             if (w$foldable(v, w)) {
               fold_call
             }
           },
           call = function(e, w) {
             exit_folder(e, w)
           },
           exit = function(e, w) {
             cat_stop(paste(
               "not a foldable expression:",
               deparse(e, width.cutoff = 500)
             ))
           },
           is_local = function(v, w) {
             FALSE
           },
           foldable = is_foldable,
           isConstant = is_constant_value,
           signal = function(e, msg, w) {
             cat_warn(msg)
           }) {
    list(
      handler = handler,
      call = call,
      exit = exit,
      leaf = leaf,
      is_local = is_local,
      foldable = is_foldable,
      isConstant = isConstant,
      signal = signal,
      ...
    )
  }
make_locals_collector <-
  function(...,
           leaf = function(e, w) {
             character(0)
           },
           handler = get_collect_locals_handler,
           is_local = function(v, w) {
             FALSE
           },
           exit = function(e, msg, w) {
             cat_stop(msg)
           },
           collect = function(v, e, w) {
             print(v)
           }) {
    make_code_walker(
      leaf = leaf,
      handler = handler,
      collect = collect,
      is_local = is_local,
      exit = exit
    )
  }
make_usage_collector <-
  function(fun,
           ...,
           name = NULL,
           enter_local = as_null,
           enter_global = as_null,
           enter_internal = as_null,
           start_collect_locals = as_null,
           finish_collect_locals = as_null,
           warn = cat_warn) {
    signal <- function(m, w) {
      if (!is_null(w$frow) && !is.na(w$frow)) {
        fname <- w$srcfile
        if (w$frow == w$lrow) {
          loc <- paste(" (", fname, ":", w$frow, ")", sep = "")
        } else {
          loc <- paste(" (", fname, ":", w$frow, "-", w$lrow,
                       ")",
                       sep = ""
          )
        }
      } else {
        loc <- NULL
      }
      w$warn(paste(paste(w$name, collapse = " : "), ": ", m, loc,
                   "\n",
                   sep = ""
      ))
    }
    if (typeof(fun) == "closure") {
      env <- environment(fun)
    } else {
      env <- .GlobalEnv
    }
    make_code_walker(
      ...,
      name = name,
      enter_local = enter_local,
      enter_global = enter_global,
      enter_internal = enter_internal,
      start_collect_locals = start_collect_locals,
      finish_collect_locals = finish_collect_locals,
      warn = warn,
      signal = signal,
      leaf = collect_usage_leaf,
      call = collect_usage_call,
      handler = get_collect_usage_handler,
      globalenv = env,
      env = env,
      name = NULL,
      srcfile = NULL,
      frow = NULL,
      lrow = NULL,
      is_local = collect_usage_is_local
    )
  }

get_collect_locals_handler <- function(v, w) {
  switch(
    v,
    `=` = ,
    `<-` = collect_locals_assign_handler,
    `for` = collect_locals_for_handler,
    `function` = ,
    `~` = function(e, w)
      character(0),
    local = if (!w$is_local(v,
                            w)) {
      function (e, w) {
        if (length(e) == 2)
          character(0)
        else
          for (a in drop_missings(e[-1]))
            walk_code(a, w)
      }
    },
    bquote = ,
    expression = ,
    Quote = ,
    quote = if (!w$is_local(v, w))
      function(e, w)
        character(0),
    delayedAssign = ,
    assign = function(e, w) {
      if (length(e) ==
          3 && is.character(e[[2]]) && length(e[[2]]) == 1) {
        w$collect(e[[2]], e, w)
        walk_code(e[[3]], w)
      } else {
        for (a in drop_missings(e[-1]))
          walk_code(a, w)
      }
    }
  )
}
get_collect_usage_handler <- function(v, w) {
  if (exists(v, envir = collect_usage_handlers, inherits = FALSE) &&
      (
        is_base_var(v, w$env) || is_pgk_var(v, w$env, "stats") ||
        is_pgk_var(v,
                   w$env, "utils") ||
        v == "Quote"
      )) {
    get(v, envir = collect_usage_handlers)
  }
}
get_assigned_var <- function (e) {
  v <- e[[2]]
  if (missing(v))
    cat_stop(paste("bad assignment:", paste_expr(e)))
  else if (typeof(v) %in% c("symbol", "character"))
    as.character(v)
  else {
    while (typeof(v) == "language") {
      if (length(v) < 2)
        cat_stop(paste("bad assignment:", paste_expr(e)))
      v <- v[[2]]
      if (missing(v))
        cat_stop(paste("bad assignment:", paste_expr(e)))
    }
    if (typeof(v) != "symbol")
      cat_stop(paste("bad assignment:", paste_expr(e)))
    as.character(v)
  }
}

paste_expr <- function(e, prefix = "\n    ") {
  de <- deparse(e)
  if (length(de) == 1) {
    sQuote(de)
  } else {
    paste(prefix, deparse(e), collapse = "")
  }
}


check_dots_assign_var <- function(v, w) {
  if (v == "...") {
    w$signal("... may be used in an incorrect context", w)
    FALSE
  } else if (is_dd_sym(v)) {
    w$signal(paste(v, "may be used in an incorrect context"), w)
    FALSE
  } else {
    TRUE
  }
}

constant_fold <- function(e, env = NULL, fail = NULL) {
  job <- function(exit) {
    is_local <- function(v, w)
      as.character(v) %in% env
    doExit <- function(e, w)
      exit(fail)
    w <- make_constant_folder(is_local = is_local, exit = doExit)
    walk_code(e, w)
  }
  callCC(job)
}
constant_fold_env <- function(e, env = .GlobalEnv, fail = NULL) {
  is_local <- function(v, w) {
    vname <- as.character(v)
    while (!identical(env, .GlobalEnv)) {
      if (exists(vname, env, inherits = FALSE)) {
        return(TRUE)
      }
      env <- parent.env(env)
    }
    FALSE
  }
  job <- function(exit) {
    doExit <- function(e, w)
      exit(fail)
    w <- make_constant_folder(is_local = is_local, exit = doExit)
    walk_code(e, w)
  }
  tryCatch(
    callCC(job),
    error = function(e)
      fail
  )
}

fold_leaf <- function(e, w) {
  if (is.name(e) &&
      as.character(e) %in% constNames && !w$is_local(e, w)) {
    e <- get(as.character(e), envir = baseenv())
  }
  if (!w$isConstant(e))
    exit_folder(e, w)
  e
}
fold_call <- function(e, w) {
  fname <- as.character(e[[1]])
  if (fname == "$") {
    args <- list(walk_code(e[[2]], w), e[[3]])
    foldable <- w$isConstant(args[[1]], w)
  } else {
    args <- lapply(e[-1], function(e)
      walk_code(e, w))
    foldable <- all(sapply(args, w$isConstant, w))
  }
  if (foldable) {
    msg <- try({
      v <- do.call(fname, args)
      NULL
    },
    silent = TRUE)
    if (!is.null(msg)) {
      w$signal(e, msg, w)
      exit_folder(e, w)
    } else if (w$isConstant(v, w)) {
      v
    } else {
      exit_folder(e, w)
    }
  } else {
    exit_folder(e, w)
  }
}

is_base_var <- function (v, env) {
  e <- find_env_owner(v, env)
  is_baseenv(e) || identical(e, .BaseNamespaceEnv) || v == "@<-"
}
is_baseenv <- function(e) {
  is_true(identical(e, baseenv()))
}
is_pgk_var <- function(v, env, pkg) {
  e <- find_env_owner(v, env)
  if (!identical(e, NA) && exists(v,
                                  envir = e,
                                  inherits = FALSE,
                                  mode = "function")) {
    f <- get(v,
             envir = e,
             inherits = FALSE,
             mode = "function")
    identical(environment(f), getNamespace(pkg))
  } else {
    FALSE
  }
}
is_simple_fun_def <- function(e, w) {
  typeof(e[[2]]) != "language" && typeof(e[[3]]) == "language" &&
    typeof(e[[3]][[1]]) %in% c("symbol", "character") &&
    e[[3]][[1]] == "function" && is_base_var("function", w$env)
}
is_closure_fun_def <- function(e, w) {
  typeof(e[[2]]) != "language" && typeof(e[[3]]) == "closure"
}
is_constant_value <- function(v, w) {
  is.null(v) ||
    (is.null(attributes(v)) && is.atomic(v)) ||
    (is.list(v) &&
       (identical(v, .Platform) || identical(v, .Machine)))
}
is_foldable <- function(v, w) {
  ((typeof(v) == "symbol" || typeof(v) == "character") &&
     as.character(v) %in% foldFuns && !w$is_local(v, w)
  )
}
is_symstr <- function(e, signal = cat_stop) {
  type <- typeof(e)
  if (type == "symbol" || type == "character") {
    e
  } else {
    signal("not a symbol or string")
  }
}
is_dd_sym <- function(name) {
  (is.symbol(name) ||
     is.character(name)) && length(grep("^\\.\\.[[:digit:]]+$",
                                        as.character(name))) != 0
}

evalseq <- function(e) {
  if (typeof(e) == "language") {
    v <- evalseq(e[[2]])
    e[[2]] <- as.name("*tmp*")
    c(v, list(e))
  } else {
    list(e)
  }
}
apdef <- function(e) {
  v <- NULL
  tmp <- as.name("*tmp*")
  tmpv <- as.name("*tmpv*")
  while (typeof(e) == "language") {
    ef <- e
    ef[[1]] <- make_assgn_fn(e[[1]])
    if (typeof(ef[[2]]) == "language") {
      ef[[2]] <- tmp
    }
    ef$value <- tmpv
    v <- c(v, list(ef))
    e <- e[[2]]
  }
  v
}
make_assgn_fn <- function(fun) {
  fun <- if (typeof(fun) == "symbol") {
    as.name(paste0(as.character(fun), "<-"))
  } else {
    fun[[3]] <- as.name(paste0(as.character(fun[[3]]), "<-"))
    fun
  }
}
flatten_assignment <- function(e) {
  if (typeof(e) == "language") {
    list(evalseq(e[[2]]), apdef(e))
  } else {
    list(NULL, NULL)
  }
}

drop_missings <- function(x) {
  lx <- as.list(x)
  ix <- rep(TRUE, length(x))
  for (i in seq_along(ix)) {
    a <- lx[[i]]
    if (missing(a)) {
      ix[i] <- FALSE
    }
  }
  lx[ix]
}
exit_folder <- function(e, w) {
  w$exit(e, w)
  cat_stop("constant folding cannot continue")
}
any_dots <- function(args) {
  for (i in 1:length(args)) {
    a <- args[[i]]
    if (!missing(a) && typeof(a) == "symbol" && a == "...") {
      return(TRUE)
    }
  }
  return(FALSE)
}

find_func_locals <- function(formals, body) {
  find_locals_list(c(list(body), drop_missings(formals)))
}
find_locals_list  <- function(elist, envir = baseenv()) {
  stop_fn <- c("expression", "quote", "Quote", "local")
  if (is.character(envir)) {
    locals <- envir
  } else {
    locals <- stop_fn[!sapply(stop_fn, is_base_var,
                              envir)]
  }
  special_fn <- c("~", "<-", "=", "for", "function")
  sf <- unique(c(locals, stop_fn))
  nsf <- length(sf)
  collect <- function(v, e, w) {
    assign(v, TRUE, envir = env)
  }
  is_local <- function(v, w) {
    as.character(v) %in% sf
  }
  w <-
    make_locals_collector(collect = collect, is_local = is_local)
  repeat {
    env <- new.env(hash = TRUE, parent = emptyenv())
    for (e in elist) {
      walk_code(e, w)
    }
    isloc <- sapply(sf, exists, envir = env, inherits = FALSE)
    last.nsf <- nsf
    sf <- unique(c(locals, sf[isloc]))
    nsf <- length(sf)
    if (last.nsf == nsf) {
      vals <- ls(env, all.names = TRUE)
      rdsf <- vals %in% special_fn
      if (any(rdsf)) {
        cat_warn(paste("local assignments to syntactic functions:",
                       vals[rdsf]))
      }
      return(vals)
    }
  }
}
find_env_owner <- function(v,env,stop = NA,default = NA) {
  while (!identical(env, stop)) {
    if (exists(v, envir = env, inherits = FALSE)) {
      return(env)
    } else if (identical(env, emptyenv())) {
      return(default)
    } else {
      env <- parent.env(env)
    }
  }
  default
}


#' find_globals
#'
#' @name  find_globals
#' @rdname find_globals
#' @export
make_code_walker <-
  function(...,
           handler = function(v, w) {
             NULL
           },
           call = function(e, w) {
             for (ee in as.list(e)) {
               if (!missing(ee)) {
                 walk_code(ee, w)
               }
             }
           },
           leaf = function(e, w) {
             print(e)
           }) {
    list(
      handler = handler,
      call = call,
      leaf = leaf,
      ...
    )
  }

#' find_globals
#'
#' @name  find_globals
#' @rdname find_globals
#' @export
walk_code <- function(e, w = make_code_walker()) {
  if (typeof(e) == "language") {
    if (typeof(e[[1]]) %in% c("symbol", "character")) {
      h <- w$handler(as.character(e[[1]]), w)
      if (!is.null(h)) {
        h(e, w)
      } else {
        w$call(e, w)
      }
    } else {
      w$call(e, w)
    }
  } else {
    w$leaf(e, w)
  }

}

# 'where' is ignored for now
mk_link_handler <- function(family, okLinks) {
  function(e, w) {
    w$enter_global("function", family, e, w)
    if (length(e) >= 2) {
      if (is.character(e[[2]])) {
        if (!(e[[2]] %in% okLinks)) {
          w$signal(paste(
            "link",
            sQuote(e[[2]]),
            "not available for",
            sQuote(family)
          ),
          w)
        }
      } else if (!is.name(e[[2]]) ||
                 !as.character(e[[2]]) %in% okLinks) {
        walk_code(e[[2]], w)
      }
    }
  }
}

add_collect_usage_handler <- function(v, where, fun) {
  assign(v, fun, envir = collect_usage_handlers)
}
#**** proceeds even if "..." or "..1", etc--is that right?
local({
  h <- function(e, w) {
    w$enter_global("function", as.character(e[[1]]), e, w)
    v <- get_assigned_var(e)
    check_dots_assign_var(v, w)
    w$enter_local("<-", v, e, w)
    if (is_simple_fun_def(e, w)) {
      collect_usage_fun(v, e[[3]][[2]], e[[3]][[3]], w)
    } else if (is_closure_fun_def(e, w)) {
      ## to handle inlined S4 methods
      fun <- e[[3]]
      w$globalenv <- environment(fun)
      w$env <- environment(fun)
      collect_usage_fun(v, formals(fun), body(fun), w)
    } else {
      if (typeof(e[[2]]) == "language") {
        fa <- flatten_assignment(e[[2]])
        for (a in fa) {
          for (b in a)
            walk_code(b, w)
        }
      }
      walk_code(e[[3]], w)
    }
  }
  add_collect_usage_handler("<-", "base", h)
  add_collect_usage_handler("=", "base", h)
})

#**** would be better to use match.call in most of these
#**** proceeds even if "..." or "..1", etc--is that right?
add_collect_usage_handler("<<-", "base", function(e, w) {
  w$enter_global("function", "<<-", e, w)
  v <- get_assigned_var(e)
  check_dots_assign_var(v, w)
  if (w$is_local(v, w)) {
    w$enter_local("<<-", v, e, w)
  } else {
    w$enter_global("<<-", v, e, w)
  }
  if (typeof(e[[2]]) == "language") {
    fa <- flatten_assignment(e[[2]])
    for (a in fa) {
      for (b in a)
        walk_code(b, w)
    }
  }
  walk_code(e[[3]], w)
})

add_collect_usage_handler("for", "base", function(e, w) {
  w$enter_global("function", "for", e, w)
  v <- as.character(e[[2]])
  w$enter_local("for", v, e, w)
  walk_code(e[[3]], w)
  walk_code(e[[4]], w)
})

add_collect_usage_handler("{", "base", function(e, w) {
  w$enter_global("function", "{", e, w)
  w$srcfile <- attr(e, "srcfile")$filename

  if (length(e) > 1) {
    for (i in 2:length(e)) {
      if (!is.null(attr(e, "srcref")[[i]])) {
        w$frow <- attr(e, "srcref")[[i]][[1]]
        w$lrow <- attr(e, "srcref")[[i]][[3]]
      }
      walk_code(e[[i]], w)
    }
  }
})

#**** is this the right way to handle :: and ::: ??
#**** maybe record package/name space?
local({
  h <- function(e, w) {
    w$enter_global("function", as.character(e[[1]]), e, w)
  }
  add_collect_usage_handler("~", "base", h)
  add_collect_usage_handler("quote", "base", h)
  add_collect_usage_handler("Quote", "methods", h)
  add_collect_usage_handler("expression", "base", h)
  add_collect_usage_handler("::", "base", h)
  add_collect_usage_handler(":::", "base", h)
})

#**** add counter to anonymous functions to distinguish??
add_collect_usage_handler("function", "base", function(e, w) {
  collect_usage_fun("<anonymous>", do_try(e[[2]],formals(e)), do_try(e[[3]],body(e)), w)
})

add_collect_usage_handler("local", "base", function(e, w) {
  w$enter_global("function", "local", e, w)
  if (length(e) == 2) {
    collect_usage_fun("<local>", NULL, e[[2]], w)
  } else {
    collect_usage_args(e, w)
  }
})

add_collect_usage_handler("assign", "base", function(e, w) {
  w$enter_global("function", "assign", e, w)
  if (length(e) == 3 &&
      is.character(e[[2]]) && length(e[[2]]) == 1) {
    w$enter_local("<-", e[[2]], e, w)
    walk_code(e[[3]], w)
  } else {
    collect_usage_args(e, w)
  }
})

add_collect_usage_handler("with", "base", function(e, w) {
  w$enter_global("function", "with", e, w)
  if (identical(w$skipWith, TRUE)) {
    walk_code(e[[2]], w)
  } else {
    collect_usage_args(e, w)
  }
})

local({
  h <- function(e, w) {
    w$enter_global("function", as.character(e[[1]]), e, w)
    walk_code(e[[2]], w)
  }
  add_collect_usage_handler("$", "base", h)
  add_collect_usage_handler("@", "base", h)
})

local({
  h <- function(e, w) {
    w$enter_global("function", as.character(e[[1]]), e, w)
    walk_code(e[[2]], w)
    walk_code(e[[4]], w)
  }
  add_collect_usage_handler("$<-", "base", h)
  add_collect_usage_handler("@<-", "base", h)
})

add_collect_usage_handler(".Internal", "base", function(e, w) {
  w$enter_global("function", ".Internal", e, w)
  if (length(e) != 2) {
    w$signal(paste("wrong number of arguments to '.Internal':",
                   paste_expr(e)),
             w)
  } else if (typeof(e[[2]]) == "language") {
    w$enter_internal(e[[2]][[1]], e[[2]], w)
    collect_usage_args(e[[2]], w)
  } else {
    w$signal(paste("bad argument to '.Internal':", paste_expr(e[[2]])), w)
  }
})

add_collect_usage_handler("substitute", "base", function(e, w) {
  w$enter_global("function", "substitute", e, w)
  if (length(e) > 3) {
    w$signal("wrong number of arguments to 'substitute'", w)
  }
  if (length(e) == 3) {
    a <- e[[3]]
    if (!missing(a))
      walk_code(a, w)
  }
})

add_collect_usage_handler("bquote", "base", function(e, w) {
  w$enter_global("function", "bquote", e, w)
  if (!any_dots(e)) {
    e <- tryCatch(
      match.call(base::bquote, e),
      error = function(e)
        NULL
    )
    if (!is.null(e) && length(e) >= 2) {
      ## check .() and ..() arguments in -expr`, but only if
      ## 'where' is not supplied
      if (!"where" %in% names(e)) {
        bqchk <- function(e) {
          if (is.call(e)) {
            ## really should only allow for ..() is 'splice =
            ## TRUE' is given, but that is awkward to check
            if (is.name(e[[1L]]) && length(e) == 2 &&
                as.character(e[[1]]) %in% c(".", "..")) {
              walk_code(e[[2]], w)
            } else {
              lapply(e, bqchk)
            }
          }
        }
        bqchk(e[[2]])
      }

      ## check usage in any additional arguments
      for (a in as.list(e)[-(1:2)]) {
        walk_code(a, w)
      }
    }
  }
})
add_collect_usage_handler("library", "base", function(e, w) {
  w$enter_global("function", "library", e, w)
  if (length(e) > 2) {
    for (a in drop_missings(e[-(1:2)]))
      walk_code(a, w)
  }
})
add_collect_usage_handler("require", "base", function(e, w) {
  w$enter_global("function", "require", e, w)
  if (length(e) > 2) {
    for (a in drop_missings(e[-(1:2)]))
      walk_code(a, w)
  }
})
add_collect_usage_handler("data", "utils", function(e, w) {
  w$enter_global("function", "data", e, w)
})

add_collect_usage_handler("detach", "base", function(e, w) {
  w$enter_global("function", "detach", e, w)
  if (length(e) > 2) {
    for (a in drop_missings(e[-(1:2)]))
      walk_code(a, w)
  }
})
add_collect_usage_handler("binomial", "stats",
                          mk_link_handler("binomial",
                                          c(
                                            "logit", "probit", "cloglog",
                                            "cauchit", "log"
                                          )))
add_collect_usage_handler("gaussian", "stats",
                          mk_link_handler("gaussian",
                                          c("inverse", "log", "identity")))
add_collect_usage_handler("Gamma", "stats",
                          mk_link_handler("Gamma",
                                          c("inverse", "log", "identity")))
add_collect_usage_handler("poisson", "stats",
                          mk_link_handler("poisson",
                                          c("log", "identity", "sqrt")))
add_collect_usage_handler("quasibinomial",
                          "stats",
                          mk_link_handler(
                            "quasibinomial",
                            c("logit", "probit", "cloglog",
                              "cauchit", "log")
                          ))
add_collect_usage_handler("quasipoisson", "stats",
                          mk_link_handler("quasipoisson",
                                          c("log", "identity", "sqrt")))
add_collect_usage_handler("quasi", "stats", function(e, w) {
  w$enter_global("function", "quasi", e, w)
  # **** don't look at arguments for now.  Need to use match.call
  # **** to get this right and trap errors. Later ...
})

add_collect_usage_handler("if", "base", function(e, w) {
  w$enter_global("function", "if", e, w)
  test <- constant_fold_env(e[[2]], w$env)
  if (is.logical(test) && length(test) == 1 && !is.na(test)) {
    walk_code(e[[2]], w)
    if (test) {
      walk_code(e[[3]], w)
    } else if (length(e) > 3)
      walk_code(e[[4]], w)
  } else {
    collect_usage_args(e, w)
  }
})

#' find_globals
#'
#' @name  find_globals
#' @rdname find_globals
#' @export
find_globals <- function(fun, merge = TRUE) {
  vars <- new.env(hash = TRUE, parent = emptyenv())
  funs <- new.env(hash = TRUE, parent = emptyenv())
  enter <- function(type, v, e, w) {
    if (type == "function") {
      assign(v, TRUE, funs)
    } else {
      assign(v, TRUE, vars)
    }
  }
  collect_usage(fun, enter_global = enter)
  fnames <- ls(funs, all.names = TRUE)
  vnames <- ls(vars, all.names = TRUE)
  if (merge) {
    sort(unique(c(vnames, fnames)))
  } else {
    list(functions = fnames, variables = vnames)
  }
}

