#' utils_print
#'
#'
#' @name utils_print
#' @rdname utils_print
#' @keywords internal
NULL

#' @rdname  utils_print
#' @export
cat_missing <- function(var_name, value_name) {
  cat_stop("Argument '{var_name}' is missing. Supply '{value_name}'.")
}

#' @rdname  utils_print
#' @export
cat_stop <- function(...,
                     .sep = "",
                     .envir = parent.frame(),
                     trace = FALSE) {
  if (trace) {
    w <- rlang::trace_back(globalenv())
    print(w, simplify = "none")
  }
  cat_prompt(
    ...,
    .sep = .sep,
    .envir = .envir,
    type = "red",
    .msg_fn = rlang::abort
  )
}

#' @rdname  utils_print
#' @export
cat_warn <- function(...,
                     .sep = "",
                     .envir = parent.frame()) {
  cat_prompt(
    ...,
    .sep = .sep,
    .envir = .envir,
    type = "yellow",
    .msg_fn = rlang::warn
  )
}
#' @keywords internal
cat_class <- function(text) {
  cat(
    cat_cumstyle(
      text = "< ",
      style = "bold",
      color = "silver"
    ),
    cat_cumstyle(
      text = text,
      style = "bold",
      color = "blue"
    ),
    cat_cumstyle(
      text = " >",
      style = "bold",
      color = "silver"
    ),
    "\n",
    sep = ""
  )
}
#' @rdname  utils_print
#' @export
cat_message <- function(...,
                        .sep = "",
                        type = c("success", "info", "go"),
                        .envir = parent.frame()) {
  type <- match.arg(type)
  if (type == "success") {
    type <- "green"
  } else if (type == "info") {
    type <- "grey"
  } else if (type == "go") {
    type <- "black"
  }

  cat_prompt(
    ...,
    .sep = .sep,
    .envir = .envir,
    type = type,
    .msg_fn = rlang::inform
  )
}

#' @rdname  utils_print
#' @export
cat_return_null <- function(...,
                            msg = NULL,
                            lgl_sum = c("any", "all"),
                            type = c("warning", "stop", "info"),
                            .envir = parent.frame()) {
  fns <- eval(substitute(alist(...)))
  res <- list()
  type <- match.arg(type)
  if (length(fns) > 0) {
    for (assertion in fns) {
      res_tmp <- is_true(eval(assertion, parent.frame()))
      res[[length(res) + 1]] <- res_tmp
    }
    lgl_sum <- match.arg(lgl_sum)
    fn_lgl <- switch(lgl_sum,
                     any = lgl_any,
                     all = lgl_all
    )
    res_f <- fn_lgl(.un(res), ~.x)
  } else {
    res_f <- FALSE
  }

  if (!res_f) {
    if (!is_empty(msg)) {
      if (type == "stop") {
        type <- "red"
        .msg_fn <- rlang::abort
      } else if (type == "info") {
        type <- "grey"
        .msg_fn <- rlang::inform
      } else if (type == "warning") {
        type <- "yellow"
        .msg_fn <- rlang::warn
      }

      cat_prompt(
        msg,
        .envir = .envir,
        type = type,
        .msg_fn = .msg_fn
      )
    }
    if (!identical(parent.frame(), globalenv())) {
      false_call <- rlang::expr(return())
      rlang::eval_bare(false_call, env = parent.frame())
    } else {
      return()
    }
  }
  invisible()
}

#' @rdname  utils_print
#' @export
cat_return_false <- function(...,
                             msg = NULL,
                             lgl_sum = c("any", "all"),
                             type = c("warning", "stop", "info"),
                             .envir = parent.frame()) {
  fns <- eval(substitute(alist(...)))


  res <- list()
  type <- match.arg(type)
  if (length(fns) > 0) {
    for (assertion in fns) {
      res_tmp <- is_true(eval(assertion, parent.frame()))
      res[[length(res) + 1]] <- res_tmp
    }
    lgl_sum <- match.arg(lgl_sum)
    fn_lgl <- switch(lgl_sum,
                     any = lgl_any,
                     all = lgl_all
    )
    res_f <- fn_lgl(.un(res), ~.x)
  } else {
    res_f <- FALSE
  }

  if (!res_f) {
    if (!is_empty(msg)) {
      if (type == "stop") {
        type <- "red"
        .msg_fn <- rlang::abort
      } else if (type == "info") {
        type <- "grey"
        .msg_fn <- rlang::inform
      } else if (type == "warning") {
        type <- "yellow"
        .msg_fn <- rlang::warn
      }

      cat_prompt(
        msg,
        .envir = .envir,
        type = type,
        .msg_fn = .msg_fn
      )
    }
    if (!identical(parent.frame(), globalenv())) {
      false_call <- rlang::expr(return(FALSE))
      rlang::eval_bare(false_call, env = parent.frame())
    } else {
      return(FALSE)
    }
  }
  TRUE
}

#' @rdname  utils_print
#' @export
cat_line <- function (..., file = stdout()) {
  out <- paste0(..., collapse = "\n")
  cat(out, "\n", sep = "", file = file, append = TRUE)
}

#' @rdname  utils_print
#' @export
cat_step <-
  function(mgs,
           head = "i",
           head_sep = "-",
           .envir = env_prnt(),
           width.cutoff = getOption("width", 70),
           msg_type = "grey",
           head_type = "blue") {
    l_margin <- 0
    if (!is_empty(head)) {
      cat_prompt(
        head,
        .sep = "",
        .envir = .envir,
        type = head_type,
        .msg_fn = cat
      )
      l_margin <- l_margin + nchar(head) + 1
      cat(" ")
      if (!is_empty(head_sep)) {
        cat_prompt(
          head_sep,
          .sep = "",
          .envir = .envir,
          type = msg_type,
          .msg_fn = cat
        )
        cat(" ")
        l_margin <- l_margin + nchar(head_sep) + 1
      }
    }
    if (length(mgs) > 1) {
      mgs <- paste(mgs, collapse = " ")
    }
    .ids_coll <-
      paste0(c(".", sample(c(
        letters, LETTERS, 1:0
      ), 10), "."), collapse = "")
    mgs <-
      strsplit_with(
        x = mgs,
        width.cutoff = width.cutoff,
        current_sep = " ",
        collapse = .ids_coll
      )
    mgs <-
      as.character(strsplit_vec(
        x = mgs,
        splits = .ids_coll,
        fixed = TRUE
      ))

    cat_prompt(
      mgs,
      .sep = "",
      .envir = .envir,
      type = msg_type,
      .msg_fn = function(...) {
        cat(..., sep = paste0(c(
          "\n", rep(" ", l_margin)
        ), collapse = ""))
      }
    )
    cat("\n")
  }

#' @rdname  utils_print
#' @export
cat_cumstyle <-
  function(text,
           background = NULL,
           style = NULL,
           color = NULL) {
    if (!is_empty(background)) {
      assert_vector(
        x = background,
        .class = "character",
        .len = 1,
        severity = "stop"
      )
      text <- color_prompt(
        prompt = text,
        type = background,
        obj = "background"
      )
    }

    if (!is_empty(color)) {
      assert_vector(
        x = color,
        .class = "character",
        .len = 1,
        severity = "stop"
      )
      text <- color_prompt(
        prompt = text,
        type = color,
        obj = "color"
      )
    }
    if (!is_empty(style)) {
      assert_vector(
        x = style,
        .class = "character",
        .min_len = 1,
        severity = "stop"
      )
      for (i in seq_along(style)) {
        text <- color_prompt(
          prompt = text,
          type = style[i],
          obj = "style"
        )
      }
    }


    text
  }

#' @rdname  utils_print
#' @export
cat_title_head <-
  function(text,
           .sep = "",
           line_color = "silver",
           text_color = "blue",
           .envir = parent.frame(),
           width = getOption("width", 80)) {
    title_style <- function(x, color) {
      cat_cumstyle(
        text = x,
        style = "bold",
        color = color
      )
    }
    line_style <- function(n, color) {
      if (n < 1) {
        n <- 1
      }
      x <- rep_chars(width = n, char = "-")
      x <- cat_cumstyle(
        text = x,
        style = "strikethrough",
        color = color
      )
      cat_cumstyle(
        text = x,
        style = "bold",
        color = color
      )
    }
    text <- paste(text, collapse = .sep)
    text<-as_chr(strwrap(glue::glue(text, .envir = .envir)))
    text_len <- nchar(text)
    l01 <- line_style(n = 2, color = line_color)
    text <- title_style(x = text, color = text_color)
    l02 <- line_style(n = width - text_len - 4, color = line_color)
    cat(l01, text, l02, "\n", sep = " ")
  }

#' @rdname  utils_print
#' @export
cat_endline <-
  function(line_color = "silver",
           width = getOption("width", 80)) {
    line_style <- function(n, color) {
      if (n < 1) {
        n <- 1
      }
      x <- rep_chars(width = n, char = "-")
      x <- cat_cumstyle(
        text = x,
        style = "strikethrough",
        color = color
      )
      cat_cumstyle(
        text = x,
        style = "bold",
        color = color
      )
    }
    l01 <- line_style(n = width, color = line_color)
    cat(l01, "\n")
  }

#' @rdname  utils_print
#' @export
cat_subtitle1 <-
  function(text,
           sub_text = NULL,
           .sep = "",
           color = "none",
           .envir = parent.frame(),
           width = min(c(getOption("width", 30), 30))) {
    title_style <- function(x, color) {
      cat_cumstyle(
        text = x,
        style = "bold",
        color = color
      )
    }
    line_style <- function(n, color) {
      if (n < 1) {
        n <- 1
      }
      x <- rep_chars(width = n, char = "-")
      x <- cat_cumstyle(
        text = x,
        style = "strikethrough",
        color = color
      )
      cat_cumstyle(
        text = x,
        style = "bold",
        color = color
      )
    }
    sub_style <- function(x,color){
      cat_cumstyle(text = x,style = "italic",color = color)
    }
    text <- paste(text, collapse = .sep)
    text <- as_chr(strwrap(glue::glue(text, .envir = .envir)))
    if(!is_empty(sub_text)){
      sub_text <- paste0("(",paste(sub_text, collapse = .sep),")")
      sub_text<-as_chr(strwrap(glue::glue(sub_text, .envir = .envir)))
      text_len <- nchar(sub_text) + 1
      sub_text <- sub_style(x = sub_text,color = color)
    } else{
      sub_text <- ""
      text_len <- 1
    }
    text_len <- nchar(text) + text_len
    l01 <- line_style(n = 2, color = color)
    text <- title_style(x = text, color = color)
    l02 <-
      line_style(n = max(c(width - text_len - 4, 4)), color = color)
    cat(l01, text,sub_text, l02, "\n", sep = " ")
  }

#' @rdname  utils_print
#' @export
cat_subtitle2 <-
  function(text,
           sub_text=NULL,
           .sep = "",
           line_color = "none",
           text_color = "none",
           subtext_color = "silver",
           .envir = parent.frame(),
           width = getOption("width", 80)) {
    title_style <- function(x, color) {
      cat_cumstyle(
        text = x,
        color = color
      )
    }
    sub_style <- function(x,color){
      cat_cumstyle(text = x,style = "italic",color = color)
    }
    line_style <- function(n, color) {
      x <- rep_chars(width = n, char = "-")
      cat_cumstyle(
        text = x,
        color = color
      )

    }


    text <- paste(text, collapse = .sep)
    text<-as_chr(strwrap(glue::glue(text, .envir = .envir)))

    if(!is_empty(sub_text)){
      sub_text <- paste0("(",paste(sub_text, collapse = .sep),")")
      sub_text<-as_chr(strwrap(glue::glue(sub_text, .envir = .envir)))
      sub_text <- sub_style(x = sub_text,color = subtext_color)
    } else{
      sub_text <- ""
    }
    l01 <- line_style(n = 2, color = line_color)
    text <- title_style(x = text, color = text_color)
    cat(l01, text,sub_text, "\n", sep = " ")
  }
#' @rdname  utils_print
#' @export
color_prompt <- function(prompt, type, obj = "color") {
  colors <- list(
    background = list(
      black = function(...) {
        as_chr(cli::bg_black(...))
      },
      blue = function(...) {
        as_chr(cli::bg_blue(...))
      },
      cyan = function(...) {
        as_chr(cli::bg_cyan(...))
      },
      green = function(...) {
        as_chr(cli::bg_green(...))
      },
      magenta = function(...) {
        as_chr(cli::bg_magenta(...))
      },
      red = function(...) {
        as_chr(cli::bg_red(...))
      },
      white = function(...) {
        as_chr(cli::bg_white(...))
      },
      yellow = function(...) {
        as_chr(cli::bg_yellow(...))
      },
      none = function(...) {
        as_chr(cli::bg_none(...))
      }
    ),
    color = list(
      black = function(...) {
        as_chr(cli::col_black(...))
      },
      blue = function(...) {
        as_chr(cli::col_blue(...))
      },
      cyan = function(...) {
        as_chr(cli::col_cyan(...))
      },
      green = function(...) {
        as_chr(cli::col_green(...))
      },
      magenta = function(...) {
        as_chr(cli::col_magenta(...))
      },
      red = function(...) {
        as_chr(cli::col_red(...))
      },
      white = function(...) {
        as_chr(cli::col_white(...))
      },
      yellow = function(...) {
        as_chr(cli::col_yellow(...))
      },
      grey = function(...) {
        as_chr(cli::col_grey(...))
      },
      silver = function(...) {
        as_chr(cli::col_silver(...))
      },
      none = function(...) {
        as_chr(cli::col_none(...))
      }
    ),
    style = list(
      dim = function(...) {
        as_chr(cli::style_dim(...))
      },
      blurred = function(...) {
        as_chr(cli::style_blurred(...))
      },
      bold = function(...) {
        as_chr(cli::style_bold(...))
      },
      hidden = function(...) {
        as_chr(cli::style_hidden(...))
      },
      inverse = function(...) {
        as_chr(cli::style_inverse(...))
      },
      italic = function(...) {
        as_chr(cli::style_italic(...))
      },
      strikethrough = function(...) {
        as_chr(cli::style_strikethrough(...))
      },
      underline = function(...) {
        as_chr(cli::style_underline(...))
      }
    )
  )
  prompt_fn <- colors[[obj]][[type]]
  prompt_fn(prompt)
}

#' @rdname  utils_print
#' @export
cat_prompt <-
  function(...,
           .sep = "",
           .envir = parent.frame(),
           type,
           .msg_fn) {
    glue::glue(paste(..., collapse = .sep), .envir = .envir) %>%
      strwrap() %>%
      color_prompt(type, obj = "color") %>%
      .msg_fn()
  }


#' @rdname utils_print
#' @export
generic_print <-
  function(x, .add_print = NULL) {
    extract_fnsclass <- function(x) {
      gen <-
        c(
          "matrix",
          "array",
          "logical",
          "integer",
          "numeric",
          "double",
          "complex",
          "character",
          "raw",
          "list",
          "expression",
          "call",
          "tbl_df",
          "tbl",
          "data.frame"
        )

      x <- class(x)[!class(x) %in% gen]
      stringr::str_to_sentence(commas(x))
    }

    if (is_true(!is.function(.add_print))) {
      .add_print <- function(x) {
        return(" ")
      }
    }

    .out <- attr(x, "outcome")
    .in <- attr(x, "predictor")
    .tar <- attr(x, "target")
    .trans <- attr(x, "transformed")
    .del <- attr(x, "removed")
    .inter <- attr(x, "interaction")

    cat_title_head(extract_fnsclass(x))

    toprint <-
      list(
        Outcome = list(role = "Outcome", list = .out),
        Target = list(role = "Target", list = .tar),
        Predictor = list(role = "Predictor", list = .in),
        Transformed = list(role = "Transformed", list = .trans),
        Removed = list(role = "Removed", list = .del),
        Interaction = list(role = "Interaction", list = .inter)
      )


    generic_print_impl <- function(x) {
      if (!is_emptyna(x$list)) {
        cat_subtitle1(text = x$role)
        bllt <- purrr::set_names(x$list, rep(">", length(x$list)))
        cli::cli_bullets(bllt)
      }
    }
    purrr::walk(toprint, generic_print_impl)

    cat(cli::col_grey(.add_print(x)))

    cat("\n")
    cat_title_head(text = "MGG")
  }

#' @rdname utils_print
#' @export
subgeneric_print <-
  function(x, nm) {
    extract_fnsclass <- function(x) {
      gen <-
        c(
          "matrix",
          "array",
          "logical",
          "integer",
          "numeric",
          "double",
          "complex",
          "character",
          "raw",
          "list",
          "expression",
          "call",
          "tbl_df",
          "tbl",
          "data.frame"
        )

      x <- class(x)[!class(x) %in% gen]
      stringr::str_to_sentence(commas(x))
    }

    .out <- attr(x, "outcome")
    .in <- attr(x, "predictor")
    .tar <- attr(x, "target")
    .trans <- attr(x, "transformed")
    .del <- attr(x, "removed")
    .inter <- attr(x, "interaction")
    cat_subtitle1(text = nm)

    toprint <-
      list(
        Outcome = list(role = "Outcome", list = .out),
        Target = list(role = "Target", list = .tar),
        Predictor = list(role = "Predictor", list = .in),
        Transformed = list(role = "Transformed", list = .trans),
        Removed = list(role = "Removed", list = .del),
        Interaction = list(role = "Interaction", list = .inter)
      )


    generic_print_impl <- function(x) {
      if (!is_emptyna(x$list)) {
        cat_subtitle2(text = x$role)
        bllt <- purrr::set_names(x$list, rep(" ", length(x$list)))
        cli::cli_bullets(bllt)
      }
    }
    purrr::walk(toprint, generic_print_impl)
  }

#' @keywords internal
collapse_interaccion <- function(data, vars = NULL) {
  if (!is_emptyna(vars)) {
    data <- data[, vars]
  }
  w <- sort(sapply(data, class))

  paste0(paste0(names(w), " (", w, ")"), collapse = ", ")
}

#' @rdname utils_print
#' @export
generic_default <-
  function(call = sys.call(sys.parent()),
           envir = parent.frame(2L)) {
    fncall <- deparse(call[[1]])
    fncall <-
      gsub(
        pattern = ".default",
        replacement = "",
        x = fncall,
        fixed = T
      )
    objcall <- as_l(call)[[2]]
    envirls <- as_l(envir)
    classobj <- class(envirls[[deparse(objcall)]])
    classobj <- paste0(classobj, collapse = ",")

    cat_warn(
      "No method for {fncall} is defined",
      "for '{objcall}' class {classobj}."
    )
    invisible(NULL)
  }


#' @rdname utils_print
#' @export
osize <- function(x, ...) {
  osize_object_size(sizes = object.size(x), ...)
}

#' @keywords internal
osize_numeric <- function(sizes,
                          digits = 1L,
                          units = "auto",
                          standard = "IEC",
                          bytes = "B",
                          ...) {
  .stop_if_not <- function(...) {
    res <- list(...)
    n <- length(res)
    if (n == 0L) {
      return()
    }

    for (ii in 1L:n) {
      res_ii <- .subset2(res, ii)
      if (length(res_ii) != 1L || is.na(res_ii) || !res_ii) {
        mc <- match.call()
        call <- deparse(mc[[ii + 1]], width.cutoff = 60L)
        if (length(call) > 1L) {
          call <- paste(call[1L], "...")
        }
        cat_stop(sQuote(call), " is not TRUE")
      }
    }
  }
  standard <- match.arg(standard, choices = c("IEC", "JEDEC", "SI"))
  .stop_if_not(is.character(units), length(units) == 1L)
  .stop_if_not(is.numeric(digits), length(digits) == 1L)
  .stop_if_not(is.character(bytes), length(bytes) == 1L)
  nsizes <- length(sizes)

  kunits <- list(
    IEC = c(
      bytes = 0,
      B = 0,
      KiB = 1,
      MiB = 2,
      GiB = 3,
      TiB = 4,
      PiB = 5,
      EiB = 6,
      ZiB = 7,
      YiB = 8
    ),
    JEDEC = c(
      bytes = 0,
      B = 0,
      KB = 1,
      MB = 2,
      GB = 3
    ),
    SI = c(
      bytes = 0,
      B = 0,
      kB = 1,
      MB = 2,
      GB = 3,
      TB = 4,
      PB = 5,
      EB = 6,
      ZB = 7,
      YB = 8
    )
  )

  ## Infer standard from unit?
  if (units != "auto") {
    idx <- which(sapply(
      kunits,
      FUN = function(x) {
        any(units == names(x))
      }
    ))
    if (length(idx) == 0L) {
      cat_stop(sprintf("Unknown units: %s", sQuote(units)))
    }
    standard <- names(idx[1])
  }
  kunits <- kunits[[standard]]
  base <- switch(standard,
                 IEC = 1024,
                 JEDEC = 1024,
                 SI = 1000
  )

  if (units == "auto") {
    ## Keep the "bytes" alternative specified
    excl <- setdiff(c("bytes", "B"), bytes)
    kunits <- kunits[-which(names(kunits) == excl)]

    exps <- log(sizes, base = base)
    exps <- floor(exps)
    exps[exps < 0] <- 0
    maxexp <- max(kunits)
    exps[exps > maxexp] <- maxexp
    units <- names(kunits)[exps + 1L]
    positions <- rep(digits, length.out = nsizes)
    positions[exps == 0] <- 0L
  } else {
    exps <- kunits[units]
    if (is.na(exps)) {
      cat_stop(sprintf(
        "Unknown units for standard %s: %s",
        sQuote(standard),
        sQuote(units)
      ))
    }
    units <- rep(units, times = nsizes)
  }

  ## Use '1 byte' (not '1 bytes')
  ones <- which(sizes == 1)
  if (length(ones) > 0) {
    units[ones] <- gsub("s$", "", units[ones])
  }

  sizes <- round(sizes / base^exps, digits = digits)
  positions <- rep(digits, length.out = nsizes)
  positions[exps == 0] <- 0L
  print(sprintf("%.*f %s", positions, sizes, units))
  invisible(sizes)
}

#' @keywords internal
osize_object_size <- function(sizes, ...) {
  osize_numeric(as_num(sizes), ...)
}

#' @rdname utils_print
#' @export
recipes_boilerplate <-
  function(name,
           which = c("step", "check"),
           .o = character(0),
           .init = character(0)) {
    .o <- unique(c(.o, "object"))
    .init <- unique(c(.init, "NULL"))

    create_documentation <- function(name, which) {
      glue::glue(
        "
# #' {name}
# #'
# #' @rdname custom_step
# #' @keywords datagen
# #' @concept preprocessing
# #' @export
  "
      )
    }
    .add_01 <-
      paste0(paste("             ", .o, " = ", .init, ",", sep = ""), collapse = "\n")
    .add_02 <-
      paste0(paste("          ", .o, " = ", .o, ",", sep = ""), collapse = "\n")
    .add_03 <- paste0(.o, collapse = ",")
    .add_04 <-
      paste0(paste("    ", .o, " = ", ifelse(
        .o == "object", .o, paste("x$", .o, sep = "")
      ), ",", sep = ""), collapse = "\n")

    create_function <- function(name, which) {
      glue::glue(
        '
{which}_{name} <-
    function(recipe,
             ...,
             role = NA,
             trained = FALSE,
{.add_01}
             skip = FALSE,
             id = rand_id("{name}")) {{
      add_{which}(
        recipe,
        {which}_{name}_new(
          terms = ellipse_check(...),
          trained = trained,
          role = role,
{.add_02}
          skip = skip,
          id = id
        )
      )
    }}

'
      )
    }
    create_generator <- function(name, which) {
      glue::glue(
        '
  {which}_{name}_new <-
    function(terms, role, trained, {.add_03}, na_rm, skip, id) {{
     recipes::step(
        subclass = "{name}",
        terms = terms,
        role = role,
        trained = trained,
{.add_02}
        skip = skip,
        id = id
      )
    }}

  '
      )
    }
    create_prep_method <- function(name, which) {
      glue::glue(
        "
prep.{which}_{name} <- function(x, training, info = NULL, ...) {{
    col_names <-
    recipes_eval_select(quos = x$terms,
                                 data = training,
                                 info = info)
  check_type(training[, col_names])

  <prepping action here>

  {which}_{name}_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
{.add_04}
    skip = x$skip,
    id = x$id
  )
}}

"
      )
    }
    create_bake_method <- function(name, which) {
      glue::glue(
        "
bake.{which}_{name} <- function(object, new_data, ...) {{
  as_tbl(predict(object$object, new_data))
}}

"
      )
    }
    create_print_method <- function(name, which) {
      glue::glue(
        '
print.{which}_{name} <-
  function(x, width = max(20, options()$width - 30), ...) {{
    cat("<{name}> ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }}

'
      )
    }
    create_tidy_method <- function(name, which) {
      glue::glue(
        "
# #' @param x A `{which}_{name}` object.
# #' @export
tidy.{which}_{name} <- function(x, ...) {{
  term_names <- sel2char(x$terms)
  if (recipes::is_trained(x)) {{
    res <- tibble(terms = term_names,value = lengths(x$object))
  }} else {{

    res <- tibble(terms = term_names,
                  value = na_dbl)
  }}
  res$id <- x$id
  res
}}
  "
      )
    }

    which <- match.arg(which)
    stopifnot(is.character(name))

    boilerplate <-
      glue::glue(
        "
{create_documentation(name, which)}
{create_function(name, which)}
{create_generator(name, which)}
{create_prep_method(name, which)}
{create_bake_method(name, which)}
{create_print_method(name, which)}
{create_tidy_method(name, which)}
    "
      )

    boilerplate <-
      gsub(
        pattern = "# #'",
        replacement = "#'",
        x = boilerplate
      )
    dput_source(cat(boilerplate, sep = "\n"))
  }


#' @rdname utils_print
#' @export
paste_test_pgk <- function(x) {
  if (length(x) > 1) {
    return(walk(x, paste_test_pgk))
  }

  .exprs <- compact(parse_all(x = readLines(x))$expr)

  .exprs <-
    .exprs[map_lgl(map(as_l(.exprs), ~ .x[[1]]), ~ is_true(.x[[1]] == quote(test_that)))]

  .exprs <- map(cmp_ftn_map(.exprs, ~ as.list(.x)), ~ as.list(.x)[[3]])

  .exprs <- map_if(
    .x = .exprs,
    .p = ~ is_true(as.list(.x)[[1]] == quote(`{`)),
    .f = ~ as.list(.x)[-1],
    .else = .x
  )

  .exprs <- map(.exprs, function(w) {
    w_bool <- map_lgl(w, ~ is_true(substr(x = lang2str(.x), 1, 7) == "expect_"))
    w[w_bool] <- map(w[w_bool], ~ as.list(.x)[[2]])
    w
  })
  dput_source(cat(deparse1collapse(.exprs)))
}
#' @rdname utils_print
#' @export
paste_examples_pgk <- function(x) {
  .eval_examples <- function(file) {
    roxi_file <- parse_file_roxygen2(file = file)
    roxi_file <- lapply(roxi_file, "[[", "tags")
    roxi_file <- compact(lapply(roxi_file, function(x) {
      w <- lapply(x, nullclass)
      w <- compact(map(w, function(z) {
        z <- z[z$tag == "example" | z$tag == "examples"]
        z <- z$raw
        z
      }))
      unlist(w)
    }))
    map_chr(roxi_file, ~ paste(.x, collapse = "\n"))
  }
  .writetemp <- function(x) {
    w <- c("\n")
    if (is_empty(x)) {
      return("")
    }
    if (!is_empty(x)) {
      w <- c(w, "\n", x, "\n\n")
      w <- c(w, "rm(list = ls())\ngc()\n")
    }
    c(w, "\n\n")
  }
  if (length(x) > 1) {
    return(walk(x, paste_examples_pgk))
  }
  x <- .eval_examples(file = x)
  x <- .writetemp(x)
  x <- gsub(
    pattern = c("\\dontrun{"),
    replacement = "{",
    x = x,
    fixed = T
  )
  x <- x[x != ""]
  rstudioapi::insertText(glue::as_glue(x))
}


tokenize_file <- function(file, srcref_path = NULL, only_block = TRUE) {
  tokenise_block <-
    utils::getFromNamespace(x = "tokenise_block", ns = "roxygen2")
  block_create <-
    utils::getFromNamespace(x = "block_create", ns = "roxygen2")
  comments <- function(refs) {
    srcfile <- attr(refs[[1]], "srcfile")
    com <- vector("list", length(refs))
    for (i in seq_along(refs)) {
      if (i == 1) {
        first_byte <- 1
        first_line <- 1
      } else {
        first_byte <- refs[[i - 1]][4] + 1
        first_line <- refs[[i - 1]][3]
      }
      last_line <- refs[[i]][3]
      last_byte <- refs[[i]][4]
      lloc <- c(first_line, first_byte, last_line, last_byte)
      com[[i]] <- srcref(srcfile, lloc)
    }
    com
  }
  tokenise_ref <- function(x) {
    tokenise_block(as.character(x),
                   file = attr(x, "srcfile")$filename,
                   offset = x[[1]]
    )
  }
  lines <- base::readLines(file,
                           n = -1,
                           encoding = "UTF-8",
                           warn = FALSE
  )

  parsed <-
    parse(
      text = lines,
      keep.source = TRUE,
      srcfile = srcfilecopy(srcref_path %||%
                              file, lines, isFile = TRUE)
    )
  if (length(parsed) == 0) {
    return(list())
  }
  refs <- utils::getSrcref(parsed)
  comment_refs <- comments(refs)
  tokens <- lapply(comment_refs, tokenise_ref)
  has_tokens <- !purrr::map_lgl(tokens, purrr::is_empty)
  blocks <- purrr::pmap(
    list(
      call = as.list(parsed)[has_tokens],
      srcref = refs[has_tokens],
      tokens = tokens[has_tokens]
    ),
    block_create
  )
  purrr::compact(blocks)
}
parse_file_roxygen2 <-
  function(file,
           env = env_file(file),
           srcref_path = NULL) {
    block_set_env <- getFromNamespace("block_set_env", "roxygen2")
    order_blocks <- getFromNamespace("order_blocks", "roxygen2")
    blocks <- tokenize_file(file, srcref_path = srcref_path)
    if (!is.null(env)) {
      blocks <- do_try(lapply(blocks, block_set_env, env = env), blocks)
    }
    blocks <- order_blocks(blocks)
    blocks
  }
deparse1collapse <- function(x) {
  x <- map(x, ~ c(.x, c("")))
  x <- flatten(x)

  chr_cllp(
    .x = x,
    .f = ~ if (.x == "") {
      c("\n\n")
    } else {
      paste0(deparse1(.x, collapse = "\n"), "\n", collapse = "\n")
    },
    collapse = "\n"
  )
}
.sys.source <- function(file,
                        envir = baseenv(),
                        chdir = FALSE,
                        keep.source = getOption("keep.source.pkgs"),
                        keep.parse.data = getOption("keep.parse.data.pkgs"),
                        toplevel.env = as.environment(envir)) {
  if (!(is.character(file) && file.exists(file))) {
    stop(gettextf("'%s' is not an existing file", file))
  }
  keep.source <- as_logical(keep.source)
  keep.parse.data <- as_logical(keep.parse.data)
  oop <- options(
    keep.source = keep.source,
    keep.parse.data = keep.parse.data,
    topLevelEnvironment = toplevel.env
  )
  on.exit(options(oop))
  if (keep.source) {
    lines <- readLines(file, warn = FALSE)
    srcfile <- srcfilecopy(file, lines, file.mtime(file),
                           isFile = TRUE)
    exprs <-
      parse(text = lines,
            srcfile = srcfile,
            keep.source = TRUE)
  } else {
    exprs <- parse(
      n = -1,
      file = file,
      srcfile = NULL,
      keep.source = FALSE
    )
  }
  if (length(exprs) == 0L) {
    return(invisible())
  }
  if (chdir && (path <- dirname(file)) != ".") {
    owd <- getwd()
    if (is.null(owd)) {
      stop("cannot 'chdir' as current directory is unknown")
    }
    on.exit(setwd(owd), add = TRUE)
    setwd(path)
  }
  for (i in seq_along(exprs)) {
    eval(exprs[i], envir)
  }
  invisible()
}
