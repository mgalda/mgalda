#' @include util_env.R
NULL

#' herramientas
#'
#'
#' @name string_manipulation
#' @rdname string_manipulation
#' @keywords internal
NULL


#' @export
make_names <- function(names, unique = FALSE, leading_ = "") {
  if (!substr(leading_, 1, 1) %in% c("", ".", letters, LETTERS)) {
    stop("'leading_' must be a lower- or uppercase letter or '.'")
  }
  names <- sub("^[^A-Za-z\\.]+", ".", names)
  names <- make.names(names, allow_ = TRUE)
  names <- gsub("\\.", "_", names)
  names <- gsub("\\_+", "_", names)
  if (unique) {
    names <- make.unique(names, sep = "_")
  }
  gsub("^_", leading_, names)
}

#' @export
padstr <- function(x, pad = " ", width = max(nchar(x)), side = c("left", "right")) {
  side <- match.arg(side)
  nchars <- nchar(x)
  nchars <- width - nchars
  nchars <- ifelse(nchars < 0, 0, nchars)
  pads <-
    map_chr(.x = nchars, ~ paste0(rep(pad, .x), collapse = ""))
  switch(side,
         left = paste(pads, x, sep = ""),
         right = paste(x, pads, sep = "")
  )
}
#' @export
grepl_vec <-
  function(patterns,
           x,
           ignore.case = FALSE,
           fixed = TRUE,
           pattern_mode = c("any", "all", "identity"),
           use_names = FALSE) {
    pattern_mode <- match.arg(pattern_mode)
    pattern_fn <- match.fun(pattern_mode)

    out <- map(
      .x = patterns,
      .f = ~ grepl(
        pattern = .x,
        x = x,
        ignore.case = ignore.case,
        fixed = fixed
      )
    )


    out <- do.call(cbind, out)

    if (pattern_mode == "identity") {
      rownames(out) <- x
      colnames(out) <- patterns
      out <- as_df(out)
    } else {
      out <- apply(out, 1, pattern_fn)
    }

    out
  }
#' @export
gsub_vec <-
  function(patterns,
           replacements = "",
           x,
           ignore.case = FALSE,
           fixed = FALSE) {
    if (length(replacements) == 1) {
      replacements <- rep(replacements, length(patterns))
    }
    if (length(replacements) != length(patterns)) {
      cat_stop(" 'replacements' and 'patterns' must have the same length")
    }
    for (i in seq_along(patterns)) {
      x <- gsub(
        pattern = patterns[i],
        replacement = replacements[i],
        x = x,
        ignore.case = ignore.case,
        fixed = fixed
      )
    }
    x
  }
#' @export
strsplit_vec <- function(x,
                         splits,
                         fixed = TRUE,
                         perl = FALSE,
                         useBytes = FALSE) {
  spat <- c(letters, LETTERS, paste(0:9))
  spat <-
    paste0("r", sample(spat, 10, replace = TRUE), collapse = "")
  x <- gsub_vec(
    patterns = splits,
    replacements = spat,
    x = x,
    fixed = fixed
  )
  x <- strsplit(
    x,
    split = spat,
    fixed = fixed,
    perl = perl,
    useBytes = useBytes
  )
  nn <- max(lengths(x))
  x <- map(x, ~ matrix(c(.x, rep(
    "", nn - length(.x)
  )),
  nrow = 1, ncol = nn
  ))
  do.call(rbind, x)
}
#' @export
grep_vec <-
  function(patterns,
           x,
           ignore.case = FALSE,
           value = FALSE,
           fixed = FALSE,
           pattern_mode = c("any", "all", "identity")) {
    w <-
      grepl_vec(
        patterns = patterns,
        x = x,
        ignore.case = ignore.case,
        fixed = fixed,
        pattern_mode = pattern_mode
      )

    if (pattern_mode == "identity") {
      w <- map(as.list(w), .f = which)
      if (value) {
        w <- map(w, ~ x[.x])
      }
    } else {
      w <- which(w)
      if (value) {
        w <- x[w]
      }
    }
    w
  }

#' @export
enbacktick <- function(x) {
  paste0("`", x, "`")
}
#' @export
strsplit_with <-
  function(x,
           width.cutoff = 60,
           current_sep = " ",
           collapse = "\n") {
    .strsplit_with <- function(x, current_sep, width.cutoff, collapse) {
      x_spl <- strsplit(
        x = x,
        split = current_sep,
        fixed = T
      )[[1]]
      x_nchar <- nchar(x_spl) + 1

      v <- 0
      g <- 1
      g_cum <- numeric(0)
      for (i in seq_along(x_nchar)) {
        if (v + x_nchar[i] > width.cutoff) {
          v <- x_nchar[i]
          g <- g + 1
        } else {
          v <- v + x_nchar[i]
        }
        g_cum <- c(g_cum, g)
      }

      x_spl <-
        map_chr(split(x_spl, g_cum), ~ paste(.x, collapse = current_sep))

      paste(x_spl, collapse = collapse)
    }
    map_chr(
      .x = x,
      .f = ~ .strsplit_with(
        x = .x,
        current_sep = current_sep,
        width.cutoff = width.cutoff,
        collapse = collapse
      )
    )
  }
#' @export
strjustify <- function(x, justify = c("left", "right", "centre")) {
  empty_str <- function(x) {
    ifelse(x == "", '""', x)
  }
  justify <- match.arg(justify)
  format(empty_str(x), justify = justify)
}
#' @export
strip_style <- function(x) {
  ansi_regex <-
    "(?:(?:\\x{001b}\\[)|\\x{009b})(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])|\\x{001b}[A-M]"
  nullclass(gsub(ansi_regex, "", x, perl = TRUE))
}
#' @export
torandomize <- function(x) {
  d <- strsplit(x, split = "")[[1]]

  ids <- sample(
    x = 1:0,
    size = length(d),
    replace = T
  )

  d[ids == 1] <- tolower(d[ids == 1])
  d[ids == 0] <- toupper(d[ids == 0])

  paste0(d, collapse = "")
}
#' @export
totitle <- function(x) {
  if (!is.character(x)) {
    stop(deparse(substitute(x)), " is not character")
  }
  gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", x, perl = TRUE)
}
#' @export
tosnakecase <- function(x) {
  .sep <- "[^\\w\\s\\.\\-]"
  if (!is.character(x)) {
    cat_stop(as.character(sys.call())[-1], " is not character")
  }
  x <- gsub("[^\\w\\s\\.\\-]", "", x, perl = TRUE)
  x <- tolower(x)
  x <- gsub(.sep, "_", x, perl = TRUE)
  x <- gsub("[\\W\\s\\.\\-]", "_", x, perl = TRUE)
  x <- gsub("__+", "_", x, perl = TRUE)
  x <- gsub("^_+", "", x, perl = TRUE)
  x <- gsub("_+$", "", x, perl = TRUE)
  x
}

#' @export
substr_right <- function(x, n) {
  x <- as.character(x)
  substr(x, nchar(x) - n + 1, nchar(x))
}

#' @export
collapse <- function(x, collapse = "") paste(x, collapse = collapse)
#' @export
commas <- function(...) {
  paste0(..., collapse = ", ")
}

#' @export
gsub_whitespace <- function(x) {
  if (!is.atomic(x)) {
    cat_stop("x must be an atomic vector")
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }

  gsub("\\s", '""', x, perl = TRUE)
}
#' @export
gsub_separators <- function(x) {
  if (!is.atomic(x)) {
    cat_stop("x must be an atomic vector")
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }

  gsub("[\\s_\\.-]", '""', x, perl = TRUE)
}
#' @export
gsub_nonword <- function(x) {
  if (!is.atomic(x)) {
    cat_stop("x must be an atomic vector")
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }

  gsub("\\W", '""', x, perl = TRUE)
}
#' @export
gsub_leading_nonword <- function(x) {
  if (!is.atomic(x)) {
    cat_stop("x must be an atomic vector")
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }

  gsub("^\\W", '""', x, perl = TRUE)
}
#' @export
gsub_space <- function(x) {
  if (!is.atomic(x)) {
    cat_stop("x must be an atomic vector")
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }

  gsub(" ", '""', x, perl = TRUE)
}
#' @export
gsub_expand_capwords <- function(x) {
  if (!is.atomic(x)) {
    cat_stop("x must be an atomic vector")
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }

  gsub("([a-z])([A-Z])", "\\1 \\2", x, perl = TRUE)
}


make_str_replace <- function(pattern, replacement) {
  function(x) {
    if (!is.atomic(x)) {
      cat_stop("x must be an atomic vector")
    }
    if (!is.character(x)) {
      x <- as.character(x)
    }

    gsub(pattern, replacement, x, perl = TRUE)
  }
}
make_str_delete <- function(pattern) {
  function(x) {
    if (!is.atomic(x)) {
      cat_stop("x must be an atomic vector")
    }
    if (!is.character(x)) {
      x <- as.character(x)
    }

    gsub(pattern, "", x, perl = TRUE)
  }
}
