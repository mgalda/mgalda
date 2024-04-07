# !diagnostics off
# !diagnostics suppress=y


#' data_summary
#'
#'
#' @name data_summary
#' @rdname data_summary
#' @examples
#'
#' data <- mgalda::datalearn$credit
#' data_summary(data)
#' @export
data_summary <- function(data) {
  short_name <- function(x, max_char = 10) {
    xs <-
      ifelse(stringr::str_length(x) > max_char + 3,
             stringr::str_sub(x, 1, max_char),
             x
      )

    xp <-
      sapply(seq_along(xs), function(w) {
        if (sum(xs[1:w] == xs[w]) > 1) {
          paste0(xs[w], sum(xs[1:w] == xs[w]))
        } else {
          xs[w]
        }
      })

    ifelse(xs == x, x, paste0(xp, "..."))
  }

  fn_data_summary <- function(x) {
    fnlist <- function(..., var_type = "") {
      stopifnot(length(var_type) == 1, is.character(var_type))
      build_names <- function(...) {
        labels <- rlang::quos_auto_name(rlang::enquos(...))
        rlang::set_names(rlang::list2(...), names(labels))
      }

      funs <- build_names(...)
      structure(list(funs = funs, var_type = var_type),
                class = "fnlist")
    }
    fnlist_num <- fnlist(
      var_type = "numeric",
      complete_rate = function(x) {
        paste0(perc_format(complete_rate(x)), " (", n_missing(x), ")")
      },
      mean = function(x) {
        round_sign(mean(x, na.rm = TRUE))
      },
      sd = function(x) {
        round_sign(sd(x, na.rm = TRUE))
      },
      hist = inline_hist
    )

    fnlist_fct <- fnlist(
      var_type = "factor",
      complete_rate = function(x) {
        paste0(perc_format(complete_rate(x)), " (", n_missing(x), ")")
      },
      ordered = is.ordered,
      n_unique = n_unique,
      top_counts = top_counts
    )

    fnlist_lgl <-
      fnlist(
        var_type = "logical",
        complete_rate = function(x) {
          paste0(perc_format(complete_rate(x)), " (", n_missing(x), ")")
        },
        mean = mean,
        count = top_counts
      )


    fnlist_date <-
      fnlist(
        var_type = "Date",
        complete_rate = function(x) {
          paste0(perc_format(complete_rate(x)), " (", n_missing(x), ")")
        },
        min = min,
        max = max,
        median = stats::median,
        n_unique = n_unique
      )

    switch(
      class(x),
      numeric = fnlist_num,
      integer = fnlist_num,
      factor = fnlist_fct,
      character = fnlist_fct,
      logical = fnlist_lgl,
      Date = fnlist_date,
      POSIXct = fnlist_date,
      difftime = fnlist_date,
      fnlist(
        var_type = class(x),
        ordered = is.ordered,
        n_unique = n_unique,
        top_counts = top_counts
      )

    )
  }

  summ_data <-
    lapply(data, function(x) {
      fn <- fn_data_summary(x)
      tibble(class = fn$var_type, purrr::map_dfc(.x = fn$funs, .f = ~
                                                   .x(x)))
    })

  summ_data <- bind_rows(summ_data, .id = "variable")

  summ_data$variable <- short_name(summ_data$variable)

  summ_data <- split(summ_data, summ_data$class)

  summ_data <-
    map(
      .x = summ_data,
      .f = ~ dplyr::select(.x, !c(class, where(~ sum(
        is.na(.x)
      ) > 0)))
    )

  out <-
    c(summ_data,
      summary = list(tibble(
        r = nrow(data),
        c = ncol(data),
        purrr::map_dfc(summ_data, nrow)
      ))
    )
  class(out) <- "data_summary"
  out
}

#' @export
print.data_summary <- function(x) {
  enf <- function(x) {
    cli::col_yellow(cli::style_bold(x))
  }
  withr::local_options(list(crayon.enabled = TRUE))
  Sys.setlocale(locale = "Chinese")
  cat_title_head("Summary dataset")

  cli::cli_bullets(c(
    "*" = paste("Number of rows :", enf(x$summary$r)),
    "*" = paste("Number of columns :", enf(x$summary$c))
  ))
  cat_subtitle1("Column type frequency")

  ftype <- x$summary[, 3:ncol(x$summary)]
  ftype <- paste(names(ftype), " : ", enf(ftype))
  ftype <- stringr::str_to_title(ftype)
  ftype <- purrr::set_names(ftype, rep("*", length(ftype)))

  cli::cli_bullets(ftype)
  cat("\n")

  x$summary <- NULL

  for (i in names(x)) {
    cat_subtitle1(" { i } ")
    print(
      x[[i]],
      n = Inf,
      width = Inf,
      n_extra = NULL,
      strip_metadata = FALSE,
      rule_width = base::options()$width
    )
  }


  Sys.setlocale("LC_COLLATE", "Spanish_Chile.1252")
  Sys.setlocale("LC_CTYPE", "Spanish_Chile.1252")
  Sys.setlocale("LC_MONETARY", "Spanish_Chile.1252")
  Sys.setlocale("LC_NUMERIC", "C")
  Sys.setlocale("LC_TIME", "Spanish_Chile.1252")

  invisible()

}

#' data summary
#'
#' @name what_is
#' @rdname data_summary
#' @export
what_is <- function(x, ..., propr = NULL) {
  if(is_empty(propr)){
    propr <-
      list(
        property = c(
          "class",
          "typeof",
          "mode",
          "storage.mode",
          "dim",
          "length",
          "is.object",
          "object.size"
        ),
        value = list(
          f(paste(class(x), collapse = " ")),
          f(typeof(x)),
          f(mode(x)),
          f(storage.mode(x)),
          f(paste(dim(x), collapse = " x ")),
          f(length(x)),
          f(is.object(x)),
          f(paste(utils::object.size(x), "Bytes"))
        )
      )

  }

  # set the warn option to -1 to temporarily ignore warnings
  # Part 1. Data Properties - class, typeof, mode, storage.mode

  properties <-
    data.frame(
      property = propr$property,
      value = map_chr(propr$value, ~ as_chr(.x(x))),
      stringsAsFactors = FALSE
    )

  # Part 2. Make a list of all x's attribute and their length
  attributes.lengths <-
    vapply(
      X = attributes(x),
      FUN = length,
      FUN.VALUE = numeric(1)
    )

  if (length(attributes.lengths) == 0) {
    attributes.lengths <- NULL
  }

  # Part 3. Test object against all "is[...]" functions
  # Look for all relevant functions
  list_id_fun <-
    suppressall(grep(utils::methods(methods::is),
                     pattern = "<-",
                     invert = TRUE,
                     value = TRUE
    ))

  # Remove functions which are not essential AND use a lot of time
  list_id_fun <-
    setdiff(
      list_id_fun,
      c(
        "is.R", "is.single", "is.na.data.frame",
        "is.na.POSIXlt"
      )
    )

  # loop over "is" functions with x as argument, and store the results
  extensive_is <- c()

  sub_objclass <- suppressall(do_try(table(map_chr(.x = x,.f = class))))

  for(i in seq_along(list_id_fun)) {
    # update progress bar
    fun <- list_id_fun[i]
    if (fun == "is.symmetric" && !is.matrix(x))
      next
    res <- try(eval(call(fun, x)), silent=TRUE)
    if(isTRUE(res))
      extensive_is <- suppressall(append(extensive_is, fun))
  }

  # Part 4. Get info on the type of object - S3, S4, attributes / slots

  output <- list()
  output$properties <- properties
  output$attributes.lengths <- attributes.lengths
  output$extensive_is <- extensive_is
  output$sub_objclass <- sub_objclass
  output
}

# descriptores

#' @export
unique_rate <- function(x) {
  vctrs::vec_unique_count(x) / length(x)
}
#' @export
n_unique <- function(x) {
  vctrs::vec_unique_count(x)
}
#' @export
n_missing <- function(x) {
  sum(is.na(x) | is.null(x))
}
#' @export
n_complete <- function(x) {
  sum(!is.na(x) & !is.null(x))
}
#' @export
complete_rate <- function(x) {
  1 - n_missing(x) / length(x)
}
#' @export
n_whitespace <- function(x) {
  whitespace <- grepl("^\\s+$", x)
  sum(whitespace)
}
#' @export
top_counts <- function(x,
                       max_char = 3,
                       max_levels = 3) {
  counts <- sorted_count(x)
  if (length(counts) > max_levels) {
    top <- counts[seq_len(max_levels)]
  } else {
    top <- counts
  }
  top_names <- substr(names(top), 1, max_char)
  paste0(top_names, ": ", top, collapse = ", ")
}
#' @export
sorted_count <- function(x) {
  tab <- table(x, useNA = "no")
  names_tab <- names(tab)
  if (is.element("", names_tab)) {
    names_tab[names_tab == ""] <- "empty"
   cat_warn(
      "Variable contains value(s) of \"\" that have been ",
      "converted to \"empty\"."
    )
  }
  out <- rlang::set_names(as_int(tab), names_tab)
  sort(out, decreasing = TRUE)
}
#' @export
inline_hist <- function(x, n_bins = 8) {
  spark_bar <- function(x, safe = TRUE) {
    stopifnot(is.numeric(x))

    bars <- vapply(0x2581:0x2588, intToUtf8, character(1))
    if (safe) {
      bars <- bars[-c(4, 8)]
    }

    factor <- cut(
      x,
      breaks = seq(0, 1, length.out = length(bars) + 1),
      labels = bars,
      include.lowest = TRUE
    )
    chars <- as_chr(factor)
    chars[is.na(chars)] <- bars[length(bars)]
    paste0(chars, collapse = "")
  }
  if (any(is.infinite(x))) {
    x[is.infinite(x)] <- NA
   cat_warn("Variable contains Inf or -Inf value(s) that were converted to NA.")
  }

  if (length(x) < 1 || all(is.na(x))) {
    return(" ")
  }

  if (all(x == 0, na.rm = TRUE)) {
    x <- x + 1
  }

  hist_dt <- table(cut(x, n_bins))
  hist_dt <- hist_dt / max(hist_dt)
  spark_bar(hist_dt)
}
