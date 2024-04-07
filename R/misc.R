#' @include datamanipulation.R
NULL

#' @export
factor_clean <- function(x, prefix = "cat") {
    if (is.factor(x)) {
      orig <- levels(x)
    } else {
      orig <- unique(x)
      if (is_dblint(x)) {
        orig <- sort(orig)
      }
    }

    prefx <-
      function(x, prefix) {
        paste0(
          prefix,
          stringr::str_pad(
            string = seq_along(x),
            width = max(stringr::str_length(seq_along(x))),
            pad = "0"
          )
        )
      }

    if (is_dblint(x)) {
      cleaned <- prefx(orig, prefix)
    } else {
      cleaned <- janitor::make_clean_names(string = orig)
    }
    clean <- rlang::set_names(x = cleaned, nm = orig)
    x <- as_chr(x)
    clean <- rlang::expr(dplyr::recode_factor(x, !!!clean))
    eval(clean)
  }

#' @export
names0 <- function(x, prefix = "id") {
  if (x == 0L) {
    return(character())
  }
  ind <- format(1:x)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

#' @export
roll_noise <-
  function(x, random_percent = .05) {
    n <- length(x)
    rolls_size <- floor(n / ncase(n))
    rolls_size <- min(c(rolls_size, 12))

    tibble(x = x) %>%
      mutate(xind = row_number()) %>%
      arrange(x) %>%
      mutate(
        roll_mean = slide_dbl(
          .x = x,
          .size = rolls_size,
          .fn = mean,
          .partial = "center",
          .fill = TRUE,
          na.rm = TRUE
        ),
        noise = runif(n, 0, random_percent) * roll_mean,
        noise = noise * sample(c(1, -1), n, T),
        x = x + noise
      ) %>%
      arrange(xind) %>%
      pull(x)
  }

#' @export
bootfn2 <-
  function(x,
           y,
           fn,
           times = 20,
           rep = 1,
           boots_result = FALSE,
           seed = NULL,
           na_rm = TRUE,
           ...) {
    assert_vector(x, .class = "numeric")
    assert_vector(y, .class = "numeric")
    assert_have_same_dims(x, y)

    if (na_rm) {
      idsx <- is.finite(x)
      idsy <- is.finite(y)
      x <- x[idsy & idsx]
      y <- y[idsy & idsx]
    }

    if (length(x) < times) {
      cat_stop("'times' debe ser mayor que el largo de x")
    }
    if (rlang::is_empty(seed)) {
      seed <- sample.int(10^5, 1)
    }
    xfolds <-
      with_seed(seed = seed, code = kfolds_vec(seq_along(x),
                                               times,
                                               rep = 3
      ))
    xfolds <- map_dbl(xfolds, function(w) {
      fn(x[w], y[w], ...)
    })
    xfolds_mean <- mean(xfolds)
    if (boots_result) {
      cint <-
        qt(0.975, length(xfolds) - 1) * sd(xfolds) / sqrt(length(xfolds))
      xfolds_cint <- xfolds_mean + c(-cint, cint)
      cat("el resultado es de: \n   ", xfolds_mean, "\n")
      cat(
        "intervalo de confianza al 95 porciento: \n   ",
        xfolds_cint,
        " \n \n \n"
      )
    }
    xfolds_mean
  }


#' @export
bootfn <-
  function(x,
           fn,
           times = 20,
           rep = 1,
           boots_result = FALSE,
           seed = NULL,
           ...) {
    if (!is.numeric(x)) {
     cat_warn("argumento no es numerico:return NA")
      return(NA_real_)
    }

    if (length(x) < times) {
      cat_stop("'times' debe ser mayor que el largo de x")
    }

    if (rlang::is_empty(seed)) {
      seed <- sample.int(10^5, 1)
    }


    xfolds <-
      with_seed(seed = seed, code = kfolds_vec(x, times, rep = 3))

    xfolds <-
      map_dbl(xfolds, function(w) {
        fn(w, ...)
      })

    xfolds_mean <- mean(xfolds)

    if (boots_result) {
      cint <-
        qt(.975, length(xfolds) - 1) * sd(xfolds) / sqrt(length(xfolds))

      xfolds_cint <- xfolds_mean + c(-cint, cint)


      cat("el resultado es de: \n   ", xfolds_mean, "\n")
      cat(
        "intervalo de confianza al 95 porciento: \n   ",
        xfolds_cint,
        " \n \n \n"
      )
    }

    xfolds_mean
  }

#' @export
ncase <- function(x) {
  if (length(x) == 1) {
    ceiling(2 * (x^(2 / 5)))
  } else if (length(x) > 1) {
    ceiling(2 * (length(x)^(2 / 5)))
  }
}

#' @export
nsplits <- function(x) {
  if (length(x) == 1) {
    ceiling(x^(1 / 3) - log10(x)) + 2
  } else if (length(x) > 1) {
    ceiling(sum(complete.cases(x))^(1 / 3) - log10(sum(complete.cases(x)))) + 2
  }
}

#' @export
nullclass <- function(x) {
  rm_class <- function(x, env) {
    x <- get(x = x, envir = env)

    if (is.environment(x)) {
      x <- as_l(x)
    } else {
      class(x) <- NULL
    }
    x
  }
  nenv <- baseenv()
  assign(
    x = "obj",
    value = x,
    envir = nenv
  )

  rm_class(x = "obj", env = nenv)
}

#' @export
nullattr <- function(x) {
  rm_attr <- function(x, env) {
    x <- get(x = x, envir = env)
    attributes(x) <- NULL
    if (is.environment(x)) {
      x <- as_l(x)
    }
    x
  }
  nenv <- baseenv()
  assign(
    x = "obj",
    value = x,
    envir = nenv
  )

  rm_attr(x = "obj", env = nenv)
}

#' @export
nullattr2 <- function(x){
  x <- nullattr(x)
  map(x,nullattr)
}


#' @export
update_namespace <-
  function(restart_description = FALSE,
           bu_dir = "C:/Users/mgaldame/Desktop/Pla&Est/desarrollos/package/back_up/") {
    backup_R <- function(bu_dir) {
      dpath <- list.dirs(path = bu_dir, full.names = F)
      dir_nm <- length(dpath[dpath != ""]) + 1
      dir_nm <- stringr::str_pad(dir_nm, 3, pad = "0")
      dir_nm <- paste0(dir_nm, "_", gsub("-", "", Sys.Date()))
      dir_nm <- paste0(bu_dir, "/", dir_nm)
      dir.create(path = dir_nm)
      r_files <- list.files(
        pattern = ".R",
        full.names = F,
        recursive = T
      )
      r_files <- r_files[tools::file_ext(x = r_files) == "R"]
      from_f <- paste0(getwd(), "/", r_files)
      to_f <- paste0(dir_nm, "/", gsub("/", "__", r_files))
      file.copy(from = from_f, to = to_f)
    }
    rstudioapi::documentSaveAll()
    backup_R(bu_dir)
    if (restart_description) {
      cat("restart description \n")
      usethis::use_description(
        fields = list(
          Package = "mgalda",
          Version = "2.1",
          Title = "360 Multi tool library",
          Description = "Libreria con el fin de aprendizaje (compac) y generar herramientas.",
          `Authors@R` = person(
            "Matias",
            "G.",
            email = "mgaldame@rfen.uchile.cl",
            role = c("aut", "cre")
          ),
          Encoding = "UTF-8",
          Language = "es"
        )
      )
      usethis::use_mit_license()
      devtools::document()
      pkgbuild::compile_dll()
      devtools::check()
      # usethis::use_rcpp()
    }
    cat("remover dir man \n")

    file.remove(list.files("man/", full.names = T))
    cat("remover NAMESPACE \n")
    file.remove(paste(getwd(), "NAMESPACE", sep = "/"))
    cat("reescribir \n")
    devtools::document()
    devtools::document()
    cat("restart R \n")
    rstudioapi::restartSession()
  }



#' @export
perc_format <- function(x){
  scales::percent_format(
    big.mark = ".",
    decimal.mark = ",",
    accuracy = .1
  )(x)
}
#' @export
numdig_format <- function(x,dig = .1) {
  scales::number_format(
    big.mark = ".",
    decimal.mark = ",",
    accuracy = dig
  )(x)
}
#' @export
num_format <- function(x) {
  dig <- significant_places(abs(x))
  dig <- max(floor(log10(abs(x))), dig * -1)
  dig <- ifelse(dig > 0, 0, dig)
  dig <- 10^dig
  scales::number_format(
    big.mark = ".",
    decimal.mark = ",",
    accuracy = dig
  )(x)
}
#' @export
codenum_format <- function(x) {
  .codenum_format <- function(x) {
    if (trunc(x) - x == 0) {
      dig <- 1
    } else {
      dig <- as_chr(abs(x))
      dig <- stringr::str_split(dig, stringr::fixed("."), 2, T)[, 2]
      dig <- stringr::str_length(dig)
      dig <- 10^-dig
    }
    scales::number_format(
      big.mark = "",
      decimal.mark = ".",
      accuracy = dig
    )(x)
  }
  vapply(
    X = x,
    FUN = .codenum_format,
    FUN.VALUE = character(1)
  )
}

#' @keywords internal
datadiff <- function(data, x) {
  data[, nmdiff(data, x)]
}
#' @keywords internal
nmdiff <- function(data, x) {
  setdiff(names(data), x)
}
#' @keywords internal
sort_filter_vec <- function(x, n = Inf) {
  if (length(x) < n) {
    return(x)
  }
  x <- sort(x)
  x[unique(round(seq_lengthout(x, n)))]
}
#' @keywords internal
c_dedupl <- function(...) {
  l <- c(...)
  l_names <- names(l)
  if (is.null(l_names)) {
    l
  } else {
    l[!duplicated(l_names) | (l_names == "")]
  }
}
#' @keywords internal
stretch_range <- function(x, ext = 10^(-6)) {
  x + ext * c(-1, 1)
}
#' @keywords internal
alternate <- function(x, y) {
  # It is assumed that `x` and `y` have the same lengths
  n <- length(x)
  comb <- c(x, y)
  inds <- rep(seq_len(n), each = 2) + rep(c(0, n), times = n)

  comb[inds]
}
#' @export
safe_vec <- function(x,ops = c("rm", "mean", "random", "fixed"),.fixed = 0) {
    ops <- match.arg(ops)
    mean_impute <- function(x) {
      xm <- x[!(is.na(x) | is.infinite(x) | is.nan(x) | is.null(x))]
      xm <- mean(xm, na.rm = T)
      case_when(
        is.na(x) ~ xm,
        is.infinite(x) ~ xm,
        is.nan(x) ~
          xm,
        is.null(x) ~ xm,
        T ~ x
      )
    }
    random_impute <- function(x) {
      if (length(x[is_nonnum(x)]) > 0) {
        fn <- function(n) {
          runif(
            n = n,
            min = min(x[!is_nonnum(x)]),
            max = max(x[!is_nonnum(x)])
          )
        }
        x[is_nonnum(x)] <- fn(n = length(x[is_nonnum(x)]))
      }
      x
    }
    fix_impute <- function(x) {
      x[!is.finite(x)] <- .fixed
      x
    }
    switch(ops,
           rm = x[is.finite(x)],
           mean = mean_impute(x),
           random = random_impute(x),
           fixed = fix_impute(x)
    )
  }
#' @export
approxinvfun <- function(f, lower, upper) {
  low_exp <- lower - abs(lower) * .25
  up_exp <- upper + abs(upper) * .25

  nn <- max(c(ncase(up_exp - low_exp), 10000))

  x_log <- seq_log(0, up_exp - low_exp, nn) + low_exp
  x_lin <- seq.default(low_exp, up_exp, length.out = nn)

  xp <- unique(sort(c(x_log, x_lin)))
  fx <- suppressall(f(xp))

  fx <- regularize_values(
    x = fx,
    y = xp,
    ties = mean,
    na.rm = TRUE
  )
  approxfun(
    x = fx$x,
    y = fx$y,
    method = "linear",
    na.rm = TRUE,
    yleft = fx$y[1],
    yright = fx$y[length(fx$y)]
  )
}

regularize_values <- function(x, y, ties, na.rm = TRUE) {
  x <-
    xy.coords(x, y, setLab = FALSE) # -> (x,y) numeric of same length
  y <- x$y
  x <- x$x
  keptNA <- FALSE
  nx <-
    if (any(na <- !is.finite(x) | !is.finite(y))) {
      ok <- !na
      if (na.rm) {
        x <- x[ok]
        y <- y[ok]
        length(x)
      } else {
        ## na.rm is FALSE
        keptNA <- TRUE
        sum(ok)
      }
    } else {
      length(x)
    }
  if (!identical(ties, "ordered")) {
    ordered <-
      if (is.function(ties) ||
          is.character(ties)) {
        # fn or name of one
        FALSE
      } else if (is.list(ties) &&
                 length(.t <-
                        ties) == 2L && is.function(.t[[2]])) {
        ## e.g. ties ==  list("ordered", mean)
        ties <- .t[[2]]
        identical(.t[[1]], "ordered")
      } else {
        stop("'ties' is not \"ordered\", a function, or list(<string>, <function>)")
      }
    if (!ordered && is.unsorted(if (keptNA) {
      x[ok]
    } else {
      x
    })) {
      o <- order(x)
      x <- x[o]
      y <- y[o]
    }
    if (length(ux <- unique(x)) < nx) {
      y <-
        as.vector(tapply(y, match(x, x), ties)) # as.v: drop dim & dimn.
      x <- ux
      stopifnot(length(y) == length(x)) # (did happen in 2.9.0-2.11.x)
      if (keptNA) {
        ok <- !is.na(x)
      }
    }
  }
  list(
    x = x,
    y = y,
    keptNA = keptNA,
    notNA = if (keptNA) {
      ok
    }
  )
}
random_between <- function(n, .min, .max, trans = identity) {
  g <- suppressall(approxinvfun(
    f = trans,
    lower = .min,
    upper = .max
  ))
  g(runif(
    n = n,
    min = min(env2list(env_fn(g))$x),
    max = max(env2list(env_fn(g))$x)
  ))
}

#' @export
between <- function(x, left, right, bounds = "[]") {
  bounds <- switch(bounds,
                   "[]" = list(`>=`, `<=`),
                   "[)" = list(`>=`, `<`),
                   "(]" = list(`>`, `<=`),
                   "()" = list(`>`, `<`),
                   cat_stop("Unknown `bounds` specfiication.")
  )

  bounds[[1]](vctrs::vec_compare(x, left), 0) &
    bounds[[2]](vctrs::vec_compare(x, right), 0)
}

#' @export
interval_match <- function(y, x, obs) {
  ord <- order(x)
  y <- y[ord]
  x <- x[ord]
  y[findInterval(x = obs, vec = x)]
}


# date

#' @export
diffmonths <- function(end, start) {
  ed <- as.POSIXlt(end)
  sd <- as.POSIXlt(start)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
#' @export
diffyears <- function(end, start) {
  as.numeric(difftime(end, start, units = "weeks")) / 52.25
}
#' @export
mmm_yyyy <- function(x, ...) {
  if (is.POSIXct(x)) {
    return(lubridate::floor_date(x, unit = "month"))
  }
  lubridate::parse_date_time(paste0(x, " 01"), "b Y d")
}
#' @export
ym <- function(x, ...) {
  if (is.POSIXct(x)) {
    return(lubridate::floor_date(x, unit = "month"))
  }
  lubridate::ymd(paste0(x, "01"), ...)
}

#' @export
monthify <- function(x) {
  lubridate::floor_date(x, unit = "month")
}
#' @export
quarterify <- function(x) {
  lubridate::floor_date(x, unit = "quarter")
}
#' @export
yearify <- function(x) {
  lubridate::floor_date(x, unit = "year")
}
