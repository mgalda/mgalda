#' @include util_env.R
NULL

#' util_base
#'
#'
#' @name util_base
#' @rdname util_base
#' @keywords internal
NULL


# dataimport --

#' @rdname util_base
#' @format data
"base_superlearner"

#' @rdname util_base
#' @format data
"curv_data"

#' @rdname util_base
#' @format data
"datalearn"

#' @rdname util_base
#' @format data
"geocodigos"

#' @rdname util_base
#' @format data
"list_sample"

#' @rdname util_base
#' @format data
"london_gpkg"

#' @rdname util_base
#' @format data
"yardsticks_metrics_info"

#' @rdname util_base
#' @format data
"info_distribucion"

#' @rdname util_base
#' @format data
"stocks_data"

#' @rdname util_base
#' @format data
"streets"

#' @rdname util_base
#' @format data
"map_hex"

#' @rdname util_base
#' @format data
"map_grid"

#' @rdname util_base
#' @format data
"map_pois"

#' @rdname util_base
#' @format data
"map_fromto"

#' @rdname util_base
#' @format data
"hexbin_data"

# librerias recurrentes --

#' @rdname util_base
#' @export
rm_ls <- function(...) {
  exclude <- rlang::ensyms(...)
  exclude <- map_chr(exclude, rlang::as_label)
  global_ls <- ls(envir = .GlobalEnv)
  rm(list = global_ls[!global_ls %in% exclude], envir = .GlobalEnv)
  try(dev.off(), silent = T)
}

#' @rdname util_base
#' @export

hfrec_libraries <-
  function(conflict_prefer = getOption("mgalda.conflict_prefer", TRUE),
           hfrec_libs = getOption("mgalda.hfrec_libs"),
           verbose = TRUE) {
    gc()
    do_try(dev.off())
    cat("\f")


    if (env_data$hflibs) {
      if (isTRUE(".conflicts" %in% search())) {
        suppressall(detach(".conflicts", unload = TRUE, force = TRUE))
      }

      .cpkgs <- sessionInfo()
      .cpkgs <- names(.cpkgs$otherPkgs)

      get("attach")(env(), name = ".conflicts")

      if (all(c("purrr", "mixOmics") %in% .cpkgs)) {
        pkgs_conflict_prefer("purrr", "mixOmics")
      }
      if (all(c("dplyr", "MASS") %in% .cpkgs)) {
        pkgs_conflict_prefer("dplyr", "MASS")
      }
      if (all(c("dplyr", "stats") %in% .cpkgs)) {
        pkgs_conflict_prefer("dplyr", "stats")
      }
      pkgs_conflict_prefer("mgalda")

      return(cat_message("already charge"))
    }
    env_data$hflibs <- TRUE

    current_session <- sessionInfo()
    current_packs <- names(current_session$otherPkgs)


    if (isTRUE(".conflicts" %in% search())) {
      suppressall(detach(".conflicts", unload = TRUE, force = TRUE))
    }

    for (n in hfrec_libs) {
      if (n %in% current_packs & verbose) {
        cat(cli::col_blue(paste(
          "   - ", n, " (previamente cargada)",
          sep = ""
        )), "\n")
      } else {
        if (!requireNamespace(n, quietly = TRUE)) {
          n1 <- -1
          while (n1 %!in% c(0, 1)) {
            n1 <-
              readline(prompt = paste("Enter 1 to install", n, " or 0 for skip: "))
          }
          if (n1 == 1) {
            install.packages(n)
          }
        }
        if (verbose) {
          cat(cli::col_grey(paste("   - ", n, sep = "")))
        }
        do_try(library(n, character.only = T,warn.conflicts = F,quietly = T))

        if (verbose) {
          cat(cli::col_grey(" (cargada)"), "\n")
          Sys.sleep(time = 0.001)
        }
      }
    }
    if (conflict_prefer) {
      get("attach")(env(), name = ".conflicts")

      .cpkgs <- sessionInfo()
      .cpkgs <- names(.cpkgs$otherPkgs)

      get("attach")(env(), name = ".conflicts")

      if (all(c("purrr", "mixOmics") %in% .cpkgs)) {
        pkgs_conflict_prefer("purrr", "mixOmics")
      }
      if (all(c("dplyr", "MASS") %in% .cpkgs)) {
        pkgs_conflict_prefer("dplyr", "MASS")
      }
      if (all(c("dplyr", "stats") %in% .cpkgs)) {
        pkgs_conflict_prefer("dplyr", "stats")
      }
      pkgs_conflict_prefer("mgalda")
    }
  }
#' @export
suppressall <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (inherits(w, "warning")) {
        tryInvokeRestart("muffleWarning")
      }
    },
    message = function(c) {
      if (inherits(c, "message")) {
        tryInvokeRestart("muffleMessage")
      }
    }
  )
}

#' @export
document <- devtools::document



# try ---


#' @export
do_try <- function(expr, error = NULL) {
  .doTryCatch <- function(expr,
                          error = NULL,
                          env = parent.frame()) {
    .Internal(.addCondHands(
      "error", list(error), env, environment(),
      FALSE
    ))
    expr
  }

  value <- suppressall(.doTryCatch(return(expr),
    error = ier(),
    env = parent.frame()
  ))
  if (is_ier(value[[3]])) {
    return(error)
  }
  value
}
do_try_ret <- function(expr, .error = NULL) {
  .doTryCatch <- function(expr,
                          error = NULL,
                          env = parent.frame()) {
    .Internal(.addCondHands(
      "error", list(error), env, environment(),
      FALSE
    ))
    expr
  }
  value <- suppressall(.doTryCatch(return(expr),
    error = ier(),
    env = parent.frame()
  ))
  if (is_ier(value[[3]])) {
    ret_inv <- rlang::expr(invisible(return(!!.error)))
    return(rlang::eval_bare(ret_inv, env = parent.frame()))
  }
  value
}
on_error <- function(f) {
  f <- rlang::as_function(f)
  old <- do.call(options, as.list(c("error" = f)))

  do.call(options,
    list("error" = f),
    envir = parent.frame()
  )

  do.call(on.exit,
    list(substitute(options(old)),
      add = TRUE
    ),
    envir = parent.frame()
  )
}
#' @export
insist_safe <-
  function(...,
           .env = parent.frame(1L),
           .error = NULL) {
    .expr <- substitute(...)

    j <- 1

    do_try(expr = {
      repeat {
        out <-
          try(eval(expr = .expr, envir = .env), T)
        if (!is_empty(out) & !is_try(out)) {
          return(out)
        }
        if (j == 5) {
          return(NULL)
        }
        j <- j + 1
      }
    }, error = NULL)
  }

#' @export
run_examples_source <-
  function(pkg = "C:/Users/mgaldame/Desktop/Pla&Est/desarrollos/package/mgalda",
           source_file = NULL,
           run_donttest = FALSE,
           run_dontrun = FALSE,
           document = TRUE) {
    cat("\f")
    detect_multi <-
      function(string, pattern) {
        vapply(string, function(x) {
          any(vapply(pattern, function(p) {
            stringr::str_detect(string = x, pattern = stringr::fixed(p))
          }, logical(1)))
        }, logical(1))
      }

    pkg <- devtools::as.package(pkg)
    # if (document) {
    #   document(pkg)
    # }
    files <- devtools:::rd_files(pkg$path)

    if (length(source_file) > 0) {
      source_file <- .un(lapply(source_file, readr::read_lines))

      source_file <-
        source_file[stringr::str_detect(source_file, "@rdname")]

      source_file <-
        stringr::str_sub(
          string = source_file,
          start = stringr::str_locate(source_file, "rdname")[, 2] + 1,
          end = -1
        )
      source_file <- paste0("/", trimws(source_file), ".Rd")

      files <- files[detect_multi(files, source_file)]
    }
    if (length(files) == 0) {
      return()
    }
    cli::cat_rule(
      left = paste0("Running ", length(files), " example files"),
      right = pkg$package
    )
    devtools::load_all(pkg$path, reset = TRUE, export_all = FALSE)
    on.exit(devtools::load_all(pkg$path, reset = TRUE))
    lapply(files,
      function(.file, run_donttest, run_dontrun) {
        pkgload::run_example(
          path = .file,
          run_donttest = run_donttest,
          run_dontrun = run_dontrun
        )
      },
      run_donttest = run_donttest,
      run_dontrun = run_dontrun
    )
    invisible()
  }


# import --

#' @import recipes
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' @import tibble
#' @import tidyr
#' @importFrom patchwork plot_layout
#' @importFrom patchwork plot_annotation
#' @importFrom patchwork patchworkGrob
#' @importFrom grid grid.draw
#' @importFrom grDevices windowsFonts
#' @importFrom generics required_pkgs
#' @importFrom generics tidy
#' @importFrom generics tunable
#' @importFrom glue glue
#' @importFrom parsnip set_engine
#' @importFrom parsnip set_mode
#' @importFrom sf st_as_sf
#' @importFrom sf st_bbox
#' @importFrom sf st_coordinates
#' @importFrom sf st_crs
#' @importFrom sf st_set_geometry
#' @importFrom sf st_transform
#' @importFrom purrr map_dfr
#' @importFrom purrr map_dfc
#' @importFrom purrr imap_dfr
#' @importFrom R6 R6Class

#' @importFrom rlang .data
#' @export
rlang::.data

#' @importFrom rlang .env
#' @export
rlang::.env

#' @importFrom rlang :=
#' @export
rlang::`:=`

#' @importFrom utils globalVariables
utils::globalVariables(c("j", "i"))
# c("loc", "n", "tmp", "aux", "engine", "Freq")


registerMethods <- function(methods) {
  lapply(methods, function(method) {
    pkg <- method[[1]]
    generic <- method[[2]]
    class <- method[[3]]
    func <- get(paste(generic, class, sep = "."))
    if (pkg %in% loadedNamespaces()) {
      registerS3method(generic, class, func, envir = asNamespace(pkg))
    }
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) {
        registerS3method(generic, class, func, envir = asNamespace(pkg))
      }
    )
  })
}

.onLoad <- function(...) {
  pkgs <-
    c(
      "dplyr",
      "forcats",
      "ggplot2",
      "osrm",
      "parsnip",
      "purrr",
      "readr",
      "sf",
      "stringr",
      "tibble",
      "tidyr"
    )
  .hfrec_libs <-
    c(
      "broom",
      "cli",
      "devtools",
      "dials",
      "dplyr",
      "embed",
      "forcats",
      "ggplot2",
      "infer",
      "lazyeval",
      "lobstr",
      "lubridate",
      "modeldata",
      "parsnip",
      "purrr",
      "readr",
      "readxl",
      "recipes",
      "reprex",
      "rlang",
      "rsample",
      "scales",
      "stringr",
      "tibble",
      "tidyr",
      "tune",
      "usethis",
      "workflows",
      "workflowsets",
      "yardstick",
      "mgalda"
    )

  .supl_libs <-
    c(
      "baguette",
      "parsnip",
      "rules",
      "discrim",
      "plsmod",
      "poissonreg",
      "tidybayes",
      "textrecipes",
      "embed",
      "themis",
      "glmnet",
      "kknn",
      "nnet",
      "ranger",
      "rpart",
      "earth",
      "kernlab",
      "LiblineaR",
      "stats"
    )

  .lm_libs <-
    c(
      "MASS",
      "modeltime",
      "baguette",
      "parsnip",
      "rules",
      "discrim",
      "plsmod",
      "poissonreg",
      "kknn",
      "rpart",
      "glmnet",
      "LiblineaR",
      "earth",
      "nnet",
      "ranger",
      "mda",
      "naivebayes",
      "C50",
      "Cubist",
      "kernlab"
    )

  .roi_pkgs <- c(
    "ROI",
    "ROI.plugin.glpk",
    "ROI.plugin.quadprog",
    "ROI.plugin.alabama",
    "ROI.plugin.nloptr",
    "ROI.plugin.ecos",
    "ROI.plugin.deoptim",
    "ROI.plugin.optimx",
    "ROI.plugin.qpoases",
    "slam",
    "ompr",
    "ompr.roi"
  )
  default_options <- list(
    osrm.server = "http://127.0.0.1:5000/",
    width = 70,
    digits = 3,
    scipen = 9999,
    Ncpus = 1,
    mgalda.severity = "warning",
    mgalda.conflict_prefer = TRUE,
    mgalda.hfrec_libs = .hfrec_libs,
    mgalda.lm_libs = .lm_libs,
    mgalda.supl_libs = .supl_libs,
    mgalda.roi_pkgs = .roi_pkgs,
    mgalda.zero_prob = .5e-4,
    mgalda.iqr_times_inf = 20,
    mgalda.n_samples = 2500,
    mgalda.inter = TRUE,
    cli.progress_show_after = 0,
    cli.progress_clear = FALSE
  )

  for (fts in env_data$fonts) {
    do_call("windowsFonts", .args = as.list(set_names(grDevices::windowsFont("fts"))))
  }

  par(mar = c(2, 2, 2, 2), family = "Helvetica")
  do.call(options, as.list(default_options))
  onload_library(pkgs)
  metacolours <<- metacolours$new()
  # cat("\f")
}

.onUnload <- function(...) {
  if (isTRUE(".conflicts" %in% search())) {
    suppressall(detach(".conflicts", unload = TRUE, force = TRUE, ))
  }
}

onload_library <- function(name, quietly = TRUE) {
  for (n in name) {
    if (!requireNamespace(n, quietly = TRUE)) {
      n1 <- -1
      while (n1 %!in% c(0, 1)) {
        n1 <-
          readline(prompt = paste0("Enter 1 to install '", n, "' or 0 for skip: "))
      }
      if (n1 == 1) {
        install.packages(n)
      }
    }
    suppressall(requireNamespace(n, quietly = TRUE))
  }
}
require_library <- function(name, quietly = TRUE) {
  for (n in name) {
    if (!requireNamespace(n, quietly = TRUE)) {
      n1 <- -1
      while (n1 %!in% c(0, 1)) {
        n1 <-
          readline(prompt = paste0("Enter 1 to install '", n, "' or 0 for skip: "))
      }
      if (n1 == 1) {
        install.packages(n)
      }
    }
    suppressall(require(
      n,
      warn.conflicts = F,
      character.only = T,
      quietly = T
    ))
  }
}
libs_packs <- function() {
  unique(
    getOption("mgalda.hfrec_libs"),
    getOption("mgalda.lm_libs")
  )
}
create_testthatfiles <- function() {
  ff <- function(x) {
    testthat_file <- paste("tests/testthat/test-", x, sep = "")

    input_file <- paste("R/", x, sep = "")

    context <-
      stringr::str_sub(
        string = x,
        start = 1,
        end = stringr::str_length(x) - 2
      )

    lf <-
      names(get_fns_ns(input_file)) %~in% ls("package:mgalda")

    print(input_file)

    sink(testthat_file)


    cat(
      " library(mgalda) \n library(testthat) \n testthat::context('",
      context,
      "') \n mgalda::hfrec_libraries()  \n  \n  \n",
      sep = ""
    )

    for (i in lf) {
      sep_name <- paste("funcion : ", i, "  ", sep = "")

      nnn <- stringr::str_length(sep_name) + 3

      cat(paste("# ", sep_name, paste(rep("-", 83 - nnn), collapse = ""),
        sep = ""
      ), rep("\n", 3))
    }
    sink()
  }
  invisible(lapply(list.files(
    path = "R/", full.names = F
  ), ff))
}
conflict_scout <- function(pkgs = NULL) {
  base_packages <- c(
    "base",
    "datasets",
    "grDevices",
    "graphics",
    "methods",
    "stats",
    "utils"
  )
  pkg_data <- function(x) {
    ns <- env_ns(x)
    lazy_data <- .getNamespaceInfo(ns, "lazydata")
    if (is_empty(lazy_data)) {
      return(character())
    }
    rlang::env_names(lazy_data)
  }
  pkg_ls <- function(pkg) {
    ns <- getNamespace(pkg)
    exports <- getNamespaceExports(ns)
    names <- intersect(exports, rlang::env_names(ns))
    int <- grepl("^.__", names)
    c(names[!int], pkg_data(pkg))
  }
  pkgs_attached <- function() {
    pkg_devtools <- function(name) {
      ns <- .getNamespace(name)
      if (is_empty(ns)) {
        return(FALSE)
      }
      !is_empty(ns$.__DEVTOOLS__)
    }
    pkgs <- gsub("package:", "", grep("package:", search(),
      value = TRUE
    ))
    is_dev <- vapply(pkgs, pkg_devtools, logical(1))
    pkgs[!is_dev]
  }
  invert <- function(x) {
    if (length(x) == 0) {
      return()
    }
    stacked <- utils::stack(x)
    tapply(as_chr(stacked$ind), stacked$values, list)
  }
  new_conflict_report <-
    function(conflicts, n = length(conflicts)) {
      structure(conflicts, n = n)
    }
  is_superset <- function(fun, pkg, base) {
    if (pkg == "dplyr" && fun == "lag") {
      return(FALSE)
    }
    pkg_obj <- getExportedValue(pkg, fun)
    if (methods::is(pkg_obj, "standardGeneric")) {
      return(TRUE)
    }
    base_obj <- getExportedValue(base, fun)
    if (!is.function(pkg_obj) || !is.function(base_obj)) {
      return(FALSE)
    }
    args_pkg <- names(rlang::fn_fmls(pkg_obj))
    if (identical(args_pkg, "...")) {
      return(TRUE)
    }
    if (is.primitive(base_obj)) {
      return(FALSE)
    }
    args_base <- names(rlang::fn_fmls(base_obj))
    all(args_base %in% args_pkg)
  }
  has_moved <- structure(function(pkg, fun, obj = NULL) {
    if (is.null(obj)) {
      obj <- getExportedValue(pkg, fun)
    }
    if (!is.function(obj)) {
      return(FALSE)
    }
    body <- body(obj)
    if (length(body) < 2 || !rlang::is_call(body, "{")) {
      return(FALSE)
    }
    if (!rlang::is_call(body[[2]], ".Deprecated")) {
      return(FALSE)
    }
    if (length(body[[2]]) < 2) {
      return(FALSE)
    }
    new <- body[[2]][[2]]
    if (!is.character(new)) {
      return(FALSE)
    }
    grepl(paste0("::", fun), new)
  },
  memoised = TRUE,
  class = c("memoised", "function")
  )
  prefs <- env()
  pkgs <- pkgs %||% pkgs_attached()
  pkgs <- unique(c(pkgs, "mgalda"))
  objs <- lapply(pkgs, pkg_ls)
  names(objs) <- pkgs
  index <- invert(objs)
  index
  potential <- Filter(function(x) {
    length(x) > 1
  }, index)
  unique <- Map(function(name, pkgs) {
    objs <- lapply(pkgs, getExportedValue, name)
    names(objs) <- pkgs
    pkgs[!duplicated(objs)]
  }, names(potential), potential)
  conflicts <- Filter(function(x) {
    length(x) > 1
  }, unique)
  nm_conflicst <- setNames(names(conflicts), names(conflicts))
  conflicts <- map2(nm_conflicst, conflicts, function(fun,
                                                      pkgs) {
    base <- intersect(pkgs, base_packages)
    non_base <- setdiff(pkgs, base_packages)
    if (length(non_base) == 0) {
      character()
    } else if (length(non_base) == 1) {
      if (is_superset(fun, non_base, base = base)) {
        character()
      } else {
        pkgs
      }
    } else {
      pkgs
    }
  })
  conflicts <- map2(nm_conflicst, conflicts, function(fun,
                                                      pkgs) {
    is_dep <- vapply(pkgs, has_moved, fun = fun, FUN.VALUE = logical(1))
    pkgs[!is_dep]
  })
  conflicts <- map2(nm_conflicst, conflicts, function(fun,
                                                      pkgs) {
    if (length(pkgs) != 2) {
      return(pkgs)
    }
    pkg1 <- getExportedValue(pkgs[[1]], fun)
    pkg2 <- getExportedValue(pkgs[[2]], fun)
    if (is.function(pkg1) != is.function(pkg2)) {
      character()
    } else {
      pkgs
    }
  })
  conflicts <- compact(conflicts)
  prefs_resolve <- function(fun, conflicts) {
    pkgs <- prefs[[fun]]
    if (length(pkgs) == 1) {
      pkgs[[1]]
    } else {
      c(pkgs[[1]], setdiff(conflicts, pkgs))
    }
  }
  for (fun in ls(prefs)) {
    if (!has_name(conflicts, fun)) {
      next
    }
    conflicts[[fun]] <- prefs_resolve(fun, conflicts[[fun]])
  }
  new_conflict_report(conflicts)
}
pkgs_conflict_prefer <- function(pkg, losers = NULL) {
  on_error(f = invisible)
  installed_packs <-
    base::rownames(utils::installed.packages())
  # prefs <- env()
  if (!"conflicted" %in% installed_packs) {
    cat_stop("conflicted pkg no instalado")
  }

  cnpkg <-
    conflict_scout() %>%
    mgalda::nullclass() %>%
    map(~ .x[.x != ".conflicts"])

  cnpkg <- cnpkg[map_lgl(cnpkg, ~ any(.x %in% pkg))]

  if (mgalda::is_empty(losers)) {
    losers <- unique(unname(unlist(cnpkg)))
    losers <- losers[losers != pkg]
  }

  nn <-
    cnpkg %>%
    purrr::map_dfr(~ tibble::as_tibble(.x) %>%
      dplyr::mutate(has = sum(.x == pkg)), .id = "fn") %>%
    dplyr::filter(value != pkg) %>%
    dplyr::filter(has == 1) %>%
    dplyr::filter(value %in% losers) %>%
    dplyr::select(!fn) %>%
    dplyr::distinct() %>%
    nrow()

  if (nn > 0) {
    fn_nm <-
      cnpkg %>%
      purrr::map_dfr(
        .f = ~ as_tibble(.x) %>%
          dplyr::mutate(has = sum(.x == pkg)),
        .id = "fn"
      ) %>%
      dplyr::filter(value != pkg) %>%
      dplyr::filter(has == 1) %>%
      dplyr::filter(value %in% losers)

    for (i in 1:nrow(fn_nm)) {
      rlang::env_bind(
        rlang::search_env(".conflicts"),
        `:=`(!!fn_nm$fn[i], getExportedValue(pkg, fn_nm$fn[i]))
      )
    }
  }
  invisible()
}
