#' helpers function
#'
#'
#' @name helpers_function
#' @rdname helpers_function
#' @keywords internal
NULL


# misc ------------------------------------------------------------------------------


#' @importFrom tibble tibble as_tibble

#' @importFrom recipes bake prep terms_select check_type add_step rand_id ellipse_check

#' @importFrom tune tunable

#' @importFrom stats variable.names

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics required_pkgs
#' @export
generics::required_pkgs

#' @importFrom generics tunable
#' @export
generics::tunable

#' @importFrom magrittr %>%
#' @keywords internal
magrittr::`%>%`

#' @importFrom rlang .data
#' @keywords internal
rlang::.data

#' @importFrom rlang :=
#' @keywords internal
rlang::`:=`

#' @importFrom utils globalVariables
utils::globalVariables(c("loc", "n", "tmp", "aux", "engine", "Freq"))

# dataimport ------------------------------------------------------------------------


#' pkg_data
#' @rdname pkg_data
#' @format A data frame with 3 variables
"base_class_otras"

#' @rdname pkg_data
#' @format A data frame with 3 variables
"base_class"

#' @rdname pkg_data
#' @format A data frame with 3 variables
"base_reg"

#' @rdname pkg_data
#' @source elavoracion propia
#' @docType data
#' @format dataframe con ejemplos de eerr de retail
"curvas_df"

#' @rdname pkg_data
#' @source modeldata
#' @docType data
#' @format dataframe ames reducida
"datalearn"

#' @rdname pkg_data
#' @source modeldata
#' @docType data
#' @format dataframe ames reducida
"geocodigos"

#' @rdname pkg_data
#' @source modeldata
#' @docType data
#' @format dataframe ames reducida
"list_sample"

# hflibs ----------------------------------------------------------------------------


#' @rdname hfrec_libraries
#' @keywords internal
hfrec_libs <-
  c(
    "broom",
    "devtools",
    "dials",
    "dplyr",
    "forcats",
    "ggplot2",
    "infer",
    "lazyeval",
    "lobstr",
    "lubridate",
    "mgalda",
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
    "yardstick"
  )

#' @rdname hfrec_libraries
#' @keywords internal
lm_libs <-
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

# "mixOmics"




#  support package ------------------------------------------------------------------
#' @rdname helpers_function
#' @keywords internal
create_testthatfiles <-
  function() {
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
        names(get_calls(input_file)) %include% ls("package:mgalda")

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
