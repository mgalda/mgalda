#' xrossdummy
#'
#'
#' @name xrossdummy
#' @rdname xrossdummy
#' @keywords internal
NULL


# core  -----------------------------------------------------------------------------

#' crossdummy
#'
#' @return crossdummy
#' @export
#' @rdname xrossdummy

xrossdummy <- function(.data, dummy_var, ...) {
  UseMethod("xrossdummy")
}

#' @export
#' @rdname xrossdummy
#'

xrossdummy.default <-
  function(.data, dummy_var, ...) {
    dummy_var <- rlang::enquo(dummy_var)
    num_vars <- rlang::enquos(..., .named = T)
    expr <- rlang::exprs(..., !!dummy_var, .named = TRUE)

    dataset <- dplyr::select(.data, !!!expr)

    rname <- names(dataset)

    dataset <- dataset %>% rowindex(random_name = T)

    rname <- setdiff(names(dataset), rname)

    dataset_split <-
      split(dataset, dataset[[rlang::as_label(dummy_var)]])

    dummy_lvs <- names(dataset_split)

    dataset_split <-
      purrr::map(
        .x = dummy_lvs,
        .f = ~ dataset_split[[.x]] %>%
          dplyr::select(!!!num_vars, !!rname) %>%
          rename_prefix(prefix = .x, !!!num_vars)
      )

    names(dataset_split) <- dummy_lvs

    crossdummy <- list(transf = NULL, info = NULL)

    crossdummy$transf[["split"]] <-
      list(lvs = dummy_lvs, var = rlang::as_label(dummy_var))

    crossdummy$transf[["target"]] <- num_vars

    dataset[[rname]] <- NULL

    dataset_split <-
      purrr::map(
        .x = dataset_split,
        .f = ~ .x %>%
          dplyr::select(!matches(rname))
      )

    crossdummy$info[["dataset"]] <- dataset
    crossdummy$info[["split"]] <- dataset_split

    crossdummy %A% "xrossdummy"
  }

# prediccion ------------------------------------------------------------------------

#' predict sl xrossdummy
#' @name xrossdummy
#' @importFrom stats predict
#' @aliases predict.xrossdummy
#' @rdname xrossdummy
#' @method predict xrossdummy
#' @export

predict.xrossdummy <-
  function(object, new_data) {
    rname <- names(new_data)

    new_data <- new_data %>% rowindex(random_name = T)

    rname <- setdiff(names(new_data), rname)

    dummy_var <- rlang::sym(object$transf$split$var)
    dummy_var <- rlang::enquo(dummy_var)
    dummy_lvs <- object$transf$split$lvs
    num_vars <- object$transf$target

    new_data_split <-
      split(new_data, new_data[[rlang::as_label(dummy_var)]])

    new_data_split <-
      purrr::map(
        .x = dummy_lvs,
        .f = ~ new_data_split[[.x]] %>%
          select(!!!num_vars, !!rname) %>%
          rename_prefix(prefix = .x, !!!num_vars)
      )

    names(new_data_split) <- dummy_lvs


    dplyr::bind_cols(
      dplyr::bind_rows(new_data_split) %>%
        dplyr::mutate_at(
          .vars = dplyr::vars(dplyr::contains(dummy_lvs)),
          .funs = ~ tidyr::replace_na(.x, 0)
        ) %>%
        dplyr::arrange(rname) %>%
        dplyr::select(-rname),
      new_data %>% dplyr::select(!c(!!!num_vars, !!dummy_var, rname))
    )
  }

# step ------------------------------------------------------------------------------

#' xrossdummy step
#' @rdname xrossdummy
#' @keywords datagen
#' @concept preprocessing
#' @export

step_xrossdummy <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           dummy_var = NA,
           object = NULL,
           skip = FALSE,
           id = recipes::rand_id("xrossdummy")) {
    recipes::add_step(
      recipe,
      step_xrossdummy_new(
        terms = recipes::ellipse_check(...),
        trained = trained,
        role = role,
        dummy_var = dummy_var,
        object = object,
        skip = skip,
        id = id
      )
    )
  }



step_xrossdummy_new <-
  function(terms,
           role,
           trained,
           dummy_var,
           object,
           skip,
           id) {
    recipes::step(
      subclass = "xrossdummy",
      terms = terms,
      role = role,
      trained = trained,
      dummy_var = dummy_var,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_xrossdummy <-
  function(x, training, info = NULL, ...) {
    col_names <- recipes::terms_select(terms = x$terms, info = info)
    recipes::check_type(training[, col_names])

    training <- dplyr::as_tibble(training)

    dummy_var <- rlang::sym(x$dummy_var)
    dummy_var <- rlang::enquo(dummy_var)
    syms_cols <- rlang::syms(col_names)
    syms_cols <- rlang::quos(!!!syms_cols)

    object <-
      xrossdummy(
        .data = training,
        dummy_var = !!dummy_var,
        !!!syms_cols
      )

    step_xrossdummy_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      dummy_var = x$dummy_var,
      object = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_xrossdummy <- function(object, new_data, ...) {
  dplyr::as_tibble(predict(object$object, new_data))
}

#' @export
print.step_xrossdummy <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("dummycross ", sep = "")
    recipes::printer(names(x$object$transf$target), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @name xrossdummy
#' @aliases print.xrossdummy
#' @rdname xrossdummy
#' @method print xrossdummy
#' @export

print.xrossdummy <-
  function(x) {
    if (!is.null(x$transf$norm)) {
      cat("utiliza metodo bestnorm \n")
      cat("\n")
    }

    if (!is.null(x$transf$clip)) {
      cat("utiliza metodo clip outliers \n")
      cat("\n")
    }
    cat("variables numericas \n")
    cat(paste0("  - ", names(x$transf$target)), sep = "\n")
    cat("\n")

    cat("variable categorica \n")
    cat(paste0("  - ", x$transf$split$var), sep = "\n")
    cat("con niveles \n")
    cat(paste0("  - ", x$transf$split$lvs), sep = "\n")
    cat("\n")
  }
