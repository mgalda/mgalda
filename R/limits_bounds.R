#' limites
#'
#'
#' @name limits_bounds
#' @rdname limits_bounds
#' @keywords internal
NULL

# core ------------------------------------------------------------------------------

#' limites
#'
#' @return objeto clase limits_bounds
#' @export
#' @rdname limits_bounds


limits_bounds <- function(.data, ...) {
  UseMethod("limits_bounds")
}


#' @export
#' @rdname limits_bounds
#'

limits_bounds.default <-
  function(.data, ...) {
    expr <- rlang::expr(...)

    pos <- tidyselect::eval_select(expr, data = .data)

    dataset <- rlang::set_names(.data[pos], names(pos))

    obj <-
      purrr::map(.x = dataset, .f = ~ limits_object(x = .x))

    class(obj) <- c("limits_bounds", "limits_bounds_list")
    obj
  }

#' @rdname limits_bounds
#' @keywords internal

limits_object <-
  function(x) {
    if (!rlang::is_bare_numeric(x)) {
      rlang::abort("calculo univariado")
    }
    check_numeric(x)

    bound <- as.double(range(x))

    out <- list(min_bound = min(bound), max_bound = max(bound))
    class(out) <- "limits_bounds"
    out
  }

# prediccion ------------------------------------------------------------------------

#' predict bounds
#' @name limits_bounds
#' @aliases predict.limits_bounds
#' @rdname limits_bounds
#' @method predict limits_bounds
#' @export

predict.limits_bounds <- function(object,
                                  new_data = NULL) {
  if (any(class(object) == "limits_bounds_list")) {
    if (!is.data.frame(new_data)) {
      rlang::abort(message = "clase limits_bounds_list pronostica dfs")
    }

    predicted <-
      purrr::map_dfc(
        .x = names(object),
        .f = function(x) {
          new_data[[x]] <- as.double(new_data[[x]])
          pred <-
            dplyr::case_when(
              new_data[[x]] > object[[x]]$max_bound ~ object[[x]]$max_bound,
              new_data[[x]] < object[[x]]$min_bound ~ object[[x]]$min_bound,
              T ~ new_data[[x]]
            )

          pred <- as.numeric(pred)
          pred <- dplyr::tibble(`:=`(!!x, pred))
        }
      )
    new_data <-
      dplyr::select(.data = new_data, !tidyselect::matches(names(object)))

    new_data <- dplyr::bind_cols(new_data, predicted)
  } else {
    if (is.data.frame(new_data)) {
      rlang::abort(message = "objectos de solo clase limits pronostican vectores")
    }

    new_data <- as.double(new_data)

    new_data <-
      dplyr::case_when(
        new_data > object$max_bound ~ object$max_bound,
        new_data < object$min_bound ~ object$min_bound,
        T ~ new_data
      )
  }
  new_data
}


# step ------------------------------------------------------------------------------

#' @rdname limits_bounds
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_limits_bounds <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("limits_bounds")) {
    add_step(
      recipe,
      step_limits_bounds_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_limits_bounds_new <-
  function(terms, role, trained, object, skip, id) {
    step(
      subclass = "limits_bounds",
      terms = terms,
      role = role,
      trained = trained,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_limits_bounds <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  recipes::check_type(training[, col_names])

  training <- dplyr::as_tibble(training)

  # syms_cols <- rlang::syms(col_names)

  object <-
    limits_bounds(.data = training[, col_names], everything())

  step_limits_bounds_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    object = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_limits_bounds <- function(object, new_data, ...) {
  dplyr::as_tibble(predict(object$object, new_data))
}

#' @export
print.step_limits_bounds <-
  function(x, width = max(20, options()$width - 30), info = NULL, ...) {
    cat("generar limites de las variables ", sep = "")
    recipes::printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }
