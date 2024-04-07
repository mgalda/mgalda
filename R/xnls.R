#' xnls
#'
#'
#' @name xnls
#' @rdname xnls
#' @keywords internal
NULL

# core ------------------------------------------------------------------------------

#' xnls
#'
#' @return objeto clase xnls
#' @export
#' @rdname xnls


xnls <- function(.data, x, y) {
  UseMethod("xnls")
}


#' @export
#' @rdname xnls
#'

xnls.default <-
  function(.data, x, y) {
    x <- rlang::enexpr(x)
    y <- rlang::enexpr(y)

    obj <- nls2expr(dataset = .data,
                    x = !!x,
                    y = !!y)
    class(obj) <- c("xnls", class(obj))
    obj
  }

# step ------------------------------------------------------------------------------

#' @rdname xnls
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_xnls <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("xnls")) {
    add_step(
      recipe,
      step_xnls_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_xnls_new <-
  function(terms, role, trained, object, skip, id) {
    step(
      subclass = "xnls",
      terms = terms,
      role = role,
      trained = trained,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_xnls <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes::recipes_eval_select(quos =  x$terms,
                                 data = training,
                                 info = info)
  training <- dplyr::as_tibble(training)
  y_names <-
    recipes::recipes_eval_select(quos = all_outcomes(),
                                 info = info,
                                 data = training)

  object <-
    map(col_names,  ~ xnls(
      .data = training,
      x = !!sym(.x),
      y = !!sym(y_names)
    ))

  names(object) <- col_names
  step_xnls_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    object = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_xnls <- function(object, new_data, ...) {

  vars_xnls<- names(object$object)

  for(i in vars_xnls){


    newcol <- paste0("xnlsx_",i)
    new_data <- mutate(new_data,{{newcol}} := eval_tidy(object$object[[i]]))
  }
  new_data

}

#' @export
print.step_xnls <-
  function(x,
           width = max(20, options()$width - 30),
           info = NULL,
           ...) {
    cat("generar nueva variable en base a funciones no lineales ", sep = "")
    recipes::printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }
