#   ' discretizar variables
#'
#'
#' @name breaktize
#' @rdname breaktize
#' @keywords internal
NULL

# core ------------------------------------------------------------------------------

#' discretizar variables
#'
#' @param dataset df
#' @param var vector sobre el cual se crea el objeto
#' @param num_breaks numero de cortes
#' @param algorithm algoritmo de corte
#' @param new_data data para predict
#'
#' @return objeto clase breaktize
#' @export
#' @rdname breaktize

breaktize <-
  function(.data = NULL,
           num_breaks = NULL,
           algorithm = c("quantile", "trees", "kmean", "equalwidth"),
           ...) {
    UseMethod("breaktize")
  }

#' @return objeto clase breaktize
#' @export
#' @rdname breaktize
#'
breaktize.default <-
  function(.data = NULL,
           num_breaks = NULL,
           algorithm = c("quantile", "trees", "kmean", "equalwidth"),
           ...) {
    expr <- rlang::expr(c(...))

    pos <- tidyselect::eval_select(expr, data = .data)

    dataset <- rlang::set_names(.data[pos], names(pos))

    xvars <- names(dataset)

    obj <-
      purrr::map(
        .x = xvars,
        .f = ~ break_object(var = dataset[[.x]], num_breaks, algorithm)
      )

    names(obj) <- xvars
    class(obj) <- c("breaktize", "breaktize_list")
    obj
  }

#' objeto para vector
#' @rdname breaktize
#' @return objeto clase breaktize
#' @export
#'

break_object <-
  function(var,
           num_breaks,
           algorithm = c("quantile", "trees", "kmean", "equalwidth")) {
    breaks <-
      switch(match.arg(algorithm, c(
        "quantile", "trees", "kmean", "equalwidth"
      )),
      quantile = do.call(
        breaktize.quantile,
        c(list(var), num_breaks)
      ),
      trees = do.call(breaktize.tree, c(list(var), num_breaks)),
      kmean = do.call(
        breaktize.kmean,
        c(list(var), num_breaks)
      ),
      equalwidth = do.call(breaktize.equalwidth, c(list(var), num_breaks))
      )

    attributes(breaks) <- NULL

    labs <- sapply(seq_along(breaks)[-1], function(x) {
      mean(breaks[x], breaks[x - 1])
    })

    breaks[min(breaks) == breaks] <- -Inf
    breaks[max(breaks) == breaks] <- Inf

    out <- list(
      breaks = breaks,
      labs = labs,
      algorithm = algorithm
    )
    class(out) <- "breaktize"
    return(out)
  }


# prediccion ------------------------------------------------------------------------

#' predict breaks
#' @name breaktize
#' @aliases predict.breaktize
#' @rdname breaktize
#' @method predict breaktize
#' @export

predict.breaktize <- function(object,
                              new_data = NULL,
                              keep = F) {
  if (any(class(object) == "breaktize_list")) {
    if (!is.data.frame(new_data)) {
      rlang::abort(message = "clase breaktize_list pronostica dfs")
    }

    predicted <-
      purrr::map_dfc(
        .x = names(object),
        .f = function(x) {
          pred <-
            cut(
              x = new_data[[x]],
              breaks = object[[x]]$breaks,
              labels = object[[x]]$labs,
              include.lowest = T
            )

          pred <- as.character(pred)
          pred <- as.numeric(pred)
          pred <- dplyr::tibble(`:=`(!!x, pred))
        }
      )

    if (keep) {
      names(predicted) <- paste("pred", names(predicted), sep = "_")
      new_data <- dplyr::bind_cols(new_data, predicted)
    } else {
      new_data <-
        dplyr::select(.data = new_data, !tidyselect::matches(names(object)))

      new_data <- dplyr::bind_cols(new_data, predicted)
    }
  } else {
    if (class(new_data) %in% c("tbl_df", "tbl", "data.frame")) {
      rlang::abort(message = "objectos de solo clase breaktize pronostican vectores")
    }

    new_data <-
      cut(
        x = new_data,
        breaks = object$breaks,
        labels = object$labs,
        include.lowest = T
      )

    new_data <- as.character(new_data)
    new_data <- as.numeric(new_data)
  }
  new_data
}


# vector ----------------------------------------------------------------------------

#' transformar vector
#' @rdname breaktize
#' @param num_breaks numero cortes
#' @param var variable a transformar
#' @param algorithm algoritmo de corte
#'
#' @return vector transformado
#' @export

breaktize_vec <-
  function(var,
           num_breaks,
           algorithm = c("quantile", "trees", "kmean", "equalwidth")) {
    numbreaktize <- break_object(var, num_breaks, algorithm)

    predict(numbreaktize, var)
  }

# tools -----------------------------------------------------------------------------

#' @rdname breaktize
#' @keywords internal
breaktize.quantile <- function(var, num_breaks) {
  breaks <-
    stats::quantile(var, probs = seq.default(0, 1, length.out = num_breaks + 1))

  breaks <- unique(breaks)

  breaks <- breaks[!is.na(breaks)]
  breaks <- breaks[order(breaks)]

  class(breaks) <- c("breaktize", "breaks")
  breaks
}

#' @rdname breaktize
#' @keywords internal
breaktize.tree <- function(var, num_breaks) {
  breaks <- list()

  if (length(var) > 1500) {
    var <- var[seq.int(
      from = 1,
      to = length(var),
      length.out = 1000
    )]
  }

  var_t <- stats::hclust(d = dist(x = var), method = "complete")

  var_t <- stats::cutree(tree = var_t, k = num_breaks)

  cmin <- stats::aggregate(var, list(var_t), min, simplify = T)$x

  cmax <- stats::aggregate(var, list(var_t), max, simplify = T)$x

  cmin <- unique(cmin[order(cmin)])

  cmax <- unique(cmax[order(cmax)])

  breaks <- sapply(1:(num_breaks - 1), function(x) {
    mean(c(cmin[x + 1], cmax[x]))
  })

  breaks <- c(min(var), breaks, max(var))
  breaks <- unique(breaks)

  breaks <- breaks[!is.na(breaks)]
  breaks <- breaks[order(breaks)]


  class(breaks) <- c("breaktize", "breaks")
  breaks
}

#' @rdname breaktize
#' @keywords internal
breaktize.kmean <- function(var, num_breaks) {
  breaks <- list()

  var <- as.numeric(var)

  var_t <- stats::kmeans(x = var, centers = num_breaks)$cluster

  cmin <- stats::aggregate(var, list(var_t), min, simplify = T)$x

  cmax <- stats::aggregate(var, list(var_t), max, simplify = T)$x

  cmin <- unique(cmin[order(cmin)])

  cmax <- unique(cmax[order(cmax)])

  breaks <- sapply(1:(num_breaks - 1), function(x) {
    mean(c(cmin[x + 1], cmax[x]))
  })

  breaks <- c(min(var), breaks, max(var))
  breaks <- unique(breaks)

  breaks <- breaks[!is.na(breaks)]
  breaks <- breaks[order(breaks)]


  class(breaks) <- c("breaktize", "breaks")
  breaks
}

#' @rdname breaktize
#' @keywords internal
breaktize.equalwidth <- function(var, num_breaks) {
  breaks <- seq.int(min(var), max(var), length.out = num_breaks + 1)

  breaks <- unique(breaks)

  breaks <- breaks[!is.na(breaks)]
  breaks <- breaks[order(breaks)]

  class(breaks) <- c("breaktize", "breaks")
  breaks
}

#' @rdname breaktize
#' @keywords internal
breaktize.equalsize <- function(var, num_breaks) {
  breaks <- seq.int(1, length(var), length.out = num_breaks + 1)

  breaks <- unique(breaks)

  breaks <- breaks[!is.na(breaks)]
  breaks <- breaks[order(breaks)]

  class(breaks) <- c("breaktize", "breaks")
  breaks
}

# step ------------------------------------------------------------------------------

#'  breaktize step
#'
#' @rdname breaktize
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_breaktize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           algorithm = "equalwidth",
           objects = NULL,
           num_breaks = 10,
           skip = FALSE,
           id = recipes::rand_id("breaktize")) {
    terms <- recipes::ellipse_check(...)

    recipes::add_step(
      recipe,
      step_breaktize_new(
        terms = terms,
        trained = trained,
        role = role,
        algorithm = algorithm,
        objects = objects,
        num_breaks = num_breaks,
        skip = skip,
        id = id
      )
    )
  }

step_breaktize_new <-
  function(terms,
           role,
           trained,
           algorithm,
           objects,
           num_breaks,
           skip,
           id) {
    recipes::step(
      subclass = "breaktize",
      terms = terms,
      role = role,
      trained = trained,
      algorithm = algorithm,
      objects = objects,
      num_breaks = num_breaks,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_breaktize <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  recipes::check_type(training[, col_names])

  training <- dplyr::as_tibble(training)

  object <-
    breaktize(
      .data = training[, col_names],
      num_breaks = x$num_breaks,
      algorithm = x$algorithm,
      tidyselect::everything()
    )

  step_breaktize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    algorithm = x$algorithm,
    objects = object,
    num_breaks = x$num_breaks,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_breaktize <- function(object, new_data, ...) {
  dplyr::as_tibble(predict(object$object, new_data))
}

#' @export
#'
print.step_breaktize <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Cut numeric for ", sep = "")
    recipes::printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname breaktize
#' @param x A `step_breaktize` object.
#' @export
tidy.step_breaktize <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- tibble::tibble(terms = recipes::sel2char(x$terms))
  } else {
    # term_names <- recipes::sel2char(x$terms)
    res <- tibble(terms = rlang::na_chr)
  }
  res$id <- x$id
  res
}

#' tunable methods
#' @keywords internal
#' @export
#' @rdname breaktize

tunable.step_breaktize <- function(x, ...) {
  tibble::tibble(
    name = c("num_breaks"),
    call_info = list(pkg = "dials", fun = "num_breaks"),
    source = "recipe",
    component = "breaktize",
    component_id = x$id
  )
}
