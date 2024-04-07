#' Principal Components Analysis
#'
#'
#' @name xpca
#' @rdname xpca
#' @keywords internal
NULL

# method ----------------------------------------------------------------------------

#' transformar pca
#'
#' @return pca
#' @export
#' @rdname xpca

xpca <-
  function(.data,
           ...,
           num_comp = 3,
           threshold = .9) {
    UseMethod("xpca")
  }



# default ---------------------------------------------------------------------------

#' @export
#' @rdname xpca
#'
xpca.default <-
  function(.data,
           ...,
           num_comp = 3,
           threshold = .9) {
    if (num_comp == 0) {
      rlang::abort("'num_comp' debe ser mayor que 0")
    }
    expr <- rlang::expr(...)

    pos <- tidyselect::eval_select(expr, data = .data)

    dataset <- rlang::set_names(.data[pos], names(pos))

    cols_ori <- names(dataset)

    if (length(names(select_if(dataset, is_fctchr))) > 0) {
      dmy <- T
      xgetdummy <-
        getdummy(.data = dataset,
                 where(is_fctchr),
                 threshold = .1)
      dataset <- predict(xgetdummy, dataset)
    } else {
      dmy <- F
    }

    xnzv <- near_zero(.data = dataset, tidyselect::everything())

    dataset <- predict(xnzv, dataset)

    terms <-
      rlang::quos(!!!rlang::syms(names(dataset)), .named = T)

    x <-
      list(num_comp = num_comp,
           threshold = threshold,
           terms = terms)

    prc_call <-
      rlang::expr(prcomp(
        retx = F,
        center = T,
        scale. = T
      ))

    pcavars <- names(x$terms)

    prc_call$x <-
      rlang::expr(dataset[, pcavars, drop = FALSE])

    prc_obj <- eval(prc_call)

    x$num_comp <-
      min(x$num_comp, length(prc_obj))

    if (!is.null(x$threshold)) {
      total_var <- sum(prc_obj$sdev ^ 2)

      num_comp <-
        which.max(cumsum(prc_obj$sdev ^ 2 / total_var) >= x$threshold)

      x$num_comp <- min(c(x$num_comp, num_comp))
    }

    x$cols <- cols_ori
    if (dmy) {
      x$dummy <- xgetdummy
    }
    x$nzv <- xnzv
    x$res <- prc_obj

    structure(list(
      object = x,
      inf = list(
        coefs = pca_coefs(x),
        variances = pca_variances(x),
        var_results = pca_var(dataset = dataset[, pcavars, drop = FALSE])
      )
    ), class = "xpca")

  }


# prediccion ------------------------------------------------------------------------
#' predict xpca
#' @name xpca
#' @aliases predict.xpca
#' @rdname xpca
#' @method predict xpca
#' @export

predict.xpca <-
  function(object, new_data) {
    object <- object$object
    data_to_pred <- new_data[, object$cols]
    keep <- new_data[, setdiff(names(new_data), object$cols)]

    if (!is.null(object$dummy)) {
      data_to_pred <- predict(object$dummy, data_to_pred)
    }

    data_to_pred <- predict(object$nzv, data_to_pred)

    comps <-
      predict(object$res, newdata = data_to_pred)
    comps <-
      comps[, 1:object$num_comp, drop = FALSE]
    new_data <-
      dplyr::bind_cols(dplyr::as_tibble(keep), dplyr::as_tibble(comps))
    new_data
  }


# utils --------------------------------------------------------------------------

#' @rdname xpca
#' @keywords internal

pca_coefs <- function(x) {
  rot <- as.data.frame(x$res$rotation)
  vars <- rownames(rot)
  if (x$num_comp > 0) {
    npc <- ncol(rot)
    res <- utils::stack(rot)
    colnames(res) <- c("value", "component")
    res$component <-
      as.character(res$component)
    res$terms <- rep(vars, npc)
    res <-
      as_tibble(res)[, c("terms", "value", "component")]
  } else {
    res <- tibble::tibble(
      terms = vars,
      value = rlang::na_dbl,
      component = rlang::na_chr
    )
  }
}

#' @rdname xpca
#' @keywords internal

pca_variances <- function(x) {
  rot <- as.data.frame(x$res$rotation)
  vars <- rownames(rot)
  if (x$num_comp > 0) {
    variances <- x$res$sdev^2
    p <- length(variances)
    tot <- sum(variances)
    y <- c(
      x$res$sdev,
      variances,
      cumsum(variances),
      variances / tot * 100,
      cumsum(variances) / tot * 100
    )
    x <-
      rep(
        c(
          "sdev",
          "variance",
          "cumulative_variance",
          "percent_variance",
          "cumulative_percent_variance"
        ),
        each = p
      )

    res <- tibble::tibble(
      terms = x,
      value = y,
      component = paste("PC", rep(1:p, 5), sep = "")
    )
  } else {
    res <- tibble::tibble(
      terms = vars,
      value = rep(rlang::na_dbl, length(vars)),
      component = rep(rlang::na_chr, length(vars))
    )
  }
  res <- split(res, res$terms)
  res <-
    purrr::map(
      res,
      ~ .x %>%
        pivot_wider(
          names_from = component,
          values_from = value
        ) %>%
        select(!terms) %>%
        as.numeric()
    )
  res
}

#' @rdname xpca
#' @export

pca_var <- function(dataset,
                    scale = TRUE,
                    center = TRUE) {
  res_pca <- prcomp(
    x = dataset,
    scale = TRUE,
    center = TRUE
  )

  var_cor_func <- function(var_loadings, comp_sdev) {
    var_loadings * comp_sdev
  }

  var_cor <-
    t(apply(res_pca$rotation, 1, var_cor_func, res_pca$sdev))
  var_cos2 <- var_cor^2 # variable qualities

  comp_cos2 <- apply(var_cos2, 2, sum)
  contrib <- function(var_cos2, comp_cos2) {
    var_cos2 * 100 / comp_cos2
  }
  var_contrib <- t(apply(var_cos2, 1, contrib, comp_cos2))

  pca_names <- paste0("pc_", 1:ncol(var_cor))

  colnames(var_cor) <- pca_names
  colnames(var_cos2) <- pca_names
  colnames(var_contrib) <- pca_names

  perc_var <- res_pca$sdev^2 / sum(res_pca$sdev^2) * 100
  names(perc_var) <- pca_names
  sdev <- res_pca$sdev
  names(sdev) <- pca_names
  var_contrib_cum <- var_contrib

  for (i in seq_len(ncol(var_contrib))) {
    var_contrib_cum[, i] <- var_contrib_cum[, i] * perc_var[i] / 100
  }

  out <-
    list(
      coord = var_cor,
      cos2 = var_cos2,
      contrib = var_contrib,
      contrib_cum = var_contrib_cum,
      sdev = sdev,
      perc_variance = perc_var
    )

  lapply(out, function(x) {
    round(x, significant_places(x))
  })
}

#' @name xpca
#' @aliases print.xpca
#' @rdname xpca
#' @method print xpca
#' @export


print.xpca <- function(x, print.x = FALSE, ...) {
  cat(sprintf("Varianza Acumulada (1, .., p=%d)\n", length(x$object$res$sdev)))
  print(x$inf$variances, ...)
  d <- dim(x$object$res$rotation)
  cat(sprintf("\nRotacion (n x k) = (%d x %d):\n", d[1], d[2]))
  r <- x$inf$coefs
  r <- tidyr::pivot_wider(r,names_from = terms,values_from = value)
  print(r, ...)
  invisible(x)
}
