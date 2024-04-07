# hexbin ------------------------------------------------------------------

#' The hexbin_points function
#'
#' @rdname spatials
#' @name spatials
#' @export
#' @examples
#'
#'
#' library(sp)
#' library(sf)
#' data <- mgalda::hexbin_data$vaccination
#'
#' # Get mean coverage per hexbin
#' hexbin_points(
#'   points = data,
#'   n = 250,
#'   fun = mean,
#'   z = "coverage"
#' )
#'
#' # Get number of points per hexbin
#' hexbin_points(
#'   points = data,
#'   n = 250,
#'   fun = length,
#'   z = "coverage"
#' )

hexbin_points <- function(points,
                          n,
                          fun,
                          z,
                          buffer = 0.1,
                          return.na = FALSE) {
  if (!requireNamespace("sp", quietly = TRUE)) {
    cat_stop("package sp required, please install it first")
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    cat_stop("package sf required, please install it first")
  }
  if (!requireNamespace("rgeos", quietly = TRUE)) {
    cat_stop("package rgeos required, please install it first")
  }
  # # Convert sf to sp points
  # # Convert sf to sp points
  points_sp <- as(points, "Spatial")

  # create convex hull
  hull <- sf::st_convex_hull(sf::st_union(points))

  # Buffer slightly
  max_dist <- max(dist(st_coordinates(points)))
  hull <- sf::st_buffer(hull, max_dist * buffer)
  hull <- as(hull, "Spatial")

  # Create hexbin
  set.seed(1981)
  HexPts <- sp::spsample(hull, n = n, type = "hexagonal")
  HexPols <- sp::HexPoints2SpatialPolygons(HexPts)


  # Calc which hexbin each point is in
  points_sp$hexbin <- sp::over(points_sp, HexPols)

  # Apply function to each hexbin group
  summ_stat <-
    aggregate(points_sp@data[, z],
              by = list(points_sp$hexbin),
              FUN = fun
    )
  names(summ_stat) <- c("ID", z)

  summ_stat <-
    merge(
      data.frame(
        ID = 1:length(HexPols),
        row.names = row.names(HexPols)
      ),
      summ_stat,
      by = "ID",
      all.x = TRUE
    )
  summ_stat[[z]][is.na(summ_stat[[z]])] <- 0

  row.names(summ_stat) <- paste0("ID", summ_stat[["ID"]])

  HexPols <- sp::SpatialPolygonsDataFrame(HexPols, summ_stat)

  st_as_sf(HexPols)
}

#' The hexbin_raster function
#'
#' @rdname spatials
#' @name spatials
#' @export
#' @examples # Generate hexbins and calculate raster mean in each bin
#'
#' hexbin_raster(
#'   mgalda::hexbin_data$evel,
#'   n = 100,
#'   fun = function(x) {
#'     mean(x, na.rm = TRUE)
#'   }
#' )
#'
hexbin_raster <- function(r, n, fun) {
  if (!requireNamespace("sp", quietly = TRUE)) {
    cat_stop("package sp required, please install it first")
  }
  if (!requireNamespace("velox", quietly = TRUE)) {
    cat_stop("package velox required, please install it first")
  }
  # create velox
  r_velox <- velox::velox(r)
  r_sp <- as(r, "SpatialPixelsDataFrame")

  # Create hexbin
  set.seed(1981)
  HexPts <- sp::spsample(r_sp, n = n, type = "hexagonal")
  HexPols <- sp::HexPoints2SpatialPolygons(HexPts)

  # Extract with function
  HexPols$stat <- r_velox$extract(sp = HexPols, fun = fun)
  st_as_sf(HexPols)
}



# Mixed-variables of distance -----------------------------------------------------------------

#' mixedvar_distance
#'
#' @export
#' @name spatials
#' @rdname spatials

mixedvar_distance <-
  function(data,
           diag = FALSE,
           upper = FALSE,
           scale_method = c("range", "sd", "none"),
           tol = 1e-8) {

    x<-prep_mixedvar_distance(df = data)

    #tbnm <- attr(x, "tabnames")

    interm <- as_l(1:length(x$blo))

    names(interm) <- paste("iteration", 1:length(x$blo), sep = "")

    nlig <- nrow(x[[1]])

    if (any(names(x$blo) == "dicotomica")) {
      napres <- TRUE
    } else {
      napres <- any(is.na(unlist(x[names(x$blo)])))
    }

    res <-
      purrr::imap(
        x[attr(x, "tabnames")],
        ~ treatment_mixedvar_distance(
          type_var = .y,
          df = .x,
          blocs = x$blo,
          tol = tol,
          scale_method = scale_method,
          napres = napres
        )
      )

    if (!napres) {
      nbvar <- sum(unlist(lapply(res, function(u) {
        u[[1]]
      })))
    } else {
      listntvar <- lapply(res, function(u) {
        as_mtx(u[[1]])
      })
      mat <- listntvar[[1]]
      if (length(listntvar) > 1) {
        for (k in 2:length(listntvar)) {
          mat <- listntvar[[k]] + mat
        }
      }
      ntvar <- as_mtx(mat) + diag(rep(1, nlig))
    }
    dis <- lapply(res, function(u) {
      as_mtx(u[[2]])
    })
    mat <- dis[[1]]^2
    if (length(dis) > 1) {
      for (k in 2:length(dis)) {
        mat <- dis[[k]]^2 + mat
      }
    }
    if (!napres) {
      disglobal <- sqrt(mat / nbvar)
    } else {
      disglobal <- as.dist(sqrt(as_mtx(mat) / ntvar))
    }
    attributes(disglobal)$Labels <- row.names(as_mtx(disglobal))
    disglobal
  }

#' @keywords internal
vartype_mixedvar_distance <- function(x) {
  res <- if (is_atomic(x)) {
    if (is_true(is_discrete_distribution(x))) {
      df <- bind_cols(y = sample(1:0, length(x), replace = T), x = x)

      nd <-
        length(dectdiscrete(
          .data = df,
          outcome = y,
          threshold_num = .1
        ))
      if (nd == 1) {
        if (n_unique(x) <= 2) {
          "dicotomica"
        } else {
          "nominal"
        }
      } else {
        "ordinal"
      }
    } else if (is.double(x)) {
      if (is_true(all(is_prob(x)))) {
        "probability"
      } else {
        "quantitative"
      }
    } else if (is_fctchr(x)) {
      if (n_unique(x) <= 2) {
        "dicotomica"
      } else {
        "nominal"
      }
    } else {
      "error"
    }
  } else {
    "error"
  }

  res
}

#' @keywords internal
prep_mixedvar_distance <- function(df) {
  util_addfactor <- function(x) {
    blocks <- x$blo
    nlig <- length(x$lw)
    nblo <- length(x$blo)
    rowname <- attr(x, "rownames")
    colname <- attr(x, "colnames")
    blocname <- attr(x, "tabnames")
    w <- cbind.data.frame(
      gl(nblo, nlig, labels = blocname),
      factor(rep(1:nlig, nblo), labels = rowname)
    )
    names(w) <- c("T", "L")
    x$TL <- w
    w <- NULL
    for (i in 1:nblo) {
      w <- c(w, 1:blocks[i])
    }
    w <-
      cbind.data.frame(
        factor(rep(1:nblo, blocks), labels = blocname),
        factor(colname)
      )
    names(w) <- c("T", "C")
    x$TC <- w
    w <-
      cbind.data.frame(gl(nblo, 4, labels = blocname), factor(rep(
        1:4,
        nblo
      )))
    names(w) <- c("T", "4")
    x$T4 <- w
    x
  }

  vtype <- vapply(df, vartype_mixedvar_distance, character(1))

  vtype <- sort(vtype)
  df <- df[, names(vtype)]

  blocks <- vaggr(
    x = cumcount(vtype),
    .group = vtype,
    .fn = max
  )
  tabnames <- unique(vtype)
  nblo <- n_unique(vtype)
  rnm <- row.names(df)
  cnm <- colnames(df)

  w_row <- rep(1, nrow(df)) / nrow(df)
  w_col <- rep(1, ncol(df))

  indica <- as_fct(rep(1:nblo, blocks))
  res <- list()
  for (i in 1:nblo) {
    res[[i]] <- df[, indica == i]
    rownames(res[[i]]) <- rnm
    colnames(res[[i]]) <- cnm[indica == i]
  }
  names(res) <- tabnames
  res$lw <- w_row
  res$cw <- w_col
  res$blo <- blocks
  attr(res, "colnames") <- cnm
  attr(res, "rownames") <- rnm
  attr(res, "tabnames") <- names(res$blo)
  res <- util_addfactor(res)
  res$call <- match.call()

  res
}

#' @keywords internal
treatment_mixedvar_distance <- function(type_var,
                                        df,
                                        napres,
                                        blocs,
                                        diag = FALSE,
                                        upper = FALSE,
                                        tol = 1e-8,
                                        scale_method = c("range", "sd", "none")) {
  fn <- switch(type_var,
               ordinal = function(df,
                                  napres,
                                  blocs,
                                  diag = FALSE,
                                  upper = FALSE,
                                  tol = 1e-8,
                                  scale_method = c("range", "sd", "none")) {
                 transrank <- function(x) {
                   rank(x, na.last = "keep")
                 }
                 dist_impl <- function(df,
                                       diag = FALSE,
                                       upper = FALSE,
                                       tol = 1e-8) {
                   nlig <- nrow(df)

                   mat <- matrix(0, nlig, nlig)
                   index <- cbind(
                     col(mat)[col(mat) < row(mat)],
                     row(mat)[col(mat) < row(mat)]
                   )
                   d_names <- row.names(df)

                   fun1.QNA <-
                     function(vect, d_names, diag, upper, nlig) {
                       fun2.QNA <- function(u) {
                         ((vect[u[1]] - vect[u[2]])^2)
                       }
                       d <- unlist(apply(index, 1, fun2.QNA))
                       attr(d, "Size") <- nlig
                       attr(d, "Labels") <- d_names
                       attr(d, "Diag") <- diag
                       attr(d, "Upper") <- upper
                       attr(d, "method") <- "ordinal"
                       attr(d, "call") <- match.call()
                       class(d) <- "dist"
                       d
                     }


                   if (ncol(df) == 1) {
                     lis <- list(df[, 1])
                   } else {
                     lis <- as_l(df)
                   }
                   listdis <-
                     lapply(
                       lis,
                       fun1.QNA,
                       d_names = d_names,
                       diag = diag,
                       upper = upper,
                       nlig = nlig
                     )
                   listmat <- lapply(listdis, as_mtx)
                   funfin1.QNA <- function(u) {
                     u[!is.na(u)] <- 1
                     u[is.na(u)] <- 0
                     u
                   }
                   interm <- lapply(listmat, funfin1.QNA)
                   mat <- interm[[1]]
                   if (length(interm) > 1) {
                     for (k in 2:length(interm)) {
                       mat <- interm[[k]] + mat
                     }
                   }
                   funfin2.QNA <- function(u) {
                     u[is.na(u)] <- 0
                     u
                   }
                   res <- lapply(listdis, funfin2.QNA)
                   mat <- res[[1]]
                   if (length(res) > 1) {
                     for (k in 2:length(res)) {
                       mat <- res[[k]] + mat
                     }
                   }
                   thedis <- mat
                   thedis[thedis < tol] <- 0
                   sqrt(thedis)
                 }
                 df <- apply(df, 2, transrank)
                 cmax <- apply(df, 2, max, na.rm = TRUE)
                 cmin <- apply(df, 2, min, na.rm = TRUE)
                 df <- as_df(scale(df,
                                   center = cmin,
                                   scale = (cmax - cmin)
                 ))
                 thedis <- dist_impl(df,
                                     diag = diag,
                                     upper = upper,
                                     tol = tol
                 )

                 nbvar <- ncol(df)
                 if (napres) {
                   ntvar <- matrix(ncol(df), nrow(df), nrow(df))
                 }

                 res <- if (!napres) {
                   list(nbvar, thedis)
                 } else {
                   list(ntvar, thedis)
                 }
                 res
               },
               quantitative = function(df,
                                       napres,
                                       blocs,
                                       diag = FALSE,
                                       upper = FALSE,
                                       tol = 1e-8,
                                       scale_method = c("range", "sd", "none")) {
                 scale_method <- match.arg(scale_method)

                 nlig <- nrow(df)

                 if (scale_method[1] == "sd") {
                   df <- as_df(center_scale(df))
                 }
                 if (scale_method[1] == "range") {
                   cmax <- apply(df, 2, max, na.rm = TRUE)
                   cmin <- apply(df, 2, min, na.rm = TRUE)
                   df <- as_df(center_scale(
                     df,
                     center = cmin,
                     scale = ifelse((cmax - cmin) < tol, 1, cmax -
                                      cmin)
                   ))
                 }

                 mat <- matrix(0, nlig, nlig)
                 index <- cbind(
                   col(mat)[col(mat) < row(mat)],
                   row(mat)[col(mat) < row(mat)]
                 )
                 d_names <- row.names(df)

                 fun1.QNA <-
                   function(vect, d_names, diag, upper, nlig) {
                     fun2.QNA <- function(u) {
                       ((vect[u[1]] - vect[u[2]])^2)
                     }
                     d <- unlist(apply(index, 1, fun2.QNA))
                     attr(d, "Size") <- nlig
                     attr(d, "Labels") <- d_names
                     attr(d, "Diag") <- diag
                     attr(d, "Upper") <- upper
                     attr(d, "method") <- "quantitative"
                     attr(d, "call") <- match.call()
                     class(d) <- "dist"
                     d
                   }


                 if (ncol(df) == 1) {
                   lis <- list(df[, 1])
                 } else {
                   lis <- as_l(df)
                 }
                 listdis <-
                   lapply(
                     lis,
                     fun1.QNA,
                     d_names = d_names,
                     diag = diag,
                     upper = upper,
                     nlig = nlig
                   )
                 listmat <- lapply(listdis, as_mtx)
                 funfin1.QNA <- function(u) {
                   u[!is.na(u)] <- 1
                   u[is.na(u)] <- 0
                   u
                 }
                 interm <- lapply(listmat, funfin1.QNA)
                 mat <- interm[[1]]
                 if (length(interm) > 1) {
                   for (k in 2:length(interm)) {
                     mat <- interm[[k]] + mat
                   }
                 }
                 funfin2.QNA <- function(u) {
                   u[is.na(u)] <- 0
                   u
                 }
                 res <- lapply(listdis, funfin2.QNA)
                 mat <- res[[1]]
                 if (length(res) > 1) {
                   for (k in 2:length(res)) {
                     mat <- res[[k]] + mat
                   }
                 }
                 thedis <- mat
                 thedis[thedis < tol] <- 0
                 nbvar <- ncol(df)
                 if (napres) {
                   ntvar <- matrix(ncol(df), nrow(df), nrow(df))
                 }
                 thedis <- sqrt(thedis)
                 res <- if (!napres) {
                   list(nbvar, thedis)
                 } else {
                   list(ntvar, thedis)
                 }
                 res
               },
               dicotomica = function(df,
                                     napres,
                                     blocs,
                                     diag = FALSE,
                                     upper = FALSE,
                                     tol = 1e-8,
                                     scale_method = c("range", "sd", "none")) {
                 verif <- function(x) {
                   lapply(x, function(u) {
                     if (!is.factor(u)) {
                       if (!is.character(u)) {
                         cat_stop("Incorrect definition of the nominal variables")
                       }
                     }
                   })
                   invisible()
                 }
                 verif(df)
                 dist_fun <- function(u) {
                   m <- model.matrix(~ -1 + as_fct(u))
                   dist(m) / sqrt(2)
                 }
                 lis <- as_l(df)
                 res <- lapply(lis, dist_fun)
                 mat <- res[[1]]
                 if (length(res) > 1) {
                   for (k in 2:length(res)) {
                     mat <- res[[k]] + mat
                   }
                 }
                 thedis <- mat
                 thedis[thedis < tol] <- 0
                 thedis <- sqrt(thedis)

                 attr(thedis, "Size") <- nrow(df)
                 attr(thedis, "Labels") <- row.names(df)
                 attr(thedis, "Diag") <- diag
                 attr(thedis, "Upper") <- upper
                 attr(thedis, "method") <- "dicotomica"
                 attr(thedis, "call") <- match.call()
                 class(thedis) <- "dist"

                 ntvar <- mat
                 list(ntvar, thedis)
               },
               nominal = function(df,
                                  napres,
                                  blocs,
                                  diag = FALSE,
                                  upper = FALSE,
                                  tol = 1e-8,
                                  scale_method = c("range", "sd", "none")) {
                 verif <- function(x) {
                   lapply(x, function(u) {
                     if (!is.factor(u)) {
                       if (!is.character(u)) {
                         cat_stop("Incorrect definition of the nominal variables")
                       }
                     }
                   })
                   invisible()
                 }
                 verif(df)

                 dist_fun <- function(u) {
                   m <- model.matrix(~ -1 + as_fct(u))
                   dist(m) / sqrt(2)
                 }
                 lis <- as_l(df)
                 res <- lapply(lis, dist_fun)
                 mat <- res[[1]]

                 if (length(res) > 1) {
                   for (k in 2:length(res)) {
                     mat <- res[[k]] + mat
                   }
                 }
                 thedis <- mat
                 thedis[thedis < tol] <- 0
                 thedis <- sqrt(thedis)

                 attr(thedis, "Size") <- nrow(df)
                 attr(thedis, "Labels") <- row.names(df)
                 attr(thedis, "Diag") <- diag
                 attr(thedis, "Upper") <- upper
                 attr(thedis, "method") <- "nominal"
                 attr(thedis, "call") <- match.call()
                 class(thedis) <- "dist"

                 nbvar <- ncol(df)
                 if (napres) {
                   ntvar <- matrix(ncol(df), nrow(df), nrow(df))
                 }

                 res <- if (!napres) {
                   list(nbvar, thedis)
                 } else {
                   list(ntvar, thedis)
                 }
                 res
               },
               probability = function(df,
                                      napres,
                                      diag = FALSE,
                                      upper = FALSE,
                                      tol = 1e-8,
                                      blocs,
                                      scale_method = c("range", "sd", "none")) {
                 dist_impl <- function(df,
                                       nlig,
                                       diag = FALSE,
                                       upper = FALSE) {
                   d <- matrix(0, nlig, nlig)

                   d_names <- row.names(df)
                   df <- as_mtx(df)

                   index <-
                     cbind(col(d)[col(d) < row(d)], row(d)[col(d) < row(d)])

                   thedis <-
                     unlist(apply(index, 1, function(x) {
                       p <- df[x[1], ]
                       q <- df[x[2], ]
                       sum(abs(p - q)) / 2
                     }))

                   attr(thedis, "Size") <- nlig
                   attr(thedis, "Labels") <- d_names
                   attr(thedis, "Diag") <- diag
                   attr(thedis, "Upper") <- upper
                   attr(thedis, "method") <- "probability"
                   attr(thedis, "call") <- match.call()
                   class(thedis) <- "dist"
                   thedis
                 }


                 nlig <- nrow(df)

                 res <-
                   lapply(df,
                          dist_impl,
                          nlig = nlig,
                          diag = diag,
                          upper = upper
                   )

                 mat <- res[[1]]
                 if (length(res) > 1) {
                   for (k in 2:length(res)) {
                     mat <- res[[k]] + mat
                   }
                 }
                 thedis <- mat
                 thedis[thedis < tol] <- 0
                 thedis <- sqrt(thedis)
                 nbvar <- length(blocs)
                 if (napres) {
                   ntvar <- matrix(length(blocs), nrow(df), nrow(df))
                 }
                 res <- if (!napres) {
                   list(nbvar, thedis)
                 } else {
                   list(ntvar, thedis)
                 }
                 res
               }
  )

  fn(
    df = df,
    napres = napres,
    blocs = blocs,
    diag = diag,
    upper = upper,
    tol = tol,
    scale_method = scale_method
  )
}




# distancia en metros desde unidad de grados (latlon) ---------------------

#' dist_pairwisedata
#'
#' @export
#' @name spatials
#' @rdname spatials

dist_pairwisedata <- function(x,
                              y,
                              method = c(
                                "euclidean",
                                "maximum",
                                "manhattan",
                                "canberra",
                                "binary",
                                "minkowski"
                              ),
                              by_row = FALSE,
                              p_mnkw = 2,
                              is_geo = TRUE) {
  matrix_dist <-
    function(x,
             y,
             method = c(
               "euclidean",
               "maximum",
               "manhattan",
               "canberra",
               "binary",
               "minkowski"
             ),
             p_mnkw) {
      matrix_dist_euclidean <- function(x, y) {
        if (ncol(x) != ncol(y)) {
          cat_stop("'x' and 'y' must have the same number of columns")
        }
        z <- matrix(0, nrow = nrow(x), ncol = nrow(y))
        for (k in 1:nrow(y)) {
          z[, k] <- sqrt(colSums((t(x) - y[k, ])^2))
        }
        z
      }
      matrix_dist_max <- function(x, y) {
        if (ncol(x) != ncol(y)) {
          cat_stop("'x' and 'y' must have the same number of columns")
        }
        z <- matrix(0, nrow = nrow(x), ncol = nrow(y))
        for (k in 1:nrow(y)) {
          z[, k] <- apply(abs(t(x) - y[k, ]), 2, max)
        }
        z
      }
      matrix_dist_manhattan <- function(x, y) {
        if (ncol(x) != ncol(y)) {
          cat_stop("'x' and 'y' must have the same number of columns")
        }
        z <- matrix(0, nrow = nrow(x), ncol = nrow(y))
        for (k in 1:nrow(y)) {
          z[, k] <- colSums(abs(t(x) - y[k, ]))
        }
        z
      }
      matrix_dist_canberra <- function(x, y) {
        if (ncol(x) != ncol(y)) {
          cat_stop("'x' and 'y' must have the same number of columns")
        }
        z <- matrix(0, nrow = nrow(x), ncol = nrow(y))
        tx <- t(x)
        for (k in 1:nrow(y)) {
          d <- abs(tx - y[k, ])
          s <- abs(tx) + abs(y[k, ])
          q <- d / s
          q[s < .Machine$double.eps] <- 0
          z[, k] <-
            colSums(q) * ncol(x) / colSums(s > .Machine$double.eps)
        }
        z
      }
      matrix_dist_jaccard <- function(x, y) {
        if (ncol(x) != ncol(y)) {
          cat_stop("'x' and 'y' must have the same number of columns")
        }
        xc <- x %*% t(y)
        nenner <-
          matrix(rowSums(x),
                 nrow = nrow(x),
                 ncol = nrow(y)
          ) +
          matrix(
            rowSums(y),
            nrow = nrow(x),
            ncol = nrow(y),
            byrow = TRUE
          ) - xc
        z <- 1 - xc / nenner
        z[nenner < sqrt(.Machine$double.eps)] <- 0
        z
      }
      matrix_dist_minkowski <- function(x, y, p = 2) {
        if (ncol(x) != ncol(y)) {
          cat_stop("'x' and 'y' must have the same number of columns")
        }
        z <- matrix(0, nrow = nrow(x), ncol = nrow(y))
        for (k in 1:nrow(y)) {
          z[, k] <- colSums(abs(t(x) - y[k, ])^p)^(1 / p)
        }
        z
      }



      if (any(is.na(x)) || any(is.na(y))) {
        cat_stop("Cannot handle missing values!")
      }
      x <- as(x, "matrix")
      if (is.vector(y) && (length(y) <= ncol(x))) {
        y <- matrix(y, nrow = 1, ncol = ncol(x))
      } else {
        y <- as(y, "matrix")
      }
      method <- match.arg(method)
      z <-
        switch(method,
               euclidean = matrix_dist_euclidean(x, y),
               maximum = matrix_dist_max(x, y),
               manhattan = matrix_dist_manhattan(x, y),
               canberra = matrix_dist_canberra(x, y),
               binary = matrix_dist_jaccard(x != 0, y != 0),
               minkowski = matrix_dist_minkowski(x, y, p = p_mnkw)
        )
      rownames(z) <- rownames(x)
      colnames(z) <- rownames(y)
      z
    }
  byelement_dist <-
    function(x,
             y,
             method = c("euclidean", "manhattan", "canberra", "minkowski"),
             p_mnkw) {
      byelement_dist_euclidean <- function(x, y) {
        if (!identical(dim(x), dim(y))) {
          cat_stop("'x' and 'y' must have the same dimension")
        }
        x <- as_mtx(x)
        y <- as_mtx(y)

        .un(sqrt(rowSums((x - y)^2)))
      }
      byelement_dist_manhattan <- function(x, y) {
        if (!identical(dim(x), dim(y))) {
          cat_stop("'x' and 'y' must have the same dimension")
        }
        x <- as_mtx(x)
        y <- as_mtx(y)

        .un(rowSums(abs(x - y)))
      }
      byelement_dist_canberra <- function(x, y) {
        if (!identical(dim(x), dim(y))) {
          cat_stop("'x' and 'y' must have the same dimension")
        }
        x <- as_mtx(x)
        y <- as_mtx(y)

        d <- rowSums(abs(x - y))
        s <- rowSums((abs(x) + abs(y)))
        q <- d / s
        q[s < .Machine$double.eps] <- 0
        .un(q)
      }
      byelement_dist_minkowski <- function(x, y, p = 2) {
        if (!identical(dim(x), dim(y))) {
          cat_stop("'x' and 'y' must have the same dimension")
        }
        x <- as_mtx(x)
        y <- as_mtx(y)

        .un(rowSums(abs(x - y)^p)^(1 / p))
      }

      method <- match.arg(method, several.ok = F)

      if (any(is.na(x)) || any(is.na(y))) {
        cat_stop("Cannot handle missing values!")
      }
      x <- as(x, "matrix")
      if (is.vector(y) && (length(y) <= ncol(x))) {
        y <- matrix(y, nrow = 1, ncol = ncol(x))
      } else {
        y <- as(y, "matrix")
      }


      z <-
        switch(method,
               euclidean = byelement_dist_euclidean(x, y),
               manhattan = byelement_dist_manhattan(x, y),
               canberra = byelement_dist_canberra(x, y),
               minkowski = byelement_dist_minkowski(x, y, p = p_mnkw)
        )
      names(z) <- rownames(y)
      z
    }


  if (is_geo) {
    x <- wgs_to_utm(x)
    y <- wgs_to_utm(y)
  }
  method <- match.arg(method, several.ok = F)
  out <- if (by_row) {
    byelement_dist(x, y, method, p_mnkw)
  } else {
    matrix_dist(x, y, method, p_mnkw)
  }

  out
}

#' @keywords  internal
wgs_to_utm <- function(x) {
  x <- as_mtx(x)
  if (is_longlat(x)) {
    x <-
      sf::sf_project(
        pts = x,
        from = st_crs(4326),
        to = st_crs(31979)
      )
  }
  x
}

#' apprx_nneighbor
#'
#' @export
#' @name spatials
#' @rdname spatials
apprx_nneighbor <- function(x, y = NULL, neighbors = 1) {
  x <- as_mtx(x)
  y_emp <- is_empty(y)
  if (y_emp) {
    y <- x
    neighbors <- neighbors + 1
  }
  y <- as_mtx(y)
  if (!is_longlat(x)) {
    cat_stop("x no es longlat")
    return(NULL)
  }
  if (!is_longlat(y)) {
    cat_stop("y no es longlat")
    return(NULL)
  }

  x <-
    sf::sf_project(
      pts = x,
      from = st_crs(4326),
      to = st_crs(31979)
    )
  y <-
    sf::sf_project(
      pts = y,
      from = st_crs(4326),
      to = st_crs(31979)
    )
  d <- RANN::nn2(data = x, query = y, k = neighbors)
  names(d) <- c("id", "dist")

  if (y_emp) {
    if (neighbors == 2) {
      d$id <- as_mtx(d$id[, -1])
      d$dist <- as.matrix(d$dist[, -1])
    } else if (neighbors > 2) {
      d$id <- d$id[, -1]
      d$dist <- d$dist[, -1]
    } else {
      return(NULL)
    }
  }


  d
}


#' distancia en metros desde unidad de grados latlon
#'
#' @export
#' @name spatials
#' @rdname spatials
distance_degree2metre <- function(data, ...) {
  UseMethod("distance_degree2metre")
}

#' @export
distance_degree2metre.default <- function(data, ...) {
  generic_default()
}

#' @export
distance_degree2metre.sf <-
  function(data,
           method = "euclidean",
           diag = FALSE,
           upper = FALSE) {

    if(!is_longlat(data)){
      cat_stop("no es longlat")
      return(NULL)
    }

    sf_column <- attributes(data)$sf_column
    crs_from <-st_crs(attributes(data[[sf_column]])$crs)


    data_dist <- data %>%
      st_coordinates() %>%
      as.matrix() %>%
      sf::sf_project(
        from = crs_from,
        to = st_crs(31979)
      )
    colnames(data_dist) <- c("X", "Y")
    dist(data_dist,
         method = method,
         diag = diag,
         upper = upper
    )
  }

#' @export
distance_degree2metre.data.frame <-
  function(data,
           x,
           y,
           method = "euclidean",
           diag = FALSE,
           upper = FALSE) {
    x <- rlang::enexpr(x)
    y <- rlang::enexpr(y)


    if (!eval_expr(is_longlat(x = !!data, lon = !!x, lat = !!y))) {
      cat_stop("no es longlat")
      return(NULL)
    }

    data_dist <- data %>%
      dplyr::select(!!x, !!y) %>%
      as.matrix() %>%
      sf::sf_project(
        from = st_crs(4326),
        to = st_crs(31979)
      )
    colnames(data_dist) <- c("X", "Y")
    dist(data_dist,
         method = method,
         diag = diag,
         upper = upper
    )
  }



# filtro por minima distancia ---------------------------------------------

#' filtro por minima distancia
#'
#' @export
#' @name spatials
#' @rdname spatials
mindistance_degree2metre <- function(data, ...) {
  UseMethod("mindistance_degree2metre")
}

#' @export
mindistance_degree2metre.default <- function(data, ...) {
  generic_default()
}

#' @export
mindistance_degree2metre.data.frame <- function(data,
                                                x,
                                                y,
                                                min_dist=NULL,
                                                method = "euclidean",
                                                diag = FALSE,
                                                upper = FALSE) {
  if (is_emptyna(min_dist)) {
    return(NULL)
  }
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)

  dist_call <- match.call()
  dist_call[[1L]] <- quote(distance_degree2metre)
  dist_call$min_dist <- NULL
  dist_call$data <- data
  dist <- eval(dist_call)
  dist_hclust <- hclust(d = dist, method = "complete")
  datasplit <- split(data, cutree(dist_hclust, h = min_dist))
  datasplit_centers <- map(datasplit, ~ .x %>%
                             dplyr::select(!!x, !!y) %>%
                             summarise_all(mean))
  for (i in seq_along(datasplit)) {
    if (nrow(datasplit[[i]]) == 2) {
      datasplit[[i]] <- sample_n(tbl = datasplit[[i]],1)
    } else if (nrow(datasplit[[i]]) > 2) {
      tmpdist <-  RANN::nn2(data = datasplit_centers[[i]],
                            query = dplyr::select(datasplit[[i]], !!x, !!y))$nn.dists
      tmpmin <- which.min(tmpdist)

      datasplit[[i]] <- datasplit[[i]][tmpmin,]
    }
  }

  bind_rows(datasplit)

}


#' @export
mindistance_degree2metre.sf <- function(data,
                                        min_dist,
                                        method = "euclidean",
                                        diag = FALSE,
                                        upper = FALSE) {

  sf_column <- attributes(data)$sf_column

  if (is_emptyna(min_dist)) {
    return(NULL)
  }
  dist_call <- match.call()
  dist_call[[1L]] <- quote(distance_degree2metre)
  dist_call$min_dist <- NULL
  dist_call$data <- data
  dist <- eval(dist_call)
  dist_hclust <- hclust(d = dist, method = "complete")
  datasplit <- split(data, cutree(dist_hclust, h = min_dist))
  datasplit_centers <- map(
    datasplit,
    ~ .x %>%
      st_coordinates() %>%
      as_tbl() %>%
      summarise_all(mean)
  )
  for (i in seq_along(datasplit)) {
    if (nrow(datasplit[[i]]) == 2) {
      datasplit[[i]] <- sample_n(tbl = datasplit[[i]], 1)
    } else if (nrow(datasplit[[i]]) > 2) {
      tmpdist <- RANN::nn2(
        data = datasplit_centers[[i]],
        query = as_tbl(st_coordinates(datasplit[[i]]))
      )$nn.dists
      tmpmin <- which.min(tmpdist)

      datasplit[[i]] <- datasplit[[i]][tmpmin, ]
    }
  }

  new_data <- bind_rows(datasplit)
  st_set_geometry(x = new_data,value = new_data[[sf_column]])
}


# is_longlat --------------------------------------------------------------


#' is_longlat
#'
#' @export
#' @name spatials
#' @rdname spatials

is_longlat <- function(x, ...) {
  UseMethod("is_longlat")
}
#' @export
is_longlat.default <- function(x, ...) {
  generic_default()
}
#' @export
is_longlat.sf <-
  function(x) {
    crs <- st_crs(x)
    if (is.na(crs)) {
      NA
    } else {
      ret <- .Call("_sf_CPL_crs_parameters", PACKAGE = "sf", crs)$IsGeographic
      if (ret && inherits(x, c("sf", "sfc", "stars"))) {
        bb <- st_bbox(x)
        eps <- sqrt(.Machine$double.eps)
        if (all(!is.na(unclass(bb))) && (bb["xmin"] <
                                         (-180 - eps) ||
                                         bb["xmax"] > (360 + eps) ||
                                         bb["ymin"] < (-90 - eps) ||
                                         bb["ymax"] >
                                         (90 + eps))) {
          cat_warn("bounding box has potentially an invalid value range for longlat data")
        }
      }
      ret
    }
  }
#' @export
is_longlat.data.frame <-
  function(x, lon, lat) {
    lon <- rlang::enexpr(lon)
    lat <- rlang::enexpr(lat)

    xrang  <- pull(x, lon)
    xrang <- all(xrang < 360 & xrang > -180)
    yrang  <- pull(x, lat)
    yrang <- all(yrang < 90 & yrang > -90)

    all(c(xrang, yrang))
  }
#' @export
is_longlat.matrix <-
  function(x) {
    if (ncol(x) != 2) {
      return(FALSE)
    }
    xrang <- as_num(x[, 1])
    yrang <- as_num(x[, 2])
    xrang <- all(xrang < 360 & xrang > -180)
    yrang <- all(yrang < 90 & yrang > -90)
    all(c(xrang, yrang))
  }



# crear_iso -----------------------------------------------------------------------------------


#' make_iso
#'
#' @export
#' @name spatials
#' @rdname spatials

make_iso <-
  function(coords,
           ninit = 1000,
           breaks = c(3, 5, 7, 10, 15),
           crs = 31979,
           osrm_server = getOption("osrm.server","http://127.0.0.1:5000/")) {
    if(!is_empty(osrm_server)){
      options(osrm.server = osrm_server)
    }

    osrm_imputs <- function(from, to, crs, server) {
      list(
        src = st_as_sf(from, coords = colnames(from), crs = crs),
        dst = st_as_sf(to, coords = colnames(to), crs = crs),
        measure = "duration",
        osrm.server = server
      )
    }

    update_search_point <- function(x_new, x_all, lim, streets) {
      id_done <- unique(as_num(rownames(x_all)))
      id_expand <-
        unique(as_num(rownames(x_new[x_new$durations < lim + 1, ])))

      if (length(id_expand) == 0) {
        return(NULL)
      }

      id_expand <- streets[id_expand, 2:3]
      id_done <- streets[-id_done, 2:3]
      # idx <- list(done = unique(id_done), expand = unique(id_expand))

      idx <- RANN::nn2(
        data = id_done,
        query = id_expand,
        k = 250,
        radius = 1000
      )


      idx <- row.names(id_done[unique(as_num(idx$nn.idx)), ])

      idx <- unique(as_num(idx))

      streets[idx, 2:3]
    }

    streets <- mgalda::streets

    geom_xy_tmp <- function(pol) {
      pol <- sf::st_sfc(sf::st_polygon(x = list(pol), dim = "XY"))
      pol <- sf::st_sf(geom = pol, crs = 31979)
      sf::st_buffer(pol,
                    dist = 1,
                    nQuadSegs = 1,
                    endCapStyle = "SQUARE"
      ) %>%
        sf::st_simplify(dTolerance = 10, preserveTopology = T) %>%
        sf::st_buffer(dist = 50) %>%
        st_transform(crs = 4326)
    }

    lim <- max(breaks)

    n <- nrow(coords)

    coords <- wgs_to_utm(coords)
    print(1)

    coords_split <- lapply(seq_len(n), function(i) {
      data.frame(x = coords[i, 1], y = coords[i, 2])
    })

    dmax <- lim * (50 * 1000 / 60)

    res_s <-
      lapply(coords_split, function(x) {
        idx <- RANN::nn2(
          data = unique(as.matrix(streets[, 2:3])),
          query = unique(matrix(x, ncol = 2)),
          k = ninit,
          radius = dmax
        )
        idx <- unique(as_num(idx$nn.idx))
        streets[idx, 2:3]
      })

    out <-
      replicate(
        n = n,
        expr = data.frame(
          x = numeric(0),
          y = numeric(0),
          min = numeric(0)
        ),
        simplify = F
      )

    iter_ctrl <- TRUE

    print(1)
    while (any(iter_ctrl)) {
      iter_ctrl <- logical(0)

      cat_subtitle2(" new ")
      for (i in seq_along(res_s)) {
        if (is_true(nrow(res_s[[i]]) > 0)) {
          l_osrm <-
            osrm_imputs(
              from = coords_split[[i]],
              to = res_s[[i]],
              crs = crs,
              server = osrm_server
            )

          osrm_call <-
            rlang::call2(.fn = "osrmTable", !!!l_osrm, .ns = "osrm")

          osrm_call <- eval(osrm_call)

          if (any(as_num(osrm_call$durations) < lim + 1)) {
            tmp <-
              cbind(res_s[[i]], durations = as_num(osrm_call$durations))


            out[[i]] <- rbind(out[[i]], tmp)

            res_s[[i]] <-
              do_try(
                expr = update_search_point(
                  x_new = tmp,
                  x_all = out[[i]],
                  lim = lim,
                  streets = streets
                ),
                error = ""
              )
          } else {
            res_s[[i]] <- ""
          }
        }

        iter_ctrl <-
          c(iter_ctrl, ifelse(res_s[[i]] == "", FALSE, TRUE))
      }
    }

    out <- lapply(out, function(x) {
      exp_coors <- expand.grid(x = c(-10, 10), y = c(-10, 10))
      res <- list()
      for (i in 1:nrow(exp_coors)) {
        tmp <- x
        rownames(tmp) <- NULL
        tmp$x <- tmp$x + exp_coors$x[i]
        tmp$y <- tmp$y + exp_coors$y[i]
        res[[i]] <- tmp
      }

      do.call(rbind, res)
    })

    out <- lapply(out, function(x) {
      df <- group_by(x, x, y) %>%
        filter(durations <= lim) %>%
        mutate(
          x = round(x),
          y = round(y)
        ) %>%
        filter(durations == min(durations)) %>%
        ungroup() %>%
        mutate(ID = row_number())

      geom <-
        st_as_sf(df[, c("x", "y")], coords = c("x", "y"), crs = 31979)
      geom <- st_set_geometry(cbind(df, geom), geom$geometry)
      geom
    })

    res <- list()


    for (i in seq_along(out)) {
      tmp <- out[[i]]
      tmp <- st_set_geometry(tmp, NULL)

      geom_tmp <- list()
      for (j in seq_along(breaks)) {
        geom_tmp[[j]] <-
          as.matrix(tmp[tmp$durations <= breaks[j], c("x", "y")])

        geom_tmp[[j]] <-
          concaveman::concaveman(geom_tmp[[j]])

        geom_tmp[[j]] <- geom_xy_tmp(geom_tmp[[j]])
        if (j >= 2) {
          geom_tmp[[j]]$geom <-
            sf:::st_union(geom_tmp[[j - 1]]$geom, geom_tmp[[j]]$geom)
        }
        geom_tmp[[j]]$iso <- breaks[j]
      }
      geom_tmp <- do.call(rbind, geom_tmp)
      geom_tmp <- st_set_geometry(geom_tmp, geom_tmp$geom)
      res[[i]] <- geom_tmp
    }

    res
  }

#' @keywords internal
is_sfiso <-
  function(x) {
    if (is_sf(x)) {
      col_sf <-
        which(vapply(x, inherits, logical(1), what = c("sf", "sfc", "sfg")))


      if (length(col_sf) == 0) {
        NA
      } else {
        pol_class <- unique(as_chr(sf::st_geometry_type(x)))
        pol_class %in% c("GEOMETRYCOLLECTION", "POLYGON", "MULTIPOINT")
      }
    } else {
      NA
    }
  }

#' make_timedistance
#'
#' @export
#' @name spatials
#' @rdname spatials

make_timedistance <-
  function(from,
           to,
           osrm_server = "http://127.0.0.1:5000/",
           by_row = FALSE,
           max_time = NULL) {
    options(osrm.server = osrm_server)

    if (st_crs(from) != st_crs(to)) {
      cat_stop("'from' y 'to' tienen distinto crs")
    }
    crs <- st_crs(from)
    from_coords <- select(as_tibblecoords(from), quote(X), quote(Y))
    to_coords <- select(as_tibblecoords(to), quote(X), quote(Y))

    osrm_imputs <- function(from, to, crs, server) {
      list(
        src = st_as_sf(from, coords = colnames(from), crs = crs),
        dst = st_as_sf(to, coords = colnames(to), crs = crs),
        measure = "duration",
        osrm.server = server
      )
    }

    if (!by_row) {
      to_search <-
        lapply(
          split(from_coords, seq_len(nrow(from_coords))),
          osrm_imputs,
          to = to_coords,
          crs = crs,
          server = osrm_server
        )
    } else {
      if (nrow(from) != nrow(to)) {
        cat_stop("distinto numero de observaciones entre bases")
      } else {
        to_search <- mapply(
          FUN =
            osrm_imputs,
          from = split(from_coords, seq_len(nrow(from_coords))),
          to = split(to_coords, seq_len(nrow(to_coords))),
          MoreArgs = list(
            crs = crs,
            server = osrm_server
          ),
          SIMPLIFY = F
        )
      }
    }

    to_search <- lapply(to_search, function(x) {
      rlang::call2(.fn = "osrmTable", !!!x, .ns = "osrm")
    })

    res_osrm <-
      lapply(to_search, eval)


    if (!by_row) {
      res_osrm <-
        lapply(lapply(res_osrm, "[[", "durations"), function(x) {
          bind_cols(select(as_tibblecoords(to), !c(quote(X), quote(Y))), min = as_num(x))
        })

      res_osrm <-
        suppressall(map2(
          .x = split(select(as_tibblecoords(from), !c(quote(X), quote(Y))), seq_len(nrow(from))),
          .y = res_osrm,
          .f = bind_cols
        ))
    } else {
      res_osrm <- suppressall(lapply(seq_along(res_osrm), function(i) {
        f <- select(as_tibblecoords(from), !c(quote(X), quote(Y)))
        t <- select(as_tibblecoords(to), !c(quote(X), quote(Y)))
        bind_cols(slice(f, i), slice(t, i), min = as_num(res_osrm[[i]]$durations))
      }))
    }


    if (!is_empty(max_time)) {
      res_osrm <- lapply(res_osrm, function(x) {
        x[x$min <= max_time, ]
      })
    }

    do.call(rbind, res_osrm)
  }



# msic ----------------------------------------------------------------------------------------

#' @export
st_buffer_mt <-
  function(x,
           dist,
           nQuadSegs = 30,
           endCapStyle = "ROUND",
           joinStyle = "ROUND",
           mitreLimit = 1,
           singleSide = FALSE,
           ...) {
    actual_crs <- st_crs(x)
    x <- st_transform(x = x, crs = 31979)
    x <- sf::st_buffer(
      x = x,
      dist = dist,
      nQuadSegs = nQuadSegs,
      endCapStyle = endCapStyle,
      joinStyle = joinStyle,
      mitreLimit = mitreLimit,
      singleSide = singleSide,
      ...
    )
    st_transform(x = x, crs = actual_crs)
  }

#' @export
st_simplify_mt <- function(x, preserveTopology, dTolerance = 0) {
  actual_crs <- st_crs(x)
  x <- st_transform(x = x, crs = 31979)
  x <-
    sf::st_simplify(
      x = x,
      preserveTopology = preserveTopology,
      dTolerance = dTolerance
    )
  st_transform(x = x, crs = actual_crs)
}

#' @export
st_centrscoords <- function(x, ...) {
  if(!is_sf(x)){
    return(NULL)
  }

  dots <- force(match.call(expand.dots = F)$...)
  cntr_x <- suppressall(sf::st_centroid(x))
  tbl_x <- as_tbl(st_set_geometry(x,NULL))
  tbl_x<-dplyr::select(tbl_x, !!!dots)
  tbl_x<- bind_cols(tbl_x,as_tbl(st_coordinates(cntr_x)))
  tbl_x %>%
    group_by(!!!dots) %>%
    summarise(X = mean(X),Y= mean(Y)) %>%
    ungroup()
}

#' @export
st_summarise <- function(x) {
  st_summarise_impl <- function(x) {
    gcol <- st_col_name(x)
    x <- as_tibble_sf(x)
    x[[gcol]][!sf::st_is_valid(x[[gcol]])] <-
      sf::st_make_valid(x[[gcol]][!sf::st_is_valid(x[[gcol]])])
    x_tbl <- st_set_geometry(x, NULL)
    all_cols <- rlang::syms(names(x_tbl))
    x_geom <- dplyr::select(x, !!rlang::sym(gcol))
    xg <-
      group_indx(x_tbl, !!!all_cols) %>% dplyr::select(!c(!!!all_cols))

    gcol_sym <- rlang::sym(gcol)
    x_geom <- group_by(
      cbind(xg, sf::st_geometry(x)),
      .grp_indx
    ) %>% summarise(!!gcol_sym := sf::st_union(geometry))
    x_tbl <- distinct(bind_cols(xg, x_tbl))
    x_tbl <-
      inner_join(x_tbl, x_geom, by = ".grp_indx") %>%
      dplyr::select(!.grp_indx)
    st_set_geometry(x_tbl, value = x_tbl[[gcol]])
  }

  if (!is_sf(x)) {
    cat_stop("no es sf")
  }
  gcol <- st_col_name(x)
  x <- as_tibble_sf(x)
  vcols <- sf::st_is_valid(x[[gcol]])
  x[[gcol]][!vcols] <- sf::st_make_valid(x[[gcol]][!vcols])

  x_tbl <- st_set_geometry(x, NULL)
  all_cols <- rlang::syms(names(x_tbl))
  #x_geom <- dplyr::select(x, !!rlang::sym(gcol))
  xg <-
    group_indx(x_tbl, !!!all_cols) %>% dplyr::select(!c(!!!all_cols))

  tx <- table(xg$.grp_indx)
  tx <- as_int(names(tx)[tx > 1])
  x_ok <- x[!xg$.grp_indx %in% tx, ]
  x_sum <- x[xg$.grp_indx %in% tx, ]

  if (nrow(x_sum) > 0) {
    x_sum <- st_summarise_impl(x_sum)
    if (st_col_name(x_sum) != gcol) {
      x_sum <- as_tbl(x_sum)
      ngcol <- st_col_name(x_sum)
      names(x_sum)[names(x_sum) == ngcol] <- gcol
      x_sum[[ngcol]] <- NULL
      x_sum <- st_set_geometry(x_sum, x_sum[[gcol]])
    }
    x <- rbind(x_sum, x_ok)
  } else {
    x <- x_ok
  }

  x
}

#' @export
as_tibble_sf <- function(x) {
  st_set_geometry(as_tbl(x), sf::st_geometry(x))
}

#' @export
st_col_name <- function(x) {
  intfn <- function(x) {
    is_true(inherits(x, c("sfc", "sf")))
  }
  lgl <- vapply(x, intfn, logical(1))

  names(x)[lgl]
}

#' @export
as_tibblecoords <- function(x) {
  if(!is_sf(x)){
    return(NULL)
  }

  tbl_x <- as_tbl(st_set_geometry(x,NULL))
  bind_cols(tbl_x,as_tbl(st_coordinates(x)))

}

#' @export
st_removeholes <- function(x) {
  if (!is_sf(x)) {
    cat_stop("no es sf")
  }
  gcol <- st_col_name(x)
  x <- as_tibble_sf(x)
  vcols <- sf::st_is_valid(x[[gcol]])
  x[[gcol]][!vcols] <- sf::st_make_valid(x[[gcol]][!vcols])
  x <- rowindex(x)
  geotype <- unique(sf::st_geometry_type(x))


  if (!all(geotype %in% c("POLYGON", "MULTIPOLYGON"))) {
    cat_stop("no soporta clase { geotype }")
    cat_stop("")
  } else if (any(geotype == "MULTIPOLYGON")) {
    x <- sf::st_cast(sf::st_cast(x, "MULTIPOLYGON"), "POLYGON")
  }

  has_hole <- lengths(x[[gcol]]) > 1
  if (sum(has_hole) > 1) {
    x[[gcol]][has_hole] <- st_sfc(map(
      .x = x[[gcol]][has_hole],
      .f = ~ st_polygon(.x[1])
    ), crs = st_crs(x))
    if (nrow(x) > n_unique(x$row)) {
      tx <- table(x$row)
      tx <- as_int(names(tx)[tx > 1])
      x_ok <- x[!x$row %in% tx, ]
      x_sum <- x[x$row %in% tx, ]
      x_sum <- st_summarise(x_sum)
      x <- rbind(x_ok, x_sum)
      x <- x[order(x$row), ]
      x$row <- NULL
    } else {
      x$row <- NULL
    }
  }
  x
}

#' @export
st_bbox_polygon <-
  function(x) {
    sf::st_sf(geom = sf::st_as_sfc(sf::st_bbox(x)), crs = sf::st_crs(x))
  }

#' inner_join sf
#' @name spatials
#' @importFrom dplyr inner_join
#' @aliases inner_join.sf
#' @method inner_join sf
#' @export

inner_join.sf <-
  function(x,
           y,
           by = NULL,
           copy = FALSE,
           suffix = c(".x", ".y"),
           ...,
           keep = FALSE,
           na_matches = c("na", "never")) {
    gcol <- st_col_name(x)
    gtype <-
      as_chr(sf::st_geometry_type(x = x, by_geometry = F))
    x <- as_tbl(x)
    if (is_sf(y)) {
      cat_stop("y debe ser data.frame")
    }
    y <- as_tbl(y)
    new_x <- inner_join(
      x = x,
      y = y,
      by = by,
      copy = copy,
      suffix = c(".x", ".y"),
      keep = keep,
      na_matches = na_matches
    )

    sf::st_cast(st_set_geometry(x = new_x, value = new_x[[gcol]]), gtype)
  }


#' @export
calc_dots <- function(sf_data, col_names, n_per_dot) {
  if (is.null(col_names)) {
    col_names <- names(sf_data)
  }

  num_dots <- as_df(sf_data)
  num_dots <- num_dots[which(names(sf_data) %in% col_names)]

  num_dots <- num_dots / n_per_dot

  random_round <- function(x) {
    v <- as_int(x)
    r <- x - v
    test <- runif(length(r), 0.0, 1.0)
    add <- rep(as_int(0), length(r))
    add[r > test] <- as_int(1)
    value <- v + add
    ifelse(is.na(value) | value < 0, 0, value)
  }


  num_dots <- do.call("cbind", lapply(names(num_dots), function(x) {
    data <- random_round(unlist(num_dots[x]))
    df <- data.frame(data)
    names(df) <- x
    return(df)
  }))

  data <- lapply(names(num_dots), function(x) {
    dots_df <-
      sf::st_sample(sf_data, size = unlist(num_dots[x]), type = "random")
    dots_df <- st_coordinates(sf::st_cast(dots_df, "POINT"))
    dots_df <- as_df(dots_df)
    names(dots_df) <- c("lon", "lat")
    dots_df$variable <- x
    return(dots_df)
  })

  sf_dots <- do.call("rbind", data)
  sf_dots <- sf_dots[sample(1:nrow(sf_dots)), ]
  sf_dots
}
NULL
