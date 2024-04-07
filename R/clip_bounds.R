# clip_bounds core

#' rescalar outliers
#'
#' @name clip_bounds
#' @rdname clip_bounds
#'
#'
#' @param .data dataset
#' @param ... variables
#' @param cutoff toleracia .95
#' @param fn funcion base de la busqueda de limites:
#' "normal", "simple", "slide"
#' @param limite que limite se truncara: "ambos", "upper", "lower"
#' @param method "quantile", "dens", "knn", "maha", "box"
#' @param times veces dentro de una muestra
#' @param rep repeticiones de las muestras
#'
#' @return data rescalada de variables
#' @export
#'
#' @examples
#'
#'
#'
#' set.seed(7)
#' data <- datalearn$iris[,1:4]
#'
#' data <-purrr::map2_dfc(data,c(1,0,0,-1), function(x,y){
#'   x <- x+ runif(1,-10,10)
#'   mu <- mean(x)
#'   s <- sd(x)
#'
#'   if(y != 0){
#'     if(y == -1){
#'       rang <- c(.01,.1)
#'     } else if(y == 1){
#'       rang <- c(.9,.99)
#'     }
#'     nx<-unscale(x = qcauchy(runif(50,rang[1],rang[2]))+mu,center = 0,scale = s)
#'     nx <- sample(safe_vec(nx,"rm"),15)
#'   } else {
#'
#'     nx<-center_scale(x = qcauchy(runif(50,0,.1))+mu,center = 0,scale = s)
#'     nx<-c(nx,center_scale(x = 1-qcauchy(runif(50,0,.1))+mu,center = 0,scale = s))
#'     nx <- sample(safe_vec(nx,"rm"),20)
#'   }
#'
#'   x[sample(seq_row(data),length(nx))]  <- nx
#'   x
#' })
#'
#'
#' objs<-clip_bounds(
#'   .data = data,
#'   where(is_dblint),
#'   cutoff = 0.95,
#'   fn = "slide",
#'   limite = "lower",
#'   method = "box",
#'   times = 3,
#'   rep = 2
#' )
#'
#'
#' purrr::map2(data, predict(objs, data), ~ list(actual = range(.x), new = range(.y)))
#'
#'
#' objs<-clip_bounds(
#'   .data = data,
#'   where(is_dblint),
#'   cutoff = 0.95,
#'   fn = "normal",
#'   limite = "lower",
#'   method = "dens",
#'   times = 3,
#'   rep = 2
#' )
#'
#' purrr::map2(data, predict(objs, data), ~ list(actual = range(.x), new = range(.y)))
#'
#' objs<-clip_bounds(
#'   .data = data,
#'   petal_width,petal_length,
#'   cutoff = 0.95,
#'   fn = "slide",
#'   limite = "lower",
#'   method = "knn",
#'   times = 3,
#'   rep = 2
#' )
#' purrr::map2(data, predict(objs, data), ~ list(actual = range(.x), new = range(.y)))
#'
#' objs<-clip_bounds(
#'   .data = data,
#'   petal_width,
#'   cutoff = 0.95,
#'   fn = "normal",
#'   limite = "upper",
#'   method = "maha",
#'   times = 3,
#'   rep = 2
#' )
#'
#' purrr::map2(data, predict(objs, data), ~ list(actual = range(.x), new = range(.y)))
#'
#' objs<-clip_bounds(
#'   .data = data,
#'   where(is_dblint),
#'   cutoff = 0.95,
#'   fn = "simple",
#'   limite = "upper",
#'   method = "quantile",
#'   times = 3,
#'   rep = 2
#' )
#'
#' purrr::map2(data, predict(objs, data), ~ list(actual = range(.x), new = range(.y)))
#'
#' vec <- safe_vec(rcauchy(1000))
#' vec <- center_scale(x = vec,center = runif(1,-20,20),scale =runif(1,1,10))
#'
#'
#' clip_object(
#'   x = vec,
#'   cutoff = 0.95,
#'   fn = "simple",
#'   limite = "ambos",
#'   method = "box"
#' )
#'
#' clip_object(
#'   x = vec,
#'   cutoff = 0.95,
#'   fn = "normal",
#'   limite = "upper",
#'   method = "dens"
#' )
#'
#' range(vec)
#'
#' range(clip_bounds_vec(
#'   x = vec,
#'   cutoff = 0.95,
#'   fn = "normal",
#'   limite = "ambos",
#'   method = "knn"
#' ))
#'
#' range(clip_bounds_vec(
#'   x = vec,
#'   cutoff = 0.95,
#'   fn = "slide",
#'   limite = "lower",
#'   method = "maha",
#'   times = 3,
#'   rep = 2
#' ))
#'
#' range(clip_bounds_vec(
#'   x = vec,
#'   cutoff = 0.95,
#'   fn = "slide",
#'   limite = "lower",
#'   method = "quantile",
#'   times = 3,
#'   rep = 2
#' ))
#'
#' limits_bounds(.data = data, sepal_length)
#'
#' limits_bounds(.data = data, sepal_width)
#'
#' limits_object(x = vec)
#'
#' limits_object(x = vec)
#'
#'

#' @export
clip_bounds <- function(.data, ...) {
  UseMethod("clip_bounds")
}

#' @export
clip_bounds.default <-
  function(.data, ...) {
    generic_default()
  }


#' @export
clip_bounds.default <-
  function(.data, ...) {
    generic_default()
  }

#' @export
clip_bounds.data.frame <-
  function(.data,
           ...,
           cutoff = 0.95,
           fn = c("normal", "simple", "slide"),
           limite = c("ambos", "upper", "lower"),
           method = c("quantile", "dens", "knn", "maha", "box"),
           times = 3,
           rep = 3) {
    #dots <- capture_dots(as_expr = F)
    x <- dplyr::select(.data, ...)

    fn <- match.arg(fn)
    limite <- match.arg(limite)
    method <- match.arg(method)
    assert_data.frame(x = x)
    if (!is_prob(cutoff)) {
      cat_warn("!is_prob(cutoff)")
      return(x)
    }

    xlist <- as_l(x)
    xlist <- map(xlist, ~ kfolds_vec(.x, rep = rep, times = times))
    out <- lapply(xlist, function(w) {
      map(
        .x = w,
        .f = ~ clip_object(
          x = .x,
          cutoff = cutoff,
          fn = fn,
          limite = limite,
          method = method
        )
      )
    })

    out <-
      lapply(out, function(w, l, m, c, f) {
        w <- purrr::map_dfr(w, ~ as_tbl(.x)) %>%
          summarise_all(mean, na.rm = TRUE)

        if (l == "lower") {
          w$min_bound <- -Inf
        } else if (l == "upper") {
          w$max_bound <- Inf
        }
        w <- as_l(w)
        attr(w, "ws") <-
          case_when(
            fn == "normal" ~ "normal_bound",
            fn == "simple" ~ "out_bounds",
            fn == "slide" ~ "slide_bounds"
          )
        attr(w, "method") <- m
        attr(w, "limite") <- l
        attr(w, "cutoff") <- scales::percent_format()(c)
        class(w) <- c(class(w), "clip_bounds", "clip_object")
        w
      }, l = limite, m = method, c = cutoff, f = fn)

    class(out) <- c("clip_bounds", "clip_bounds_list")
    out


  }

#' @export
#' @rdname clip_bounds
clip_object <-
  function(x,
           cutoff = 0.95,
           fn = c("normal", "simple", "slide"),
           limite = c("ambos", "upper", "lower"),
           method = c("quantile", "dens", "knn", "maha", "box")) {
    check_inherits(x, "numeric")

    fn <- paste0(match.arg(fn),"_bounds")
    method <- match.arg(method)
    limite <- match.arg(limite)


    fnargs <-
      do_call(modify_args,list(x = str2lang(fn), call_args(match.call())))

    fnargs$x <- x

    if ("limite" %in% names(fnargs)) {
      fnargs$limite <- limite
    }

    if ("method" %in% names(fnargs)) {
      fnargs$method <- method
    }

    out <- do_try(do_call(fn, as_l(fnargs)))

    if (is_empty(out)) {
      out <- data.frame(min_bound = min(x,na.rm = TRUE),max_bound = max(x,na.rm = TRUE))
    }

    class(out) <- c(class(out), "clip_bounds", "clip_object")
    out
  }
#' @export
print.clip_bounds <- function(x) {
  if (any(class(x) == "clip_bounds_list")) {
    print_clip_bounds_list(x)
  } else {
    print_clip_object(x)
  }
}

print_clip_bounds_list <- function(x) {
  rng <-
    sapply(seq_along(x), function(i) {
      paste0(
        names(x)[i],
        ": [",
        ifelse(
          is.infinite(x[[i]]$min_bound),
          x[[i]]$min_bound,
          round_sign(x[[i]]$min_bound)
        ),
        "-",
        ifelse(
          is.infinite(x[[i]]$max_bound),
          x[[i]]$max_bound,
          round_sign(x[[i]]$max_bound)
        ),
        "]"
      )
    })

  cat_title_head("Clip Bounds")
  cli::cli_bullets(purrr::set_names(rng, rep("*", length(rng))))
}

print_clip_object <- function(x) {
  rng <-
    paste0(
      "[",
      ifelse(
        is.infinite(x$min_bound),
        x$min_bound,
        round_sign(x$min_bound)
      ),
      " - ",
      ifelse(
        is.infinite(x$max_bound),
        x$max_bound,
        round_sign(x$max_bound)
      ),
      "]"
    )


  cat_title_head("Clip Object")
  cat("    ", rng)
}



# limits_bounds core

#' @export
#' @rdname clip_bounds
limits_bounds <- function(.data, ...) {
  UseMethod("limits_bounds")
}

#' @export
#' @rdname clip_bounds
limits_bounds.default <-
  function(.data, ...) {
    generic_default()
  }

#' @export
#' @rdname clip_bounds
limits_bounds.data.frame <-
  function(.data, ...) {
    dataset <- dplyr::select(.data,...)

    obj <-
      map(.x = dataset, .f = ~ limits_object(x = .x))

    class(obj) <- c("limits_bounds", "limits_bounds_list")
    obj
  }

#' @export
#' @rdname clip_bounds
limits_object <-
  function (x) {
    if (!rlang::is_bare_numeric(x)) {
      cat_stop("calculo univariado")
    }
    bound <- as_dbl(range(x))
    out <- list(min_bound = min(bound), max_bound = max(bound))
    class(out) <- "limits_bounds"
    out
  }


#' @export
print.limits_bounds <- function(x) {
  if (any(class(x) == "limits_bounds_list")) {
    print_clip_bounds_list(x)
  } else {
    print_clip_object(x)
  }
}

# prediccion
#' @export
predict.clip_bounds <- function(object,
                                new_data = NULL) {
  if (any(class(object) == "clip_bounds_list")) {
    if (!is.data.frame(new_data)) {
      cat_stop("clase clip_bounds_list pronostica dfs")
    }

    new_data[, names(object)] <-
      purrr::map_dfc(
        .x = names(object),
        .f = function(x) {
          new_data[[x]] <- as_dbl(new_data[[x]])
          pred <-
            case_when(
              new_data[[x]] > object[[x]]$max_bound ~ object[[x]]$max_bound,
              new_data[[x]] < object[[x]]$min_bound ~ object[[x]]$min_bound,
              T ~ new_data[[x]]
            )

          pred <- as_num(pred)
          pred <- tibble(`:=`(!!x, pred))
        }
      )
  } else {
    if (is.data.frame(new_data)) {
      cat_stop("objectos de solo clase clip_bounds pronostican vectores")
    }

    new_data <- as_dbl(new_data)

    new_data <-
      case_when(
        new_data > object$max_bound ~ object$max_bound,
        new_data < object$min_bound ~ object$min_bound,
        T ~ new_data
      )
  }
  new_data
}

#' @export
predict.limits_bounds <- function(object,
                                  new_data = NULL) {
  if (any(class(object) == "limits_bounds_list")) {
    if (!is.data.frame(new_data)) {
      cat_stop("clase limits_bounds_list pronostica dfs")
    }

    new_data[, names(object)] <-
      purrr::map_dfc(
        .x = names(object),
        .f = function(x) {
          new_data[[x]] <- as_dbl(new_data[[x]])
          pred <-
            case_when(
              new_data[[x]] > object[[x]]$max_bound ~ object[[x]]$max_bound,
              new_data[[x]] < object[[x]]$min_bound ~ object[[x]]$min_bound,
              T ~ new_data[[x]]
            )

          pred <- as_num(pred)
          pred <- tibble(`:=`(!!x, pred))
        }
      )
  } else {
    if (is.data.frame(new_data)) {
      cat_stop("objectos de solo clase limits pronostican vectores")
    }

    new_data <- as_dbl(new_data)

    new_data <-
      case_when(
        new_data > object$max_bound ~ object$max_bound,
        new_data < object$min_bound ~ object$min_bound,
        T ~ new_data
      )
  }
  new_data
}

# vector
#' @export
clip_bounds_vec <-
  function(x,
           cutoff = 0.95,
           fn = c("normal", "simple", "slide"),
           limite = c("ambos", "upper", "lower"),
           method = c("quantile", "dens", "knn", "maha", "box"),
           times = 5,
           rep = 1) {
    assert_vector(x = x, .class = "numeric")
    fn <- match.arg(fn)
    limite <- match.arg(limite)
    clip_call <- match.call()
    clip_call[[1L]] <- quote(clip_object)
    clip_call$times <- NULL
    clip_call$rep <- NULL
    clip_call$x <- quote(y)

    c_agrs <- call_args(call = clip_call)
    c_agrs <- c_agrs[names(c_agrs) != "x"]
    for (i in names(c_agrs)) {
      if (is_call(clip_call[[i]]) | is_symname(clip_call[[i]])) {
        clip_call[[i]] <- eval.parent(clip_call[[i]])
      }
    }
    nfun <- rlang::new_function(args = alist(y = ), body = clip_call)

    xlist <- kfolds_vec(x = x, times = times, rep = rep)

    out <- lapply(xlist, nfun)
    out <- purrr::map_dfr(out, as_tbl) %>%
      summarise_all(mean, na.rm = TRUE)

    if (limite == "upper") {
      out$min_bound <- -Inf
    } else if (limite == "lower") {
      out$max_bound <- Inf
    }
    x <-
      case_when(
        x > out$max_bound ~ out$max_bound,
        x < out$min_bound ~ out$min_bound,
        T ~ x
      )
    x
  }

# internals

#' @keywords internal
out_box <- function(x) {
  x <- sort_filter_vec(x = x,n = 500)

  stats <- stats::fivenum(x, na.rm = TRUE)
  iqr <- diff(stats[c(2, 4)])
  loc <-
    which(x < (stats[2L] - 1.5 * iqr) |
            x > (stats[4L] + 1.5 * iqr))

  xout <- x[loc]

  xoutmax <- min(xout[xout > mean(x)])
  xoutmax <- ifelse(is.infinite(xoutmax), max(x), xoutmax)

  xoutmin <- max(xout[xout < mean(x)])
  xoutmin <- ifelse(is.infinite(xoutmin), min(x), xoutmin)
  preout <- range(x[-loc])


  data.frame(
    min_bound = mean(preout[1], xoutmin),
    max_bound = mean(preout[2], xoutmax)
  )
}

#' @keywords internal
out_quantile <- function(x, cutoff = 0.95) {
  x <- sort_filter_vec(x = x,n = 500)
  rout_teo <-
    qnorm(
      p = c(1 - cutoff, cutoff),
      mean = mean(x),
      sd = sd(x)
    )
  rout_emp <- quantile(x, c(1 - cutoff, cutoff))


  xoutmax <- mean(max(rout_teo), max(rout_emp))
  xoutmax <- ifelse(is.infinite(xoutmax), max(x), xoutmax)

  xoutmin <- mean(min(rout_teo), min(rout_emp))
  xoutmin <- ifelse(is.infinite(xoutmin), min(x), xoutmin)

  data.frame(
    min_bound = xoutmin,
    max_bound = xoutmax
  )
}

#' @keywords internal
out_maha <- function(x, cutoff = 0.95) {
  x <- sort_filter_vec(x = x,n = 500)
  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  } else {
    x <- as_mtx(x)
  }

  m <- mahalanobis(x, colMeans(x), cov(x))
  loc <- which(m > qchisq(cutoff, ncol(x)))

  xout <- x[loc]

  xoutmax <- min(xout[xout > mean(x)])
  xoutmax <- ifelse(is.infinite(xoutmax), max(x), xoutmax)

  xoutmin <- max(xout[xout < mean(x)])
  xoutmin <- ifelse(is.infinite(xoutmin), min(x), xoutmin)
  preout <- range(x[-loc])


  data.frame(
    min_bound = mean(preout[1], xoutmin),
    max_bound = mean(preout[2], xoutmax)
  )
}

#' @keywords internal
out_knn <- function(x, cutoff = 0.95) {
  x <- sort_filter_vec(x = x,n = 500)
  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  } else {
    x <- as_mtx(x)
  }

  k <- ncase(nrow(x))

  d <- RANN::nn2(data = x, k = k)$nn.dists[, k]

  loc <- which(d > quantile(d, cutoff))

  xout <- x[loc]

  xout
  preout <- x[-loc]

  xout <- xout %out% preout

  xoutmax <- min(xout[xout > mean(x)])
  xoutmax <- ifelse(is.infinite(xoutmax), max(x), xoutmax)

  xoutmin <- max(xout[xout < mean(x)])
  xoutmin <- ifelse(is.infinite(xoutmin), min(x), xoutmin)
  preout <- range(x[-loc])


  data.frame(
    min_bound = mean(preout[1], xoutmin),
    max_bound = mean(preout[2], xoutmax)
  )
}

#' @keywords internal
out_dens <- function(x, cutoff = 0.95) {
  x <- sort_filter_vec(x = x,n = 500)

  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  } else {
    x <- as_mtx(x)
  }

  d <- DDoutlier::RKOF(x, k = ncase(length(x)))

  loc <- which(d > quantile(d, cutoff))

  xout <- x[loc]

  xout
  preout <- x[-loc]

  xout <- xout %out% preout

  xoutmax <- min(xout[xout > mean(x)])
  xoutmax <- ifelse(is.infinite(xoutmax), max(x), xoutmax)

  xoutmin <- max(xout[xout < mean(x)])
  xoutmin <- ifelse(is.infinite(xoutmin), min(x), xoutmin)
  preout <- range(x[-loc])


  data.frame(
    min_bound = mean(preout[1], xoutmin),
    max_bound = mean(preout[2], xoutmax)
  )
}

simple_bounds <-
  function(x,
           method = c("quantile", "dens", "knn", "maha", "box"),
           cutoff = 0.95,
           limite = c("ambos", "upper", "lower")) {

    x<- x[!is_nonnum(x)]

    method <- match.arg(method)
    limite <- match.arg(limite)
    bound <- switch(method,
                    dens = out_dens(x, cutoff),
                    knn = out_knn(x, cutoff),
                    maha = out_maha(x, cutoff),
                    box = out_box(x),
                    quantile = out_quantile(x, cutoff)
    )

    if (limite == "upper") {
      bound$min_bound <- min(x)
    } else if (limite == "lower") {
      bound$max_bound <- max(x)
    }

    bound <-
      list(
        min_bound = bound$min_bound,
        max_bound = bound$max_bound
      )

    attr(bound, "bounds") <- "out_bounds"
    attr(bound, "method") <- method
    attr(bound, "limite") <- limite
    attr(bound, "cutoff") <- scales::percent_format()(cutoff)
    bound
  }

slide_bounds <-
  function(x,
           cutoff = .95,
           times = 2,
           rep = 1,
           limite = c("ambos", "upper", "lower")) {
    slide_bounds_impl <-
      function(x,
               cutoff = .95,
               times = 3,
               rep = 1,
               limite = c("ambos", "upper", "lower")) {
        limite <- match.arg(limite)

        get_slidebounds <-
          function(x, cutoff = 0.95) {
            check_inherits(x, "numeric")
            suppressall(gbound(x, cutoff))
          }

        get_concentric_list <- function(x, times = 2, rep = 1) {
          out_q <- c(0, .025, .05)

          boundlist <- c()

          for (i in out_q) {
            boundlist <-
              c(
                boundlist,
                kfolds_vec(
                  x %inside% quantile(x, c(i, 1 - i)),
                  times = times,
                  rep = rep
                )
              )
          }

          boundlist
        }

        bound <-
          summarise_all (purrr::map_dfr(
            get_concentric_list(x, times = times, rep = rep),
            ~ get_slidebounds(x = .x, cutoff = cutoff)
          ), mean)

        bound <-
          list(
            min_bound = bound$min_bound,
            max_bound = bound$max_bound
          )

        if (limite == "upper") {
          bound$min_bound <- -Inf
        } else if (limite == "lower") {
          bound$min_bound <- -Inf
        }
        bound
      }

    x<- x[!is_nonnum(x)]

    bound <-
      suppressWarnings(slide_bounds_impl(x, cutoff, times, rep, limite))

    attr(bound, "bounds") <- "slide_bounds"
    attr(bound, "limite") <- limite
    attr(bound, "times") <- times
    attr(bound, "rep") <- rep
    attr(bound, "cutoff") <- scales::percent_format()(cutoff)
    bound
  }

normal_bounds <-
  function(x,
           cutoff = 0.95,
           limite = c("ambos", "upper", "lower")) {

    x<- x[!is_nonnum(x)]
    limite <- match.arg(limite)

    bound <-
      suppressWarnings(summarise_all(gbound(x, cutoff), mean))

    if (limite == "upper") {
      bound$min_bound <- -Inf
    } else if (limite == "lower") {
      bound$min_bound <- -Inf
    }
    bound <-
      list(
        min_bound = bound$min_bound,
        max_bound = bound$max_bound
      )

    attr(bound, "bounds") <- "normal_bounds"
    attr(bound, "limite") <- limite
    attr(bound, "cutoff") <- scales::percent_format()(cutoff)
    bound
  }

gbound <- function(x, cutoff = .95) {
  x<- x[!is_nonnum(x)]

  arg_list <-
    list(
      x = x,
      cutoff = cutoff
    )

  l <-
    list(
      box = try(do.call(out_box, list(x = x)), silent = TRUE),
      quantile = try(do.call(out_quantile, arg_list), silent = TRUE),
      maha = try(do.call(out_maha, arg_list), silent = TRUE)
    )

  l <-
    bind_rows(l[!map_lgl(l, ~ inherits(.x, "try-error"))])
  l$min_bound <-
    ifelse(is_nonnum(l$min_bound), min(x[!is_nonnum(x)]), l$min_bound)
  l$max_bound <-
    ifelse(is_nonnum(l$max_bound), max(x[!is_nonnum(x)]), l$max_bound)
  l
}
