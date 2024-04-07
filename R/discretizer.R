#' discretizer
#'
#' @name discretizer
#' @rdname discretizer
#' @keywords internal
#'
#' @examples
#' set.seed(7)
#' data <- iris[, -5]
#' data <- dplyr::mutate_all(data, roll_noise) %>%
#' as_tbl()
#' object <- discrkknn(data, everything())
#' predict(object, data)
#'
#' data <- mgalda::datalearn$credit %>%
#'   tidyr::drop_na() %>%
#'   dplyr::select(status, where(is_dblint)) %>%
#'   dplyr::group_by(status) %>%
#'   sample_n(size = 200) %>%
#'   dplyr::ungroup()
#'
#' dectdiscrete(.data = data, outcome = status)
#'
#' data <- datalearn$housing_market
#'  data <- data[,are_dblint(data)]
#'
#' object <-
#'   discrete(x = data,outcome = sales_price)
#' do_discrete(x = data, outcome = sales_price)
#' predict(object, data)
#'
#' # data <- datalearn$wa_sales_price %>% tidyr::drop_na()
#'
#' object <-
#'   discrchi(.data = data, y = sales_price, where(is_dblint))
#' do_discrchi(.data = data, y = sales_price, where(is_dblint))
#' predict(object, data)
#'
#' object <-
#'   discrmdlp(.data = data, y = sales_price, where(is_dblint))
#' predict(object, data)
#' do_discrmdlp(.data = data, y = sales_price, where(is_dblint))
#'
#' object <-
#'   discrtopdown(.data = data, y = sales_price, where(is_dblint))
#' do_discrtopdown(.data = data, y = sales_price, where(is_dblint))
#' predict(object, data)
#'
NULL


# discrkknn -

#' @rdname discretizer
#' @export

discrkknn <- function(data, ...) {
  UseMethod("discrkknn")
}

#' @export
discrkknn.default <- function(data, ...) {
  generic_default()
}

#' @rdname discretizer
#' @export
discrkknn.data.frame <- function(data,
                                 ...,
                                 threshold = 0.1,
                                 times = 4,
                                 rep = 2,
                                 interval = c(3, 25)) {
  expr_wss <- interp(obj_ = quote(abs(mean(map_dbl(k, ~ 1 - .x$betweenss / .x$totss)) - threshold)),
                     threshold = threshold,
                     k = quote(map(dat, ~ kmeans(x = .x, centers = round(x))))
  )
  interval <- sort(interval)
  data <- dplyr::select(data, ...) %>% as_tbl()
  data
  data <- map(data[, are_dblint(data)], ~ nullattr(.x))
  data_lgl <- map_dbl(data, n_unique) > interval[1] + 1
  nms <- names(data)
  if (length(nms) == 0) {
    return(NULL)
  }
  res_out <-
    setNames(vector(mode = "list", length = length(nms)), nms)
  out <- list()

  for (i in nms) {
    if(data_lgl[i]){
      data_res <- data.frame(x = data[[i]])
      data_res <-
        eval_expr(kfolds(
          dataset = !!data_res,
          times = !!times,
          rep = !!rep
        )$splits,
        envir = env_curr()
        )
      data_res <- map(data_res, rsample::analysis)
      int_res <-
        do.call(rbind, map(.x = data_res, .f = ~ c(
          interval[1], min(interval[2], n_unique(.x$x) - 2)
        )))
      expr_dat <-
        eval_expr(substitute(!!expr_wss, list(dat = !!data_res)))
      int_res <-
        range(c(max(int_res[, 1], na.rm = T), min(int_res[, 2], na.rm = T)))
      fn_dat <-
        rlang::new_function(args = alist(x = ), body = expr_dat)
      out[[i]] <-
        do_try_ier(round(optimize(f = fn_dat, interval = int_res)$minimum))
    } else {
      out[[i]] <- n_unique(data[[i]])
    }
  }
  out <- out[!map_lgl(out, is_ier)]
  out <-
    purrr::imap(
      .x = out,
      .f = ~ do_try_ier(
        break_object(
          var = data[[.y]],
          num_breaks = .x,
          algorithm = "kmean"
        )$breaks
      )
    )
  out <- compact(out[!map_lgl(out, is_ier)])
  out <-
    map(out, ~ list(breaks = safe_vec(.x), labels = make_labels(safe_vec(.x))))
  class(out) <- "discrkknn"
  attr(out, "transformed") <- names(out)
  attr(out, "call") <- match.call()
  out
}


#' @export
predict.discrkknn <- function(object, new_data, type = c("factor","numeric")) {
  type <- match.arg(type)
  object <- object[map_lgl(object, ~ is_dblint(.x$breaks))]
  object <-
    object[map_lgl(object, ~ length(.x$breaks) > 0)]

  if (length(object) > 0) {
    if(type == "factor"){
      new_data[, names(object)] <-
        purrr::map_dfc(
          .x = names(object),
          .f = ~ tibble(
            !!.x := cut(
              x = new_data[[.x]],
              breaks = c(-Inf, object[[.x]]$breaks, Inf),
              labels = object[[.x]]$labels,
              include.lowest = T
            )
          )
        )
    } else if(type == "numeric") {
      new_data[, names(object)] <-
        purrr::map_dfc(
          .x = names(object),
          .f = ~ tibble(
            .g = cut(
              x = new_data[[.x]],
              breaks = c(-Inf, object[[.x]]$breaks, Inf),
              labels = object[[.x]]$labels,
              include.lowest = T
            ),
            .v = new_data[[.x]]
          ) %>%
            group_by(.g) %>%
            mutate(!!.x := mean(.v)) %>%
            ungroup() %>%
            dplyr::select(!!.x)
        )
    } else {
      cat_stop("type must be factor or numeric")
    }
  }
  new_data
}


#' @export
print.discrkknn <- function(x) {
  generic_print(x)
}

#' @export
do_discrkknn <- function(.data, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(discrkknn)
  predict(eval.parent(fncall), .data)
}

# discrchi -

#' @rdname discretizer
#' @export
discrchi <- function(.data, y, ...) {
  UseMethod("discrchi")
}

#' @export
discrchi.default <- function(.data,y,  ...) {
  generic_default()
}

#' @export
discrchi.data.frame <- function(.data, y, ...) {
  cdots <- dots(...)

  y <- substitute(y)


  alp <- .data

  num_breaks <-
    map(
      .x = alp[, are_dblint(alp)],
      .f = ~ trunc(nsplits(n_unique(.x)) + 2 * ncase(n_unique(.x)) / 3)
    )


  alp[, names(num_breaks)] <-
    alp[, names(num_breaks)] <- map2(
      alp[, names(num_breaks)],
      num_breaks,
      ~ breaktize_vec(
        var = .x,
        num_breaks = .y,
        algorithm = "quantile"
      )
    )
  data_y <- if (is_fctchr(.data[[deparse(y)]])) {
    .data[, deparse(y)]
  } else {
    alp[, deparse(y)]
  }
  data_x <- dplyr::select(alp, !!!cdots)

  data_x <- dplyr::select(data_x, where(is_dblint))
  data_x <- data_x[, setdiff(names(data_x), deparse(y))]
  alp <- map_dbl(data_x, ~ xi_impl(cbind(.x, data_y)))
  out <-
    map2(
      .x = data_x,
      .y = alp,
      .f = ~ try(chi2_impl(.x,
                           y = data_y[[deparse(y)]],
                           alp = .y
      ))
    )

  out <- compact(out)
  out <- map(out, ~ do_try(.x[!is_nonnum(.x)]))
  out <- compact(out)
  out <-
    map(out, ~ list(
      breaks = .x, labels = make_labels(.x)
    ))
  class(out) <- "discrchi"
  attr(out, "modelo") <- c("chi_discr", "chi2")
  attr(out, "target") <- deparse(y)
  attr(out, "transformed") <- names(out)
  attr(out, "call") <- match.call()
  out
}



#' @export
do_discrchi <- function(.data, y, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(discrchi)
  predict(eval.parent(fncall), .data)
}

#' @export
predict.discrchi <- function(object, new_data) {
  object <- object[map_lgl(object, ~ is_dblint(.x$breaks))]
  object <-
    object[map_lgl(object, ~ length(.x$breaks) > 0)]

  if (length(object) > 0) {
    new_data[, names(object)] <-
      purrr::map_dfc(
        .x = names(object),
        .f = ~ tibble(
          !!.x := cut(
            x = new_data[[.x]],
            breaks = c(-Inf, object[[.x]]$breaks, Inf),
            labels = object[[.x]]$labels,
            include.lowest = T
          )
        )
      )
  }
  new_data
}

#' @export
print.discrchi <- function(x) {
  generic_print(x)
}

#' @keywords internal
xi_impl <- function(x) {
  x <- as_df(x)
  p <- ncol(x)
  #n <- nrow(x)

  class_level <- dim(table(x[, p]))
  data_ex <- x[, -p]
  data_ex <- as_mtx(data_ex)
  mat <- unique(data_ex)
  if (dim(mat)[1] == dim(x)[1]) {
    return(0)
  }
  w <- which(duplicated(data_ex) == TRUE)
  dup <- unique(data_ex[w, ])
  if (is.numeric(dup) == TRUE) {
    dup <- as_mtx(dup)
  }
  n_dup <- nrow(dup)
  num <- 0
  for (i in 1:n_dup) {
    same <- apply(
      as_num(dup[i, 1:(p - 1)]) == t(data_ex),
      2, prod
    )
    ww <- as_num(which(same == 1))
    ww_class <- as_num(x[ww, p])
    #fac <- factor(ww_class)
    firs <- ww[1]
    num <- as_num(ifelse(same == 0, 0, firs)) + num
    fnum <- ifelse(num == 0, rownames(x), num)
    fnum <- as_num(fnum)
    freq <- table(fnum, x[, p])
  }
  ced <- numeric()
  for (j in 1:dim(freq)[2]) {
    c_table <- freq[freq[, j] != 0, ]
    if (class(c_table) == "integer") {
      c_table <- as_mtx(c_table, ncol = class_level)
    }
    c_sum <- sum(c_table)
    r_sum <- apply(c_table, 1, sum)
    r_max <- apply(c_table, 1, max)
    mis_class <- sum(r_sum - r_max)
    ced[j] <- 1 - (mis_class / c_sum)
  }
  m1 <- ifelse(all(0.5 <= ced) == FALSE, 0, 1 - min(ced[0.5 <=
                                                          ced]))
  m2 <- ifelse(all(ced <= 0.5) == FALSE, 0, max(ced[ced <=
                                                      0.5]))
  xi <- max(m1, m2)
  xi
}

#' @keywords internal
chi2_impl <- function(x, y, alp = .5, del = 0.05) {
  if (alp <= 0.1) {
    return(NULL)
  }

  ord_x <- order(x)
  x <- as_dbl(x)
  x <- x[ord_x]
  y <- y[ord_x]

  cent_x <- center_scale(x)

  if (is_fctchr(y)) {
    cent_y <- vaggr(x = x, .group = y, .fn = mean)
  } else {
    cent_y <- y
  }

  cent_x <- as_mtx(cent_x)
  cent_y <- as_mtx(cent_y)

  dat <- cbind(cent_x, cent_y)
  p <- ncol(dat) - 1

  alpha <- alp
  delta <- del
  d <- 0.1
  incon_rate <- incon(dat)
  discredata <- chi_merge_impl(dat, alpha = alpha)

  while (incon_rate < delta) {
    if (alpha <= 0.1) {
      break
    }
    alpha <- alpha - d
    discredata <- chi_merge_impl(discredata, alpha = alpha)
    incon_rate <- incon(discredata)
  }

  # Phase 2
  eps <- 0.01
  sig <- array(alpha, p)
  d <- 0.05
  while (TRUE) {
    discredata <- value_disc(1, discredata, alpha = sig[1])
    incon_rate <- incon(discredata) # incon_rate for data
    sig[1] <- sig[1] - d
    if (incon_rate > delta || sig[1] <= eps) {
      break
    }
  }

  cutp_vec <- discredata[, 1]
  cuts <- unique(cutp_vec)

  cutp <-
    sapply(1:(length(cuts) - 1), function(i) {
      up <- max(x[cutp_vec == i], na.rm = T)
      lw <- min(x[cutp_vec == i + 1], na.rm = T)
      mean(c(up, lw))
    })


  if (length(cutp) == 0) {
    cutp <- NULL
  }
  cutp
}


#' @keywords internal
incon <- function(x) {
  x <- as_df(x)
  p <- ncol(x)
  n <- nrow(x)
  x[, p] <- as_num(x[, p])
  #class_level <- dim(table(x[, p]))

  data_ex <- x[, 1:(p - 1)]
  if ((p - 1) == 1) {
    data_ex <- as_mtx(data_ex)
  }

  mat <- unique(data_ex)

  if (dim(mat)[1] == dim(x)[1]) {
    return(0)
  }

  w <-
    which(duplicated(data_ex) == TRUE)
  dup <- unique(data_ex[w, ])

  if (is.numeric(dup) == TRUE) {
    dup <- as_mtx(dup)
  }

  n_dup <- nrow(dup)

  num <- 0
  for (i in 1:n_dup) {
    same <-
      apply(as_num(dup[i, 1:(p - 1)]) == t(data_ex), 2, prod)
    ix <-
      as_num(which(same == 1))
    firs <- ix[1]
    num <-
      as_num(ifelse(same == 0, 0, firs)) + num
    fnum <-
      ifelse(num == 0, rownames(x), num)
    fnum <- as_num(fnum)
    freq <-
      table(fnum, x[, p])
  }
  ins_mat <- apply(freq, 1, sum)
  ins_max <- apply(freq, 1, max)
  in_con_count <- ins_mat - ins_max
  in_con_rate <- sum(in_con_count) / n
  in_con_rate
}

#' @keywords internal
value_disc <- function(i, data, alpha) {
  p1 <- length(data[1, ])
  #p <- p1 - 1
  #y <- as_int(data[, p1])
  class <- dim(table(data[, p1]))
  discredata <- data
  threshold <- qchisq(1 - alpha, class - 1)
  #cuts <- numeric()
  z <- sort(unique(data[, i]))
  if (length(z) <= 1) {
    return(list(cuts = "", disc = discredata))
  }
  dff <- diff(z) / 2
  lenz <- length(z)
  cutpoint <- z[1:(lenz - 1)] + dff
  midpoint <- c(z[1], cutpoint, z[lenz])

  a <- cut(data[, i], breaks = midpoint, include.lowest = TRUE)
  b <- table(a, data[, p1])
  b <- as.array(b)
  repeat {
    m <- dim(b)[1]
    if (length(dim(b)) < 2 || m < 2) {
      break
    }
    test <- numeric()
    for (k in 1:(m - 1)) {
      d <- b[c(k, k + 1), ]
      test[k] <- chi_sqrt_impl(d)
    }
    k <- which.min(test)
    if (test[k] > threshold) {
      break
    }
    b[k + 1, ] <- b[k, ] + b[k + 1, ]
    cutpoint <- cutpoint[-k]
    midpoint <- midpoint[-(k + 1)]
    b <- b[-k, ]
  }
  cuts <- cutpoint
  discredata[, i] <- cut(data[, i],
                         breaks = midpoint,
                         include.lowest = TRUE,
                         label = FALSE
  )
  discredata
}

#' @keywords internal
chi_merge_impl <- function(x, alpha = 0.05) {
  p <- dim(x)[2]
  out <- x
  for (i in 1:(p - 1)) {
    val <- value_disc(i, x, alpha)
    out[, i] <- val[, i]
  }
  out
}

#' @keywords internal
chi_sqrt_impl <- function(x) {
  x <- x + 0.0001
  e <- x
  n <- sum(x)
  p <- length(x[, 1])
  q <- length(x[1, ])
  mi <- numeric(p)
  ni <- numeric(q)
  for (i in 1:q) {
    ni[i] <- sum(x[, i])
  }
  for (i in 1:p) {
    mi[i] <- sum(x[i, ])
    e[i, ] <- mi[i] * ni / n
  }
  val <- sum((x - e)^2 / e)
  val
}

# discrtopdown -

#' @rdname discretizer
#' @export
discrtopdown <- function(.data,y, ...) {
  UseMethod("discrtopdown")
}

#' @export
discrtopdown.default <- function(.data,y, ...) {
  generic_default()
}

#' @export
discrtopdown.data.frame <-
  function(.data,
           y,
           ...,
           method = c("caim", "cacc", "ameva")) {
    cdots <- dots(...)

    y <- substitute(y)

    method <- match.arg(method)
    data_x <- dplyr::select(.data, !!!cdots)
    data_x <- dplyr::select(data_x, where(is_dblint))
    data_x <- data_x[, setdiff(names(data_x), deparse(y))]

    out <-
      map(data_x, ~ do_try(disc_topdown_impl(.x, y = .data[[deparse(y)]])))
    out <- compact(out)
    out <- lapply(out, function(x) {
      list(breaks = x, labels = make_labels(x))
    })

    class(out) <- "discrtopdown"
    attr(out, "modelo") <- c("topdown", method)
    attr(out, "target") <- deparse(y)
    attr(out, "transformed") <- names(out)
    attr(out, "call") <- match.call()
    out
  }


#' @export
do_discrtopdown <-
  function(.data,
           y,
           ...,
           method = c("caim", "cacc", "ameva")) {
    fncall <- match.call()
    method <- match.arg(method)
    fncall[[1L]] <- quote(discrtopdown)
    fncall$method <- method
    predict(eval.parent(fncall), .data)
  }

#' @export
predict.discrtopdown <- function(object, new_data) {
  object <- object[map_lgl(object, ~ is_dblint(.x$breaks))]
  object <-
    object[map_lgl(object, ~ length(.x$breaks) > 0)]

  if (length(object) > 0) {
    new_data[, names(object)] <-
      purrr::map_dfc(
        .x = names(object),
        .f = ~ tibble(
          !!.x := cut(
            x = new_data[[.x]],
            breaks = c(-Inf, object[[.x]]$breaks, Inf),
            labels = object[[.x]]$labels,
            include.lowest = T
          )
        )
      )
  }
  new_data
}

#' @export
print.discrtopdown <- function(x) {
  generic_print(x)
}

#' @keywords internal
disc_topdown_impl <-
function(x, y, method = c("caim", "cacc", "ameva")) {
    ord_x <- order(x)
    x <- as_dbl(x)
    x <- x[ord_x]
    y <- y[ord_x]


    cent_x <- round(center_scale(x), 4)

    if (is_fctchr(y)) {
      cent_y <- vaggr(x = x, .group = y, .fn = mean)
    } else {
      if (n_unique(y) > 10) {
        cent_y <- round(center_scale(y), 4)
        nb <-
          trunc(nsplits(n_unique(cent_y)) + 2 * ncase(n_unique(cent_y)) / 3)
        nb <- max(c(5, nb))
        cent_y <-
          breaktize_vec(
            var = cent_y,
            num_breaks = nb,
            algorithm = "kmean"
          )
      } else {
        cent_y <- y
      }
    }


    if (n_unique(cent_x) > 10) {
      nb <-
        trunc(nsplits(n_unique(cent_x)) + 2 * ncase(n_unique(cent_x)) / 3)
      nb <- max(c(5, nb))
      cent_x <-
        breaktize_vec(
          var = cent_x,
          num_breaks = nb,
          algorithm = "kmean"
        )
    }

    cent_x <- as_mtx(cent_x)
    cent_y <- as_mtx(cent_y)

    cuts <- topdown(cbind(cent_x, cent_y))

    cutp <- unique(interval_match(y = x, x = cent_x, obs = cuts))
    if (length(cutp) == 0) {
      cutp <- NULL
    }
    cutp
  }


#' @keywords internal
topdown <- function(data, method = c("caim", "cacc", "ameva")) {
  .find_best <- function(x, y, bd, di, method) {
    f_method <- switch(method,
                       ameva = function(x) {
                         nr <- dim(x)[1]
                         nc <- dim(x)[2]
                         den <- nr * (nc - 1)
                         val <- chi_sqrt_impl(x) / den
                         val
                       },
                       cacc = function(x) {
                         n <- sum(x)
                         nr <- dim(x)[1] + 1
                         logn <- 0
                         if (nr > 1) {
                           logn <- log(nr)
                         }
                         yp <- chi_sqrt_impl(x)
                         val <- sqrt(yp / (yp + n * logn))
                         val
                       },
                       caim = function(x) {
                         nr <- dim(x)[1]
                         #nc <- dim(x)[2]
                         maxr <-
                           apply(x, 1, max) ## max_j of n_ij, i=1,...,nr
                         m_r <- apply(x, 1, sum) ## sum_j n_ij
                         sum(maxr^2 / m_r) / nr
                       }
    )
    n <- length(y)
    best_c <- 0
    cacc <- 0
    i <- 0
    add_cut <- NULL
    i_best <- NULL
    new_di <- di
    nb <- numeric()
    for (i in 1:length(di)) {
      iw <- which(di[i] > bd)
      nb[iw] <- bd[iw]
      nl <- length(iw)
      nb[nl + 1] <- di[i]
      nb[(nl + 2):(length(bd) + 1)] <- bd[(nl + 1):length(bd)]
      bd1 <- nb

      dff <-
        findInterval(x, bd1, rightmost.closed = TRUE) ## faster than cut

      tb <- table(dff, y)
      cacc <- f_method(tb)
      if (cacc > best_c) {
        best_c <- cacc
        i_best <- i
        add_cut <- di[i]
      }
    }
    if (!is.na(i_best)) {
      new_di <- di[-i_best]
    }
    list(
      add_cut = add_cut,
      cacc = best_c,
      new_di = new_di,
      bd = bd
    )
  }
  .insert <- function(x, a) {
    p <- length(a)
    i <- which(a > x)
    len <- length(i)
    if (len == p) {
      return(c(x, a))
    }
    if (len == 0) {
      return(c(a, x))
    }
    i1 <- i[1]
    c(a[1:(i1 - 1)], x, a[i1:p])
  }

  method <- match.arg(method)

  p1 <- length(data[1, ])
  p <- p1 - 1
  y <- as_int(data[, p1])
  .s <- dim(table(y))[1]
  n <- length(y)
  d_cuts <-
    array(list(), p) ## cut points obtained for p X-variables

  for (i in 1:p) {
    x <- data[, i]
    od <- order(x)
    xo <- x[od]
    yo <- y[od]

    globa_ic <- 0
    cacc <- 0
    k <- 1
    add_cut <- NULL
    ci <- NULL
    ci <- which(diff(xo) != 0)
    if (!is.null(ci)) {
      cuts <- (xo[ci] + xo[ci + 1]) / 2
    }
    bd <- c(xo[1], xo[n])
    di <- cuts

    while (length(di) > 0) {
      ret <- .find_best(xo, yo, bd, di, method)
      cacc <- ret$cacc
      d_p <- .insert(ret$add_cut, bd) ## sort(c(bd,ret$add_cut))
      di <- ret$new_di

      if ((cacc > globa_ic || k < .s) & (k < n)) {
        bd <- d_p
        globa_ic <- cacc
        k <- k + 1
      } else {
        d_p <- bd
        break
      }
    }
    d_cuts[[i]] <- bd
  }
  d_cuts[[1]]
}

# discrmdlp -

#' @rdname discretizer
#' @export
discrmdlp <- function(.data, y, ...) {
  UseMethod("discrmdlp")
}

#' @export
discrmdlp.default <- function(.data, y, ...) {
  generic_default()
}

#' @export
discrmdlp.data.frame <- function(.data, y, ...) {
  cdots <- dots(...)

  y <- rlang::enexpr(y)

  data_x <- dplyr::select(.data, !!!cdots)
  data_x <-
    dplyr::select(data_x, where(is_dblint))

  data_x <- data_x[, setdiff(names(data_x), deparse(y))]

  out <-
    map(data_x, ~ try(mdlp_impl(.x, y = .data[[deparse(y)]])))

  out <- compact(out)

  out <-
    lapply(out, function(x) {
      list(breaks = x, labels = make_labels(x))
    })
  class(out) <- "discrmdlp"
  attr(out, "modelo") <- c("min_description length")
  attr(out, "target") <- deparse(y)
  attr(out, "transformed") <- names(out)
  attr(out, "call") <- match.call()
  out
}

#' @export
do_discrmdlp <- function(.data, y, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(discrmdlp)
  predict(eval.parent(fncall), .data)
}

#' @export
predict.discrmdlp <- function(object, new_data) {
  object <- object[map_lgl(object, ~ is_dblint(.x$breaks))]
  object <-
    object[map_lgl(object, ~ length(.x$breaks) > 0)]

  if (length(object) > 0) {
    new_data[, names(object)] <-
      purrr::map_dfc(
        .x = names(object),
        .f = ~ tibble(
          !!.x := cut(
            x = new_data[[.x]],
            breaks = c(-Inf, object[[.x]]$breaks, Inf),
            labels = object[[.x]]$labels,
            include.lowest = T
          )
        )
      )
  }
  new_data
}

#' @export
print.discrmdlp <- function(x) {
  generic_print(x)
}

#' @keywords internal
mdlp_impl <- function(x, y) {
  ord_x <- order(x)
  x <- as_dbl(x)
  x <- x[ord_x]
  y <- y[ord_x]


  cent_x <- center_scale(x)
  if (n_unique(x) > 10) {
    nn <- trunc(nsplits(n_unique(cent_x)) + 2 * ncase(n_unique(cent_x)) / 3)
    cent_x <- breaktize_vec(cent_x, nn, "quantile")
  }

  if (is_dblint(y)) {
    cent_y <- center_scale(y)
    if (n_unique(y) > 10) {
      nn <- trunc(nsplits(n_unique(cent_x)) + 2 * ncase(n_unique(cent_x)) / 3)
      cent_y <- breaktize_vec(cent_y, nn, "quantile")
    }
  } else {
    cent_y <- y
  }

  cuts <- cut_points(cent_x, cent_y)
  cuts_vec <- c(-Inf, cuts, Inf)
  cuts_vec <-
    as_int(cut(cent_x, cuts_vec, include.lowest = TRUE))


  cutp <-
    sapply(seq_along(cuts), function(i) {
      mean(
        max(x[cuts_vec == i], na.rm = TRUE),
        min(x[cuts_vec == i + 1], na.rm = TRUE)
      )
    })

  if (length(cutp) == 0) {
    cutp <- NULL
  }
  cutp
}


#' @keywords internal
cut_points <- function(x, y) {
  cut_index <- function(x, y) {
    n <- length(y)
    init_ent <- 9999
    entropy <- init_ent
    ci <- NULL

    for (i in 1:(n - 1)) {
      if (x[i + 1] != x[i]) {
        ct <- (x[i] + x[i + 1]) / 2
        wx <- which(x <= ct)
        wn <- length(wx) / n
        e1 <- wn * ent(y[wx])
        e2 <- (1 - wn) * ent(y[-wx])
        val <- e1 + e2
        if (val < entropy) {
          entropy <- val
          ci <- i
        }
      }
    }
    if (is.null(ci)) {
      return(NULL)
    }
    c(ci, entropy)
  }
  mdl_stop <- function(ci, y, entropy) {
    n <- length(y)
    es <- ent(y)
    left <- 1:ci
    right <- (ci + 1):n
    gain <- es - entropy
    l0 <- levels(factor(y))
    l1 <-
      levels(factor(y[left]))
    l2 <- levels(factor(y[right]))
    k <- length(l0)
    k1 <- length(l1)
    k2 <- length(l2)
    delta <-
      log_cp(3^k - 2) - (k * es - k1 * ent(y[left]) - k2 * ent(y[right]))
    cond <- log_cp(n - 1) / n + delta / n
    if (gain < cond) {
      return(NULL)
    }
    gain
  }
  gr <- function(low, upp, depth = depth) {
    x <- xo[low:upp]
    y <- yo[low:upp]
    #n <- length(y)
    ct <- cut_index(x, y)
    if (is.null(ct)) {
      return(NULL)
    } ## when cut index=NULL
    ci <- ct[1]
    entropy <- ct[2]
    ret <- mdl_stop(ci, y, entropy) # MDL Stop
    if (is.null(ret)) {
      return(NULL)
    }
    c(ci, depth + 1)
  }
  part <- function(low = 1,
                   upp = length(xo),
                   c_points = NULL,
                   depth = depth) {
    x <- xo[low:upp]
    #y <- yo[low:upp]
    n <- length(x)
    if (n < 2) {
      return(c_points)
    }
    cc <- gr(low, upp, depth = depth)
    ci <- cc[1]
    depth <- cc[2]
    if (is.null(ci)) {
      return(c_points)
    }
    c_points <- c(c_points, low + ci - 1)
    c_points <- as_int(sort(c_points))
    c(
      part(low, low + ci - 1, c_points, depth = depth),
      part(low + ci, upp, c_points, depth = depth)
    )
  }
  od <- order(x)
  xo <- x[od]
  yo <- y[od]
  depth <- 1
  res <- part(depth = depth)
  ci <- NULL
  cv <- numeric()
  if (!is.null(res)) {
    ci <- as_int(res)
    cv <- (xo[ci] + xo[ci + 1]) / 2
  }
  unique(cv)
}

#' @keywords internal
ent <- function(y) {
  p <- table(y) / length(y)
  -sum(p * log_cp(p))
}

#' @keywords internal
log_cp <- function(x) {
  x[which(x <= 1.0e-10)] <- 1
  log(x)
}

# discrete -

#' @rdname discretizer
#' @export
discrete <- function(x, ...) {
  UseMethod("discrete")
}

#' @export
discrete.default <- function(x, ...) {
  generic_default()
}

#' @export
discrete.data.frame <-
  function(x,
           ...,
           outcome,
           threshold = .05) {
    f_wss <- function(dat, t) {
      function(par) {
        k <- kmeans(x = dat, centers = round(par))
        abs(1 - k$betweenss / k$totss - t)
      }
    }
    outcome <- substitute(outcome)

    disc_call <- match.call()

    disc_call[[1]] <- quote(dectdiscrete)

    names(disc_call)[names(disc_call) == "x"] <- ".data"
    disc_call$.data <- x

    if (is_empty(threshold)) {
      disc_call$threshold <- .05
    } else {
      disc_call$threshold <- threshold
    }

    if (is_empty(outcome)) {
      cat_stop("'outcome' is require")
    }
    disc_nm <- rlang::eval_bare(disc_call, parent.frame(2L))

    disc_nm

    out <- list()

    for (i in disc_nm) {
      data_res <- data.frame(x = x[[i]])

      if (n_unique(data_res$x) > 5) {
        int_res <- c(2, ncase(unique(data_res$x)))

        op_wss <-
          f_wss(dat = data_res, t = threshold)

        nk <-
          round(optimize(f = op_wss, interval = int_res)$minimum)

        out[[i]] <-
          data.frame(
            lab = kmeans(x = data_res, centers = nk)$cluster,
            int = data_res$x
          )
        out[[i]] <-
          group_mutate(
            .data = out[[i]],
            .groups = "lab",
            lab = mean(int)
          ) %>%
          mutate(lab = dplyr::dense_rank(lab)) %>%
          distinct() %>%
          arrange(lab, int) %>%
          mutate(lab = factor_clean(lab))
      } else {
        out[[i]] <-
          tibble(lab = factor_clean(unique(data_res$x)), int = unique(data_res$x))
      }
    }
    out <- compact(out)
    out <- lapply(out, as.list)
    class(out) <- "discrete"
    attr(out, "modelo") <- c("test_discr")
    attr(out, "target") <- deparse(rlang::enexpr(outcome))
    attr(out, "transformed") <- names(out)
    attr(out, "call") <- match.call()
    out
  }

#' @export

do_discrete <- function(x, ...,
                        outcome,
                        threshold = .05) {
  fncall <- match.call()
  fncall[[1L]] <- quote(discrete)
  predict(eval.parent(fncall), x)
}


#' @export
dectdiscrete <- function(.data, outcome, threshold = .05) {
  outcome <- substitute(outcome)
  y <- .data[[deparse(outcome)]]

  if (is_dblint(y)) {
    if (n_unique(y) / length(y) > .25) {
      y_is_int <- is_int(y)

      y <- clip_bounds_vec(x = y)
      nn <- nsplits(length(unique(y)))
      y <-
        breaktize_vec(
          var = y,
          num_breaks = nn,
          algorithm = "equalwidth"
        )
      if (y_is_int) {
        y <- round(y)
      }
    }
  }

  data_int <- .data[, are_integerish(.data)]

  data_int[[deparse(outcome)]] <- NULL

  if (ncol(data_int) == 0) {
    return(character(0))
  }

  distinct_int <- map_dbl(data_int, unique_rate)

  data_int <-
    data_int[, names(distinct_int[distinct_int < threshold])]

  data_int <-
    data_int[, apply(data_int, 2, dplyr::n_distinct) > 1]


  data_table <-
    map(data_int, ~ sort(table(.x)))

  data_zero <-
    map(data_int, t_propzero)

  if (is_fctchr(y)) {
    d_filtro <-
      map2_lgl(data_table,
               data_zero,
               .f = ~ sum(.x[names(.x) %in% .y] / sum(.x)) < threshold * 4
      )


    data_int <- data_int[d_filtro]
    data_zero <- data_zero[d_filtro]
  }

  if (ncol(data_int) == 0) {
    return(character(0))
  }

  data_int <-
    map(data_int, ~ do_try(data.frame(x = .x, y = y)))

  data_int <- compact(data_int)

  data_int <-
    map2(.x = data_int, .y = data_zero, ~ .x[!.x$x %in% .y, ])

  if (is_dblint(y)) {
    data_int <-
      map_lgl(
        .x = data_int,
        .f = ~ t_meanequal(data = .x, x = x, y = y) > .75
      )
  }

  if (is_fctchr(y)) {
    data_int <-
      map_lgl(
        .x = data_int,
        .f = ~ do_try(t_propor(
          data = .x, x = x, y = y
        ) > .75, FALSE)
      )
  }

  if (length(data_int) == 0) {
    return(character(0))
  }

  data_int <-
    do_try(names(data_int)[data_int], character(0))

  distinct_int <-
    names(distinct_int)[distinct_int < threshold]

  unique(distinct_int, data_int)
}

#' @export
predict.discrete <- function(object, new_data) {
  object <- object[map_lgl(object, ~ is_dblint(.x$int))]
  object <-
    object[map_lgl(object, ~ length(.x$lab) > 0)]

  if (length(object) > 0) {
    new_data[, names(object)] <-
      purrr::map_dfc(
        .x = names(object),
        .f = ~ tibble(!!.x := interval_match(
          y = object[[.x]]$lab,
          x = object[[.x]]$int,
          obs = new_data[[.x]]
        ))
      )
  }
  new_data
}

#' @export
print.discrete <- function(x) {
  generic_print(x)
}

#' @keywords internal
t_propzero <- function(dataset, threshold = .05) {
  x <- table(dataset)
  x <- sort(x)
  x_zero <- c()

  for (i in seq_along(x)) {
    xi <- as.table(c(x[i], sum(x) - x[i]))
    names(xi)[2] <- "todos"
    prop_test <-
      do_try(prop.test(
        x = xi[1],
        n = xi[2],
        p = .05,
        alternative = "less"
      ),0)
    if (prop_test$p.value < .05) {
      x_zero <- c(x_zero, names(xi)[1])
    } else {
      break
    }
  }
  x_zero <- as_int(x_zero)

  sum(x[names(x) %in% x_zero] / sum(x))
}

#' @keywords internal
t_propor <- function(data, x, y) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  xun <- unique(pull(data, !!x))
  x_diff <- c()
  x_prop <- c()
  yun <- unique(as_chr(pull(data, !!y)))

  for (i in xun) {
    if (length(yun) == 2) {
      xtest <- pull(filter(data, !!x == i), !!y)
      xtest <- table(xtest)
      ytest <- pull(filter(data, !!x != i), !!y)
      ytest <- table(ytest)
      prop_test <- prop.test(ytest, xtest)
      x_diff <-
        c(x_diff, ifelse(prop_test$p.value < .05, 1, 0))
      x_prop <- c(x_prop, sum(xtest) / nrow(data))
    } else {
      xl <- c()
      for (w in yun) {
        xtest <- pull(filter(data, !!x == i), !!y)
        xtest <- table(xtest)
        ytest <-
          pull(filter(data, !!x != i), !!y)
        ytest <-
          ifelse(ytest == w, as_chr(ytest), "zzz")
        ytest <- table(ytest)

        prop_test <- prop.test(ytest, xtest)
        xl <- c(xl, prop_test$p.value)
      }
      prop_test <- mean(xl)
      x_diff <-
        c(x_diff, ifelse(prop_test < .05, 1, 0))
      x_prop <- c(x_prop, sum(xtest) / nrow(data))
    }
  }
  sum(x_diff * x_prop)
}

#' @keywords internal
t_meanequal <- function(data, x, y) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  xun <- unique(pull(data, !!x))
  x_diff <- c()
  x_prop <- c()
  for (i in xun) {
    xtest <- pull(filter(data, !!x == i), !!y)
    ytest <- pull(filter(data, !!x != i), !!y)
    mean_test <- t.test(xtest, ytest, var.equal = TRUE)
    x_diff <-
      c(x_diff, ifelse(mean_test$p.value < .05, 1, 0))
    x_prop <- c(x_prop, length(xtest) / nrow(data))
  }
  sum(x_diff * x_prop)
}

# -


#' @keywords internal
make_labels <- function(x) {
  x <- sapply(x, round_sign)
  x <- c(-Inf, x, Inf)
  factor(map_chr(seq_len(length(x) - 1), function(i) {
    paste0("[", x[i], "-", x[i + 1], "]")
  }))
}
