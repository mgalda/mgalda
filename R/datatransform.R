# !diagnostics off
# !diagnostics suppress=y

#' datatransform
#'
#' @name datatransform
#' @rdname datatransform
#' @keywords internal
#'
#' @examples
#'
#' set.seed(7)
#' data <- iris[, -5]
#' data <- dplyr::mutate_all(data, roll_noise) %>% as_tbl()
#' object <- discrkknn(data, everything())
#'
#' simplifycategorical(.data = mgalda::datalearn$housing_market,where(is_fctchr))
#' do_simplifycategorical(.data = mgalda::datalearn$housing_market,where(is_fctchr))
#'
#' simplifycategorical(.data = mgalda::datalearn$ames_lite,where(is_fctchr))
#' do_simplifycategorical(.data = mgalda::datalearn$ames_lite,where(is_fctchr))
#'
#' set.seed(7)
#' data <- mgalda::datalearn$housing_market
#' binarize(.data = data, algorithm = "kmean", where(is_dblint))
#' do_binarize(.data = data, algorithm = "kmean", where(is_dblint))
#' x <- mgalda::datalearn$credit$price
#' binarize_vec(x)
#' x <- mgalda::datalearn$credit$price
#' # binarize_object(var = x)
#' data <- mgalda::datalearn$housing_market
#' breaktize(
#'   .data = data,
#'   num_breaks = 5,
#'   algorithm = "quantile",
#'   where(is_dblint)
#' )
#' do_breaktize(
#'   .data = data,
#'   num_breaks = 5,
#'   algorithm = "quantile",
#'   where(is_dblint)
#' )
#' breaktize(
#'   .data = data,
#'   num_breaks = c(3, 5),
#'   algorithm = "quantile",
#'   sales_price,
#'   sales_price_coefficient_variance
#' )
#' do_breaktize(
#'   .data = data,
#'   num_breaks = c(3, 5),
#'   algorithm = "quantile",
#'   sales_price,
#'   sales_price_coefficient_variance
#' )
#' x <- mgalda::datalearn$credit$price
#' breaktize_vec(x, num_breaks = 5)
#' x <- mgalda::datalearn$credit$price
#'
#' cartdiscretize(
#'   .data = mgalda::datalearn$housing_market,
#'   y = sales_price,
#'   where(is_dblint)
#' )
#' do_cartdiscretize(
#'   .data = mgalda::datalearn$housing_market,
#'   y = sales_price,
#'   where(is_dblint)
#' )
#' cartdiscrmulti(
#'   .data = mgalda::datalearn$housing_market,
#'   y = sales_price,
#'   where(is_dblint)
#' )
#' do_cartdiscrmulti(
#'   .data = mgalda::datalearn$housing_market,
#'   y = sales_price,
#'   where(is_dblint)
#' )
#' categoricaloptim(
#'   .data = mgalda::datalearn$credit,
#'   y = status,
#'   where(is_fctchr)
#' )
#' do_categoricaloptim(
#'   .data = mgalda::datalearn$credit,
#'   y = status,
#'   where(is_fctchr)
#' )
#' getdummy(mgalda::datalearn$credit, where(is_fctchr))
#' do_getdummy(mgalda::datalearn$credit, where(is_fctchr))
#' dataset <- mgalda::datalearn$credit
#' dataset <- dplyr::select(dataset, where(is_dblint))
#' impute_kknn(dataset, dplyr::everything(), to_impute = income)
#' do_impute_kknn(dataset, dplyr::everything(), to_impute = income)
#' dataset <- mgalda::datalearn$housing_market
#' dataset <- dplyr::select(dataset, where(is_dblint))
#' dataset$sales_price[sample(seq_len(nrow(dataset)), 20)] <- NA_real_
#' impute_linear(dataset, dplyr::everything(), to_impute = sales_price)
#' do_impute_linear(dataset, dplyr::everything(), to_impute = sales_price)
#' keeplevels(mgalda::datalearn$credit, where(is_fctchr))
#' do_keeplevels(mgalda::datalearn$credit, where(is_fctchr))
#' lenc_glm(
#'   .data = mgalda::datalearn$credit,
#'   y = status,
#'   where(is_fctchr)
#' )
#' do_lenc_glm(
#'   .data = mgalda::datalearn$credit,
#'   y = status,
#'   where(is_fctchr)
#' )
#' lenc_mixed(
#'   .data = mgalda::datalearn$credit,
#'   y = status,
#'   where(is_fctchr)
#' )
#' do_lenc_mixed(
#'   .data = mgalda::datalearn$credit,
#'   y = status,
#'   where(is_fctchr)
#' )
#' multicollin(.data = mgalda::datalearn$housing_market, where(is_dblint))
#' do_multicollin(.data = mgalda::datalearn$housing_market, where(is_dblint))
#' near_zero(.data = mgalda::datalearn$housing_market, dplyr::everything())
#' do_near_zero(.data = mgalda::datalearn$housing_market, dplyr::everything())
#' select_boruta(
#'   .data = mgalda::datalearn$housing_market,
#'   y = sales_price,
#'   dplyr::everything()
#' )
#' do_select_boruta(
#'   .data = mgalda::datalearn$housing_market,
#'   y = sales_price,
#'   dplyr::everything()
#' )
#' data <- datalearn$housing_market
#' select_pca(
#'   .data = data,
#'   threshold = 0.8,
#'   num_comp = 4,
#'   where(is_dblint)
#' )
#'
#' select_vip(
#'   .data = mgalda::datalearn$housing_market,
#'   y = sales_price,
#'   dplyr::everything()
#' )
#' do_select_vip(
#'   .data = mgalda::datalearn$housing_market,
#'   y = sales_price,
#'   dplyr::everything()
#' )
#' x <- runif(10)
#' data <- data.frame(
#'   x = x,
#'   x2 = rescalar(x, c(2, 4)),
#'   y = rnorm(10)
#' )
#' xcorrr(.data = data, everything())
#' do_xcorrr(.data = data, everything())
#' xnls(
#'   mgalda::datalearn$housing_market,
#'   sales_price_coefficient_variance,
#'   sales_price
#' )
#' do_xnls(
#'   mgalda::datalearn$housing_market,
#'   sales_price_coefficient_variance,
#'   sales_price
#' )
#' xpca(.data = mgalda::datalearn$housing_market, dplyr::everything())
#' do_xpca(.data = mgalda::datalearn$housing_market, dplyr::everything())
#' xprepro(.data = mgalda::datalearn$housing_market, dplyr::everything())
#' do_xprepro(.data = mgalda::datalearn$housing_market, dplyr::everything())
#' xrossdummy(.data = mgalda::datalearn$credit, dummy_var = time, where(is_dblint))
#' do_xrossdummy(.data = mgalda::datalearn$credit, dummy_var = time, where(is_dblint))
#' xstandarice(.data = mgalda::datalearn$housing_market, where(is_dblint))
#' do_xstandarice(.data = mgalda::datalearn$housing_market, where(is_dblint))
#' smoooth_vec_samples(
#'   x = mgalda::datalearn$credit$price,
#'   times = sample(2:10, 1),
#'   rep = sample(2:4, 1),
#'   na_rm = T
#' )
#' data <- mgalda::datalearn$iris
#' expr <-
#'   quote(
#'     dplyr::mutate(sepal_length = sepal_length / 2) %>% dplyr::group_by(species) %>% dplyr::summarise_all(mean) %>% dplyr::ungroup()
#'   )
#' object <- save_pipe(.data = data, expr = expr)
#' predict(object, data)
NULL

# binarize--

#' @rdname datatransform
#' @export
binarize <- function(.data, ...) {
  UseMethod("binarize")
}
#' @rdname datatransform
#' @export
binarize.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
binarize.data.frame <-
  function(.data,
           algorithm = c("kmean", "mean", "median", "dens"),
           ...) {
    algorithm <- match.arg(algorithm)
    expr <- dots(...)
    dataset <- dplyr::select(as_tbl(.data), !!!expr)
    xvars <- names(dataset)
    xvars <-
      base::intersect(xvars, names(dataset[, are_dblint(dataset)]))
    obj <-
      map(
        .x = xvars,
        .f = ~ binarize_object(
          var = dataset[[.x]],
          algorithm
        )
      )

    obj <- .bincut(.data, obj, xvars)
    attr(obj, "transformed") <- names(obj)
    class(obj) <- c("binarize", "binarize_list")
    obj
  }
#' @rdname datatransform
#' @export
predict.binarize <- function(object, new_data) {
  if (is_empty(new_data)) {
    new_data <- do_try_ier(expr = dplyr::cur_data())
    if (!is.data.frame(new_data)) {
      cat_stop("new_data is empty")
    }
  }
  .is_binobj <- function(x) {
    map_lgl(x, ~ is_true(.x[1] == -Inf) &
      is_dblint(.x[2]) & is_true(.x[3] == Inf))
  }
  object <- cmp_map(object[.is_binobj(object)], ~ attr(.x, "fn"))
  for (nm in names(object)) {
    expr_pred <- substitute(object[[nm]])
    new_data <-
      mutate(new_data, {{ nm }} := eval_tidy(expr = object[[nm]]))
  }
  new_data
}
#' @export
print.binarize <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
binarize_vec <-
  function(var,
           algorithm = c("kmean", "mean", "median", "dens")) {
    algorithm <- match.arg(algorithm)
    .p_bin <- binarize_object(var, algorithm)
    cut(
      x = var,
      breaks = .p_bin,
      labels = c(1, 0)
    )
  }

#' @keywords internal
binarize_object <- function(var, algorithm) {
  on_error(~ ret_invis(ier()))
  .binned_kmean <- function(x) {
    cls <- kmeans(sort(x), 2)

    slp_x <- split(sort(x), cls$cluster)

    slp_x <- sort(unlist(map(slp_x, range)))

    mean(slp_x[2:3])
  }
  .binned_density <- function(x) {
    if (is_int(x)) {
      dns_x <- denstable_discrete(x = x)
      dns_x$x[which.max(dns_x$prob)] + .5
    } else {
      dns_x <- density(x)
      dns_x$x[which.max(dns_x$y)]
    }
  }

  breaks <- do_try_ier(switch(algorithm,
    kmean = .binned_kmean(var),
    mean = mean(var),
    median = median(var),
    dens = .binned_density(var)
  ))

  if (is_ier(breaks)) {
    return()
  }

  breaks <- unique(breaks)
  breaks <- breaks[!is.na(breaks)]
  breaks <- breaks[order(breaks)]
  breaks <- c(-Inf, breaks, Inf)
  class(breaks) <- c("binarize", "breaks")
  attr(breaks, "algorithm") <- algorithm
  breaks
}
.bincut <- function(.data, obj, xvars) {
  names(obj) <- xvars

  obj_attr <- map(obj, attributes)
  call_cut <-
    quote(cut(
      x = x,
      breaks = c(-Inf, .p_bin, Inf),
      labels = c(1, 0)
    ))
  call_bin <-
    quote(round_any(x = .x[2], accuracy = precision(.data[[.y]])))
  fns_cut <- imap(
    .x = obj,
    .f = ~ interp(
      obj_ = call_cut,
      .p_bin = eval(call_bin),
      x = str2lang(.y)
    )
  )

  obj <-
    map2(
      .x = obj,
      .y = fns_cut,
      .f = function(x, y) {
        attr(x, "fn") <- y
        x
      }
    )
}

#' @rdname datatransform
#' @export
do_binarize <-
  function(.data,
           algorithm = c("kmean", "mean", "median", "dens"),
           ...) {
    fncall <- match.call()
    fncall[[1L]] <- quote(binarize)
    predict(eval.parent(fncall), .data)
  }

# breaktize--

#' @rdname datatransform
#' @export
breaktize <- function(.data, ...) {
  UseMethod("breaktize")
}
#' @export
#' @rdname datatransform
breaktize.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
breaktize.data.frame <- function(.data = NULL,
                                 num_breaks = NULL,
                                 algorithm = c("quantile", "trees", "kmean", "equalwidth"),
                                 ...) {
  algorithm <- match.arg(algorithm)
  expr <- dots(...)
  dataset <- dplyr::select(as_tbl(.data), !!!expr)
  xvars <- names(dataset)
  xvars <-
    base::intersect(xvars, names(dataset[, are_dblint(dataset)]))
  if (length(num_breaks) == 1) {
    num_breaks <- rep(num_breaks, length(xvars))
  }
  obj <-
    map2(
      .x = xvars,
      .y = num_breaks,
      .f = ~ break_object(
        var = dataset[[.x]],
        num_breaks = .y,
        algorithm = algorithm
      )
    )
  names(obj) <- xvars
  attr(obj, "transformed") <- names(obj)
  class(obj) <- c("breaktize", "breaktize_list")
  obj
}

#' @rdname datatransform
#' @export
predict.breaktize <- function(object, new_data) {
  if (is_empty(new_data)) {
    new_data <- do_try_ier(expr = dplyr::cur_data())
    if (!is.data.frame(new_data)) {
      cat_stop("new_data is empty")
    }
  }
  .is_breakt <- function(x) {
    map_lgl(x, ~ is_true(inherits(.x, "breaktize")) &
      is_dblint(.x$breaks))
  }
  object <- compact(object[.is_breakt(object)])
  for (nm in names(object)) {
    tmp_obj <- object[[nm]]
    .breaks <- tmp_obj$breaks
    .labs <- tmp_obj$labs
    new_data[[nm]] <- as_num(as_chr(
      cut(
        x = new_data[[nm]],
        breaks = .breaks,
        labels = .labs,
        include.lowest = T
      )
    ))
  }
  new_data
}
#' @export
print.breaktize <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
breaktize_vec <-
  function(var,
           num_breaks,
           algorithm = c(
             "quantile",
             "trees", "kmean", "equalwidth"
           )) {
    .break <-
      break_object(as_num(var[!is_nonnum(var)]), num_breaks, algorithm)
    as_num(as_chr(
      cut(
        x = var,
        breaks = .break$breaks,
        labels = .break$labs,
        include.lowest = T
      )
    ))
  }
#' @keywords internal
breaktize_quantile <- function(var, num_breaks) {
  breaks <-
    seq_percent(x = as_dbl(var[!is_nonnum(var)]), n = num_breaks + 1)
  breaks <- sort(unique(breaks[!is_nonnum(breaks)]))
  class(breaks) <- c("breaktize", "breaks")
  breaks
}
#' @keywords internal
breaktize_equalwidth <- function(var, num_breaks) {
  breaks <- seq_range(x = var, n = num_breaks + 1)
  breaks <- sort(unique(breaks[!is.na(breaks)]))
  class(breaks) <- c("breaktize", "breaks")
  breaks
}
#' @keywords internal
breaktize_tree <- function(var, num_breaks) {
  var <- sort(as_num(var[!is_nonnum(var)]))
  if (length(var) > 750) {
    var <- sort_filter_vec(x = var, n = 750)
  }
  breaks <- split(x = var, hierarchical(x = var, n = num_breaks))
  breaks <-
    imap_dfr(breaks, ~ tibble(value = range(.x), n = as_num(.y) + c(0, 1))) %>%
    group_summarise(.groups = n, value = mean(value)) %>%
    arrange(value) %>%
    pull(value)
  class(breaks) <- c("breaktize", "breaks")
  breaks
}
#' @keywords internal
breaktize_kmean <- function(var, num_breaks) {
  var <- sort(as_num(var[!is_nonnum(var)]))
  if (length(var) > 750) {
    var <- sort_filter_vec(x = var, n = 750)
  }
  if (num_breaks > n_unique(var)) {
    num_breaks <- n_unique(var)
  }

  breaks <-
    split(x = var, stats::kmeans(x = var, centers = num_breaks)$cluster)
  breaks <- unname(breaks[order(map_dbl(breaks, mean))])
  breaks <-
    imap_dfr(breaks, ~ tibble(value = range(.x), n = as_num(.y) + c(0, 1))) %>%
    group_summarise(.groups = n, value = mean(value)) %>%
    arrange(value) %>%
    pull(value)

  class(breaks) <- c("breaktize", "breaks")
  breaks
}
#' @keywords internal
break_object <- function(var,
                         num_breaks,
                         algorithm = c(
                           "quantile",
                           "trees", "kmean", "equalwidth"
                         )) {
  algorithm <- match.arg(algorithm)
  callargs <- list(var = var, num_breaks = num_breaks)
  breaks <-
    switch(algorithm,
      quantile = do.call(
        breaktize_quantile,
        callargs
      ),
      trees = do.call(breaktize_tree, callargs),
      kmean = do.call(breaktize_kmean, callargs),
      equalwidth = do.call(
        breaktize_equalwidth,
        callargs
      )
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
  out
}
#' @rdname datatransform
#' @export
do_breaktize <-
  function(.data,
           num_breaks = NULL,
           algorithm = c(quantile, "trees", "kmean", "equalwidth"),
           ...) {
    fncall <- match.call()
    fncall[[1L]] <- quote(breaktize)
    predict(eval.parent(fncall), .data)
  }

# cartdiscretize--


#' @rdname datatransform
#' @export
cartdiscretize <- function(.data, y, ...) {
  UseMethod("cartdiscretize")
}
#' @export
#' @rdname datatransform
cartdiscretize.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
cartdiscretize.data.frame <- function(.data, y, ...) {
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data)
  col_names <- names(rlang::set_names(.data[pos], names(pos)))
  col_names <-
    base::intersect(x = col_names, names(.data[, are_dblint(.data)]))
  y <- rlang::enexpr(y)
  if (!is.character(y)) {
    y <- rlang::expr_deparse(y)
  }
  if (sum(names(.data) == y) != 1) {
    cat_stop("length(names('.data') %in% 'y') != 1")
  }
  col_names <- setdiff(col_names, y)
  rules <-
    map(
      .x = .data[, col_names],
      .f = ~ try(cartdiscretize_impl(
        x = .x,
        y = .data[[y]]
      ))
    )
  has_splits <-
    map_lgl(rules, ~ length(.x) > 0 &
      !inherits(.x, "try-error"))
  out <- compact(rules[has_splits])
  out <-
    map(out, ~ list(breaks = safe_vec(.x), labels = make_labels(safe_vec(.x))))
  attr(out, "transformed") <- names(out)
  attr(out, "outcome") <- y
  class(out) <- "cartdiscretize"
  out
}
#' @rdname datatransform
#' @export
predict.cartdiscretize <- function(object, new_data) {
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
print.cartdiscretize <-
  function(x) {
    generic_print(x)
  }
cartdiscretize_impl <- function(x, y) {
  df <- data.frame(y = y, x = x)
  if (is_dblint(y)) {
    rsq <- function(truth, estimate) {
      cor(truth, estimate)^2
    }
    resfn <- function(y, y_hat) {
      res <- try(rsq(truth = y, estimate = y_hat) * -1,
        silent = T
      )
      if (any(class(res) == "try-error")) {
        res <- 0
      }
      res * 1000
    }
  } else if (is_fctchr(y)) {
    accuracy <- function(x) {
      sum(diag(x)) / sum(x)
    }
    resfn <- function(y, y_hat) {
      res <- try(accuracy(x = table(y, y_hat)) * -1, silent = T)
      if (any(class(res) == "try-error")) {
        res <- 0
      }
      res * 1000
    }
  }
  objfn <- function(df, par) {
    rpopm <- rpart::rpart(
      y ~ x,
      data = df,
      cp = par[1],
      minsplit = par[2],
      maxdepth = par[3],
      maxcompete = 0,
      maxsurrogate = 0
    )
    resfn(y = df$y, y_hat = predict(rpopm, df))
  }
  mm <- nsplits(x)
  par <- optim(
    par = c(0.01, mm, 5),
    fn = objfn,
    lower = c(
      0.0001,
      trunc(mm / 4), 2
    ),
    upper = c(0.5, mm * 10, 15),
    method = "L-BFGS-B",
    df = df,
    control = list(maxit = 1000)
  )$par
  cart_mdl <- try(rpart::rpart(
    y ~ x,
    data = df,
    cp = par[1],
    minsplit = par[2],
    maxdepth = par[3],
    maxcompete = 0,
    maxsurrogate = 0
  ),
  silent = TRUE
  )
  if (inherits(cart_mdl, "try-error")) {
    cart_split <- NULL
  }
  if (any(names(cart_mdl) == "splits")) {
    cart_split <- sort(unique(cart_mdl$splits[, "index"]))
  } else {
    cart_split <- NULL
  }
  cart_split
}
#' @rdname datatransform
#' @export
do_cartdiscretize <- function(.data, y, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(cartdiscretize)
  predict(eval.parent(fncall), .data)
}

# cartdiscrmulti--

#' @rdname datatransform
#' @export
cartdiscrmulti <- function(.data, ...) {
  UseMethod("cartdiscrmulti")
}
#' @export
#' @rdname datatransform
cartdiscrmulti.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
cartdiscrmulti.data.frame <- function(.data, y, ...) {
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data)
  x <- rlang::set_names(.data[pos], names(pos))
  y <- rlang::enexpr(y)
  if (!is.character(y)) {
    y <- rlang::expr_deparse(y)
  }
  if (sum(names(.data) == y) != 1) {
    cat_stop("length(names('.data') %in% 'y') != 1")
  }
  x <- x[, setdiff(names(x), y)]
  xnames <- names(x)
  if (any(sapply(x, is_fctchr))) {
    klvs <- rlang::expr(keeplevels(.data = .data, where(is_fctchr)))
    klvs <- eval(klvs)
    x <- predict(object = klvs, x)
  } else {
    klvs <- NULL
  }
  if (any(sapply(x, is_dblint))) {
    lbnd <- rlang::expr(limits_bounds(.data = x, where(is_dblint)))
    lbnd <- eval(lbnd)
  }
  df <- bind_cols(x, .data[, y])
  if (is_dblint(df[[y]])) {
    rsq <- function(truth, estimate) {
      cor(truth, estimate)^2
    }
    resfn <- function(y, y_hat) {
      res <- try(rsq(truth = y, estimate = y_hat) * -1,
                 silent = T
      )
      if (any(class(res) == "try-error")) {
        res <- 0
      }
      res * 1000
    }
  } else if (is_fctchr(df[[y]])) {
    accuracy <- function(x) {
      sum(diag(x)) / sum(x)
    }
    resfn <- function(y, y_hat) {
      res <- try(accuracy(x = table(y, y_hat)) * -1, silent = T)
      if (any(class(res) == "try-error")) {
        res <- 0
      }
      res * 1000
    }
  }
  formula_cart <-
    paste0(y, " ~ ", paste0(names(x), collapse = " + "))
  formula_cart <-
    as_frm(x = formula_cart, env = env_curr())
  objfn <- function(df, par) {
    rpopm <- rpart::rpart(
      formula_cart,
      data = df,
      cp = par[1],
      minsplit = par[2],
      maxdepth = par[3],
      maxcompete = 0,
      maxsurrogate = 0
    )
    resfn(y = df[[y]], y_hat = predict(rpopm, df))
  }
  mm <- nsplits(nrow(x))
  par <- try(optim(
    par = c(0.01, mm, 5),
    fn = objfn,
    lower = c(
      0.0001,
      trunc(mm / 4), 2
    ),
    upper = c(0.5, mm * 10, 15),
    method = "L-BFGS-B",
    df = df,
    control = list(maxit = 20000)
  )$par)
  expr_rpart <-
    rlang::expr(
      rpart::rpart(
        !!formula_cart,
        data = df,
        cp = !!par[1],
        minsplit = !!par[2],
        maxdepth = !!par[3],
        maxcompete = 0,
        maxsurrogate = 0
      )
    )
  cart_mdl <-
    try(rlang::eval_bare(expr_rpart, env_curr()))
  party_mdl <- partykit::as.party(cart_mdl)
  lvls <- unique(predict(
    object = party_mdl,
    newdata = df,
    type = "node"
  ))
  out <-
    list(
      keeplevels = klvs,
      limits_bounds = lbnd,
      discretize = party_mdl,
      cart_model = cart_mdl,
      lvls = lvls,
      cols = xnames,
      new_name = rand_id(
        prefix = "cartdm",
        len = 3
      )
    )
  attr(out, "outcome") <- y
  attr(out, "transformed") <- out$cols
  class(out) <- "cartdiscrmulti"
  out
}
#' @rdname datatransform
#' @export
predict.cartdiscrmulti <- function(object, new_data) {
  binned_data <- new_data[, object$cols]
  if (!is.null(object$keeplevels)) {
    binned_data <- predict(object$keeplevels, new_data = binned_data)
  }
  if (!is.null(object$limits_bounds)) {
    binned_data <- predict(object$limits_bounds, new_data = binned_data)
  }
  binned_data <-
    predict(
      object = object$discretize,
      newdata = binned_data,
      type = "node"
    )
  binned_data <- factor(x = binned_data, levels = object$lvls)
  nnama <- object$new_name
  bind_cols(
    new_data[, setdiff(names(new_data), object$cols)],
    tibble(`:=`({{ nnama }}, binned_data))
  )
}
#' @export
print.cartdiscrmulti <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
do_cartdiscrmulti <- function(.data, y, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(cartdiscrmulti)
  predict(eval.parent(fncall), .data)
}

# categoricaloptim-


#' @rdname datatransform
#' @export
categoricaloptim <- function(.data, y, ...) {
  UseMethod("categoricaloptim")
}
#' @export
#' @rdname datatransform
categoricaloptim.default <- function(.data, y, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
categoricaloptim.data.frame <- function(.data, y, ...) {
  dots <- capture_dots(as_expr = F)
  y <- rlang::enexpr(y)
  data_y <- dplyr::select(.data, !!y)
  y_name <- names(data_y)
  dataset <- dplyr::select(.data, !!!dots)
  dataset <- dataset[, setdiff(names(dataset), names(data_y))]
  dataset <- bind_cols(data_y, dplyr::select(
    dataset,
    where(is.factor)
  ))
  x_names <- names(dataset)
  x_names <- setdiff(x_names, y_name)
  lcode <- eval_expr(lenc_glm(
    .data = dataset, y = !!y,
    everything()
  ),
  envir = env_curr()
  )
  dataset <- predict(lcode, dataset)
  dataset[, x_names] <-
    purrr::map_dfc(dataset[, x_names], .f = ~ roll_noise(.x,
                                                         random_percent = 0.05
    ))
  cartd <-
    eval_expr(cartdiscretize(
      .data = dataset, y = !!y_name,
      everything()
    ),
    envir = env_curr()
    )
  out <- list(
    lenc_glm = lcode,
    cartdiscretize = cartd,
    cols = x_names
  )
  c_attr <- attributes(out$cartdiscretize)
  out$cartdiscretize <- map(out$cartdiscretize, function(x) {
    x_attr <- attributes(x)
    x$labels <- factor_clean(x = seq_along(x$labels))
    attributes(x) <- x_attr
    x
  })
  attributes(out$cartdiscretize) <- c_attr

  attr(out, "outcome") <- y_name
  attr(out, "transformed") <- x_names
  class(out) <- c("categoricaloptim")
  out
}
#' @rdname datatransform
#' @export
predict.categoricaloptim <- function(object, new_data) {
  new_data <-
    predict(object = object$lenc_glm, new_data = new_data)
  new_data <-
    predict(object = object$cartdiscretize, new_data = new_data)
  new_data[, object$cols] <-
    purrr::map_dfc(
      new_data[, object$cols],
      factor
    )
  new_data
}
#' @export
print.categoricaloptim <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
do_categoricaloptim <- function(.data, y, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(categoricaloptim)
  predict(eval.parent(fncall), .data)
}

# getdummy--

#' @rdname datatransform
#' @export
getdummy <- function(.data, ..., threshold = 0.1) {
  UseMethod("getdummy")
}
#' @export
#' @rdname datatransform
getdummy.default <- function(.data, ..., threshold = 0.1) {
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data)
  x <- rlang::set_names(.data[pos], names(pos))
  getdummy <-
    names(select_if(x, ~ is.factor(.x) | is.character(.x)))
  if (is.numeric(threshold)) {
    keep_lvs <-
      keeplevels(
        .data = x[, getdummy],
        everything(),
        threshold = threshold
      )
    x <- predict(object = keep_lvs, new_data = x)
  } else {
    keep_lvs <- NULL
  }
  lvs <- lapply(x[, getdummy, drop = FALSE], unique)
  lvs <- lapply(lvs, levels)
  out <-
    purrr::map2_dfr(
      .x = lvs,
      .y = names(lvs),
      .f = ~ tibble(
        terms = .y,
        new_name = paste(.y, .x, sep = "_")
      )
    )
  out <- list(
    data = out,
    dummylvs = lvs,
    keeplevels = keep_lvs
  )
  attr(out, "transformed") <- names(out$dummylvs)
  class(out) <- "getdummy"
  out
}
#' @rdname datatransform
#' @export
predict.getdummy <- function(object, new_data) {
  if (nrow(object$data) == 0) {
    new_data
  } else {
    if (!all(names(object$dummylvs) %in% names(new_data))) {
      cat_stop("columnas perdidas")
    }
    if (!all(names(object$keeplevels) %in% names(new_data))) {
      cat_stop("columnas perdidas")
    }
    if (!is.null(object$keeplevels)) {
      new_data <- predict(object = object$keeplevels, new_data = new_data)
    }
    datadummy <-
      purrr::map2_dfc(
        .x = object$dummylvs,
        .y = names(object$dummylvs),
        .f = function(x, y) {
          purrr::map_dfc(.x = x, .f = ~ tibble(`:=`(
            !!.x,
            dplyr::if_else(as_chr(new_data[[y]]) ==
                             .x, 1, 0)
          ))) %>% rename_with(~ stringr::str_c(y,
                                               .x,
                                               sep = "_"
          ))
        }
      )
    new_data <- bind_cols(
      dplyr::select(
        new_data,
        -dplyr::matches(rlang::names2(
          object$dummylvs
        ))
      ),
      datadummy
    )
  }
  new_data
}
#' @export
print.getdummy <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
do_getdummy <- function(.data, ..., threshold = 0.1) {
  fncall <- match.call()
  fncall[[1L]] <- quote(getdummy)
  predict(eval.parent(fncall), .data)
}

# impute_kknn--


#' @rdname datatransform
#' @export
impute_kknn <- function(.data, ...) {
  UseMethod("impute_kknn")
}
#' @export
#' @rdname datatransform
impute_kknn.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
impute_kknn.data.frame <-
  function(.data,
           to_impute,
           ...,
           options = list(
             nthread = 1,
             eps = 1e-08
           ),
           neighbors = 5) {
    nn_index <- function(miss_data, ref_data, vars, K, opt) {
      gower::gower_topn(
        ref_data[, vars],
        miss_data[, vars],
        n = K,
        nthread = opt$nthread,
        eps = opt$eps
      )$index
    }
    nn_pred <- function(index, dat) {
      mode_est <- function(x) {
        if (!is.character(x) & !is.factor(x)) {
          cat_stop("The data should be character or factor to compute the mode.")
        }
        tab <- table(x)
        modes <- names(tab)[tab == max(tab)]
        sample(modes, size = 1)
      }
      dat <- dat[index, ]
      dat <- getElement(dat, names(dat))
      dat <- dat[!is.na(dat)]
      est <- if (is.factor(dat) | is.character(dat)) {
        mode_est(dat)
      } else {
        mean(dat)
      }
      est
    }
    dots <- capture_dots(as_expr = TRUE)
    to_impute <- rlang::enexpr(to_impute)
    pos <- unname(tidyselect::eval_select(dots, data = .data))
    pos <- unique(c(unname(
      tidyselect::eval_select(to_impute,
                              data = .data
      )
    ), pos))
    dataset <- .data[, pos]
    var_lists <-
      impute_var_lists(.data = dataset, to_impute = deparse(to_impute))
    all_x_vars <- lapply(var_lists, function(x) {
      c(x$x, x$y)
    })
    all_x_vars <- unique(unlist(all_x_vars))
    out <- list(
      neighbors = neighbors,
      ref_data = .data[, all_x_vars],
      options = options,
      columns = var_lists
    ) %class% "impute_kknn"
    attr(out, "predictor") <- out$columns[[1]]$x
    attr(out, "outcome") <- out$columns[[1]]$y
    out
  }
#' @rdname datatransform
#' @export
predict.impute_kknn <- function(object, new_data) {
  nn_index <- function(miss_data, ref_data, vars, K, opt) {
    gower::gower_topn(
      ref_data[, vars],
      miss_data[, vars],
      n = K,
      nthread = opt$nthread,
      eps = opt$eps
    )$index
  }
  nn_pred <- function(index, dat) {
    mode_est <- function(x) {
      if (!is.character(x) & !is.factor(x)) {
        cat_stop("The data should be character or factor to compute the mode.")
      }
      tab <- table(x)
      modes <- names(tab)[tab == max(tab)]
      sample(modes, size = 1)
    }
    dat <- dat[index, ]
    dat <- getElement(dat, names(dat))
    dat <- dat[!is.na(dat)]
    est <- if (is.factor(dat) | is.character(dat)) {
      mode_est(dat)
    } else {
      mean(dat)
    }
    est
  }
  missing_rows <- !complete.cases(new_data)
  if (!any(missing_rows)) {
    return(new_data)
  }
  old_data <- new_data
  imp_var <- object$columns[[1]]$y
  missing_rows <- !complete.cases(new_data[, imp_var])
  if (any(missing_rows)) {
    preds <- object$columns[[1]]$x
    imp_data <- old_data[missing_rows, preds, drop = FALSE]
    if (all(is.na(imp_data))) {
      cat_warn("All predictors are missing; cannot impute")
    } else {
      imp_var_complete <- !is.na(object$ref_data[[imp_var]])
      nn_ind <- nn_index(
        object$ref_data[imp_var_complete, ],
        imp_data,
        preds,
        object$neighbors,
        object$options
      )
      pred_vals <-
        apply(nn_ind, 2, nn_pred, dat = object$ref_data[
          imp_var_complete,
          imp_var
        ])
      pred_vals <- cast(pred_vals, object$ref_data[[imp_var]])
      new_data[[imp_var]] <- vctrs::vec_cast(
        new_data[[imp_var]],
        pred_vals
      )
      new_data[missing_rows, imp_var] <- pred_vals
    }
  }
  new_data
}
#' @export
print.impute_kknn <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
do_impute_kknn <-
  function(.data,
           to_impute,
           ...,
           options = list(nthread = 1, eps = 0.00000001),
           neighbors = 5) {
    fncall <- match.call()
    fncall[[1L]] <- quote(impute_kknn)
    predict(eval.parent(fncall), .data)
  }

# impute_linear--


#' @rdname datatransform
#' @export
impute_linear <- function(.data, ...) {
  UseMethod("impute_linear")
}
#' @export
#' @rdname datatransform
impute_linear.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
impute_linear.data.frame <- function(.data, to_impute, ...) {
  lm_wrap <- function(vars, dat) {
    dat <- as_df(dat[, c(vars$y, vars$x)])
    dat <- na.omit(dat)
    if (nrow(dat) == 0) {
      cat_stop(
        "The data used by impute_linear() did not have any rows",
        "where the imputation values were all complete."
      )
    }
    if (!is.numeric(dat[[vars$y]])) {
      cat_stop(
        "Variable '{vars$y}' chosen for linear regression imputation ",
        "must be of type numeric."
      )
    }
    out <- lm(as_frm(paste0(vars$y, "~", ".")),
              data = dat,
              model = FALSE
    )
    out$..imp_vars <- vars$x
    attr(out$terms, ".Environment") <- baseenv()
    out$call <- NULL
    out$assign <- NULL
    out$fitted.values <- NULL
    out$df.residual <- NULL
    out$residuals <- NULL
    out$qr$qr <- NULL
    out$effects <- NULL
    out
  }
  cdots <- capture_dots(as_expr = F)
  to_impute <- rlang::enexpr(to_impute)
  .data <- .data[!is.na(.data[[deparse(to_impute)]]), ]
  dataset <- dplyr::select(.data, !!!cdots)
  vip_imp <-
    eval_expr(
      expr = select_vip(.data = dataset, y = !!to_impute, !!!cdots),
      envir = env_curr()
    )
  dataset <- predict(vip_imp, dataset)
  var_lists <-
    impute_var_lists(.data = dataset, to_impute = deparse(to_impute))
  models <- lapply(var_lists, lm_wrap, dat = dataset)
  models <-
    map2(
      .x = models,
      .y = var_lists,
      ~ list(
        models = .x,
        impute_with = .y$x
      )
    )
  names(models) <- vapply(var_lists, function(x) {
    x$y
  }, c(""))
  attr(models, "predictor") <- models[[1]]$models$..imp_vars
  attr(models, "outcome") <- names(models)
  models %class% "impute_linear"
}
#' @rdname datatransform
#' @export
predict.impute_linear <- function(object, new_data) {
  missing_rows <- !complete.cases(new_data)
  if (!any(missing_rows)) {
    return(new_data)
  }
  old_data <- new_data
  imp_var <- names(object)
  missing_rows <- !complete.cases(new_data[, imp_var])
  if (any(missing_rows)) {
    preds <- object[[imp_var]]$models$..imp_vars
    pred_data <- old_data[missing_rows, preds, drop = FALSE]
    if (any(is.na(pred_data))) {
      cat_warn(
        "There were missing values in the predictor(s) used to impute;",
        "imputation did not occur."
      )
    } else {
      pred_vals <- predict(object[[imp_var]]$models, pred_data)
      pred_vals <- cast(pred_vals, new_data[[imp_var]])
      new_data[[imp_var]] <- vctrs::vec_cast(
        new_data[[imp_var]],
        pred_vals
      )
      new_data[missing_rows, imp_var] <- pred_vals
    }
  }
  as_tbl(new_data)
}
#' @keywords internal
cast <- function(x, ref) {
  if (is.factor(ref)) {
    x <- factor(x, levels = levels(ref), ordered = is.ordered(ref))
  } else {
    if (is.integer(ref) & !is.factor(ref)) {
      x <- as_int(round(x, 0))
    }
  }
  x
}
#' @keywords internal
impute_var_lists <- function(.data, to_impute) {
  impute_using <- names(.data)
  var_lists <- vector(mode = "list", length = length(to_impute))
  for (i in seq_along(var_lists)) {
    var_lists[[i]] <-
      list(y = to_impute[i], x = impute_using[!(impute_using %in%
                                                  to_impute[i])])
  }
  var_lists
}
#' @export
print.impute_linear <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
do_impute_linear <- function(.data, to_impute, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(impute_linear)
  predict(eval.parent(fncall), .data)
}

# keeplevels--


#' @export
#' @rdname datatransform
keeplevels <- function(.data, ..., threshold = 0.05) {
  UseMethod("keeplevels")
}
#' @export
#' @rdname datatransform
keeplevels.default <- function(.data, ..., threshold = 0.05) {
  expr <- dots(c(...))
  x <- dplyr::select(.data, !!!expr)
  cols_selc <- names(select_if(x, is_fctchr))
  out <- lapply(x[, cols_selc, drop = FALSE],
                keeplevels_impl,
                threshold = threshold,
                other = "other"
  )
  attr(out, "transformed") <- names(out)
  class(out) <- "keeplevels"
  out
}
#' @rdname datatransform
#' @export
predict.keeplevels <- function(object, new_data) {
  if (!is.null(object)) {
    for (i in names(object)) {
      if (object[[i]]$collapse) {
        if (!is.character(new_data[, i])) {
          tmp <- as_chr(getElement(new_data, i))
        } else {
          tmp <- getElement(new_data, i)
        }
        tmp <- ifelse(!(tmp %in% object[[i]]$keep) &
                        !is.na(tmp),
                      object[[i]]$other,
                      tmp
        )
        tmp <- factor(tmp, levels = c(
          object[[i]]$keep,
          object[[i]]$other
        ))
        new_data[, i] <- tmp
      }
    }
  }
  new_data
}
#' @export
print.keeplevels <- function(x) {
  generic_print(x)
}
#' @keywords internal
keeplevels_impl <- function(x, threshold, other) {
  if (!is.factor(x)) {
    x <- factor(x)
  }
  xtab <- sort(table(x, useNA = "no"), decreasing = TRUE)
  if (threshold < 1) {
    xtab <- xtab / sum(!is.na(x))
  }
  dropped <- which(xtab < threshold)
  orig <- levels(x)
  if (any(orig == other)) {
    candidates <- c(letters, LETTERS, paste(0:9))
    other <- paste0(other, sample(candidates, 2, replace = TRUE),
                    collapse = ""
    )
  }
  if (length(dropped) > 0) {
    keepers <- names(xtab[-dropped])
  } else {
    keepers <- orig
  }
  if (length(keepers) == 0) {
    keepers <- names(xtab)[which.max(xtab)]
  }
  if (other %in% keepers) {
    cat_stop(
      paste0(
        "The level ",
        other,
        " is already a factor level that will be retained. ",
        "Please choose a different value."
      )
    )
  }
  out <-
    list(
      keep = orig[orig %in% keepers],
      collapse = length(dropped) > 0,
      other = other
    )
  out
}
#' @rdname datatransform
#' @export
do_keeplevels <- function(.data, ..., threshold = 0.05) {
  fncall <- match.call()
  fncall[[1L]] <- quote(keeplevels)
  predict(eval.parent(fncall), .data)
}

# lenc_glm--


#' @export
#' @rdname datatransform
lenc_glm <- function(.data, y, ...) {
  UseMethod("lenc_glm")
}
#' @export
#' @rdname datatransform
lenc_glm.default <- function(.data, y, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
lenc_glm.data.frame <- function(.data, y, ...) {
  glm_coefs <- function(x, y, ...) {
    fam <- if (is.factor(y[[1]])) {
      binomial
    } else {
      gaussian
    }
    form <- as_frm(paste0(names(y), "~ 0 + value"))
    if (is.vector(x) | is.factor(x)) {
      x <- tibble(value = x)
    } else {
      x <- as_tbl(x)
    }
    mod <- stats::glm(
      form,
      data = bind_cols(x, y),
      family = fam,
      na.action = na.omit,
      ...
    )
    coefs <- coef(mod)
    names(coefs) <- gsub("^value", "", names(coefs))
    mean_coef <- mean(coefs, na.rm = TRUE, trim = 0.1)
    coefs[is.na(coefs)] <- mean_coef
    coefs <- c(coefs, ..new = mean_coef)
    if (is.factor(y[[1]])) {
      coefs <- -coefs
    }
    tibble(..level = names(coefs), ..value = unname(coefs))
  }
  dots <- dots(...)
  y <- rlang::enexpr(y)
  data_y <- dplyr::select(.data, !!y)
  dataset <- dplyr::select(.data, !!!dots)
  dataset <- dplyr::select(dataset, where(is.factor))
  x_names <- names(dataset)
  y_name <- lang2str(y)
  x_names <- setdiff(x_names, y_name)
  out <- map(dataset[, x_names], glm_coefs, y = data_y)
  out <- compact(out)
  class(out) <- c("lenc_glm")
  attr(out, "outcome") <- y_name
  attr(out, "transformed") <- names(out)
  out
}
#' @rdname datatransform
#' @export
predict.lenc_glm <- function(object, new_data) {
  if (length(object) == 0) {
    cat_stop("length(object) == 0")
  }
  .map_glm_coef <- function(dat, mapping) {
    new_val <- mapping$..value[mapping$..level == "..new"]
    dat <-
      dat %>%
      mutate(..order = 1:nrow(dat)) %>%
      purrr::set_names(c(
        "..level",
        "..order"
      )) %>%
      mutate(..level = as_chr(..level))
    mapping <- mapping %>% filter(.data$..level != "..new")
    dat <-
      left_join(dat, mapping, by = "..level") %>% arrange(..order)
    dat$..value[is.na(dat$..value)] <- new_val
    dat$..value
  }
  for (col in names(object)) {
    new_data[, col] <- .map_glm_coef(new_data[, col], object[[col]])
  }
  as_tbl(new_data)
}
#' @export
print.lenc_glm <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
do_lenc_glm <- function(.data, y, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(lenc_glm)
  predict(eval.parent(fncall), .data)
}

# lenc_lm--

#' @export
#' @rdname datatransform
lenc_lm <- function(.data, y, ...) {
  UseMethod("lenc_lm")
}
#' @export
#' @rdname datatransform
lenc_lm.default <- function(.data, y, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
lenc_lm.data.frame <- function(.data, y, ...) {
  glm_coefs <- function(x, y, ...) {
    form <- as_frm(paste0(names(y), "~ 0 + value"))
    if (is.vector(x) | is.factor(x)) {
      x <- tibble(value = x)
    } else {
      x <- as_tbl(x)
    }
    mod <- stats::lm(
      form,
      data = bind_cols(x, y),
      na.action = na.omit,
      ...
    )
    coefs <- coef(mod)
    names(coefs) <- gsub("^value", "", names(coefs))
    mean_coef <- mean(coefs, na.rm = TRUE, trim = 0.1)
    coefs[is.na(coefs)] <- mean_coef
    coefs <- c(coefs, ..new = mean_coef)
    if (is.factor(y[[1]])) {
      coefs <- -coefs
    }
    tibble(..level = names(coefs), ..value = unname(coefs))
  }
  dots <- dots(...)
  y <- rlang::enexpr(y)
  data_y <- dplyr::select(.data, !!y)
  dataset <- dplyr::select(.data, !!!dots)
  dataset <- dplyr::select(dataset, where(is.factor))
  x_names <- names(dataset)
  y_name <- lang2str(y)
  x_names <- setdiff(x_names, y_name)
  out <- map(dataset[, x_names], glm_coefs, y = data_y)
  out <- compact(out)
  class(out) <- c("lenc_lm")
  attr(out, "outcome") <- y_name
  attr(out, "transformed") <- names(out)
  out
}
#' @rdname datatransform
#' @export
predict.lenc_lm <- function(object, new_data) {
  if (length(object) == 0) {
    cat_stop("length(object) == 0")
  }
  .map_glm_coef <- function(dat, mapping) {
    new_val <- mapping$..value[mapping$..level == "..new"]
    dat <-
      dat %>%
      mutate(..order = 1:nrow(dat)) %>%
      purrr::set_names(c(
        "..level",
        "..order"
      )) %>%
      mutate(..level = as_chr(..level))
    mapping <- mapping %>% filter(.data$..level != "..new")
    dat <-
      left_join(dat, mapping, by = "..level") %>% arrange(..order)
    dat$..value[is.na(dat$..value)] <- new_val
    dat$..value
  }
  for (col in names(object)) {
    new_data[, col] <- .map_glm_coef(new_data[, col], object[[col]])
  }
  as_tbl(new_data)
}
#' @export
print.lenc_lm <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
do_lenc_lm <- function(.data, y, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(lenc_lm)
  predict(eval.parent(fncall), .data)
}

# lenc_mixed--


#' @export
#' @rdname datatransform
lenc_mixed <- function(.data, y, ...) {
  UseMethod("lenc_mixed")
}
#' @export
#' @rdname datatransform
lenc_mixed.default <- function(.data, y, ...) {
  expr <- dots(...)
  lme_coefs <- function(x, y, ...) {
    rlang::check_installed("lme4")
    args <-
      list(
        formula = y ~ 1 + (1 | value),
        data = data.frame(
          value = x,
          y = y
        ),
        na.action = na.omit
      )
    dots <- list(...)
    if (length(dots) > 0) {
      args <- c(args, dots[[1]])
    }
    if (!is.factor(y[[1]])) {
      cl <- rlang::call2("lmer", .ns = "lme4", !!!args)
      mod <- rlang::eval_tidy(cl)
    } else {
      args$data$y <- as_num(args$data$y) - 1
      args$family <- stats::binomial
      cl <- rlang::call2("glmer", .ns = "lme4", !!!args)
      mod <- rlang::eval_tidy(cl)
    }
    coefs <- coef(mod)$value
    ..levels <- rownames(coefs)
    coefs <- coefs[, 1]
    names(coefs) <- ..levels
    mean_coef <- mean(coefs, na.rm = TRUE, trim = 0.1)
    coefs[is.na(coefs)] <- mean_coef
    coefs <- c(coefs, ..new = mean_coef)
    if (is.factor(y[[1]])) {
      coefs <- -coefs
    }
    tibble(..level = names(coefs), ..value = unname(coefs))
  }
  y <- rlang::enexpr(y)
  dataset <- dplyr::select(.data, !!!expr)
  y_name <- deparse(rlang::enexpr(y))
  out <- map(dataset, lme_coefs, y = .data[[y_name]])
  out <- compact(out)
  class(out) <- "lenc_mixed"
  attr(out, "outcome") <- y_name
  attr(out, "transformed") <- names(out)
  out
}
#' @rdname datatransform
#' @export
predict.lenc_mixed <- function(object, new_data) {
  map_lme_coef <- function(dat, mapping) {
    new_val <- mapping$..value[mapping$..level == "..new"]
    dat <-
      dat %>%
      mutate(..order = 1:nrow(dat)) %>%
      purrr::set_names(c(
        "..level",
        "..order"
      )) %>%
      mutate(..level = as_chr(..level))
    mapping <- mapping %>% filter(.data$..level != "..new")
    dat <-
      left_join(dat, mapping, by = "..level") %>% arrange(..order)
    dat$..value[is.na(dat$..value)] <- new_val
    dat$..value
  }
  for (col in names(object)) {
    new_data[, col] <-
      map_lme_coef(new_data[, col], object[[col]])
  }
  new_data
}
#' @export
print.lenc_mixed <-
  function(x) {
    generic_print(x)
  }
#' @rdname datatransform
#' @export
do_lenc_mixed <- function(.data, y, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(lenc_mixed)
  predict(eval.parent(fncall), .data)
}

# multicollin--


#' @export
#' @rdname datatransform
multicollin <- function(.data, ...) {
  UseMethod("multicollin")
}
#' @export
#' @rdname datatransform
multicollin.default <- function(.data, ...) {
  generic_default()
}

#' @export
#' @rdname datatransform
multicollin.data.frame <-
  function(.data,
           ...,
           max_steps = 10,
           min_var = 0.1,
           max_cor = 0.9) {
    expr <- rlang::expr(c(...))
    pos <- tidyselect::eval_select(expr, data = .data)
    x <- cat2num(rlang::set_names(.data[pos], names(pos)))
    if (is.null(colnames(x))) {
      cat_stop("`x` should have column names")
    }
    orig_names <- colnames(x)
    if (!is.matrix(x)) {
      x <- as_mtx(x)
    }
    name_df <- data.frame(
      orig = orig_names,
      current = colnames(x),
      stringsAsFactors = FALSE
    )
    for (i in 1:max_steps) {
      if (i == max_steps) {
        (break)()
      }
      lcs <- multicollin_impl(x)
      if (length(lcs) == 0) {
        (break)()
      } else {
        x <- x[, !(colnames(x) %in% lcs)]
      }
    }
    name_df <- name_df[!(name_df$current %in% colnames(x)), ]
    multicollin <- name_df$orig
    if (length(multicollin) == 0) {
      multicollin <- NULL
    }
    multi_coll <-
      multi_coll(
        .data[, setdiff(orig_names, multicollin)],
        min_var, max_cor
      )
    out <- list(
      multicollin = multicollin,
      low_var = multi_coll$low_var,
      high_cor = multi_coll$high_cor
    )
    class(out) <- "multicollin"
    attr(out, "transformed") <- unique(unlist(out))
    out
  }
#' @rdname datatransform
#' @export
predict.multicollin <- function(object, new_data) {
  new_data[, setdiff(names(new_data), unlist(object))]
}
#' @export
print.multicollin <- function(x, ...) {
  print_multicollin <- function(x) {
    vec_cons <- c()
    if (mgalda::is_true(length(x$multicollin) > 0)) {
      c01 <- paste("\n\ncombos lineales:\n   ",
                   paste(x$multicollin, collapse = "\n   "),
                   .sep = " "
      )
      vec_cons <- c(vec_cons, c01)
    }
    if (mgalda::is_true(length(x$low_var) > 0)) {
      c02 <- paste("\n\nBaja varianza:\n   ",
                   paste(x$low_var, collapse = "\n   "),
                   .sep = " "
      )
      vec_cons <- c(vec_cons, c02)
    }
    if (mgalda::is_true(length(x$high_cor) > 0)) {
      c03 <- paste("\n\nAlta correlacion:\n   ",
                   paste(x$high_cor, collapse = "\n   "),
                   .sep = " "
      )
      vec_cons <- c(vec_cons, c03)
    }
    if (mgalda::is_true(sum(lengths(x)) == 0)) {
      vec_cons <- "No se encontraron variables con combos lineales"
    }
    paste0(vec_cons, collapse = "\n")
  }
  generic_print(x, .add_print = print_multicollin)
}
multicollin_impl <- function(x) {
  eps <- 0.000001
  if (!is.matrix(x)) {
    x <- as_mtx(x)
  }
  if (is.null(colnames(x))) {
    cat_stop("`x` should have column names")
  }
  qr_decomp <- qr(x)
  qr_decomp_R <- qr.R(qr_decomp)
  num_cols <- ncol(qr_decomp_R)
  rank <- qr_decomp$rank
  pivot <- qr_decomp$pivot
  if (is.null(num_cols) || rank == num_cols) {
    rm_list <- character(0)
  } else {
    p1 <- 1:rank
    x_qr <- qr_decomp_R[p1, p1]
    Y <- qr_decomp_R[p1, -p1, drop = FALSE]
    b <- qr(x_qr)
    b <- qr.coef(b, Y)
    b[abs(b) < eps] <- 0
    combos <- lapply(1:ncol(Y), function(i) {
      c(pivot[rank + i], pivot[which(b[, i] != 0)])
    })
    rm_list <- unlist(lapply(combos, function(x) {
      x[1]
    }))
    rm_list <- colnames(x)[rm_list]
  }
  rm_list
}
multi_coll <- function(data,
                       min_var = 0.1,
                       max_cor = 0.9) {
  if (!is.numeric(min_var) || !is.numeric(max_cor)) {
    cat_stop("min_var & max_cor deben ser numericos")
  }
  if (is.null(colnames(data))) {
    colnames(data) <- paste0("var", seq_len(ncol(data)))
  }
  data <- as_df(data)
  low_var <- names(data)[diag(stats::var(data)) < min_var]
  if (length(low_var) == ncol(data)) {
    cat_stop(
      "All variables have been removed based on the min_var\n",
      "level. Consider adjusting minimum acceptable variance\n",
      "levels to allow for some variables to be retained."
    )
  } else {
    data <- data[, setdiff(names(data), low_var), drop = FALSE]
  }
  if (ncol(data) > 1) {
    cor_mat <- stats::cor(data)
    cor_mat[lower.tri(cor_mat, diag = TRUE)] <- 0
    high_cor <- names(data[, sapply(
      as_df(cor_mat),
      function(x) {
        max(abs(x)) > max_cor
      }
    )])
  }
  list(low_var = low_var, high_cor = high_cor)
}
cat2num <- function(.data) {
  .cat2num <- function(.data) {
    .r_num <- are_dblint(.data)
    .r_num <- names(.r_num[.r_num])
    .r_num <- .r_num[.r_num != "row"]

    .r_cat <- are_fctchr(.data)
    .r_cat <- names(.r_cat[.r_cat])
    .r_cat <- .r_cat[.r_cat != "row"]

    if (length(.r_cat) == 0) {
      ret_invis(.data)
    }

    if (length(.r_num) == 0) {
      ret_invis(.data)
    }

    .ntbl <- f(tibble(num = rescalar(.x)))

    .sum_x<- f(do_try(rescalar(rowMeans(x,na.rm = TRUE))))

    for (nm in .r_cat) {

      .data[[nm]] <- .sum_x(map_dfc(
        .x = .data[, .r_num],
        .f = ~ do_lenc_glm(
          .data = bind_cols(.ntbl(.x), .data[, nm]),
          y = num,
          !!sym(nm)
        ) %>%
          pull(var = !!sym(nm))
      ))

    }
    .data
  }
  .folds_data <-
    rset2dflist(kfolds(
      dataset = rowindex(.data),
      times = 10,
      rep = 5
    ))
  map_dfr(.folds_data, .cat2num) %>%
    group_summarise_all(.groups = .data$row, ~mean(.x,na.rm = TRUE)) %>%
    select(!.data$row)
}
#' @rdname datatransform
#' @export
do_multicollin <-
  function(.data,
           ...,
           max_steps = 10,
           min_var = 0.1,
           max_cor = 0.9) {
    fncall <- match.call()
    fncall[[1L]] <- quote(multicollin)
    predict(eval.parent(fncall), .data)
  }

# near_zero--


#' @export
#' @rdname datatransform
near_zero <- function(.data, ...) {
  UseMethod("near_zero")
}
#' @export
#' @rdname datatransform
near_zero.default <- function(.data, ...) {
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data)
  x <- rlang::set_names(.data[pos], names(pos))
  x <-
    mutate_if(
      x,
      ~ sum(is.nan(.x)) > 0,
      .funs = function(x) {
        0
      }
    )
  out <- near_zero_impl(x)
  attr(out, "removed") <- out$term[out$nzv]
  out
}
#' @rdname datatransform
#' @export
predict.near_zero <- function(object, new_data) {
  new_data[, setdiff(names(new_data), object$term[object$nzv])]
}
#' @export
print.near_zero <- function(x, ...) {
  generic_print(x)
  if (sum(x$nzv) == 0) {
    cat_warn("No se encontraron variables desvalanceadas")
  }
  cat("\n")
  invisible(x)
}
near_zero_impl <- function(x) {
  freq_cut <- 95 / 5
  unique_cut <- 10
  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  }
  fr_foo <- function(data) {
    t <- table(data[!is.na(data)])
    if (length(t) <= 1) {
      return(0)
    }
    w <- which.max(t)
    return(max(t, na.rm = TRUE) / max(t[-w], na.rm = TRUE))
  }
  freq_ratio <- vapply(x, fr_foo, c(ratio = 0))
  uni_foo <- function(data) {
    length(unique(data[!is.na(data)]))
  }
  lunique <- vapply(x, uni_foo, c(num = 0))
  pct_unique <- 100 * lunique / vapply(x, length, c(num = 0))
  zero_func <- function(data) {
    all(is.na(data))
  }
  zero_var <- (lunique == 1) | vapply(x, zero_func, c(zv = TRUE))
  out <- which((freq_ratio > freq_cut & pct_unique <= unique_cut) |
                 zero_var)
  names(out) <- NULL
  tibble(term = colnames(x)) %>% mutate(
    nzv = row_number() %in%
      out,
    pct_unique = pct_unique,
    zero_var = zero_var,
    freq_ratio = freq_ratio
  ) %class%
    c("near_zero")
}
#' @rdname datatransform
#' @export
do_near_zero <- function(.data, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(near_zero)
  predict(eval.parent(fncall), .data)
}

# select_boruta-


#' @export
#' @rdname datatransform
select_boruta <-
  function(.data,
           y,
           ...,
           options = list(
             pValue = 0.01,
             mcAdj = TRUE,
             maxRuns = 20,
             doTrace = 9
           )) {
    UseMethod("select_boruta")
  }
#' @export
#' @rdname datatransform
select_boruta.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
select_boruta.data.frame <-
  function(.data,
           y,
           ...,
           options = list(
             pValue = 0.01,
             mcAdj = TRUE,
             maxRuns = 20,
             doTrace = 9
           )) {
    cdots <- capture_dots(as_expr = F)
    y <- rlang::enexpr(y)
    x_names <- names(dplyr::select(.data, !!!cdots))
    y_name <- deparse(y)
    x_names <- setdiff(x_names, y_name)
    if (length(x_names) > 0) {
      call <- rlang::call2(
        .fn = "Boruta",
        .ns = "Boruta",
        x = rlang::quo(.data[, x_names]),
        y = rlang::quo(.data[[y_name]]),
        !!!options
      )
      res <- rlang::eval_tidy(call)
      exclude <- names(res$finalDecision[res$finalDecision ==
                                           "Rejected"])
    } else {
      exclude <- character()
    }
    attr(res$ImpHistory, "imp_source") <- res$impSource
    out <- structure(
      list(
        exclude = exclude,
        options = options,
        imp_history = as_tbl(res$ImpHistory),
        call = res$call
      ),
      class = c("select_boruta")
    )
    attr(out, "removed") <- out$exclude
    attr(out, "outcome") <- y_name
    out
  }
#' @rdname datatransform
#' @export
predict.select_boruta <- function(object, new_data) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  as_tbl(new_data)
}
#' @export
print.select_boruta <- function(x) {
  generic_print(x)
  if (length(x$exclude) == 0) {
    cat_warn("No se eliminaron variables")
  }
  cat("\n")
  invisible(x)
}
#' @rdname datatransform
#' @export
do_select_boruta <-
  function(.data,
           y,
           ...,
           options = list(
             pValue = 0.01,
             mcAdj = TRUE,
             maxRuns = 20,
             doTrace = 9
           )) {
    fncall <- match.call()
    fncall[[1L]] <- quote(select_boruta)
    predict(eval.parent(fncall), .data)
  }

# select_pca --


#' @export
#' @rdname datatransform
select_pca <- function(.data, ...) {
  UseMethod("select_pca")
}
#' @export
select_pca.default <- function(.data, ...) {
  generic_default()
}
#' @export
select_pca.data.frame <-
  function(.data,
           ...,
           threshold = .8,
           num_comp = 4) {
    if (!is_prob(threshold)) {
      return(NULL)
    }
    obj_pca <- xpca(
      .data = .data,
      threshold = threshold,
      num_comp = num_comp,
      ...
    )
    cum_perc_var <-
      obj_pca$inf$variances$cumulative_percent_variance
    n <- length(cum_perc_var)
    if (n > num_comp) {
      cum_perc_var <- cum_perc_var[1:num_comp]
      n <- num_comp
    }
    cum_perc_var <-
      cum_perc_var[c(0, cum_perc_var[1:(n - 1)]) < threshold * 100]
    n <- length(cum_perc_var)
    contrib <-
      as_df(obj_pca$inf$var_results$contrib[, 1:n])
    contrib_comp <- obj_pca$inf$variances$percent_variance[1:n]
    vars <- rownames(contrib)
    value <-
      rowSums(data.frame(mapply(`*`, contrib, contrib_comp, SIMPLIFY = FALSE)))
    include <- tibble(vars = vars, value = value) %>%
      arrange(dplyr::desc(value)) %>%
      mutate(value = cumweight(value)) %>%
      filter(value <= threshold) %>%
      pull(vars)
    exclude <- vars[!vars %in% include]
    out <- list(exclude = exclude, res = obj_pca)
    attr(out, "removed") <- out$exclude
    class(out) <- "select_pca"
    out
  }
#' @export
predict.select_pca <- function(object, new_data) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  as_tbl(new_data)
}
#' @export
print.select_pca <- function(x) {
  generic_print(x)
  if (length(x$exclude) == 0) {
    cat_warn("No se eliminaron variables")
  }
  cat("\n")
  invisible(x)
}
#' @export
do_select_pca <-
  function(.data,
           ...,
           threshold = .8,
           num_comp = 4) {
    fncall <- match.call()
    fncall[[1L]] <- quote(select_pca)
    predict(eval.parent(fncall), .data)
  }
# select_vip--


#' @export
#' @rdname datatransform
select_vip <- function(.data, ...) {
  UseMethod("select_vip")
}
#' @export
#' @rdname datatransform
select_vip.default <-
  function(.data, ...) {
    generic_default()
  }
#' @export
#' @rdname datatransform
select_vip.data.frame <-
  function(.data,
           y,
           ...,
           top_p = floor(ncol(.data) / 2),
           threshold = 0.9) {
    cdots <- capture_dots(as_expr = F)
    y <- rlang::enexpr(y)
    x_names <- names(dplyr::select(.data, !!!cdots))
    y_name <- deparse(y)
    x_names <- setdiff(x_names, y_name)
    if (is_emptyna(top_p) & is_emptyna(threshold)) {
      cat_stop("For {match.call()[[1]]} `top_p` and `threshold` cannot both be missing.")
    } else {
      if (is.numeric(threshold)) {
        if (threshold >= 1 | threshold < 0) {
          cat_stop("`threshold` should be on (0, 1), not { threshold }.")
        }
        top_p <- NULL
      } else if (is_emptyna(threshold) & is.numeric(top_p)) {
        threshold <- NULL
        if (!is.integer(top_p)) {
          top_p <- as_int(top_p)
        }
        if (top_p >= length(x_names) | top_p <= 0) {
          top_p <- min(ncase(length(x_names)), floor(threshold *
                                                       length(x_names)))
        }
      }
    }
    if (inherits(.data[[y_name]], "factor")) {
      base_model <- parsnip::rand_forest(mode = "classification") %>%
        parsnip::set_engine("ranger", importance = "permutation")
    } else if (inherits(.data[[y_name]], c("integer", "numeric"))) {
      base_model <-
        parsnip::rand_forest(mode = "regression") %>% parsnip::set_engine("ranger",
                                                                          importance = "permutation"
        )
    }
    if (length(x_names) > 0) {
      x_data <- .data[, x_names]
      y <- .data[[y_name]]
      initial_model <- parsnip::fit_xy(base_model, x_data, y)
      res <- pull_importances(initial_model)
      names(res) <- c("variable", "score")
      res$score <- rlang::set_names(res$score, res$variable)
      exclude <- select_percentile(res$score, top_p, threshold,
                                   maximize = TRUE
      )
    } else {
      exclude <- character()
    }
    out <- list(exclude = exclude, res = res)
    attr(out, "removed") <- out$exclude
    attr(out, "outcome") <- y_name
    class(out) <- "select_vip"
    out
  }
#' @rdname datatransform
#' @export
predict.select_vip <- function(object, new_data) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  as_tbl(new_data)
}
select_percentile <- function(x, top_p, threshold, maximize) {
  if (missing(threshold) | is_emptyna(threshold)) {
    threshold <- NA
  }
  x <- x[!is.na(x)]
  if (!is_emptyna(threshold) & isTRUE(threshold > 0)) {
    p_to_exceed <- stats::quantile(x, threshold)
    if (maximize) {
      removals <- x < p_to_exceed
    } else {
      removals <- x >= p_to_exceed
    }
    removals <- names(removals[removals])
  } else {
    if (maximize) {
      x <- sort(x, decreasing = TRUE)
    } else {
      x <- sort(x, decreasing = FALSE)
    }
    removals <- names(x[-seq_len(top_p)])
  }
  removals
}
#' @export
print.select_vip <- function(x) {
  generic_print(x)
  if (length(x$exclude) == 0) {
    cat_warn("No se eliminaron variables")
  }
  cat("\n")
  invisible(x)
}
#' @rdname datatransform
#' @export
do_select_vip <-
  function(.data,
           y,
           ...,
           top_p = floor(ncol(.data) / 2),
           threshold = 0.9) {
    fncall <- match.call()
    fncall[[1L]] <- quote(select_vip)
    predict(eval.parent(fncall), .data)
  }

# simplifycategorical -

#' @rdname datatransform
#' @export
simplifycategorical <- function(.data, ...) {
  UseMethod("simplifycategorical")
}
#' @export
simplifycategorical.default <- function(.data, ...) {
  generic_default()
}
#' @export
simplifycategorical.data.frame <- function(.data, ..., min_f = 4) {
  ldots <- dots(...)
  fct_names <-
    map_lgl(select(.data, !!!ldots), ~ n_unique(.x) > min_f)
  .zerocat<-structure(list(),
                      transformed = "",
                      class = "simplifycategorical"
  )
  if(sum(fct_names)==0){
    return(.zerocat)
  }

  fct_names <- names(fct_names)[fct_names]
  fct_names <- setNames(fct_names, fct_names)

  out <-
    lapply(fct_names, function(x) {
      bind_cols(
        as_tbl(.data[, x]),
        .clean_data_prepro(datadiff(.data, x))
      )
    })

  if(length(out)==0){
    return(.zerocat)
  }

  out <- map(
    .x = out,
    .f = do_try(function(x) {
      x <- group_by(x, across(where(is.factor))) %>%
        mutate_at(vars(-group_cols()), mean) %>%
        ungroup() %>%
        do_xpca(where(is_dbl), threshold = .7, num_comp = 3)
    }))

  if(length(out)==0){
    return(.zerocat)
  }

  out <- map(
    .x = out,
    .f = do_try(function(x) {
      w <- c(2, trunc(max(n_unique(x[[1]]) / 2, 3)))

      k <-
        suppressall(do_call(
          f = "runskmeans",
          .args = c(
            x = list(x[, -1]),
            krange = list(w),
            limit = 5,
            runs = 5
          )
        ))

      x$.cluster <- k$partition
      x
    }))

  if(length(out)==0){
    return(.zerocat)
  }

  out <- map(
    .x = out,
    .f = do_try(function(x) {
      x %<>%
        select(factor = 1, .cluster) %>%
        count(factor, .cluster) %>%
        group_by(factor) %>%
        filter(n == max(n)) %>%
        ungroup() %>%
        mutate(.cluster = paste0("cat", .cluster))
      sort(set_names(x$.cluster, x$factor))
    })
  )
  out <- compact(out)

  if(length(out)==0){
    return(.zerocat)
  }

  attr(out, "transformed") <- names(out)
  class(out) <- "simplifycategorical"
  out
}
#' @export
predict.simplifycategorical <- function(object, new_data) {
  object <- object[lengths(object) > 0]
  for (nms in names(object)) {
    new_data[[nms]] <-
      recode(new_data[[nms]], !!!object[[nms]])
  }
  new_data
}
#' @export
print.simplifycategorical <- function(x) {
  generic_print(x)
}
#' @export
do_simplifycategorical <- function(.data, ..., min_f = 2) {
  fncall <- match.call()
  fncall[[1L]] <- quote(simplifycategorical)
  predict(eval.parent(fncall), .data)
}

.clean_data_prepro <- function(x) {
  recipes::recipe(~., data = x) %>%
    recipes::step_normalize(recipes::all_numeric()) %>%
    recipes::step_other(recipes::all_nominal(),threshold = .1) %>%
    recipes::step_dummy(recipes::all_nominal()) %>%
    recipes::step_pca(recipes::all_numeric(),threshold = .7) %>%
    recipes::prep() %>%
    recipes::juice()
}

# xcorrr--


#' @export
#' @rdname datatransform
xcorrr <-
  function(.data,
           ...,
           cutoff = 0.9,
           use = "pairwise.complete.obs",
           method = "pearson") {
    UseMethod("xcorrr")
  }
#' @export
#' @rdname datatransform
xcorrr.default <-
  function(.data,
           ...,
           cutoff = 0.9,
           use = "pairwise.complete.obs",
           method = "pearson") {
    expr <- rlang::expr(c(...))
    pos <- tidyselect::eval_select(expr, data = .data)
    x <- rlang::set_names(.data[pos], names(pos))
    x <- correlate_all(x, use = use, method = method)
    if (any(!complete.cases(x))) {
      all_na <- apply(x, 2, function(x) {
        all(is.na(x))
      })
      if (sum(all_na) >= nrow(x) - 1) {
        rlang::warn("skipping, muchos NAs")
        return(numeric(0))
      } else {
        na_cols <- which(all_na)
        if (length(na_cols) > 0) {
          x[na_cols, ] <- 0
          x[, na_cols] <- 0
          rlang::warn(
            paste0(
              "The correlation matrix has missing values. ",
              length(na_cols),
              " columns were excluded from the filter."
            )
          )
        }
      }
      if (any(is.na(x))) {
        rlang::warn(
          paste0(
            "The correlation matrix has sporadic missing values. ",
            "Some columns were excluded from the filter."
          )
        )
        x[is.na(x)] <- 0
      }
      diag(x) <- 1
    }
    average_corr <- colMeans(abs(x))
    average_corr <- as_num(as_fct(average_corr))
    x[lower.tri(x, diag = TRUE)] <- NA
    combs_above_cutoff <- which(abs(x) > cutoff)
    cols_to_check <- ceiling(combs_above_cutoff / nrow(x))
    rows_to_check <- combs_above_cutoff %% nrow(x)
    cols_to_discard <-
      average_corr[cols_to_check] > average_corr[rows_to_check]
    rows_to_discard <- !cols_to_discard
    deletecol <-
      c(cols_to_check[cols_to_discard], rows_to_check[rows_to_discard])
    deletecol <- unique(deletecol)
    if (length(deletecol) > 0) {
      deletecol <- colnames(x)[deletecol]
    }
    out <- list(deletecol = deletecol, corr_matrix = x)
    class(out) <- "xcorrr"
    attr(out, "removed") <- out$deletecol
    out
  }
#' @rdname datatransform
#' @export
predict.xcorrr <- function(object, new_data) {
  new_data[, setdiff(names(new_data), object$deletecol)]
}
#' @export
print.xcorrr <- function(x) {
  generic_print(x)
  if (length(x$deletecol) == 0) {
    cat_warn("No se encontraron variables con alta correlacion")
  }
  cat("\n")
  invisible(x)
}
#' @rdname datatransform
#' @export
do_xcorrr <-
  function(.data,
           ...,
           cutoff = 0.9,
           use = "pairwise.complete.obs",
           method = "pearson") {
    fncall <- match.call()
    fncall[[1L]] <- quote(xcorrr)
    predict(eval.parent(fncall), .data)
  }

# xnls--


#' @export
#' @rdname datatransform
xnls <- function(.data, ...) {
  UseMethod("xnls")
}
#' @export
#' @rdname datatransform
xnls.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
xnls.data.frame <- function(.data, x, y) {
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  obj <-
    try(nls2expr(
      .data = .data,
      x = !!x,
      y = !!y
    ), silent = T)
  if (inherits(obj, "try-error") | is_emptyna(obj)) {
    obj <- list()
  }
  class(obj) <- "xnls"
  attr(obj, "predictor") <- deparse(x)
  attr(obj, "outcome") <- deparse(y)
  obj
}
#' @rdname datatransform
#' @export
predict.xnls <- function(object, new_data) {
  vars_xnls <- attr(object, "predictor")
  newcol <- paste0("xnlsx_", vars_xnls)
  if (length(object) == 0) {
    return(new_data)
  }
  mutate(new_data, `:=`({{ newcol }}, rlang::eval_tidy(object)))
}
#' @export
print.xnls <- function(x) {
  pxnls <- function(x) {
    cat_title_head("Formula")
    cli::col_blue(paste0(attr(x, "outcome"), " ~ ", deparse(x)))
  }
  generic_print(x, .add_print = pxnls)
}
#' @rdname datatransform
#' @export
do_xnls <- function(.data, x, y) {
  fncall <- match.call()
  fncall[[1L]] <- quote(xnls)
  predict(eval.parent(fncall), .data)
}

# xpca --


#' @export
#' @rdname datatransform
xpca <- function(.data, ...) {
  UseMethod("xpca")
}
#' @export
#' @rdname datatransform
xpca.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
xpca.data.frame <-
  function(.data,
           ...,
           num_comp = 3,
           threshold = .9) {
    if (num_comp == 0) {
      cat_stop("'num_comp' debe ser mayor que 0")
    }
    expr <- rlang::expr(...)
    pos <- tidyselect::eval_select(expr, data = .data)
    dataset <- rlang::set_names(.data[pos], names(pos))
    cols_ori <- names(dataset)
    if (length(names(select_if(dataset, is_fctchr))) > 0) {
      dmy <- T
      xgetdummy <-
        getdummy(
          .data = dataset,
          where(is_fctchr),
          threshold = .1
        )
      dataset <- predict(xgetdummy, dataset)
    } else {
      dmy <- F
    }
    xnzv <- near_zero(.data = dataset, everything())
    dataset <- predict(xnzv, dataset)
    terms <-
      rlang::quos(!!!rlang::syms(names(dataset)), .named = T)
    x <-
      list(
        num_comp = num_comp,
        threshold = threshold,
        terms = terms
      )
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
      total_var <- sum(prc_obj$sdev^2)
      num_comp <-
        which.max(cumsum(prc_obj$sdev^2 / total_var) >= x$threshold)
      x$num_comp <- min(c(x$num_comp, num_comp))
    }
    x$cols <- cols_ori
    if (dmy) {
      x$dummy <- xgetdummy
    }
    x$nzv <- xnzv
    x$res <- prc_obj
    out <- list(
      object = x,
      inf = list(
        coefs = pca_coefs(x),
        variances = pca_variances(x),
        var_results = pca_var(dataset = dataset[, pcavars, drop = FALSE]),
        data = .data
      )
    )
    attr(out, "transformed") <- cols_ori
    class(out) <- "xpca"
    out
  }
#' @rdname datatransform
#' @export
predict.xpca <- function(object, new_data) {
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
    bind_cols(as_tbl(keep), as_tbl(comps))
  new_data
}
#' @rdname datatransform
#' @export
print.xpca <- function(x) {
  print_xpca <- function(x) {
    filtr_pnt <- x$inf$variances$cumulative_percent_variance <= 80
    if (length(filtr_pnt) > 5) {
      filtr_pnt[1:5] <- TRUE
    }
    cat_title_head(sprintf(
      "Varianza Acumulada (1, .., p=%d)\n",
      length(x$object$res$sdev)
    ))
    for (i in seq_along(x$inf$variances)) {
      cat_subtitle2(names(x$inf$variances)[i])
      print(purrr::set_names(x$inf$variances[[i]][filtr_pnt], paste0(
        "PC", seq_along(x$inf$variances[[i]][filtr_pnt])
      )))
    }
    d <- dim(x$object$res$rotation)
    cat_title_head(sprintf("\nRotacion (n x k) = (%d x %d):\n", d[1], d[2]))
    r <- x$inf$coefs
    r <-
      pivot_wider(r, names_from = terms, values_from = value)
    print(r[filtr_pnt, ])
    return(NULL)
  }
  generic_print(x = x, .add_print = print_xpca)
}
#' @rdname datatransform
pca_coefs <- function(x) {
  rot <- as_df(x$res$rotation)
  vars <- rownames(rot)
  if (x$num_comp > 0) {
    npc <- ncol(rot)
    res <- utils::stack(rot)
    colnames(res) <- c("value", "component")
    res$component <-
      as_chr(res$component)
    res$terms <- rep(vars, npc)
    res <-
      as_tbl(res)[, c("terms", "value", "component")]
  } else {
    res <- tibble(
      terms = vars,
      value = rlang::na_dbl,
      component = rlang::na_chr
    )
  }
}
#' @rdname datatransform
pca_variances <- function(x) {
  rot <- as_df(x$res$rotation)
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
    res <- tibble(
      terms = x,
      value = y,
      component = paste("PC", rep(1:p, 5), sep = "")
    )
  } else {
    res <- tibble(
      terms = vars,
      value = rep(rlang::na_dbl, length(vars)),
      component = rep(rlang::na_chr, length(vars))
    )
  }
  res <- split(res, res$terms)
  res <-
    map(
      res,
      ~ .x %>%
        pivot_wider(
          names_from = component,
          values_from = value
        ) %>%
        dplyr::select(!terms) %>%
        as_num()
    )
  res
}
#' @rdname datatransform
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
  var_cos2 <- var_cor^2
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
    round_sign(x)
  })
}
#' @rdname datatransform
#' @export
do_xpca <- function(.data,
                    ...,
                    num_comp = 3,
                    threshold = 0.9) {
  fncall <- match.call()
  fncall[[1L]] <- quote(xpca)
  predict(eval.parent(fncall), .data)
}

# xprepro-


#' @export
#' @rdname datatransform
xprepro <- function(.data, ...) {
  UseMethod("xprepro")
}
#' @export
#' @rdname datatransform
xprepro.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
xprepro.data.frame <- function(.data,
                               ...,
                               outcome = NULL,
                               pca = F) {
  expr <- dots(...)
  outcome <- new_quosure(outcome,env_call())
  data <- dplyr::select(.data, !!!expr) %>%
    mutate_if(is_fct, drop_levels)
  data <- drop_na(data)
  ini_names <- names(data)
  .frm <-
    str2lang(paste(setdiff(ini_names, lang2str(outcome)), collapse = " + "))

  outcome <- eval_quosures(outcome)

  if(is_empty(outcome)){

    .frm <- as_lazy(make_formula(lhs = 1, rhs = .frm, env = df2env(data)))
  }else {
    .frm <- as_lazy(make_formula(lhs = outcome, rhs = .frm, env = df2env(data)))
  }

  rec <- start_recipe(formula = .frm, data = data)
  rec <- prepro_recipe(rec = rec, pca = pca)
  structure(list(rec), class = "xprepro", transformed = ini_names)
}

#' @rdname datatransform
#' @export
predict.xprepro <- function(object, new_data) {
  bake(object = object[[1]], new_data = new_data)
}
#' @export
print.xprepro <- function(x) {
  cat_title_head("Pipe-Preproceso")
  nms <-
    map(x[[1]]$steps, ~ substr(x = class(.x)[1], 6, nchar(class(.x)[1])))
  .print <- map(
    x[[1]]$steps,
    function(x) {
      capture.output(printer(
        if (inherits(x, "step_multicollin")) {
          attr(x$object,"transformed")
        } else {
          names(x$object) %||%
            names(x$levels) %||%
            x$removals
        },
        x$terms,
        x$trained,
        width = max(20, options()$width - 30)
      ))
    }
  )

  purrr::walk2(
    .x = nms,
    .y = .print,
    .f = ~ cat(paste(
      cat_cumstyle(text = .x, style = "bold"), .y,
      sep = ": "
    ), sep = "\n")
  )
  cat_title_head("MGG")
}
#' @rdname datatransform
#' @export
do_xprepro <- function(.data, ...,outcome = NULL, pca = F) {
  fncall <- match.call()
  fncall[[1L]] <- quote(xprepro)
  predict(eval.parent(fncall), .data)
}


start_recipe <- function(formula, data) {
  .recipe_has.numeric_predictors <- function(x) {
    any(x$var_info$role == "predictor" & x$var_info$type == "numeric")
  }

  .recipe_has_nominal_predictors <- function(x) {
    any(x$var_info$role == "predictor" & x$var_info$type == "nominal")
  }
  if (inherits(formula, "lazy")) {
    formula <- formula$expr
  }

  rec <- recipes::recipe(formula, data)

  .nom_p <- .recipe_has_nominal_predictors(rec)
  .num_p <- .recipe_has.numeric_predictors(rec)
  if (.nom_p) {
    rec %<>%
      recipes::step_novel(recipes::all_nominal_predictors(),
                          new_level = ".lvl_other"
      ) %>%
      recipes::step_other(recipes::all_nominal_predictors(),
                          other = ".lvl_other",
                          threshold = 0.03
      )
    fac2sim<-rec %>%
      step_select(all_nominal_predictors()) %>%
      prep() %>%
      juice() %>%
      purrr::map(n_unique) %>%
      purrr::imap_dbl(~ifelse(.x > 5,.x,double(0))) %>%
      .[!is.na(.)] %>%
      names() %>%
      syms()

    if(length(fac2sim)>0){
      rec %<>%
        step_simplifycategorical(!!!fac2sim, min_f = 3)
    }

  }

  if (.num_p) {
    rec <-
      step_limits_bounds(recipe = rec, recipes::all_numeric_predictors()) %>%
      step_clip_bounds(
        recipes::all_numeric_predictors(),
        cutoff = .95,
        times = 10,
        rep = 3,
        method = "maha"
      )
  }
  rec
}
prepro_recipe <- function(rec, pca = FALSE) {
  .recipe_has.numeric_predictors <- function(x) {
    any(x$var_info$role == "predictor" & x$var_info$type == "numeric")
  }

  .recipe_has_nominal_predictors <- function(x) {
    any(x$var_info$role == "predictor" & x$var_info$type == "nominal")
  }
  .check_rec <- function(x) {
    x <- do_try_ier(prep(x))
    if (is_ier(x)) {
      return(TRUE)
    } else {
      return(x)
    }
  }

  rec_p <- prep(rec)

  .nom_p <- .recipe_has_nominal_predictors(rec_p)


  if (.nom_p) {
    cat_vars <- rec_p$term_info %>%
      filter(role == "predictor") %>%
      filter(type == "nominal") %>%
      pull(variable)

    for (.v in cat_vars) {

      out_type <- rec_p$term_info %>%
        filter(role == "outcome") %>%
        pull(type)

      if(is_empty(out_type)){
        rec <- step_dummy(recipe = rec, !!sym(.v))
      } else {

        if(out_type == "numeric"){
          tmp_rec <- step_lenc_lm(recipe = rec, !!sym(.v))
        } else if(out_type == "nominal"){
          tmp_rec <- step_lenc_glm(recipe = rec, !!sym(.v))
        }
        tmp_rec <- do_try_ier(prep(tmp_rec))

        if (is_ier(tmp_rec)) {
          rec <- step_dummy(recipe = rec, !!sym(.v))
        } else {
          rec <- tmp_rec
        }
      }}
    rec_p <- prep(rec)
  }

  .num_p <- .recipe_has.numeric_predictors(rec_p)

  if (.num_p) {
    num_vars <- rec_p$term_info %>%
      filter(role == "predictor") %>%
      filter(type == "numeric") %>%
      pull(variable)

    rec <- rec %>%
      step_bestnorm(all_numeric_predictors())
  }

  if (pca) {
    rec <-
      step_pca(recipe = rec, all_numeric_predictors())
  }

  rec %>%
    step_multicollin(recipes::all_predictors()) %>%
    step_xcorrr(recipes::all_predictors()) %>%
    recipes::step_nzv(recipes::all_predictors())%>%
    prep()
}
drop_levels <- function(f) {
  check_factor <- function(f) {
    if (is.character(f)) {
      factor(f)
    } else if (is.factor(f)) {
      f
    } else {
      cat_stop("`f` must be a factor (or character vector).")
    }
  }
  refactor <- function(f, new_levels) {
    new_f <-
      factor(f,
             levels = new_levels,
             exclude = NULL,
             ordered = is.ordered(f)
      )
    attributes(new_f) <-
      utils::modifyList(attributes(f), attributes(new_f))
    new_f
  }
  f <- check_factor(f)
  levels <- levels(f)
  count <- table(f)
  to_drop <- levels[count == 0]
  refactor(f, new_levels = setdiff(levels, to_drop))
}



# xrossdummy--


#' @rdname datatransform
#' @export
xrossdummy <- function(.data, dummy_var, ...) {
  UseMethod("xrossdummy")
}
#' @export
#' @rdname datatransform
xrossdummy.default <- function(.data, ...) {
  generic_default()
}
#' @rdname datatransform
#' @export
xrossdummy.data.frame <- function(.data, dummy_var, ...) {
  dummy_var <- substitute(dummy_var)
  num_vars <- eval(substitute_w(alist(...)), envir = env_call())
  if (length(num_vars) == 0) {
    num_vars <- quote(where(is_dblint))
  }
  dataset <- select_str(.data, lang2str(c(num_vars, dummy_var)))
  rname <- names(dataset)
  dataset <- dataset %>% rowindex(random_name = T)
  rname <- setdiff(names(dataset), rname)
  d_split <- split(dataset, dataset[[lang2str(dummy_var)]])
  dummy_lvs <- names(d_split)
  d_split <- purrr::map(
    .x = dummy_lvs,
    .f = ~ select_str(
      d_split[[.x]],
      lang2str(num_vars), rname
    ) %>% rename_prefix(
      .prefix = .x,
      -all_of(rname)
    )
  )
  names(d_split) <- dummy_lvs
  out <- list(transf = NULL, info = NULL)
  out$transf[["split"]] <-
    list(lvs = dummy_lvs, var = lang2str(dummy_var))
  dataset[[rname]] <- NULL
  out$transf[["target"]] <-
    nmdiff(data = dataset, x = lang2str(dummy_var))
  d_split <-
    map(.x = d_split, .f = ~ .x %>% dplyr::select(!matches(rname)))
  out$info[["dataset"]] <- dataset
  out$info[["split"]] <- d_split
  attr(out, "interaction") <- collapse_interaccion(out$info$dataset)
  class(out) <- "xrossdummy"
  out
}

#' @rdname datatransform
#' @export
predict.xrossdummy <- function(object, new_data) {
  rname <- names(new_data)
  new_data <- new_data %>% rowindex(random_name = T)
  rname <- setdiff(names(new_data), rname)
  dummy_var <- str2lang(object$transf$split$var)
  dummy_lvs <- object$transf$split$lvs
  num_vars <- object$transf$target
  new_data_split <-
    split(new_data, new_data[[object$transf$split$var]])
  new_data_split <- map(
    .x = dummy_lvs,
    .f = ~ select_str(
      new_data_split[[.x]],
      deparse(num_vars), rname
    ) %>% rename_prefix(
      .prefix = .x,
      -all_of(rname)
    )
  )
  names(new_data_split) <- dummy_lvs
  bind_cols(
    bind_rows(new_data_split) %>% mutate_at(
      .vars = vars(dplyr::contains(dummy_lvs)),
      .funs = ~ tidyr::replace_na(.x, 0)
    ) %>% arrange_str(rname) %>%
      select_str(paste0("-", rname)),
    new_data %>% dplyr::select(!all_of(c(
      num_vars,
      deparse(dummy_var), rname
    )))
  )
}

#' @export
print.xrossdummy <- function(x) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
do_xrossdummy <- function(.data, dummy_var, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(xrossdummy)
  predict(eval.parent(fncall), .data)
}

# xstandarice--


#' @rdname datatransform
#' @export
xstandarice <- function(.data, ...) {
  UseMethod("xstandarice")
}
#' @export
#' @rdname datatransform
xstandarice.default <- function(.data, ...) {
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data)
  x <- rlang::set_names(.data[pos], names(pos))
  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  }
  x <- as_df(x)
  if (!all(are_dblint(x))) {
    cat_stop("data no numericas")
  }
  out <- lapply(x, function(x) {
    list(
      mu = mean(x, na.rm = TRUE),
      sigma = sd(x, na.rm = TRUE)
    )
  })
  attr(out, "transformed") <- names(out)
  class(out) <- "xstandarice"
  out
}
#' @rdname datatransform
#' @export
predict.xstandarice <- function(object, new_data) {
  if (length(object) == 0) {
    new_data
  } else {
    if (!all(names(object) %in% names(new_data))) {
      cat_stop("columnas perdidas")
    }
    new_data[, names(object)] <- purrr::map2_dfc(
      .x = object,
      .y = names(object),
      .f = ~ tibble(`:=`(
        !!.y,
        (
          new_data[[.y]] - .x$mu
        ) / .x$sigma
      ))
    )
  }
  new_data
}
#' @export
print.xstandarice <- function(x, width = max(20, options()$width -
                                               30), ...) {
  generic_print(x)
}
#' @rdname datatransform
#' @export
do_xstandarice <- function(.data, ...) {
  fncall <- match.call()
  fncall[[1L]] <- quote(xstandarice)
  predict(eval.parent(fncall), .data)
}

# smoooth_vec_samples --


#' @rdname datatransform
#' @export
smoooth_vec_samples <- function(x, ...) {
  UseMethod("smoooth_vec_samples")
}
#' @export
#' @rdname datatransform
smoooth_vec_samples.default <- function(x, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
smoooth_vec_samples.numeric <-
  function(x,
           times = 4,
           rep = 3,
           na_rm = TRUE,
           ...) {
    if (na_rm) {
      x <- sort(x[!is_nonnum(x)])
    }
    x <- sort(x)
    xm <- mean(x)
    xs <- sd(x)
    train_xfolds <- kfolds_vec(x = x, times = times, rep = rep)
    train_xfolds <- lapply(train_xfolds, sort)
    train_xfolds <-
      lapply(train_xfolds, function(x) {
        data.frame(orig = x, norm = (x - xm) / xs)
      })
    lapprox <-
      suppressall(lapply(train_xfolds, function(dat) {
        approxfun(
          x = dat$norm,
          y = dat$orig,
          rule = 2
        )
      }))
    lspline <-
      suppressall(lapply(train_xfolds, function(dat) {
        splinefun(y = dat$orig, x = dat$norm)
      }))
    lloess <-
      lapply(train_xfolds, function(dat) {
        loess(formula = orig ~ norm, data = dat)
      })
    llm <-
      lapply(train_xfolds, function(dat) {
        lm(formula = orig ~ norm, data = dat)
      })
    limits <- clip_object(x = x)
    lmods <-
      list(
        lineal = llm,
        loess = lloess,
        approx = lapprox,
        spline = lspline,
        limits = limits,
        par = c(mean = xm, sd = xs)
      )
    class(lmods) <- "smoooth_vec_samples"
    attr(lmods, "data") <- x
    lmods
  }
#' @export
#' @rdname datatransform
smoooth_vec_samples.integer <-
  function(x,
           times = 4,
           rep = 3,
           na_rm = TRUE,
           ...) {
    if (na_rm) {
      x <- sort(x[!is_nonnum(x)])
    }
    x <- sort(x)
    xm <- mean(x)
    xs <- sd(x)
    train_xfolds <- kfolds_vec(x = x, times = times, rep = rep)
    train_xfolds <- lapply(train_xfolds, sort)
    train_xfolds <-
      lapply(train_xfolds, function(x) {
        data.frame(orig = x, norm = (x - xm) / xs)
      })
    lapprox <-
      suppressall(lapply(train_xfolds, function(dat) {
        approxfun(
          x = dat$norm,
          y = dat$orig,
          rule = 2
        )
      }))
    lspline <-
      suppressall(lapply(train_xfolds, function(dat) {
        splinefun(y = dat$orig, x = dat$norm)
      }))
    lloess <-
      lapply(train_xfolds, function(dat) {
        loess(formula = orig ~ norm, data = dat)
      })
    llm <-
      lapply(train_xfolds, function(dat) {
        lm(formula = orig ~ norm, data = dat)
      })
    limits <- clip_object(x = x)
    lmods <-
      list(
        lineal = llm,
        loess = lloess,
        approx = lapprox,
        spline = lspline,
        limits = limits,
        par = c(mean = xm, sd = xs)
      )
    class(lmods) <- "smoooth_vec_samples"
    lmods
  }
#' @rdname datatransform
#' @export
predict.smoooth_vec_samples <- function(object, new_vec) {
  dat <- data.frame(norm = (new_vec - object$par[1]) / object$par[2])
  plm <- matrix(sapply(object$lineal, function(x) {
    predict(object = x, dat)
  }), nrow = length(new_vec))
  ploess <- matrix(sapply(object$loess, function(x) {
    predict(object = x, dat)
  }), nrow = length(new_vec))
  papp <-
    matrix(sapply(object$approx, function(x) {
      x(dat$norm)
    }), nrow = length(new_vec))
  pspl <-
    matrix(sapply(object$spline, function(x) {
      x(dat$norm)
    }), nrow = length(new_vec))
  pm <-
    apply(cbind(plm, ploess, papp, pspl), 2, function(x) {
      predict(object$limits, x)
    })
  rowMeans(x = pm, na.rm = TRUE)
}
#' @export
print.smoooth_vec_samples <- function(x) {
  cat_title_head("Smoooth vec samples")
  num <- mgalda::num_format(x = range(attr(x, "data")))
  num <- paste0("[", num[1], " , ", num[2], "]")
  cli::cli_bullets(text = c("*" = num))
}

# save_pipe --


#' @rdname datatransform
#' @export
save_pipe <- function(.data, ...) {
  UseMethod("save_pipe")
}
#' @export
#' @rdname datatransform
save_pipe.default <- function(.data, ...) {
  generic_default()
}
#' @export
#' @rdname datatransform
save_pipe.data.frame <- function(.data, expr) {
  if (!is.call(expr)) {
    cat_stop("expr debe ser tipo call")
  }
  check_eval <-
    do_try(eval_parse(paste0(".data %>% ", deparse1(expr))), "error_obj")
  if (is_true(check_eval == "error_obj")) {
    cat_stop("expr o new_data no cumplen con los requisitos")
  }
  out <- list(
    expr = expr,
    cols = names(.data)
  )
  class(out) <- "save_pipe"
  out
}
#' @rdname datatransform
#' @export
predict.save_pipe <- function(object, new_data) {
  if (is_empty(new_data)) {
    new_data <- dplyr::cur_data_all()
  }
  if (!all(object$cols %in% colnames(new_data))) {
    cat_warn("no existen todas las columnas en new_data")
    return(new_data)
  }
  eval_parse(paste0("new_data %>% ", deparse1(object$expr)))
}
#' @export
print.save_pipe <- function(x) {
  cat("Save Pipe Object")
}
