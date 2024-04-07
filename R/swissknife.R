# stats-statistic -------------------------------------------------------------------------
#' @keywords internal
shapiro_test <- function(x) {
  if (length(x) > 5000) {
    x <- x[unique(round(seq.int(1, length(x), length.out = 5000)))]
  }

  if (length(x) < 5) {
    stop("sample size must be between 5 and 5000")
  }

  x <- sort(x[complete.cases(x)])
  n <- length(x)
  y <- qnorm(ppoints(n, a = 3 / 8))
  W <- cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2 / u)
  z <- (log(1 - W) - mu) / sig
  pval <- pnorm(z, lower.tail = FALSE)
  dplyr::tibble(
    metrica = "shapiro_test",
    statistic = W,
    pvalue = round(pval, 5)
  )
}
#' @keywords internal
pearson_test <- function(x) {
  x <- x[complete.cases(x)]

  n <- length(x)

  k <- ncase(n)

  num <- floor(1 + k * pnorm(x, mean(x), sd(x)))

  count <- tabulate(num, k)

  prob <- rep(1 / k, k)

  xpec <- n * prob
  h <- ((count - xpec)^2) / xpec
  P <- sum(h)
  pvalue <- pchisq(P, k - 1, lower.tail = FALSE)
  dplyr::tibble(
    metrica = "pearson_test",
    value = P / (k - 1),
    statistic = P,
    pvalue = pvalue,
    df = k - 1
  )
}
#' @keywords internal
cramervon_test <- function(x) {
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 8) {
    stop("sample size must be greater than 7")
  }
  p <- pnorm((x - mean(x)) / sd(x))
  W <- (1 / (12 * n) + sum((p - (2 * seq(
    1:n
  ) - 1) / (2 * n))^2))
  WW <- (1 + 0.5 / n) * W
  dplyr::tibble(
    metrica = "cramervon_test",
    statistic = WW
  )
}
#' @keywords internal
normaltest <- function(x) {
  pearson_test <- pearson_test(x)
  pearson_test <- pearson_test[, c("metrica", "statistic")]
  shapiro_test <- shapiro_test(x)
  shapiro_test <- shapiro_test[, c("metrica", "statistic")]
  cramervon_test <- cramervon_test(x)

  dplyr::bind_rows(pearson_test, shapiro_test, cramervon_test)
}
#' @keywords internal
glmn_coefs <- function(x, penalty = 0.01) {
  if (!inherits(x, "workflow")) {
    rlang::abort("'object' debe ser clase clase workflow, tidymodels")
  }
  x <- workflows::extract_fit_parsnip(x) %>%
    purrr::pluck("fit")
  x <- glmnet::coef.glmnet(x, s = penalty)
  x <- as.matrix(x)
  colnames(x) <- "estimate"
  rn <- rownames(x)
  x <-
    tibble::as_tibble(x) %>% dplyr::mutate(terms = rn, penalty = penalty)
  x <- dplyr::select(x, terms, estimate, penalty)
  if (is.list(x$estimate)) {
    x$estimate <-
      purrr::map(
        x$estimate,
        ~ tibble::as_tibble(as.matrix(.x), rownames = "terms")
      )
    x <-
      tidyr::unnest(x, cols = c(estimate), names_repair = "minimal")
    names(x) <- c("class", "terms", "estimate", "penalty")
  }
  x
}
#' @keywords internal
pseudo_rsq <- function(object, dataset) {
  if (!inherits(object, "workflow")) {
    rlang::abort("'object' debe ser clase clase workflow, tidymodels")
  }

  x <- workflows::extract_fit_parsnip(x = object)
  rec <- workflows::extract_preprocessor(x = object)


  if (!inherits(x$fit, c("multinom", "glm"))) {
    rlang::abort("'object' debe ser clase modelo multinom o glm")
  }

  ycol <- rec$var_info$variable[rec$var_info$role == "outcome"]

  null_data <- dataset[, as.character(ycol), drop = FALSE]

  null_formula <- paste(ycol, " ~ 1", sep = "")

  null_formula <-
    as.formula(str2lang(null_formula), env = rlang::current_env())

  l_full <- logLik(x$fit)
  d_full <- -2 * l_full

  null_mod <-
    fit(
      object = x$spec,
      data = null_data,
      formula = null_formula
    )

  l_base <- logLik(null_mod$fit)

  if (inherits(x = x$fit, what = "multinom")) {
    edf <- x$fit$edf
  } else if (inherits(x = x$fit, what = "glm")) {
    edf <- x$fit$rank
  }

  d_base <- -2 * l_base # deviance(update(x, ~1))
  g2 <- -2 * (l_base - l_full)

  # n <- if(length(weights(x)))
  #   sum(weights(x))
  # else
  n <-
    attr(l_full, "nobs") # alternative: n <- dim(x$residuals)[1]

  # mc_fadden
  mc_fadden <- 1 - (l_full / l_base)
  # adjusted to penalize for the number of predictors (k) in the model
  mc_fadden_adj <- 1 - ((l_full - edf) / l_base)

  # nagelkerke / CraggUhler
  nagelkerke <-
    (1 - exp((d_full - d_base) / n)) / (1 - exp(-d_base / n))

  # CoxSnell / Maximum Likelihood R2
  cox_snell <- 1 - exp(-g2 / n)

  res <- c(
    mc_fadden = mc_fadden,
    mc_fadden_adj = mc_fadden_adj,
    cox_snell = cox_snell,
    nagelkerke = nagelkerke,
    AldrichNelson = NA,
    VeallZimmermann = NA,
    Efron = NA,
    McKelveyZavoina = NA,
    Tjur = NA
  )

  if (inherits(x = x$fit, what = "glm")) {
    fam <- x$fit$family$family
    link <- x$fit$family$link
    y <- x$fit$y

    s2 <- switch(link,
      probit = 1,
      logit = pi^2 / 3,
      NA
    )

    res["AldrichNelson"] <- g2 / (g2 + n)

    res["VeallZimmermann"] <-
      res["AldrichNelson"] * (2 * l_base - n) / (2 * l_base)

    y.hat <-
      predict(x$fit, type = "link")

    y.hat.resp <-
      predict(x$fit, type = "response")

    sse <- sum((y.hat - mean(y.hat))^2)
    res["McKelveyZavoina"] <- sse / (n * s2 + sse)

    # EfronR2
    res["Efron"] <- (1 - (sum((y - y.hat.resp)^2)) /
      (sum((y - mean(
        y
      ))^2)))

    # Tjur's D
    # compare with binomTools::Rsq.glm()
    if (identical(fam, "binomial")) {
      res["Tjur"] <-
        unname(diff(tapply(y.hat.resp, y, mean, na.rm = TRUE)))
    }
  }
  res[!is.na(res)]
}
#' @keywords internal
rescalar_outliers <- function(x, scaleporc = 2) {
  w <- length(x %out% c(1 * IQR(x) + mean(x), -1 * IQR(x) + mean(x)))

  if (w > 0) {
    xdf <- dplyr::tibble(x = x)

    xdf$xcb <- clip_bounds_vec(xdf$x)

    xdf$xbn <- bestnorm_vec(x = xdf$xcb)

    xdf$scale <-
      ifelse(xdf$x != xdf$xcb, rescalar(xdf$xbn, to = range(xdf$xcb)), xdf$x)

    x <- ifelse(xdf$x != xdf$xcb,
      (xdf$x + xdf$scale * scaleporc) / (scaleporc + 1),
      xdf$x
    )
  }
  x
}
#' @keywords internal
entropy <- function(x) {
  x <- x / sum(x)
  -sum(ifelse(x > 0, x * log2(x), 0))
}
#' @keywords internal
qsr <- function(x) {
  sum(x[(x > quantile(x, 0.8))]) / sum(x[(x < quantile(x, 0.2))])
}
#' @keywords internal
gini <- function(x) {
  n <- length(x)
  x <- sort(x)
  G <- sum(x * 1L:n)
  G <- 2 * G / sum(x) - (n + 1L)
  G / n
}
#' @keywords internal
geometricmean <- function(x) {
  exp(mean(log(x)))
}
#' @keywords internal
skewness <- function(x) {
  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3 / 2)
}
#' @keywords internal
quartile_skewness <- function(x) {
  q <- quantile(x, c(.25, .50, .75))

  ((q[3] - q[2]) - (q[2] - q[1])) / (q[3] - q[1])
}
#' @keywords internal
kurtosis <- function(x) {
  n <- length(x)
  n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}
#' @keywords internal
rescalar <- function(x,
                     to = c(0, 1),
                     from = range(x)) {
  (x - from[1]) / diff(from) * diff(to) + to[1]
}
#' @keywords internal
iqrize <- function(x) {
  med <- median(x)
  iqr <- IQR(x, na.rm = TRUE)
  (x - med) / iqr
}
#' @keywords internal
rank_percent <- function(x) {
  min_rank <- rank(x, ties.method = "min", na.last = "keep")

  (min_rank - 1) / (sum(!is.na(x)) - 1)
}
#' @keywords internal
center_scale <- function(x) {
  x <- as.matrix(x)
  center <- colMeans(x, na.rm = TRUE)
  x <- sweep(x, 2L, center, check.margin = FALSE)

  f <- function(v) {
    v <- v[!is.na(v)]
    sqrt(sum(v^2) / max(1, length(v) - 1L))
  }

  scale <- apply(x, 2L, f)

  x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
  as.numeric(x)
}
#' @keywords internal
cov_alt <- function(x, y) {
  n <- length(x)
  (sum(x * y) / (n - 1)) - (n / (n - 1)) * mean(x) * mean(y)
}
#' @keywords internal
heaviside <-
  function(x, a = 0) {
    (sign(x - a) + 1) / 2
  }


# stats-cluster tools ---------------------------------------------------------------------

#' @keywords internal
dunn <-
  function(clustering,
           dmatrix = NULL,
           method = "euclidean") {
    if ("dist" %in% class(dmatrix)) {
      distance <- as.matrix(dmatrix)
    } else {
      distance <- as.matrix(dist(dmatrix))
    }
    nc <- max(clustering)
    inter_clust <- matrix(NA, nc, nc)
    intra_clust <- rep(NA, nc)
    for (i in 1:nc) {
      c1 <- which(clustering == i)
      for (j in i:nc) {
        if (j == i) {
          intra_clust[i] <- max(distance[c1, c1])
        }
        if (j > i) {
          c2 <- which(clustering == j)
          inter_clust[i, j] <- min(distance[c1, c2])
        }
      }
    }
    list(
      dunn = min(inter_clust, na.rm = TRUE) / max(intra_clust, na.rm = TRUE),
      inter_clust = inter_clust,
      intra_clust = intra_clust
    )
  }

#' @keywords internal

weightedss <- function(x, clustering, w = NULL) {
  x <- as.matrix(x)
  sizecolX <- ncol(x)
  if (is.null(w)) {
    w <- rep(1, sizecolX)
  }
  K <- length(unique(clustering))
  X.scaled <- t(t(x) * sqrt(w))
  cluster.centers <-
    stats::aggregate(X.scaled, by = list(clustering), FUN = mean)[, -1]
  cluster.densities <- table(clustering)
  matrix.wss <-
    t(sapply(
      1:K,
      FUN = wss_components,
      x = X.scaled,
      cl = clustering,
      simplify = T
    ))
  matrix.bss <-
    (cluster.centers - matrix(
      apply(X.scaled, 2, mean),
      nrow = K,
      ncol = sizecolX,
      byrow = T
    ))^2 * cluster.densities
  wss.per.feature <- apply(matrix.wss, 2, sum)
  wss.per.cluster <- apply(matrix.wss, 1, sum)
  bss.per.feature <- apply(matrix.bss, 2, sum)
  bss.per.cluster <- apply(matrix.bss, 1, sum)
  bss <- sum(bss.per.cluster)
  wss <- sum(wss.per.cluster)
  names(bss.per.feature) <- colnames(x)
  names(wss.per.feature) <- colnames(x)
  names(bss.per.cluster) <- 1:K
  names(wss.per.cluster) <- 1:K
  list(
    bss.per.feature = bss.per.feature,
    wss.per.feature = wss.per.feature,
    bss.per.cluster = bss.per.cluster,
    wss.per.cluster = wss.per.cluster,
    bss = bss,
    wss = wss,
    totss = bss + wss
  )
}

#' @keywords internal
wss_components <- function(k, x, cl) {
  res <- x[cl == k, ]
  if (is.null(dim(res)) == F) {
    res <- apply(res, 2, scale, center = T, scale = F)
    res <- apply(res, 2, function(x) {
      sum(x^2)
    })
  } else {
    res <- rep(0, dim(x)[2])
  }
  return(res)
}

#' @keywords internal
connectivity <-
  function(clustering,
           dmatrix = NULL,
           method = "euclidean",
           neighbSize = trunc(length(clustering) * .025)) {
    if ("dist" %in% class(dmatrix)) {
      distance <- as.matrix(dmatrix)
    } else {
      distance <- as.matrix(dist(dmatrix))
    }

    nearest <-
      apply(distance, 2, function(x) {
        sort(x, ind = TRUE)$ix[2:(neighbSize + 1)]
      })
    nr <- nrow(nearest)
    nc <- ncol(nearest)
    same <- matrix(clustering,
      nrow = nr,
      ncol = nc,
      byrow = TRUE
    ) !=
      matrix(clustering[nearest], nrow = nr, ncol = nc)
    sum(same * matrix(1 / 1:neighbSize, nrow = nr, ncol = nc))
  }

#' @keywords internal
calinhara <-
  function(x, clustering) {
    cn <- max(clustering)
    x <- as.matrix(x)
    p <- ncol(x)
    n <- nrow(x)
    cln <- rep(0, cn)
    W <- matrix(0, p, p)
    for (i in 1:cn) {
      cln[i] <- sum(clustering == i)
    }
    for (i in 1:cn) {
      clx <- x[clustering == i, ]
      cclx <- cov(as.matrix(clx))
      if (cln[i] < 2) {
        cclx <- 0
      }
      W <- W + ((cln[i] - 1) * cclx)
    }
    S <- (n - 1) * cov(x)
    B <- S - W
    out <- (n - cn) * sum(diag(B)) / ((cn - 1) * sum(diag(W)))
    out
  }



# stats-normalize -------------------------------------------------------------------------

#' @keywords internal

transform_powertwo <- function(x) {
  stopifnot(is.numeric(x))

  tx <- x^2
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(x^2)
  fn_transform <- rlang::expr((x - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "powertwo")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "powertwo")
    )
  }
  val
}


#' @keywords internal

transform_powerthree <- function(x) {
  stopifnot(is.numeric(x))

  tx <- x^3
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(x^3)
  fn_transform <- rlang::expr((x - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "powerthree")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "powerthree")
    )
  }
  val
}

#' @keywords internal

transform_exp <- function(x) {
  stopifnot(is.numeric(x))

  tx <- exp(x)
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(exp(x))
  fn_transform <- rlang::expr((x - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "exp")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "exp")
    )
  }
  val
}

#' @keywords internal

transform_log <- function(x) {
  stopifnot(is.numeric(x))

  tx <- log(x)
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr((log(x) - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "log")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "log")
    )
  }
  val
}

#' @keywords internal

transform_log10 <- function(x) {
  stopifnot(is.numeric(x))

  tx <- log10(x)
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr((log10(x) - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "log10")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "log10")
    )
  }
  val
}

#' @keywords internal

transform_michmen <- function(x) {
  stopifnot(is.numeric(x))

  tx <- x / (1 + x)
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(x / (1 + x))
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "michmen")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "michmen")
    )
  }
  val
}

#' @keywords internal

transform_sqrt <- function(x) {
  stopifnot(is.numeric(x))

  tx <- sign(x) * sqrt(abs(x))
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(sign(x) * sqrt(abs(x)))
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "sqrt")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "sqrt")
    )
  }
  val
}

#' @keywords internal

transform_asinh <- function(x) {
  stopifnot(is.numeric(x))

  tx <- asinh(x)
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(asinh(x))
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "asinh")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "asinh")
    )
  }
  val
}

#' @keywords internal

transform_logit <- function(x) {
  stopifnot(is.numeric(x))

  tx <- log(x / (1 + x), 10)
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(log(x / (1 + x), 10))
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "logit")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "logit")
    )
  }
  val
}

#' @keywords internal

transform_invlogit <- function(x) {
  stopifnot(is.numeric(x))

  tx <- (1 / (1 + exp(x)))
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(1 / (1 + exp(x)))
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "invlogit")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "invlogit")
    )
  }
  val
}

#' @keywords internal

transform_linear <- function(x) {
  stopifnot(is.numeric(x))

  fn_transform <-
    simplificar_expr(rlang::expr((x - !!min(x)) / !!diff(range(x))))

  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)
  tx <- fn_transform(x)

  attributes(fn_transform) <-
    list(clase = "transform", transform = "linear")
  val <- structure(
    list(
      tx = tx,
      x = x,
      fn_transform = fn_transform,
      statistic = normaltest(tx)
    ),
    class = c("transform", "linear")
  )
  val
}

#' @keywords internal

transform_binarize_mean <- function(x) {
  stopifnot(is.numeric(x))

  fn_transform <-
    simplificar_expr(rlang::expr(ifelse(x > !!mean(x), 1, 0)))

  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  attributes(fn_transform) <-
    list(clase = "transform", transform = "binarize_mean")
  val <- structure(
    list(
      tx = tx,
      x = x,
      fn_transform = fn_transform,
      statistic = normaltest(tx)
    ),
    class = c("transform", "binarize_mean")
  )
  val
}

#' @keywords internal

transform_binarize_dens <- function(x) {
  stopifnot(is.numeric(x))

  dens <- density(x[!is.na(x)])

  dens <- dens$x[which.max(dens$y)]

  fn_transform <-
    simplificar_expr(rlang::expr(ifelse(x > !!dens, 1, 0)))

  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  attributes(fn_transform) <-
    list(clase = "transform", transform = "binarize_dens")
  val <- structure(
    list(
      tx = tx,
      x = x,
      fn_transform = fn_transform,
      statistic = normaltest(tx)
    ),
    class = c("transform", "binarize_dens")
  )
  val
}

#' @keywords internal

transform_tanh <- function(x) {
  stopifnot(is.numeric(x))

  tx <- tanh(x)
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(tanh(x))
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "tanh")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "tanh")
    )
  }
  val
}

#' @keywords internal

transform_invasn <- function(x) {
  stopifnot(is.numeric(x))

  tx <- sin(x / 2)^2
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(sin(x / 2)^2)
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <- simplificar_expr(fn_transform)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "invasn")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "invasn")
    )
  }
  val
}

#' @keywords internal

transform_reciprocal <- function(x) {
  stopifnot(is.numeric(x))

  tx <- 1 / x
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(1 / (x))
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "reciprocal")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "reciprocal")
    )
  }
  val
}

#' @keywords internal

transform_asn <- function(x) {
  stopifnot(is.numeric(x))

  tx <- (2 * sign(x) * asin(sqrt(abs(x))))

  mu <- mean(tx, na.rm = TRUE)

  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(2 * sign(x) * asin(sqrt(abs(x))))
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "asn")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "asn")
    )
  }
  val
}

#' @keywords internal

transform_atanh <- function(x) {
  stopifnot(is.numeric(x))

  tx <- atanh(x)
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- rlang::expr(atanh(x))
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "atanh")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "atanh")
    )
  }
  val
}

#' @keywords internal

transform_yeojohnson <- function(x) {
  stopifnot(is.numeric(x))

  yeojohnson <- function(x, eps = .001) {
    estimate_yj <- function(x, eps = .001) {
      na_rm <- TRUE
      limits <- c(-10, 10)
      num_unique <- 5
      na_rows <- which(is.na(x))
      if (length(na_rows) > 0) {
        if (na_rm) {
          x <- x[-na_rows]
        } else {
          rlang::abort("Missing values in data. See `na_rm` option")
        }
      }
      yj_obj <- function(lambda, x, eps) {
        x <- x[complete.cases(x)]
        ll_yj <- function(lambda, y, eps) {
          eps <- .001
          y <- y[!is.na(y)]
          n <- length(y)
          nonneg <- all(y > 0)
          y_t <- yj_transform(y, lambda)
          mu_t <- mean(y_t)
          var_t <- var(y_t) * (n - 1) / n
          const <- sum(sign(y) * log(abs(y) + 1))
          res <- -.5 * n * log(var_t) + (lambda - 1) * const
          res
        }
        ll_yj(
          lambda = lambda,
          y = x,
          eps = eps
        )
      }

      if (length(unique(x)) < num_unique) {
        return(NA)
      }
      res <- optimize(
        yj_obj,
        interval = limits,
        maximum = TRUE,
        x = x,
        eps = eps,
        tol = .0001
      )
      lam <- res$maximum
      if (abs(limits[1] - lam) <= eps |
        abs(limits[2] - lam) <= eps) {
        lam <- NA
      }
      lam
    }

    lambda <- estimate_yj(x)

    if (is.na(lambda)) {
      return(x)
    }
    if (!inherits(x, "tbl_df") || is.data.frame(x)) {
      x <- unlist(x, use.names = FALSE)
    } else {
      if (!is.vector(x)) {
        x <- as.vector(x)
      }
    }

    not_neg <- which(x >= 0)
    is_neg <- which(x < 0)

    nn_trans <- function(lambda) {
      if (abs(lambda) < eps) {
        nn <- rlang::expr(log(x + 1))
      } else {
        nn <- rlang::expr(((x + 1)^!!lambda - 1) / !!lambda)
      }
      nn
    }

    ng_trans <- function(lambda) {
      if (abs(lambda - 2) < eps) {
        ng <- rlang::expr(-log(-x + 1))
      } else {
        ng <- rlang::expr(-((-x + 1)^(2 - !!lambda) - 1) / (2 - !!lambda))
      }
      ng
    }

    nn <- nn_trans(lambda)
    ng <- ng_trans(lambda)

    new_function(
      args = alist(x = ),
      body = expr({
        ifelse(x > 0, !!nn, !!ng)
      })
    )
  }

  yj <- yeojohnson(x)
  tx <- yj(x)
  mu <- mean(tx, na.rm = TRUE)
  sigma <- sd(tx, na.rm = TRUE)

  fn_transform <- body(yj)
  fn_transform <- rlang::expr((!!fn_transform - !!mu) / !!sigma)
  fn_transform <-
    rlang::new_function(args = alist(x = ), body = fn_transform)

  tx <- fn_transform(x)

  if (any(is_numinvalid(tx))) {
    val <- NULL
  } else {
    attributes(fn_transform) <-
      list(clase = "transform", transform = "yeojohnson")
    val <- structure(
      list(
        tx = tx,
        x = x,
        fn_transform = fn_transform,
        statistic = normaltest(tx)
      ),
      class = c("transform", "yeojohnson")
    )
  }
  val
}


# micelaneas ------------------------------------------------------------------------

#' @keywords internal
mcd <- function(...) {
  numeros <- c(...)

  mcd2 <- function(x, y) {
    mayor <- max(x, y)
    menor <- min(x, y)
    r <- mayor %% menor
    if (!(x & y)) {
      menor <- mayor
    } else {
      while (r != 0) {
        mayor <- r
        r <- menor %% r
        menor <- mayor
      }
    }
    abs(menor)
  }

  resultado <- mcd2(numeros[1], numeros[2])

  if (length(numeros) > 2) {
    for (n in numeros[3:length(numeros)]) {
      resultado <- mcd2(resultado, n)
    }
  }

  abs(resultado)
}
#' @keywords internal
mcm <- function(...) {
  numeros <- c(...)

  if (prod(numeros) != 0) {
    resultado <- prod(numeros) / mcd(numeros)^(length(numeros) - 1)
  } else {
    resultado <- NaN
  }
  abs(resultado)
}
#' @keywords internal
seq_power <- function(from, to, length.out, degree) {
  round(seq.default(from^(1 / degree), to^(1 / degree), length.out = length.out)^
    degree)
}
#' @keywords internal
seq_log <- function(from, to, length.out, base) {
  round(base^seq.default(log(from, base), log(to, base), length.out = length.out))
}
#' @keywords internal
names0 <- function(x, prefix = "id") {
  if (x == 0L) {
    return(character())
  }
  ind <- format(1:x)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}
#' @keywords internal
vec2table <- function(truth, estimate) {
  dnn <- c("Prediction", "Truth")
  if (!is.factor(truth)) {
    truth <- factor(truth)
  }

  if (!is.factor(estimate)) {
    if (!isTRUE(all.equal(sort(unique(estimate)), sort(levels(truth))))) {
      stop("There are different possible values in `truth` and `estimate`")
    }
    estimate <- factor(estimate, levels = levels(truth))
  }

  est_lev <- levels(estimate)
  tru_lev <- levels(truth)

  if (length(est_lev) != length(tru_lev)) {
    stop(
      "The columns containing the predicted and true ",
      "classes must have the same levels.",
      call. = FALSE
    )
  }

  if (!any(est_lev %in% tru_lev)) {
    stop("`estimate` must contain levels that overlap `truth`.",
      call. = FALSE
    )
  }

  if (any(tru_lev != est_lev)) {
    stop(
      "Levels are not in the same (or in the same order) for ",
      "true and predicted classes. ",
      call. = FALSE
    )
  }

  if (length(est_lev) < 2) {
    stop(
      "There must be at least 2 factors levels in the ",
      "column containing the predicted class",
      call. = FALSE
    )
  }

  xtab <- table(estimate, truth, dnn = dnn)

  xtab
}
#' @keywords internal
fixlevls <- function(.data, truth, estimate) {
  enquo_truth <- rlang::enquo(truth)
  enquo_estimate <- rlang::enquo(estimate)

  truth <- dplyr::pull(.data, !!enquo_truth)
  estimate <- dplyr::pull(.data, !!enquo_estimate)

  if (!is.factor(truth)) {
    if (is.factor(estimate)) {
      lvls <- levels(estimate)
    } else {
      lvls <- table(truth)
      lvls <- names(lvls)
    }
    lvls <- lvls[order(lvls)]
    truth <- factor(x = truth, levels = lvls)
  }

  if (!is.factor(estimate)) {
    lvls <- levels(truth)
    estimate <- factor(x = estimate, levels = lvls)
  } else {
    if (!identical(levels(truth), levels(estimate))) {
      estimate <- forcats::fct_relevel(estimate, levels(truth))
    }
  }

  .data[[rlang::as_label(enquo_truth)]] <- truth
  .data[[rlang::as_label(enquo_estimate)]] <- estimate

  .data
}
#' @keywords internal
calc_dots <- function(sf_data, col_names, n_per_dot) {
  if (is.null(col_names)) {
    col_names <- names(sf_data)
  }

  num_dots <- as.data.frame(sf_data)
  num_dots <- num_dots[which(names(sf_data) %in% col_names)]

  num_dots <- num_dots / n_per_dot

  num_dots <-
    do.call("cbind", lapply(names(num_dots), function(x) {
      data <- random_round(unlist(num_dots[x]))
      df <- data.frame(data)
      names(df) <- x
      return(df)
    }))

  data <- lapply(names(num_dots), function(x) {
    dots_df <-
      sf::st_sample(sf_data, size = unlist(num_dots[x]), type = "random")
    dots_df <- sf::st_coordinates(sf::st_cast(dots_df, "POINT"))
    dots_df <- as.data.frame(dots_df)
    names(dots_df) <- c("lon", "lat")
    dots_df$variable <- x
    return(dots_df)
  })

  sf_dots <- do.call("rbind", data)
  sf_dots <- sf_dots[sample(1:nrow(sf_dots)), ]
  sf_dots
}
#' @keywords internal
mean_impute <- function(x) {
  xm <- x[!(is.na(x) | is.infinite(x) | is.nan(x) | is.null(x))]
  xm <- mean(xm, na.rm = T)

  dplyr::case_when(
    is.na(x) ~ xm,
    is.infinite(x) ~ xm,
    is.nan(x) ~ xm,
    is.null(x) ~ xm,
    T ~ x
  )
}
#' @keywords internal
xrandom <- function(n = 100,
                    sd = 1,
                    mean = 0) {
  ndist <- sample(3:7, 1)

  prandom <-
    function(n) {
      over_ratio <- sample(1:20, 1)
      x <- runif(n = n, min = 1, max = over_ratio)

      x / sum(x)
    }

  p <- rep(0, 8)

  p[1:ndist] <- prandom(ndist)

  p <- ceiling(sample(p) * n)

  n_rnbinom <- sample(2:5, 1)

  n_r01 <-
    prandom(n_rnbinom)

  n_r02 <-
    prandom(n_rnbinom)

  out <-
    list(
      runif = rlang::expr(runif(
        n = !!p[1], min = 0, max = 1
      )),
      rnorm = rlang::expr(rnorm(
        n = !!p[2], mean = 1, sd = 1
      )),
      rbinom = rlang::expr(rbinom(
        n = !!p[3],
        size = !!n_rnbinom,
        prob = !!n_r01
      )),
      rgamma = rlang::expr(rgamma(
        n = !!p[4],
        shape = !!sample(2:3, 1),
        rate = !!sample(1:3, 1)
      )),
      rnbinom = rlang::expr(rnbinom(
        n = !!p[5],
        size = !!n_rnbinom,
        prob = !!n_r02
      )),
      rlnorm = rlang::expr(rlnorm(n = !!p[6])),
      rchisq = rlang::expr(rchisq(
        n = !!p[7],
        df = !!sample(1:2, 1),
        ncp = !!sample(2 * 1:3, 1)
      )),
      rcauchy = rlang::expr(rcauchy(
        n = !!p[8],
        location = 0,
        scale = 1
      ))
    )

  out
  out <- suppressWarnings(lapply(out[p > 0], eval))

  if (length(out$rbinom) > 0) {
    out$rbinom <-
      out$rbinom * runif(length(out$rbinom), .8, 1.2)
  }


  if (length(out$rnbinom) > 0) {
    out$rnbinom <-
      out$rnbinom * runif(length(out$rnbinom), .8, 1.2)
  }

  out <- unname(unlist(lapply(out, function(x) {
    (x - mean(x)) / sd(x)
  })))


  out * sd + mean
}

#' @keywords internal
xrandom_cat <- function(n = 100,
                        lev = 3,
                        tab = F) {
  x <- sample(
    x = letters[1:lev],
    size = n,
    replace = T,
    prob = prandom(n = lev)
  )
  if (tab) {
    x <- table(x)
    x <- x[rev(order(x))]
  }
  x
}
#' @keywords internal
prandom <- function(n, over_ratio = 5) {
  if (over_ratio > 100 | over_ratio < 1) {
    over_ratio <- sample(1:100, 1)
  }

  x <- runif(n = n, min = 1, max = over_ratio)

  x / sum(x)
}
#' @keywords internal
ndim2point <- function(x, y) {
  if (!is_df(x)) {
    rlang::abort("x no es data.frame o matrz")
  }

  if (!is_df(y)) {
    rlang::abort("y no es data.frame o matrz")
  }

  if (all(class(x) != class(y))) {
    rlang::abort("las clases de x e y deben ser iguales")
  }
  commun_cols <- intersect(names(x), names(y))

  x <- x[, commun_cols]
  y <- y[, commun_cols]

  as.numeric(RANN::nn2(data = y, query = x)$nn.dists)
}
#' @keywords internal
rollfn <- function(x, n, fn, ...) {
  out <- rep(NA, length(x))

  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- fn(x[(i - offset):(i + offset - 1)], ...)
  }
  out
}
#' @keywords internal
crossboot_fn <- function(data = NULL,
                         x,
                         y,
                         fn,
                         times = 20,
                         na.rm = T) {
  if (rlang::is_empty(data)) {
    xvar <- x
    yvar <- y
  } else {
    y <- rlang::enquo(y)
    x <- rlang::enquo(x)

    if (!all(c(rlang::as_label(x), rlang::as_label(y)) %in% names(data))) {
      rlang::abort(message = "no existen los argumentos en la data")
    }

    xvar <- dplyr::pull(.data = data, var = !!x)
    yvar <- dplyr::pull(.data = data, var = !!y)
  }

  if (!is.numeric(xvar)) {
    warning("argumento x no es numerico:return NA")
    return(NA_real_)
  }

  if (!is.numeric(yvar)) {
    warning("argumento y no es numerico:return NA")
    return(NA_real_)
  }


  if (length(xvar) < times | length(yvar) < times) {
    stop("'times' debe ser mayor que el largo de x")
  }
  xindices <- c()
  yindices <- c()

  n <- length(yvar)

  indices <-
    c(
      kfolds_vec(seq_along(xvar), times, rep = 3),
      invkfolds_vec(seq_along(xvar), rep = times, times = 3)
    )

  xindices <-
    c(xindices, purrr::map(indices, ~ xvar[-.x]))

  yindices <-
    c(yindices, purrr::map(indices, ~ yvar[-.x]))

  mean(purrr::map2_dbl(xindices, yindices, .f = ~ fn(.x, .y)))
}
#' @keywords internal
bootfn <-
  function(x,
           fn,
           times = 20,
           rep = 1,
           boots_result = FALSE,
           seed = NULL,
           ...) {
    if (!is.numeric(x)) {
      warning("argumento no es numerico:return NA")
      return(NA_real_)
    }

    if (length(x) < times) {
      stop("'times' debe ser mayor que el largo de x")
    }

    if (rlang::is_empty(seed)) {
      set.seed(sample.int(10^5, 1))
    } else {
      set.seed(seed)
    }


    xfolds <-
      kfolds_vec(x, times, rep = 3)

    xfolds <-
      purrr::map_dbl(xfolds, function(w) {
        fn(w, ...)
      })

    xfolds_mean <- mean(xfolds)

    if (boots_result) {
      cint <-
        qt(.975, length(xfolds) - 1) * sd(xfolds) / sqrt(length(xfolds))

      xfolds_cint <- xfolds_mean + c(-cint, cint)


      cat("el resultado es de: \n   ", xfolds_mean, "\n")
      cat(
        "intervalo de confianza al 95 porciento: \n   ",
        xfolds_cint,
        " \n \n \n"
      )
    }

    xfolds_mean
  }
#' @keywords internal
pctfrc_table <-
  function(.data,
           x,
           y = NULL,
           method = c("sum", "min", "max")) {
    method <- match.arg(method)

    if (is_empty(rlang::enquo(y))) {
      x <- rlang::enquo(x)
      ftable <- table(dplyr::select(.data, !!x))
    } else {
      v <- rlang::enquos(x, y)
      ftable <- table(dplyr::select(.data, !!!v))
    }
    fn <- switch(method,
      sum = sum,
      min = min,
      max = max
    )

    ftable / fn(ftable)
  }
#' @keywords internal
significant_places <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }
  z <- diff(r <- range(x[!mgalda::is_numinvalid(x)]))
  if (z == 0) {
    z <- abs(r[1L])
  }
  if (z == 0) {
    z <- 1
  }

  3 - floor(log10(z))
}
#' @keywords internal
round_significant <- function(x) {
  round(x, significant_places(x))
}
#' @keywords internal
fct2num <- function(x) {
  as.numeric(as.character(x))
}
#' @keywords internal
n_unique <- function(x) {
  length(unique(x))
}
#' @keywords internal
random_range <- function(lower = 10, upper = 200) {
  if (lower > upper) {
    tmp <- lower
    upper <- lower
    lower <- tmp
  }

  xcut <- diff(c(lower, upper)) * runif(1, .1, .9)

  c(runif(1, lower, lower + xcut), runif(1, lower + xcut, upper))
}
#' @keywords internal
ncase <- function(x) {
  if (length(x) == 1) {
    ceiling(2 * (x^(2 / 5)))
  } else if (length(x) > 1) {
    ceiling(2 * (length(x)^(2 / 5)))
  }
}
#' @keywords internal
nsplits <- function(x) {
  ceiling(sum(complete.cases(x))^(1 / 3) - log10(sum(complete.cases(x)))) + 2
}
#' @keywords internal
break_nscott <-
  function(x) {
    h <- 3.5 * sqrt(stats::var(x)) * length(x)^(-1 / 3)
    if (h > 0) {
      max(1, ceiling(diff(range(x)) / h))
    } else {
      1L
    }
  }
#' @keywords internal
break_sturges <-
  function(x) {
    ceiling(log2(length(x)) + 1)
  }
#' @keywords internal
break_fd <-
  function(x) {
    h <- 2 * stats::IQR(x. <- signif(x, digits = 5))
    if (h == 0) {
      x. <- sort(x.)
      al <- 1 / 4
      al.min <- 1 / 512
      while (h == 0 &&
        (al <- al / 2) >= al.min) {
        h <- diff(stats::quantile(x.,
          c(al, 1 - al),
          names = FALSE
        )) /
          (1 - 2 * al)
      }
    }
    if (h == 0) {
      h <- 3.5 * sqrt(stats::var(x))
    }
    if (h > 0) {
      ceiling(diff(range(x)) / h * length(x)^(1 / 3))
    } else {
      1L
    }
  }
#' @keywords internal
tibble_transpose <- function(x) {
  dplyr::as_tibble(cbind(nms = names(x), t(x)))
}
#' @keywords internal
simulate_num <- function(n, xsd, xmean) {
  p <- runif(3, 1, sample(1:20, 1))
  p <- p / sum(p) * n

  truth <-
    list(
      runif = runif(n = p[1], min = 0, max = 1),
      rnorm = rnorm(n = p[2], mean = 1, sd = 1),
      rchisq = rchisq(p[3], sample(0:2, 1), sample(2 * 0:3, 1))
    )

  truth <- unname(unlist(lapply(truth, function(x) {
    x <- (x - mean(x)) / sd(x)
    x * xsd + xmean + xsd * order(x) / length(x)
  })))

  s <- sample(1:3, length(truth), replace = T)
  estimate <- truth
  estimate[s == 1] <-
    estimate[s == 1] + rnorm(sum(s == 1), xsd * runif(1, 0, 1), xsd * runif(1, 0, 1))

  estimate[s == 2] <-
    estimate[s == 2] * runif(sum(s == 2), min = .8, max = 1.2)

  estimate[s == 3] <-
    jitter(estimate[s == 3], amount = xsd * runif(1, .6, 1.4))

  s <- sample(1:0,
    length(estimate),
    replace = T,
    prob = c(.05, .95)
  )

  estimate[s == 1] <-
    estimate[s == 1] + sample(c(-1, 1), sum(s == 1), replace = T) * runif(sum(s ==
      1), 1, 3) * xsd
  env_test <- new.env()
  env_test$truth <- truth
  env_test$estimate <- estimate
  as.list(env_test)
}
#' @keywords internal
simulate_probclass <- function(n, n_lvs, random) {
  truth <- xrandom_cat(n, n_lvs)
  lvs <- unique(truth)
  lvs <- lvs[order(lvs)]
  truth <- factor(truth, lvs)
  pred <- truth
  s <- sample(seq_along(pred), random)
  pred[s] <- sample(unique(pred), random, replace = T)
  estimate <-
    purrr::map_dfr(
      pred,
      .f = function(x) {
        mat <- matrix(rep(0, length(lvs)), nrow = 1)
        nmax <- which(lvs == x)
        mat[, nmax] <- runif(1, 50, 100)
        mat[, -nmax] <- runif(length(lvs) - 1, 10, 40)
        colnames(mat) <- lvs
        as.data.frame(mat / sum(mat[1, ]))
      }
    )
  x <- table(truth, pred)

  env_test <- new.env()
  env_test$truth <- truth
  env_test$pred <- pred
  env_test$estimate <- estimate
  env_test$x <- x
  as.list(env_test)
}
#' @keywords internal
xrandomdata <-
  function(nrows = 400,
           numcosl = 3,
           factcols = 3) {
    l <- mgalda::list_sample
    if (!is.null(numcosl) & numcosl > 0) {
      num <- l$numerical[sample(length(l$numerical), numcosl)]
      num <-
        purrr::map_dfc(
          .x = num,
          .f = ~ sample(.x, nrows, replace = ifelse(length(.x) < nrows, T, F))
        )
    }
    if (!is.null(factcols) & factcols > 0) {
      fct <- l$factor[sample(length(l$factor), factcols)]
      fct <-
        purrr::map_dfc(
          .x = fct,
          .f = ~ sample(.x, nrows, replace = ifelse(length(.x) < nrows, T, F))
        )
    }

    if (is.null(numcosl) & numcosl == 0) {
      randdata <- fct
    }
    if (is.null(factcols) & factcols == 0) {
      randdata <- num
    } else {
      randdata <- dplyr::bind_cols(num, fct)
    }

    randdata
  }


# swissknife ------------------------------------------------------------------------

#' @export
#' @keywords internal

sts <- list(
  trf = list(
    transform_asinh = transform_asinh,
    transform_asn = transform_asn,
    transform_atanh = transform_atanh,
    transform_binarize_dens = transform_binarize_dens,
    transform_binarize_mean = transform_binarize_mean,
    transform_exp = transform_exp,
    transform_invasn = transform_invasn,
    transform_invlogit = transform_invlogit,
    transform_linear = transform_linear,
    transform_log = transform_log,
    transform_log10 = transform_log10,
    transform_logit = transform_logit,
    transform_michmen = transform_michmen,
    transform_powerthree = transform_powerthree,
    transform_powertwo = transform_powertwo,
    transform_reciprocal = transform_reciprocal,
    transform_sqrt = transform_sqrt,
    transform_tanh = transform_tanh,
    transform_yeojohnson = transform_yeojohnson
  ),
  sts = list(
    center_scale = center_scale,
    cov_alt = cov_alt,
    cramervon_test = cramervon_test,
    entropy = entropy,
    geometricmean = geometricmean,
    gini = gini,
    glmn_coefs = glmn_coefs,
    iqrize = iqrize,
    kurtosis = kurtosis,
    normaltest = normaltest,
    pearson_test = pearson_test,
    pseudo_rsq = pseudo_rsq,
    qsr = qsr,
    quartile_skewness = quartile_skewness,
    rank_percent = rank_percent,
    rescalar = rescalar,
    rescalar_outliers = rescalar_outliers,
    shapiro_test = shapiro_test,
    skewness = skewness,
    boots_var = boots_var
  ),
  cls = list(
    calinhara = calinhara,
    connectivity = connectivity,
    dunn = dunn,
    weightedss = weightedss,
    wss_components = wss_components
  )
)

#' @export
#' @keywords internal

mets <- list(
  num = list(
    ccc = function(truth, estimate) {
      m_e <- mean(estimate)
      m_t <- mean(truth)
      v_e <- var(estimate)
      v_t <- var(truth)
      cross <-
        scale(truth, scale = FALSE) * scale(estimate, scale = FALSE)
      cross <- mean(cross)
      cross <- 2 * cross / (v_e + v_t + (m_e - m_t)^2)
      tibble::tibble(score = cross, direction = "max")
    },
    huber_loss = function(truth, estimate) {
      delta <- 1
      if (!rlang::is_bare_numeric(delta, n = 1L)) {
        abort("`delta` must be a single numeric value.")
      }
      if (!(delta >= 0)) {
        abort("`delta` must be a positive value.")
      }
      a <- truth - estimate
      abs_a <- abs(a)
      loss <- ifelse(abs_a <= delta, 0.5 * a^2, delta * (abs_a -
        0.5 * delta))
      tibble::tibble(score = mean(loss), direction = "min")
    },
    huber_loss_pseudo = function(truth, estimate) {
      delta <- 1
      if (!rlang::is_bare_numeric(delta, n = 1L)) {
        abort("`delta` must be a single numeric value.")
      }
      if (!(delta >= 0)) {
        abort("`delta` must be a positive value.")
      }
      a <- truth - estimate
      score <- mean(delta^2 * (sqrt(1 + (a / delta)^2) - 1))
      tibble::tibble(score = score, direction = "min")
    },
    iic = function(truth, estimate) {
      deltas <- truth - estimate
      delta_neg <- deltas[deltas < 0]
      delta_pos <- deltas[deltas >= 0]
      mae_neg <- mean(abs(delta_neg))
      mae_pos <- mean(abs(delta_pos))
      adjustment <-
        min(c(mae_neg, mae_pos)) / max(c(mae_neg, mae_pos))
      tibble::tibble(
        score = cor(truth, estimate) * adjustment,
        direction = "max"
      )
    },
    maecf = function(truth, estimate) {
      tibble::tibble(score = mean(abs(truth - estimate)), direction = "min")
    },
    mape = function(truth, estimate) {
      tibble::tibble(score = mean(abs((truth - estimate) / truth)) *
        100, direction = "min")
    },
    mpe = function(truth, estimate) {
      tibble::tibble(
        score = mean((truth - estimate) / truth) * 100,
        direction = "zero"
      )
    },
    msd = function(truth, estimate) {
      tibble::tibble(score = mean(truth - estimate), direction = "zero")
    },
    rmse = function(truth, estimate) {
      tibble::tibble(
        score = sqrt(mean((truth - estimate)^2)),
        direction = "min"
      )
    },
    rpd = function(truth, estimate) {
      tibble::tibble(
        score = sd(truth) / sqrt(mean((truth - estimate)^2)),
        direction = "max"
      )
    },
    rpiq = function(truth, estimate) {
      tibble::tibble(
        score = IQR(truth) / sqrt(mean((truth - estimate)^2)),
        direction = "max"
      )
    },
    rsq = function(truth, estimate) {
      tibble::tibble(score = cor(truth, estimate)^2, direction = "max")
    },
    rsq_trad = function(truth, estimate) {
      n <- length(truth)
      ss <- sum((estimate - truth)^2)
      tibble::tibble(score = 1 - (ss / ((n - 1) * var(truth))), direction = "max")
    },
    smape = function(truth, estimate) {
      percent_scale <- 100
      numer <- abs(estimate - truth)
      denom <- (abs(truth) + abs(estimate)) / 2
      tibble::tibble(
        score = mean(numer / denom) * percent_scale,
        direction = "min"
      )
    }
  ),
  classprob = list(
    accuracy = function(x) {
      tibble::tibble(score = sum(diag(x)) / sum(x), direction = "max")
    },
    j_index = function(x) {
      if (max(dim(x)) == 2) {
        score <-
          {
            relevant <- colnames(x)[[1]]
            numer <- sum(x[relevant, relevant])
            denom <- sum(x[, relevant])
            undefined <- denom <= 0
            if (undefined) {
              not_relevant <- setdiff(colnames(x), relevant)
              count <- x[relevant, not_relevant]
              return(NA_real_)
            }
            numer / denom
          } +
          {
            negative <- colnames(x)[[2]]
            numer <- sum(x[negative, negative])
            denom <- sum(x[, negative])
            undefined <- denom <= 0
            if (undefined) {
              positive <- setdiff(colnames(x), negative)
              count <- x[negative, positive]
              return(NA_real_)
            }
            numer / denom
          } - 1
      } else {
        w <- {
          n <- ncol(x)
          rep(1 / n, times = n)
        }
        out_vec <-
          {
            numer <- diag(x)
            denom <- colSums(x)
            undefined <- denom <= 0
            if (any(undefined)) {
              counts <- rowSums(x) - numer
              counts <- counts[undefined]
              events <- colnames(x)[undefined]
              numer[undefined] <- NA_real_
              denom[undefined] <- NA_real_
            }
            numer / denom
          } +
          {
            n <- sum(x)
            tp <- diag(x)
            tpfp <- rowSums(x)
            tpfn <- colSums(x)
            tn <- n - (tpfp + tpfn - tp)
            fp <- tpfp - tp
            numer <- tn
            denom <- tn + fp
            undefined <- denom <= 0
            if (any(undefined)) {
              counts <- tpfn - tp
              counts <- counts[undefined]
              events <- colnames(x)[undefined]
              numer[undefined] <- NA_real_
              denom[undefined] <- NA_real_
            }
            numer / denom
          } - 1
        score <- weighted.mean(out_vec, w)
      }
      tibble::tibble(score = score, direction = "max")
    },
    kap = function(x) {
      full_sum <- sum(x)
      row_sum <- rowSums(x)
      col_sum <- colSums(x)
      expected <- outer(row_sum, col_sum) / full_sum
      n_levels <- nrow(x)
      make_weighting_matrix <- function(weighting, n_levels) {
        if (weighting == "none") {
          w <- matrix(1L, nrow = n_levels, ncol = n_levels)
          diag(w) <- 0L
          return(w)
        }
        if (weighting == "linear") {
          power <- 1L
        } else if (weighting == "quadratic") {
          power <- 2L
        }
        w <- rlang::seq2(0L, n_levels - 1L)
        w <- matrix(w, nrow = n_levels, ncol = n_levels)
        w <- abs(w - t(w))^power
        w
      }
      w <- make_weighting_matrix("none", n_levels)
      n_disagree <- sum(w * x)
      n_chance <- sum(w * expected)
      tibble::tibble(
        score = 1 - n_disagree / n_chance,
        direction = "max"
      )
    },
    mn_log_loss = function(truth, estimate) {
      if (!is.matrix(estimate)) {
        estimate_matrix <- as.matrix(estimate)
      }
      if (ncol(estimate_matrix) == 2) {
        score <- {
          estimate_matrix <-
            estimate_matrix[, str_detect(
              colnames(estimate_matrix),
              levels(truth)[[1]]
            )]
          estimate_matrix <- matrix(c(estimate_matrix, 1 -
            estimate_matrix), ncol = 2)
          score <- {
            y <- model.matrix(~ truth - 1)
            eps <- .Machine$double.eps
            estimate_matrix <- pmax(pmin(
              estimate_matrix,
              1 - eps
            ), eps)
            -sum(y * log(estimate_matrix)) / length(truth)
          }
        }
      } else {
        score <- {
          y <- model.matrix(~ truth - 1)
          eps <- .Machine$double.eps
          estimate_matrix <- pmax(pmin(estimate_matrix, 1 -
            eps), eps)
          -sum(y * log(estimate_matrix)) / length(truth)
        }
      }
      tibble::tibble(score = score, direction = "min")
    },
    npv = function(x) {
      if (max(dim(x)) == 2) {
        score <- {
          positive <- colnames(x)[[1]]
          negative <- colnames(x)[[2]]
          prevalence <- sum(x[, positive]) / sum(x)
          sens <- {
            relevant <- colnames(x)[[1]]
            numer <- sum(x[relevant, relevant])
            denom <- sum(x[, relevant])
            undefined <- denom <= 0
            if (undefined) {
              not_relevant <- setdiff(colnames(x), relevant)
              count <- x[relevant, not_relevant]
              return(NA_real_)
            }
            numer / denom
          }
          spec <- {
            negative <- colnames(x)[[2]]
            numer <- sum(x[negative, negative])
            denom <- sum(x[, negative])
            undefined <- denom <= 0
            if (undefined) {
              positive <- setdiff(colnames(x), negative)
              count <- x[negative, positive]
              return(NA_real_)
            }
            numer / denom
          }
          (spec * (1 - prevalence)) / (((1 - sens) * prevalence) +
            ((spec) * (1 - prevalence)))
        }
      } else {
        w <- {
          n <- ncol(x)
          rep(1 / n, times = n)
        }
        out_vec <- {
          tpfn <- colSums(x)
          tptnfpfn <- rep(sum(x), times = nrow(x))
          prevalence <- tpfn / tptnfpfn
          .sens_vec <- {
            numer <- diag(x)
            denom <- colSums(x)
            undefined <- denom <= 0
            if (any(undefined)) {
              counts <- rowSums(x) - numer
              counts <- counts[undefined]
              events <- colnames(x)[undefined]
              numer[undefined] <- NA_real_
              denom[undefined] <- NA_real_
            }
            numer / denom
          }
          .spec_vec <- {
            n <- sum(x)
            tp <- diag(x)
            tpfp <- rowSums(x)
            tpfn <- colSums(x)
            tn <- n - (tpfp + tpfn - tp)
            fp <- tpfp - tp
            numer <- tn
            denom <- tn + fp
            undefined <- denom <= 0
            if (any(undefined)) {
              counts <- tpfn - tp
              counts <- counts[undefined]
              events <- colnames(x)[undefined]
              numer[undefined] <- NA_real_
              denom[undefined] <- NA_real_
            }
            numer / denom
          }
          numer <- .spec_vec * (1 - prevalence)
          denom <- (1 - .sens_vec) * prevalence + .spec_vec *
            (1 - prevalence)
          denom[denom <= 0] <- NA_real_
          numer / denom
        }
        score <- weighted.mean(out_vec, w)
      }
      tibble::tibble(score = score, direction = "max")
    },
    ppv = function(x) {
      if (max(dim(x)) == 2) {
        score <- {
          positive <- colnames(x)[[1]]
          prevalence <- sum(x[, positive]) / sum(x)
          sens <- {
            relevant <- colnames(x)[[1]]
            numer <- sum(x[relevant, relevant])
            denom <- sum(x[, relevant])
            undefined <- denom <= 0
            if (undefined) {
              not_relevant <- setdiff(colnames(x), relevant)
              count <- x[relevant, not_relevant]
              return(NA_real_)
            }
            numer / denom
          }
          spec <- {
            negative <- colnames(x)[[2]]
            numer <- sum(x[negative, negative])
            denom <- sum(x[, negative])
            undefined <- denom <= 0
            if (undefined) {
              positive <- setdiff(colnames(x), negative)
              count <- x[negative, positive]
              return(NA_real_)
            }
            numer / denom
          }
          (sens * prevalence) / ((sens * prevalence) + ((1 -
            spec) * (1 - prevalence)))
        }
      } else {
        w <- {
          n <- ncol(x)
          rep(1 / n, times = n)
        }
        out_vec <- {
          tpfn <- colSums(x)
          tptnfpfn <- rep(sum(x), times = nrow(x))
          prevalence <- tpfn / tptnfpfn
          .sens_vec <- {
            numer <- diag(x)
            denom <- colSums(x)
            undefined <- denom <= 0
            if (any(undefined)) {
              counts <- rowSums(x) - numer
              counts <- counts[undefined]
              events <- colnames(x)[undefined]
              numer[undefined] <- NA_real_
              denom[undefined] <- NA_real_
            }
            numer / denom
          }
          .spec_vec <- {
            n <- sum(x)
            tp <- diag(x)
            tpfp <- rowSums(x)
            tpfn <- colSums(x)
            tn <- n - (tpfp + tpfn - tp)
            fp <- tpfp - tp
            numer <- tn
            denom <- tn + fp
            undefined <- denom <= 0
            if (any(undefined)) {
              counts <- tpfn - tp
              counts <- counts[undefined]
              events <- colnames(x)[undefined]
              numer[undefined] <- NA_real_
              denom[undefined] <- NA_real_
            }
            numer / denom
          }
          numer <- .sens_vec * prevalence
          denom <- .sens_vec * prevalence + (1 - .spec_vec) *
            (1 - prevalence)
          denom[denom <= 0] <- NA_real_
          numer / denom
        }
        score <- weighted.mean(out_vec, w)
      }
      tibble::tibble(score = score, direction = "max")
    },
    precision = function(x) {
      if (max(dim(x)) == 2) {
        score <- {
          relevant <- colnames(x)[[1]]
          numer <- x[relevant, relevant]
          denom <- sum(x[relevant, ])
          undefined <- denom <= 0
          if (undefined) {
            not_relevant <- setdiff(colnames(x), relevant)
            count <- x[not_relevant, relevant]
            return(NA_real_)
          }
          numer / denom
        }
      } else {
        w <- {
          n <- ncol(x)
          rep(1 / n, times = n)
        }
        out_vec <- {
          numer <- diag(x)
          denom <- rowSums(x)
          undefined <- denom <= 0
          if (any(undefined)) {
            counts <- colSums(x) - numer
            counts <- counts[undefined]
            events <- colnames(x)[undefined]
            numer[undefined] <- NA_real_
            denom[undefined] <- NA_real_
          }
          numer / denom
        }
        score <- weighted.mean(out_vec, w, na.rm = TRUE)
      }
      tibble::tibble(score = score, direction = "max")
    },
    recall = function(x) {
      if (max(dim(x)) == 2) {
        score <- {
          relevant <- colnames(x)[[1]]
          numer <- sum(x[relevant, relevant])
          denom <- sum(x[, relevant])
          undefined <- denom <= 0
          if (undefined) {
            not_relevant <- setdiff(colnames(x), relevant)
            count <- x[relevant, not_relevant]
            return(NA_real_)
          }
          numer / denom
        }
      } else {
        w <- {
          n <- ncol(x)
          rep(1 / n, times = n)
        }
        out_vec <- {
          numer <- diag(x)
          denom <- colSums(x)
          undefined <- denom <= 0
          if (any(undefined)) {
            counts <- rowSums(x) - numer
            counts <- counts[undefined]
            events <- colnames(x)[undefined]
            numer[undefined] <- NA_real_
            denom[undefined] <- NA_real_
          }
          numer / denom
        }
        score <- weighted.mean(out_vec, w, na.rm = TRUE)
      }
      tibble::tibble(score = score, direction = "max")
    },
    roc_auc = function(truth, estimate) {
      roc_auc_binary <- function(truth, estimate) {
        if (!is.matrix(estimate)) {
          estimate <- as.matrix(estimate)
        }
        estimate <-
          estimate[, stringr::str_detect(
            colnames(estimate),
            levels(truth)[[1]]
          )]
        lvls <- rev(levels(truth))
        control <- lvls[[1]]
        event <- lvls[[2]]
        n_occur <- function(x, what) {
          sum(x == what)
        }
        if (n_occur(truth, control) == 0L) {
          return(NA_real_)
        }
        if (n_occur(truth, event) == 0L) {
          return(NA_real_)
        }
        args <- rlang::quos(
          response = truth,
          predictor = estimate,
          levels = lvls,
          quiet = TRUE,
          direction = "<"
        )
        pROC_auc <- pROC::auc(
          response = truth,
          predictor = estimate,
          levels = lvls,
          quiet = TRUE,
          direction = "<"
        )
        as.numeric(pROC_auc)
      }
      if (ncol(estimate) == 2) {
        score <- roc_auc_binary(truth, estimate)
      } else {
        roc_auc_multiclass_impl <- function(truth, estimate) {
          lvls <- levels(truth)
          colnames(estimate) <- lvls
          pred <- apply(estimate, 1, function(x) {
            lvls[which.max(x)]
          })
          pred <- factor(pred, lvls)
          combinations <- gtools::permutations(
            n = length(lvls),
            r = 2,
            v = lvls
          )
          ht <- c()
          for (i in 1:nrow(combinations)) {
            ind <- which(truth %in% combinations[i, ])
            tcom <- as.character(truth[ind])
            test <- estimate[ind, combinations[i, ]]
            test <- test / rowSums(test)
            tcom <- factor(tcom, combinations[i, ])
            ht <-
              c(ht, roc_auc_binary(truth = tcom, estimate = test))
          }
          ht <- mean(ht)
          w <- ({
            n <- ncol(matrix(table(truth), nrow = 1))
            rep(1 / n, times = n)
          })
          mc <- c()
          for (i in lvls) {
            tcom <- as.character(truth)
            tcom <- ifelse(tcom == i, tcom, "..other")
            tcom <- factor(tcom, levels = c(i, "..other"))
            test_t <- estimate[, i]
            test_n <- rowMeans(estimate[, setdiff(
              names(estimate),
              i
            )])
            test <- cbind(test_t, test_n)
            colnames(test) <- c(i, "..other")
            test <- test / rowSums(test)
            mc <-
              c(mc, roc_auc_binary(truth = tcom, estimate = test))
          }
          (ht + weighted.mean(mc, w)) / 2
        }

        roc_auc_multiclass <- function(truth, estimate) {
          tryCatch(
            expr = roc_auc_multiclass_impl(truth, estimate),
            error = function(e) {
              NA
            }
          )
        }

        score <- roc_auc_multiclass(truth, estimate)
      }
      tibble::tibble(score = score, direction = "max")
    },
    sens = function(x) {
      if (max(dim(x)) == 2) {
        score <- {
          relevant <- colnames(x)[[1]]
          numer <- sum(x[relevant, relevant])
          denom <- sum(x[, relevant])
          undefined <- denom <= 0
          if (undefined) {
            not_relevant <- setdiff(colnames(x), relevant)
            count <- x[relevant, not_relevant]
            return(NA_real_)
          }
          numer / denom
        }
      } else {
        w <- {
          n <- ncol(x)
          rep(1 / n, times = n)
        }
        out_vec <- {
          numer <- diag(x)
          denom <- colSums(x)
          undefined <- denom <= 0
          if (any(undefined)) {
            counts <- rowSums(x) - numer
            counts <- counts[undefined]
            events <- colnames(x)[undefined]
            numer[undefined] <- NA_real_
            denom[undefined] <- NA_real_
          }
          numer / denom
        }
        score <- weighted.mean(out_vec, w, na.rm = TRUE)
      }
      tibble::tibble(score = score, direction = "max")
    },
    spec = function(x) {
      if (max(dim(x)) == 2) {
        score <- {
          negative <- colnames(x)[[2]]
          numer <- sum(x[negative, negative])
          denom <- sum(x[, negative])
          undefined <- denom <= 0
          if (undefined) {
            positive <- setdiff(colnames(x), negative)
            count <- x[negative, positive]
            return(NA_real_)
          }
          numer / denom
        }
      } else {
        w <- {
          n <- ncol(x)
          rep(1 / n, times = n)
        }
        out_vec <- {
          n <- sum(x)
          tp <- diag(x)
          tpfp <- rowSums(x)
          tpfn <- colSums(x)
          tn <- n - (tpfp + tpfn - tp)
          fp <- tpfp - tp
          numer <- tn
          denom <- tn + fp
          undefined <- denom <= 0
          if (any(undefined)) {
            counts <- tpfn - tp
            counts <- counts[undefined]
            events <- colnames(x)[undefined]
            numer[undefined] <- NA_real_
            denom[undefined] <- NA_real_
          }
          numer / denom
        }
        score <- weighted.mean(out_vec, w, na.rm = TRUE)
      }
      tibble::tibble(score = score, direction = "max")
    },
    w_accuracy = function(x) {
      tibble::tibble(score = mean(diag(x) / colSums(x)), direction = "max")
    }
  )
)

#' @export
#' @keywords internal

mics <-
  list(
    misc = list(
      bootfn = bootfn,
      calc_dots = calc_dots,
      crossboot_fn = crossboot_fn,
      fct2num = fct2num,
      fixlevls = fixlevls,
      mcd = mcd,
      mcm = mcm,
      mean_impute = mean_impute,
      n_unique = n_unique,
      names0 = names0,
      ncase = ncase,
      break_nscott = break_nscott,
      break_sturges = break_sturges,
      break_fd = break_fd,
      ndim2point = ndim2point,
      nsplits = nsplits,
      pctfrc_table = pctfrc_table,
      prandom = prandom,
      random_range = random_range,
      rollfn = rollfn,
      round_significant = round_significant,
      seq_log = seq_log,
      seq_power = seq_power,
      significant_places = significant_places,
      simulate_num = simulate_num,
      simulate_probclass = simulate_probclass,
      tibble_transpose = tibble_transpose,
      vec2table = vec2table,
      xrandom = xrandom,
      xrandom_cat = xrandom_cat,
      xrandomdata = xrandomdata
    )
  )
