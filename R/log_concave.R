#' log-concave density
#'
#'
#' @name log_concave
#' @rdname log_concave
#' @export
log_concave <- function(x) {
  prec <- 1e-10
  xn <- sort(x)

  tmp <- logconcave_param(x)
  x <- tmp$x
  w <- tmp$w
  sig <- tmp$sig

  n <- length(x)
  phi <- logconcave_norm(x, 1:n * 0)
  is_knot <- 1:n * 0
  is_knot[c(1, n)] <- 1

  res1 <- logconcave_mle(x, w, is_knot, phi, prec)
  phi <- res1$phi
  L <- res1$L
  conv <- res1$conv
  H <- res1$H
  iter1 <- 1
  while ((iter1 < 500) & (max(H) > prec * mean(abs(H)))) {
    is_knot_old <- is_knot
    iter1 <- iter1 + 1
    tmp <- max(H)
    k <- (1:n) * (H == tmp)
    k <- min(k[k > 0])
    is_knot[k] <- 1
    res2 <- logconcave_mle(x, w, is_knot, phi, prec)
    phi_new <- res2$phi
    L <- res2$L
    conv_new <- res2$conv
    H <- res2$H
    while ((max(conv_new) > prec * max(abs(conv_new)))) {
      JJ <- (1:n) * (conv_new > 0)
      JJ <- JJ[JJ > 0]
      tmp <- conv[JJ] / (conv[JJ] - conv_new[JJ])
      lambda <- min(tmp)
      KK <- (1:length(JJ)) * (tmp == lambda)
      KK <- KK[KK > 0]
      is_knot[JJ[KK]] <- 0
      phi <- (1 - lambda) * phi + lambda * phi_new
      conv <- pmin(c(logconcave_convexity(x, phi), 0))
      res3 <- logconcave_mle(x, w, is_knot, phi, prec)
      phi_new <- res3$phi
      L <- res3$L
      conv_new <- res3$conv
      H <- res3$H
    }
    phi <- phi_new
    conv <- conv_new
    if (sum(is_knot != is_knot_old) == 0) {
      break
    }
  }
  #Fhat <- logconcave_f(x, phi)
  list(
    xn = xn,
    x = x, #### este
    w = w,
    phi = as_vct(phi), #### este
    is_knot = is_knot,
    L = L,
    Fhat = as_vct(Fhat), #### este
    H = as_vct(H),
    n = length(xn),
    m = n,
    knots = x[is_knot == 1],
    mode = x[phi == max(phi)],
    sig = sig
  )

}


qloglin <- function(u, t){
  if (abs(t) > 1e-6){
    z <- log(1 + ((exp(t) - 1) * u)) / t} else {
      z <- u + t * u * (1 - u) / 2}
  z
}

logconcave_param <- function(x) {
  n <- length(x)
  # Default setting:
  xx <- sort(x)
  tmp <- c(xx[1:(n - 1)] < xx[2:n], TRUE)
  ww <- c(0, (1:n)[tmp])
  ww <- (ww[2:length(ww)] - ww[1:(length(ww) - 1)]) / n
  xx <- xx[tmp]

  est_m <- sum(ww * xx)
  est_sd <- sum(ww * (xx - est_m)^2)
  est_sd <- sqrt(est_sd * n / (n - 1))
  list(
    x = xx,
    w = ww,
    sig = est_sd,
    n = n
  )
}

logconcave_norm <- function(x, phi) {
  n <- length(x)
  dx <- diff(x)
  phi_new <- phi - log(sum(dx * j_00(phi[1:(n - 1)], phi[2:n])))
  matrix(phi_new, ncol = 1)
}

logconcave_mle <- function(x, w, is_knot, phi_o, prec) {
  MLE <- function(x,
                  w = NA,
                  phi_o = NA) {
    prec <- 1e-7
    n <- length(x)
    if (sum(x[2:n] <= x[1:n - 1]) > 0) {
      cat("We need strictly increasing numbers x(i)!\n")
    }
    ww <- w / sum(w)

    phi <- logconcave_norm(x, phi_o)

    iter0 <- 0
    res <- logconcave_ll_all(x, ww, phi)
    L <- res$ll
    phi_new <- res$phi_new
    dirderiv <- res$dirderiv
    while ((dirderiv >= prec) & (iter0 < 100)) {
      iter0 <- iter0 + 1
      L_new <- logconcave_ll(x, ww, phi_new)
      iter1 <- 0
      while ((L_new < L) & (iter1 < 20)) {
        iter1 <- iter1 + 1
        phi_new <- 0.5 * (phi + phi_new)
        L_new <- logconcave_ll(x, ww, phi_new)
        dirderiv <- 0.5 * dirderiv
      }
      if (L_new >= L) {
        tstar <- max((L_new - L) / dirderiv)
        if (tstar >= 0.5) {
          phi <- logconcave_norm(x, phi_new)
        } else {
          tstar <- max(0.5 / (1 - tstar))
          phi <-
            logconcave_norm(x, (1 - tstar) * phi + tstar * phi_new)
        }
        res <- logconcave_ll_all(x, ww, phi)
        L <- res$ll
        phi_new <- res$phi_new
        dirderiv <- res$dirderiv
      } else {
        dirderiv <- 0
      }
    }

    list(
      phi = phi,
      L = L,
      Fhat = logconcave_f(x, phi)
    )
  }

  n <- length(x)
  res1 <- logconcave_coarsen(x, w, is_knot)
  x2 <- res1$x2
  w2 <- res1$w2
  K <- (1:n) * is_knot
  K <- K[K > 0]
  res2 <- MLE(x2, w2, phi_o[K])
  phi <- res2$phi
  L <- res2$L
  phi <- logconcave_extend(x, is_knot, x2, phi)
  conv <- as_vct(logconcave_convexity(x, phi)) * is_knot
  Fhat <- logconcave_f(x, phi)
  H <- 1:n * 0
  JJ <- (1:n) * is_knot
  JJ <- JJ[JJ > 0]

  for (i in 1:(length(JJ) - 1)) {
    if (JJ[i + 1] > JJ[i] + 1) {
      dtmp <- x[JJ[i + 1]] - x[JJ[i]]
      ind <- (JJ[i] + 1):(JJ[i + 1] - 1)
      mtmp <- length(ind)
      xtmp <- (x[ind] - x[JJ[i]]) / dtmp
      wtmp <- w[ind]
      #cstmp <- cumsum(xtmp)
      H[ind] <-
        dtmp * (cumsum(wtmp * xtmp) - xtmp * cumsum(wtmp) + xtmp * sum(wtmp * (1 - xtmp)))
      jtmp1 <- xtmp * j_10(phi[ind], phi[JJ[i]] * rep(1, mtmp))
      jtmp2 <-
        (1 - xtmp) * j_10(phi[ind], phi[JJ[i + 1]] * rep(1, mtmp))
      H[ind] <-
        H[ind] - dtmp^2 * (xtmp * (1 - xtmp)) * (jtmp1 + jtmp2)
    } ## end if
  } ## end for

  res <-
    list(
      phi = matrix(phi, ncol = 1),
      L = L,
      conv = matrix(conv, ncol = 1),
      H = matrix(H, ncol = 1)
    )
  return(res)
}

logconcave_coarsen <- function(x, w, is_knot) {
  n <- length(x)
  K <- (1:n) * is_knot
  K <- K[K > 0]
  x2 <- x[K]
  w2 <- w[K]
  for (k in 1:(length(K) - 1)) {
    if (K[k + 1] > (K[k] + 1)) {
      ind <- (K[k] + 1):(K[k + 1] - 1)
      lambda <- (x[ind] - x2[k]) / (x2[k + 1] - x2[k])
      w2[k] <- w2[k] + sum(w[ind] * (1 - lambda))
      w2[k + 1] <- w2[k + 1] + sum(w[ind] * lambda)
    }
  }
  w2 <- w2 / sum(w2)
  list(x2 = matrix(x2, ncol = 1), w2 = matrix(w2, ncol = 1))
}

logconcave_convexity <- function(x, phi) {
  n <- length(x)
  deriv <- diff(phi) / diff(x)
  conv <- 1:n * 0
  conv[2:(n - 1)] <- diff(deriv[1:(n - 1)])
  matrix(conv, ncol = 1)
}

logconcave_extend <- function(x, is_knot, x2, phi2) {
  n <- length(x)
  K <- (1:n) * is_knot
  K <- K[K > 0]
  phi <- 1:n * 0
  phi[K] <- phi2
  for (k in 1:(length(K) - 1)) {
    if (K[k + 1] > (K[k] + 1)) {
      ind <- (K[k] + 1):(K[k + 1] - 1)
      lambda <- (x[ind] - x2[k]) / (x2[k + 1] - x2[k])
      phi[ind] <- (1 - lambda) * phi2[k] + lambda * phi2[k + 1]
    }
  }
  matrix(phi, ncol = 1)
}

logconcave_f <- function(x, phi) {
  n <- length(x)
  Fhat <- 1:n * 0
  dx <- diff(x)
  Fhat[2:n] <- cumsum(dx * j_00(phi[1:(n - 1)], phi[2:n]))
  matrix(Fhat, ncol = 1)
}

logconcave_ll <- function(x, w, phi) {
  n <- length(x)
  dx <- diff(x)
  L <- sum(w * phi) - sum(dx * j_00(phi[1:(n - 1)], phi[2:n]))
  return(L)
}

logconcave_ll_all <- function(x, w, phi) {
  n <- length(x)
  dx <- diff(x)
  ll <- sum(w * phi) - sum(dx * j_00(phi[1:(n - 1)], phi[2:n]))
  grad <- matrix(w, ncol = 1)
  grad[1:(n - 1)] <-
    grad[1:(n - 1)] - (dx * j_10(phi[1:(n - 1)], phi[2:n]))
  grad[2:n] <- grad[2:n] - (dx * j_10(phi[2:n], phi[1:(n - 1)]))
  tmp <-
    c(dx * j_20(phi[1:(n - 1)], phi[2:n]), 0) + c(0, dx * j_20(phi[2:n], phi[1:(n - 1)]))
  tmp <- tmp + mean(tmp) * 1e-12
  mhess2 <- matrix(0, nrow = n, ncol = n)
  mhess3 <- mhess2
  mhess1 <- tmp
  tmp <- c(0, dx * j_11(phi[1:(n - 1)], phi[2:n]))
  tmp.up <- diag(tmp[2:n], nrow = n - 1, ncol = n - 1)
  mhess2[1:(n - 1), 2:n] <- tmp.up
  mhess3[2:n, 1:(n - 1)] <-
    diag(tmp[2:n], nrow = n - 1, ncol = n - 1)
  mhess <- diag(mhess1) + mhess2 + mhess3
  phi_new <- phi + solve(mhess) %*% grad
  dirderiv <- t(grad) %*% (phi_new - phi)
  return(list(
    ll = ll,
    phi_new = phi_new,
    dirderiv = dirderiv
  ))
}

j_00 <- function(x, y, v = 1) {
  m <- length(x)
  z <- exp(x)
  d <- y - x
  II <- (1:m)[abs(d) > 0.005]
  z[II] <- z[II] * (exp(v * d[II]) - 1) / d[II]
  II <- (1:m)[abs(d) <= 0.005]
  z[II] <- z[II] *
    (v +
       d[II] * (v / 2 +
                  d[II] * (v / 6 +
                             d[II] * (v / 24 + d[II] * v / 120))))
  return(z)
}

j_10 <- function(x, y) {
  m <- length(x)
  z <- exp(x)
  d <- y - x
  II <- (1:m)[abs(d) > 0.01]
  z[II] <- z[II] * (exp(d[II]) - 1 - d[II]) / (d[II]^2)
  II <- (1:m)[abs(d) <= 0.01]
  z[II] <- z[II] *
    (1 / 2 +
       d[II] * (1 / 6 +
                  d[II] * (1 / 24 +
                             d[II] * (1 / 120 + d[II] / 720))))
  return(z)
}

j_11 <- function(x, y) {
  m <- length(x)
  z <- exp(x)
  d <- y - x
  II <- (1:m)[abs(d) > 0.02]
  z[II] <- z[II] *
    (d[II] * (exp(d[II]) + 1) - 2 * (exp(d[II]) - 1)) / (d[II]^3)
  II <- (1:m)[abs(d) <= 0.02]
  z[II] <- z[II] *
    (1 / 6 +
       d[II] * (1 / 12 +
                  d[II] * (1 / 40 +
                             d[II] * (1 / 180 + d[II] / 1008))))
  return(z)
}

j_20 <- function(x, y) {
  m <- length(x)
  z <- exp(x)
  d <- y - x
  II <- (1:m)[abs(d) > 0.02]
  z[II] <- 2 * z[II] *
    (exp(d[II]) - 1 - d[II] - d[II]^2 / 2) / (d[II]^3)
  II <- (1:m)[abs(d) <= 0.02]
  z[II] <- z[II] *
    (1 / 3 +
       d[II] * (1 / 12 +
                  d[II] * (1 / 60 +
                             d[II] * (1 / 360 + d[II] / 2520))))
  return(z)
}

#' @rdname log_concave
#' @export
quant_logcon <- function(p, logcon) {
  if (any(p < 0 |
          p > 1)) {
    cat_stop("All entries of the argument ps given to quantilesLogConDens() must be in [0, 1]!\n")
  }

  x <- logcon$x
  phi <- logcon$phi
  Fhat <- logcon$Fhat
  n <- length(x)
  logcon <- matrix(NA, ncol = 2, nrow = length(p))

  for (i in 1:length(p)) {
    p0 <- p[i]

    if (p0 == 0) {
      q <- -Inf
    }
    if (p0 == 1) {
      q <- x[n]
    }

    if ((p0 > 0) && (p0 < 1)) {
      n <- length(x)
      xj <- max(x[Fhat <= p0])
      j <- length(x[x <= xj])
      q <-
        xj + (x[j + 1] - x[j]) * qloglin((p0 - Fhat[j]) / (Fhat[j + 1] - Fhat[j]), (x[j + 1] - x[j]) * (phi[j + 1] - phi[j]))
    }

    logcon[i, ] <- c(p0, as_num(q))
  }

  colnames(logcon) <- c("ps", "quantile")
  logcon
}
