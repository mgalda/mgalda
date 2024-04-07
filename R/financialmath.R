#' financialmath
#'
#'
#' @name financialmath
#' @rdname financialmath
#' @keywords internal
#'
#' @examples
#' library(lubridate)
#' set.seed(7)
#'
#' cf <- c(-2000,runif(20,400,700))
#' dates <- today() +years(0:20)
#' r <- .12
#'
#' annuity_value(cf =cf,r = r,date = dates)
#' disc_payback(cf = cf,r = r,date = dates)
#' discounted_cf(cf = cf,r = r,date = dates)
#' err(cf = cf,rate = .03,date = dates)
#' irr(cf = cf,date = dates)
#' netpresentvalue(cf = cf,r = r,date = dates)
#' payback(cf = cf,date = dates)
#' profitability_index(cf = cf[-1],capex = cf[1],MARR = r,date = dates[-1])
#' rstated2annual(r = r,m = 12)
#'

NULL


#' @rdname financialmath
#' @export
discounted_cf <- function(cf, r, date = NULL) {
  if (length(r) != 1) {
    cat_stop("r debe ser de lenght = 1")
  }
  if (is.null(date)) {
    cf / ((1 + r)^(seq_along(cf) - 1))
  } else {
    if (length(cf) != length(date)) {
      cat_stop("length('cf') != length('date')")
    }
    cf / ((1 + r)^(as_int(date - date[1]) / 365))
  }
}

#' @rdname financialmath
#' @export
disc_payback <- function(cf, r, date = NULL) {
  payback(discounted_cf(cf = cf, r = r, date = date))
}

#' @rdname financialmath
#' @export
irr <- function(cf, date = NULL) {
  f <- function(i) {
    netpresentvalue(cf = cf, r = i, date = date)
  }

  if (f(0) < 0) {
    res <- 0
  } else {
    res <- stats::uniroot(f,
                          maxiter = 1e6,
                          lower = 0,
                          upper = 20
    )$root
  }

  res
}

#' @rdname financialmath
#' @export
netpresentvalue <- function(cf, r, date = NULL) {
  sum(discounted_cf(cf = cf, r = r, date = date))
}

#' @rdname financialmath
#' @export
payback<-function (cf, date = NULL) {
  i <- match(1, sign(cumsum(cf)))
  pb <- i - 2 + (-cumsum(cf)[i - 1]/cf[i])
  if (!is.null(date)) {
    if (length(cf) != length(date)) {
      cat_stop("length('cf') != length('date')")
    }
    pb_date <- c()
    if(is_true(pb<0)| is_nonnum(pb)){
      return(NA)
    }

    for (j in seq_len(ceiling(pb))) {
      ddif <- as_int(date[j + 1] - date[j])
      if (pb <= j) {
        ddif <- ddif * (1 - (j - pb))
      }
      pb_date <- c(pb_date, ddif)
    }
    pb <- sum(pb_date)/365
  }
  pb
}

#' @rdname financialmath
#' @export
annuity_value <- function(cf, r, date = NULL) {
  netpresentvalue(c(0, cf[-1]), r, date) * r / (1 - 1 / (1 + r)^length(cf))
}

#' @rdname financialmath
#' @export
rstated2annual <- function(r, m) {
  # stated annual rate
  # number of compounding periods per year
  (1 + r / m)^m - 1
}

#' @rdname financialmath
#' @export

err <- function(cf, rate, date = NULL) {
  f <- function(i) {
    netpresentvalue(cf = cf, r = i + rate, date = date)
  }

  if (f(0) < 0) {
    res <- 0
  } else {
    res <- stats::uniroot(f,
                          maxiter = 1e6,
                          lower = 0,
                          upper = 20
    )$root
  }

  res
}

#' @rdname financialmath
#' @export
profitability_index <- function(cf, capex, MARR, date = NULL) {
  1 + (netpresentvalue(cf, r = MARR, date) / abs(capex))
}
