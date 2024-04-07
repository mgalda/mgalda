#' Increasing sequence of integers in an interval
#'
#' @name seqs
#' @rdname seqs
#'
#' @param from The starting point of the sequence.
#' @param to The end point.
#' @param x A vector whose length is the end point in `seq2_along()` or
#' a data frame `seq_col()` or `seq2_row()`
#' @param range range de seq
#' @return An integer vector containing a strictly increasing
#'   sequence.
#' @export
#' @examples
#' seq2(2, 10)
#' seq2(10, 2)
#' seq(10, 2)
#'
#' seq2_along(10, letters)
#'
#' seq_row(mtcars)
#'
#' seq_col(mtcars)
#'
#' seq_between(range = c(4, 10))
#'
#' seq_log(from = 1, to = 1000, length.out = 10, base = 10)
#'
#' seq_power(from = 1, to = 1000, length.out = 10, degree = 3)
#'
#' x <- rcauchy(100)
#' seq_range(x, n = 10)
#' seq_range(x, n = 10, trim = 0.1)
#' seq_range(x, by = 1, trim = 0.1)
#'
#' # Make pretty sequences
#' y <- runif(100)
#' seq_range(y, n = 10)
#' seq_range(y, n = 10, pretty = TRUE)
#' seq_range(y, n = 10, expand = 0.5, pretty = TRUE)
#'
#' seq_range(y, by = 0.1)
#' seq_range(y, by = 0.1, pretty = TRUE)
#'

seq2 <- function(from, to) {
  if (length(from) != 1) {
    cat_stop("`from` must be length one")
  }
  if (length(to) != 1) {
    cat_stop("`to` must be length one")
  }

  if (from > to) {
    integer()
  } else {
    seq.int(from, to)
  }
}
#' @rdname seqs
#' @export
seq2_along <- function(from, x) {
  seq2(from, length(x))
}
#' @rdname seqs
#' @export
seq_row <- function(x) {
  seq_len(nrow(x))
}
#' @rdname seqs
#' @export
seq_col <- function(x) {
  seq_len(ncol(x))
}
#' @rdname seqs
#' @export
seq_between <- function(range, ...) {
  seq(from = range[1], to = range[2], ...)
}
#' @rdname seqs
#' @export
seq_power <- function(from, to, length.out, degree = 2) {
  seq.default(from^(1 / degree), to^(1 / degree), length.out = length.out)^
    degree
}
#' @rdname seqs
#' @export
seq_log <-
  function(from,
           to,
           length.out,
           base = 10,
           plus1 = TRUE,
           start = c("lower", "mid", "upper")) {
    start <- match.arg(start)

    xrange <- abs(to - from)

    if (start == "mid") {
      xrange <- xrange / 2
    }

    from_pr <- 0
    from_pr <-
      log(from_pr + as_int(plus1), base)

    to_pr <- xrange
    to_pr <-
      log(to_pr + as_int(plus1), base)

    if (start == "mid") {
      pmean <- (to + from) / 2

      if (is_odd(length.out)) {
        lout <- ceiling(length.out / 2)
      } else {
        lout <- ceiling(length.out / 2) + 1
      }
    } else {
      lout <- length.out
    }

    log_s <-
      base^seq(from_pr, to_pr, length.out = lout) - as_int(plus1)

    if (start == "mid") {
      if (!is_odd(length.out)) {
        log_s <- unique(sort(c(pmean - rev(log_s), pmean + log_s)))
        indx <- trunc(length.out / 2) + -1:1
        mean_x <- (log_s[1:2] + log_s[2:3]) / 2
        log_s <-
          sort(c(log_s[!seq_along(log_s) %in% indx], mean_x))
      } else {
        log_s <- unique(sort(c(pmean - rev(log_s[-1]), pmean + log_s[-1])))
        indx <- c(trunc(length.out / 2), ceiling(length.out / 2))
        mean_x <- mean(log_s[indx])
        log_s <-
          sort(c(log_s, mean_x))
      }
    } else {
      if (start == "lower") {
        log_s <- from + sort(log_s)
      } else if (start == "upper") {
        log_s <- to - rev(sort(log_s))
      }
    }
    log_s
  }
#' @rdname seqs
#' @export
seq_range <- function(x, n, by, trim = NULL, expand = NULL, pretty = FALSE) {
  if (!missing(n) && !missing(by)) {
    cat_stop("May only specify one of `n` and `by`")
  }

  if (!is.null(trim)) {
    rng <- stats::quantile(x, c(trim / 2, 1 - trim / 2), na.rm = TRUE)
  } else {
    rng <- range(x, na.rm = TRUE)
  }

  if (!is.null(expand)) {
    rng <- rng + c(-expand / 2, expand / 2) * (rng[2] - rng[1])
  }

  if (missing(by)) {
    if (pretty) {
      pretty(rng, n)
    } else {
      seq(rng[1], rng[2], length.out = n)
    }
  } else {
    if (pretty) {
      rng[1] <- floor(rng[1] / by) * by
      rng[2] <- ceiling(rng[2] / by) * by
    }
    seq(rng[1], rng[2], by = by)
  }
}
#' @rdname seqs
#' @export
seq_full <- function(range, size, pad = FALSE) {
  if (diff(range) < 1e-6) return(c(range[1] - size / 2, range[1] + size / 2))

  x <- seq(
    round_any(range[1], size, floor),
    round_any(range[2], size, ceiling),
    by=size
  )

  if (pad) {
    # Add extra bin on bottom and on top, to guarantee that we cover complete
    # range of data, whether right = T or F
    c(min(x) - size, x, max(x) + size)
  } else {
    x
  }
}
#' @rdname seqs
#' @export
rep_each_n <- function(n, times) {
  if (length(n) > 1) {
    n <- length(n)
  }

  x <- rep(
    x = 1:times,
    each = round(n / times),
    length.out = n
  )
  sort(x)
}

#' @rdname seqs
#' @export
rep_full <- function(x, times, length) {
  if (!xor(missing(times), missing(length))) {
    cat_stop("Must supply exactly one of `times` and `length`")
  } else if (!missing(times)) {
    vctrs::vec_assert(times, numeric(), 1L)
    length <- times * base::length(x)
  } else if (!missing(length)) {
    vctrs::vec_assert(length, numeric(), 1L)
  }
  rep_len(x, length)
}

#' @rdname seqs
#' @export
rep_each <- function(x, times) {
  assert_dblint(times)
  times <- vctrs::vec_recycle(times, vctrs::vec_size(x))

  rep.int(x, times)
}
#' @rdname seqs
#' @export
rep_along  <- function(x,along) {
  base::rep_len(x, length(along))
}
#' @export
rep.data.frame <- function(x, ...) {
  as_df(lapply(x, rep, ...))
}

#' @export
rep_chars <- function(width, char = " ") {
  paste(rep(char, width), collapse = "")
}

#' @export
seq_fromalong <- function(from,along.with,...){
  seq.int(from = from,to = length(along.with),...)
}

#' @export
seq_lengthout <- function(x, n) {
  seq.default(
    from = 1,
    to = length(x),
    length.out = n
  )
}

#' @export
seq_prob <- function(n) {
  seq.default(
    from = 0,
    to = 1,
    length.out = n
  )
}

#' @export
seq_percent <- function(x,n){
  .un(quantile(x = x,probs = seq_prob(n = n)))
}
