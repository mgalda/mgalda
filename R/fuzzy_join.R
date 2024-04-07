#' distance_join
#'
#'
#' @name distance_join
#' @rdname distance_join
#' @param x A tbl
#' @param y A tbl
#' @param by Columns by which to join the two tables
#' @param max_dist Maximum distance to use for joining
#' @param method Method to use for computing distance, either euclidean (default)
#' or manhattan.
#' @param distance_col If given, will add a column with this
#' name containing the distance between the two
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#'
#' @examples
#'
#' library(dplyr)
#'
#' head(iris)
#' sepal_lengths <- data.frame(
#'   Sepal.Length = c(5, 6, 7),
#'   Sepal.Width = 1:3
#' )
#'
#' iris %>%
#'  distance_join(sepal_lengths, max_dist = 2, mode = "inner",distance_col = "dist")
#' @export
distance_join <-
  function(x,
           y,
           by = NULL,
           max_dist = 1,
           method = c("euclidean", "manhattan"),
           mode = c("inner", "left", "right", "full", "semi", "anti"),
           distance_col = NULL) {
    method <- match.arg(method)

    match_fun <- function(v1, v2) {
      if (is.null(dim(v1))) {
        # If the vectors are one-dimensional, turn them into 1-column matrices
        v1 <- t(t(v1))
        v2 <- t(t(v2))
      }

      if (method == "euclidean") {
        d <- sqrt(rowSums((v1 - v2)^2))
      } else if (method == "manhattan") {
        d <- rowSums(abs(v1 - v2))
      }
      ret <- tibble(instance = d <= max_dist)
      if (!is.null(distance_col)) {
        ret[[distance_col]] <- d
      }
      ret
    }

    ensure_distance_col(
      fuzzy_join(
        x,
        y,
        multi_by = by,
        multi_match_fun = match_fun,
        mode = mode
      ),
      distance_col,
      mode
    )
  }

#' difference_join
#'
#'
#' @name difference_join
#' @rdname difference_join
#' @param x A tbl
#' @param y A tbl
#' @param by Columns by which to join the two tables
#' @param max_dist Maximum distance to use for joining
#' @param distance_col If given, will add a column with this
#' name containing the difference between the two
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#'
#' @examples
#'
#' library(dplyr)
#'
#' head(iris)
#' sepal_lengths <- data.frame(Sepal.Length = c(5, 6, 7), Type = 1:3)
#'
#' iris %>%
#'    difference_join(sepal_lengths, max_dist = .5,mode = "left")
#'
#' @export

difference_join <-
  function(x,
           y,
           by = NULL,
           max_dist = 1,
           mode = c("inner", "left", "right", "full", "semi", "anti"),
           distance_col = NULL) {
    match_fun <- function(v1, v2) {
      dist <- abs(v1 - v2)
      ret <- data.frame(include = (dist <= max_dist))
      if (!is.null(distance_col)) {
        ret[[distance_col]] <- dist
      }
      ret
    }

    ensure_distance_col(
      fuzzy_join(
        x,
        y,
        by = by,
        match_fun = match_fun,
        mode = mode
      ),
      distance_col,
      mode
    )
  }

#' stringdist_join
#'
#'
#' @name stringdist_join
#' @rdname stringdist_join
#' @param x A tbl
#' @param y A tbl
#' @param by Columns by which to join the two tables
#' @param max_dist Maximum distance to use for joining
#' @param ignore_case Whether to be case insensitive (default yes)
#' @param method Method for computing string distance, see
#' \code{stringdist-metrics} in the stringdist package.
#' @param distance_col If given, will add a column with this
#' name containing the difference between the two
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#' @param ... Arguments passed on to \code{\link{stringdist}}
#'
#' @details If \code{method = "soundex"}, the \code{max_dist} is
#' automatically set to 0.5, since soundex returns either a 0 (match)
#' or a 1 (no match).
#'
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' data(diamonds)
#'
#' d <- data.frame(approximate_name = c("Idea", "Premiums", "Premioom",
#'                                      "VeryGood", "VeryGood", "Faiir"),
#'                 type = 1:6)
#'
#' # no matches when they are inner-joined:
#' diamonds %>%
#'   inner_join(d, by = c(cut = "approximate_name"))
#'
#' # but we can match when they're fuzzy joined
#' diamonds %>%
#'  stringdist_join(d, by = c(cut = "approximate_name"))
#'
#' @export

stringdist_join <-
  function(x,
           y,
           by = NULL,
           max_dist = 2,
           method = c(
             "osa",
             "lv",
             "dl",
             "hamming",
             "lcs",
             "qgram",
             "cosine",
             "jaccard",
             "jw",
             "soundex"
           ),
           mode = c("inner", "left", "right", "full", "semi", "anti"),
           ignore_case = FALSE,
           distance_col = NULL,
           ...) {
    method <- match.arg(method)

    if (method == "soundex") {
      # soundex always returns 0 or 1, so any other max_dist would
      # lead either to always matching or never matching
      max_dist <- .5
    }

    match_fun <- function(v1, v2) {
      if (ignore_case) {
        v1 <- stringr::str_to_lower(v1)
        v2 <- stringr::str_to_lower(v2)
      }

      # shortcut for Levenshtein-like methods: if the difference in
      # string length is greater than the maximum string distance, the
      # edit distance must be at least that large

      # length is much faster to compute than string distance
      if (method %in% c("osa", "lv", "dl")) {
        length_diff <-
          abs(stringr::str_length(v1) - stringr::str_length(v2))
        include <- length_diff <= max_dist

        dists <- rep(NA, length(v1))

        dists[include] <-
          stringdist::stringdist(v1[include], v2[include], method = method, ...)
      } else {
        # have to compute them all
        dists <-
          stringdist::stringdist(v1, v2, method = method, ...)
      }
      ret <- tibble(include = (dists <= max_dist))
      if (!is.null(distance_col)) {
        ret[[distance_col]] <- dists
      }
      ret
    }

    ensure_distance_col(
      fuzzy_join(
        x,
        y,
        by = by,
        mode = mode,
        match_fun = match_fun
      ),
      distance_col,
      mode
    )
  }

#' @keywords internal
unrowwname <- function(x) {
  rownames(x) <- NULL
  x
}

#' @keywords internal
ensure_distance_col <- function(ret, distance_col, mode) {
  if (!(mode %in% c("semi", "anti")) &&
      !is.null(distance_col) &&
      is.null(ret[[distance_col]])) {
    if (nrow(ret) == 0) {
      ret[[distance_col]] <- numeric(0)
    } else {
      ret[[distance_col]] <- NA
    }
  }
  ret
}

#' @keywords internal
fuzzy_join <-
  function(x,
           y,
           by = NULL,
           match_fun = NULL,
           multi_by = NULL,
           multi_match_fun = NULL,
           index_match_fun = NULL,
           mode = "inner",
           ...) {
    # preserve the grouping of x
    x_groups <- dplyr::groups(x)
    x <- ungroup(x)
    regroup <- function(d) {
      if (length(x_groups) == 0) {
        return(d)
      }

      g <- map_chr(x_groups, as_chr)
      missing <- !(g %in% colnames(d))
      # add .x to those that are missing; they've been renamed
      g[missing] <- paste0(g[missing], ".x")

      group_by_at(d, g)
    }

    mode <-
      match.arg(mode, c("inner", "left", "right", "full", "semi", "anti"))

    non_nulls <- (!is.null(multi_match_fun)) +
      (!is.null(match_fun)) +
      (!is.null(index_match_fun))
    if (sum(non_nulls) != 1) {
      cat_stop("Must give exactly one of match_fun, multi_match_fun, and index_match_fun")
    }

    if (!is.null(match_fun)) {
      by <- common_by(by, x, y)

      # Support formula notation for functions
      if (is.list(match_fun)) {
        match_fun <- map(match_fun, purrr::as_mapper)
      } else {
        match_fun <- purrr::as_mapper(match_fun)
      }

      if (length(match_fun) == 1) {
        match_fun <- rep(c(match_fun), length(by$x))
      }
      if (length(match_fun) != length(by$x)) {
        cat_stop("Length of match_fun not equal to columns specified in 'by'.")
      }

      matches <-
        bind_rows(lapply(seq_along(by$x), function(i) {
          col_x <- x[[by$x[i]]]
          col_y <- y[[by$y[i]]]

          indices_x <- tibble(
            col = col_x,
            indices = seq_along(col_x)
          ) %>%
            group_by(col) %>%
            nest() %>%
            mutate(indices = map(data, "indices"))

          indices_y <- tibble(
            col = col_y,
            indices = seq_along(col_y)
          ) %>%
            group_by(col) %>%
            nest() %>%
            mutate(indices = map(data, "indices"))

          u_x <- indices_x$col
          u_y <- indices_y$col

          if (!is.null(names(match_fun))) {
            # match_fun is a named list, use the names in x
            mf <- match_fun[[by$x[[i]]]]
          } else {
            mf <- match_fun[[i]]
          }

          extra_cols <- NULL

          n_x <- length(u_x)
          n_y <- length(u_y)
          m <- mf(rep(u_x, n_y), rep(u_y, each = n_x), ...)

          if (is.data.frame(m)) {
            if (ncol(m) > 1) {
              # first column is logical, others are included as distance columns
              extra_cols <- m[, -1, drop = FALSE]
            }
            m <- m[[1]]
          }

          # return as a data frame of x and y indices that match
          w <- which(m) - 1

          if (length(w) == 0) {
            # there are no matches
            ret <-
              tibble(
                i = numeric(0),
                x = numeric(0),
                y = numeric(0)
              )
            return(ret)
          }

          x_indices_l <- indices_x$indices[w %% n_x + 1]
          y_indices_l <- indices_y$indices[w %/% n_x + 1]

          xls <- sapply(x_indices_l, length)
          yls <- sapply(y_indices_l, length)

          x_rep <-
            unlist(map2(x_indices_l, yls, function(x, y) {
              rep(x, each = y)
            }))
          y_rep <-
            unlist(map2(y_indices_l, xls, function(y, x) {
              rep(y, x)
            }))

          ret <- tibble(i = i, x = x_rep, y = y_rep)

          if (!is.null(extra_cols)) {
            extra_indices <- rep(w, xls * yls)
            extra_cols_rep <-
              extra_cols[extra_indices + 1, , drop = FALSE]
            ret <- bind_cols(ret, extra_cols_rep)
          }

          ret
        }))

      if (length(by$x) > 1) {
        # only take cases where all pairs have matches
        accept <- matches %>%
          count(x, y) %>%
          ungroup() %>%
          filter(n == length(by$x))

        matches <- matches %>%
          dplyr::semi_join(accept, by = c("x", "y"))

        if (ncol(matches) > 3) {
          # include one for each
          matches <- matches %>%
            dplyr::semi_join(accept, by = c("x", "y")) %>%
            mutate(name = by$x[i]) %>%
            dplyr::select(-i) %>%
            tidyr::gather(key, value, -x, -y, -name) %>%
            tidyr::unite(newname, name, key, sep = ".") %>%
            tidyr::spread(newname, value)
        } else {
          matches <- distinct(matches, x, y)
        }
      }
    } else if (!is.null(multi_match_fun)) {
      multi_match_fun <- purrr::as_mapper(multi_match_fun)

      # use multiple matches
      by <- common_by(multi_by, x, y)

      number_x_rows <- nrow(x)
      number_y_rows <- nrow(y)

      indices_x <- x %>%
        select_at(by$x) %>%
        mutate(indices = seq_len(number_x_rows)) %>%
        group_by_at(vars(-dplyr::one_of("indices"))) %>%
        nest() %>%
        mutate(indices = map(data, "indices"))
      indices_y <- y %>%
        select_at(by$y) %>%
        mutate(indices = seq_len(number_y_rows)) %>%
        group_by_at(vars(-dplyr::one_of("indices"))) %>%
        nest() %>%
        mutate(indices = map(data, "indices"))

      ux <- as_mtx(indices_x[by$x])
      uy <- as_mtx(indices_y[by$y])

      pairs <- matrix(NA, nrow(ux), nrow(uy))
      ix <- row(pairs)
      iy <- col(pairs)
      ux_input <- ux[ix, ]
      uy_input <- uy[iy, ]

      m <- multi_match_fun(ux_input, uy_input)

      extra_cols <- NULL
      if (is.data.frame(m)) {
        if (ncol(m) > 1) {
          extra_cols <- m[, -1, drop = FALSE]
        }
        m <- m[[1]]
      }

      if (sum(m) == 0) {
        # there are no matches
        matches <- tibble(x = numeric(0), y = numeric(0))
      } else {
        x_indices_l <- indices_x$indices[ix[m]]
        y_indices_l <- indices_y$indices[iy[m]]
        xls <- map_dbl(x_indices_l, length)
        yls <- map_dbl(y_indices_l, length)
        x_rep <-
          unlist(map2(x_indices_l, yls, function(x, y) {
            rep(x, each = y)
          }))
        y_rep <-
          unlist(map2(y_indices_l, xls, function(y, x) {
            rep(y, x)
          }))

        matches <- tibble(x = x_rep, y = y_rep)
        if (!is.null(extra_cols)) {
          extra_indices <- rep(which(m), xls * yls)
          extra_cols_rep <-
            extra_cols[extra_indices, , drop = FALSE]
          matches <- bind_cols(matches, extra_cols_rep)
        }
      }
    } else {
      # raw index-index function
      index_match_fun <- purrr::as_mapper(index_match_fun)
      by <- common_by(multi_by, x, y)

      d1 <- x[, by$x, drop = FALSE]
      d2 <- y[, by$y, drop = FALSE]

      matches <- index_match_fun(d1, d2)
    }
    matches$i <- NULL

    if (mode == "semi") {
      # just use the x indices to include
      return(regroup(x[sort(unique(matches$x)), , drop = FALSE]))
    }
    if (mode == "anti") {
      if (nrow(matches) == 0) {
        return(regroup(x))
      }
      # just use the x indices to exclude
      return(regroup(x[-sort(unique(matches$x)), , drop = FALSE]))
    }

    matches <- arrange(matches, x, y)

    # in cases where columns share a name, rename each to .x and .y
    n <- intersect(colnames(x), colnames(y))
    x <- rename_at(x, .vars = n, ~ paste0(.x, ".x"))
    y <- rename_at(y, .vars = n, ~ paste0(.x, ".y"))

    # fill in indices of the x, y, or both
    # curious if there's a higher performance approach
    if (mode == "left") {
      matches <- tibble(x = seq_len(nrow(x))) %>%
        left_join(matches, by = "x")
    } else if (mode == "right") {
      matches <- tibble(y = seq_len(nrow(y))) %>%
        left_join(matches, by = "y")
    } else if (mode == "full") {
      matches <- matches %>%
        full_join(tibble(x = seq_len(nrow(x))), by = "x") %>%
        full_join(tibble(y = seq_len(nrow(y))), by = "y")
    }

    ret <-
      bind_cols(
        unrowwname(x[matches$x, , drop = FALSE]),
        unrowwname(y[matches$y, , drop = FALSE])
      )
    if (ncol(matches) > 2) {
      extra_cols <- unrowwname(matches[, -(1:2), drop = FALSE])
      ret <- bind_cols(ret, extra_cols)
    }

    ret <- regroup(ret)

    # Base the type (data.frame vs tbl_df) on x, not on y
    if (!inherits(x, "tbl_df")) {
      ret <- as_df(ret)
    }

    ret
  }

#' @keywords internal
common_by <- function(by = NULL, x, y) {
  if (is.list(by)) {
    return(by)
  }

  if (!is.null(by)) {
    x <- names(by) %||% by
    y <- unname(by)

    # If x partially named, assume unnamed are the same in both tables
    x[x == ""] <- y[x == ""]

    return(list(x = x, y = y))
  }

  by <- intersect(dplyr::tbl_vars(x), dplyr::tbl_vars(y))
  if (length(by) == 0) {
    cat_stop("No common variables. Please specify `by` param.")
  }
 cat_message("Joining by: ", utils::capture.output(dput(by)))

  list(
    x = by,
    y = by
  )
}
