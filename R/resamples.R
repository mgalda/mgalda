#' helpers function
#'
#' @name custom_resamples
#' @rdname custom_resamples
#' @keywords internal
#'
#' @examples
#'
#' data <- datalearn$iris
#'
#' bootsplits(
#'   dataset = data,
#'   times = 5,
#'   outcome = species
#' )
#' kfolds(
#'   dataset = data,
#'   times = 5,
#'   outcome = species,
#'   rep = 3
#' )
#' manual_resamples(
#'   dataset = data,
#'   times = 5,
#'   outcome = species,
#'   rango = c(.4, .5)
#' )
#' sliding_resamples(dataset = data, times = 6)
#' split_resamples(
#'   dataset = data,
#'   times = 4,
#'   split_col = species
#' )
#' vfolds(
#'   dataset = data,
#'   times = 4,
#'   outcome = species
#' )
#' random_rset(
#'   dataset = data,
#'   times = 5,
#'   rep = 3,
#'   outcome = species
#' )
#'
#' invkfolds_vec(
#'   x = seq(1, 1000, 1),
#'   times = 4,
#'   rep = 4
#' )
#' kfolds_vec(
#'   x = seq(1, 1000, 1),
#'   times = 4,
#'   rep = 4
#' )
#'
NULL

#' @export
analysis <- rsample::analysis

#' @export
rset2dflist <- function(x){
  map(x$splits,analysis)
}

# manual_resamples ------------------------------------------------------------------

#' Resamples
#' @rdname  custom_resamples
#' @export

manual_resamples <-
  function(dataset,
           times = 5,
           outcome = NULL,
           rango = c(.4, .6)) {
    check_rango_muestras(rango)


    if (is_empty(match.call()$outcome)) {
      outcome <- NULL
      out <-
        manual_resamples_null(
          dataset = dataset,
          times = times,
          rango = rango
        )
    } else {
      outcome <- rlang::enexpr(outcome)
      if (!is_chr(outcome)) {
        outcome <- lang2str(outcome)
      }

      arg_list <-
        list(
          dataset = dataset,
          times = times,
          outcome = outcome,
          rango = rango
        )
      colclas <-
        function(.data, x) {
          x <- rlang::enquo(x)
          class(pull(.data, var = !!x))
        }

      classcol <-
        match.arg(
          arg = colclas(dataset, outcome),
          choices = c("numeric", "integer", "double", "factor", "character")
        )
      out <-
        switch(classcol,
               "numeric" = do.call(manual_resamples_num, args = arg_list),
               "integer" = do.call(manual_resamples_num, args = arg_list),
               "double" = do.call(manual_resamples_num, args = arg_list),
               "factor" = do.call(manual_resamples_class, args = arg_list),
               "character" = do.call(manual_resamples_class, args = arg_list)
        )
    }
    out
  }

#' @rdname custom_resamples
#' @keywords internal

manual_resamples_num <-
  function(dataset,
           times = 5,
           outcome,
           rango = c(.4, .6)) {
    check_rango_muestras(rango)

    ids_splits <-
      map(
        .x = seq_len(times),
        .f = function(x) {
          p <- stats::runif(1, min(rango), max(rango))

          dataset_splits <-
            split(x = dataset, f = dataset[[outcome]])

          ord_samp <- c()

          for (i in seq_along(dataset_splits)) {
            ord_samp <-
              c(ord_samp, sample(1:0, nrow(dataset_splits[[i]]),
                                 replace = T, c(p, 1 - p)
              ))
          }
          ord_samp
        }
      )

    ids_splits <-
      map(
        .x = ids_splits,
        .f = ~ list(analysis = which(.x == 1), assessment = which(.x == 0))
      )

    ids_splits <-
      purrr::map_dfr(
        .x = ids_splits,
        .f = ~ tibble(splits = list(
          rsample::make_splits(.x,
                               data = dataset,
                               class = "manual_splits"
          )
        ))
      )

    ids_splits <-
      rsample::manual_rset(
        splits = ids_splits$splits,
        ids = names0(length(ids_splits$splits), "Manual")
      )

    ids_splits
  }

#' @rdname custom_resamples
#' @keywords internal

manual_resamples_class <-
  function(dataset,
           times = 5,
           outcome,
           rango = c(.4, .6)) {
    check_rango_muestras(rango)
    ids_splits <-
      map(
        .x = seq_len(times),
        .f = function(x) {
          p <- stats::runif(1, min(rango), max(rango))
          dataset_splits <-
            split(x = dataset, f = dataset[[outcome]])
          ord_samp <- c()
          for (i in seq_along(dataset_splits)) {
            ord_samp <-
              c(ord_samp, sample(1:0, nrow(dataset_splits[[i]]), replace = T, c(p, 1 - p)))
          }
          ord_samp
        }
      )

    ids_splits <-
      map(
        .x = ids_splits,
        .f = ~ list(analysis = which(.x == 1), assessment = which(.x == 0))
      )

    ids_splits <-
      purrr::map_dfr(
        .x = ids_splits,
        .f = ~ tibble(splits = list(
          rsample::make_splits(.x,
                               data = dataset,
                               class = "manual_splits"
          )
        ))
      )

    ids_splits <-
      rsample::manual_rset(
        splits = ids_splits$splits,
        ids = names0(length(ids_splits$splits), "Manual")
      )

    ids_splits
  }

#' @rdname custom_resamples
#' @keywords internal

manual_resamples_null <-
  function(dataset,
           times = 5,
           rango = c(.4, .6)) {
    check_rango_muestras(rango)

    ids_splits <- map(
      .x = seq_len(times),
      .f = function(x) {
        p <- stats::runif(1, min(rango), max(rango))
        sample(1:0, nrow(dataset), replace = T, c(p, 1 - p))
      }
    )

    ids_splits <-
      map(
        .x = ids_splits,
        .f = ~ list(analysis = which(.x == 1), assessment = which(.x == 0))
      )

    ids_splits <-
      purrr::map_dfr(
        .x = ids_splits,
        .f = ~ tibble(splits = list(
          rsample::make_splits(.x,
                               data = dataset,
                               class = "manual_splits"
          )
        ))
      )

    ids_splits <-
      rsample::manual_rset(
        splits = ids_splits$splits,
        ids = names0(length(ids_splits$splits), "Manual")
      )

    ids_splits
  }


# fols ------------------------------------------------------------------------------

#' @rdname custom_resamples
#' @export

vfolds <-
  function(dataset,
           times = 10,
           outcome = NULL,
           reverse = F) {
    if (is.null(rlang::enexpr(outcome))) {
      strata <- NULL
    } else {
      strata <-
        tidyselect::vars_select(names(dataset), !!rlang::enquo(outcome))
    }

    if (!is.numeric(times) || length(times) != 1) {
      cat_stop("`v` must be a single integer.")
    }

    n <- nrow(dataset)
    if (is.null(strata)) {
      folds <- sample(rep(1:times, length.out = n))
      idx <- seq_len(n)
      indices <- unname(split(idx, folds))
    } else {
      stratas <-
        tibble(
          idx = 1:n,
          strata =
            rsample::make_strata(
              x = getElement(dataset, strata),
              breaks = floor(n / 10),
              depth = 10
            )
        )

      stratas <- unname(split(stratas, stratas$strata))

      stratas <- purrr::map_dfr(stratas, function(x, v) {
        x$folds <- sample(rep(1:v, length.out = nrow(x)))
        x
      }, v = times)

      indices <- unname(split(stratas$idx, stratas$folds))
    }

    default_complement <- function(ind, n) {
      list(
        analysis = setdiff(1:n, ind),
        assessment = unique(ind)
      )
    }

    reverse_complement <- function(ind, n) {
      list(
        analysis = unique(ind),
        assessment = setdiff(1:n, ind)
      )
    }

    if (reverse) {
      indices <- lapply(indices, reverse_complement, n = n)
    } else {
      indices <- lapply(indices, default_complement, n = n)
    }

    split_objs <-
      map(indices,
          rsample::make_splits,
          data = dataset,
          class = "vfold_split"
      )

    splits <-
      tibble(
        splits = split_objs,
        id = names0(length(split_objs), "vFold")
      )


    splits$splits <-
      map2(
        splits$splits,
        unname(split(
          dplyr::select(splits, id), rlang::seq2(1L, nrow(splits))
        )),
        function(split, id) {
          split$id <- id
          split
        }
      )
    class(splits) <- c("rset", class(splits))
    splits
  }

# kfolds ----------------------------------------------------------------------------

#' @rdname  custom_resamples
#' @export

kfolds <-
  function(dataset,
           times = 10,
           outcome = NULL,
           rep = 1,
           reverse = F) {
    if (is.null(rlang::enexpr(outcome))) {
      strata <- NULL
      splits <- purrr::map_dfr(
        .x = seq_len(rep),
        .f = ~ vfolds(
          dataset = dataset,
          times = times,
          reverse = reverse
        ),
        .id = "id2"
      ) %>%
        dplyr::transmute(splits, id = paste0(stringr::str_pad(id2, max(
          stringr::str_length(id2)
        ), pad = "0"), id)) %>%
        mutate(id = stringr::str_replace(id, "vFold", "kFold"))
    } else {
      strata <-
        tidyselect::vars_select(names(dataset), !!rlang::enquo(outcome))
      splits <- purrr::map_dfr(
        .x = seq_len(rep),
        .f = ~ vfolds(
          dataset = dataset,
          times = times,
          outcome = !!strata,
          reverse = reverse
        ),
        .id = "id2"
      ) %>%
        dplyr::transmute(splits, id = paste0(stringr::str_pad(id2, max(
          stringr::str_length(id2)
        ), pad = "0"), id)) %>%
        mutate(id = stringr::str_replace(id, "vFold", "kFold"))
    }


    splits$splits <-
      map2(
        splits$splits,
        unname(split(
          dplyr::select(splits, id), rlang::seq2(1L, nrow(splits))
        )),
        function(split, id) {
          split$id <- id
          split
        }
      )
    class(splits) <- c("rset", class(splits))

    attr(x = splits, "v") <- times
    attr(x = splits, "repeats") <- rep

    if (is.null(strata)) {
      attr(x = splits, "strata") <- FALSE
    } else {
      attr(x = splits, "strata") <- TRUE
    }
    splits
  }


# folds_vec -------------------------------------------------------------------------

#' @rdname  custom_resamples
#' @keywords internal

folds_vec <- function(x, times = ncase(length(x))) {
  folds <- cut(seq_along(x), breaks = times, labels = FALSE)
  x <- sample(x)
  folds <-
    map(
      .x = 1:times,
      .f = ~ which(folds == .x, arr.ind = TRUE)
    )
  folds <- map(.x = folds, .f = ~ x[-.x])
  folds
}

#' @rdname  custom_resamples
#' @keywords internal

invfolds_vec <- function(x, times = ncase(length(x))) {
  folds <- cut(seq_along(x), breaks = times, labels = FALSE)
  x <- sample(x)
  folds <-
    map(
      .x = 1:times,
      .f = ~ which(folds == .x, arr.ind = TRUE)
    )
  folds <- map(.x = folds, .f = ~ x[.x])
  folds
}

#' @rdname  custom_resamples
#' @export

kfolds_vec <-
  function(x,
           times = ncase(length(x)),
           rep = nsplits(length(x))) {
    kfolds <- c()

    for (i in seq_len(rep)) {
      kfolds <- c(kfolds, folds_vec(x, times))
    }
    kfolds
  }

#' @rdname  custom_resamples
#' @export

invkfolds_vec <-
  function(x,
           times = ncase(length(x)),
           rep = nsplits(length(x))) {
    kfolds <- c()

    for (i in seq_len(rep)) {
      kfolds <- c(kfolds, invfolds_vec(x, times))
    }
    kfolds
  }


# sliding ----------------------------------------------------------------------

#' @rdname  custom_resamples
#' @export

sliding_resamples <-
  function(dataset, times = 10) {
    if (!is.numeric(times) || length(times) != 1) {
      cat_stop("`v` must be a single integer.")
    }

    n <- nrow(dataset)
    folds <- rep_each_n(n = n, times = times)

    folds <- rep_each_n(n = n, times = times + 2)
    indices <-
      lapply(1:times + 1, function(i) {
        seq_len(n)[folds >= i - 1 & folds <= i + 1]
      })

    default_complement <- function(ind, n) {
      list(
        analysis = setdiff(1:n, ind),
        assessment = unique(ind)
      )
    }

    indices <- lapply(indices, default_complement, n = n)


    split_objs <-
      map(indices,
          rsample::make_splits,
          data = dataset,
          class = "sliding_split"
      )

    splits <-
      tibble(
        splits = split_objs,
        id = names0(length(split_objs), "Slice")
      )


    splits$splits <-
      map2(
        splits$splits,
        unname(split(
          dplyr::select(splits, id), rlang::seq2(1L, nrow(splits))
        )),
        function(split, id) {
          split$id <- id
          split
        }
      )
    class(splits) <- c("rset", class(splits))
    splits
  }




# boots ------------------------------------------------------------------------

#' @rdname  custom_resamples
#' @export

bootsplits <-
  function(dataset,
           times = 10,
           outcome = NULL,
           prop = 1) {
    bcall <- match.call()

    bcall[[1]] <- quote(bootsplits_impl)

    bcall$dataset <- dataset

    bcall$times <- times

    bcall$prop <- prop

    bcall$strata <- bcall$outcome

    bcall$outcome <- NULL


    bootsplits_impl <-
      function(dataset,
               times,
               strata = NULL,
               prop = 1) {
        if (!missing(strata)) {
          strata <-
            tidyselect::vars_select(names(dataset), !!rlang::enquo(strata))
        } else {
          strata <- NULL
        }

        n <- nrow(dataset)

        if (is.null(strata)) {
          indices <- map(rep(n, times), sample, replace = TRUE)
        } else {
          stratas <- tibble(
            idx = 1:n,
            strata = rsample::make_strata(
              getElement(dataset, strata),
              breaks = floor(n / max(10, times)),
              depth = 10,
              pool = .1
            )
          )

          stratas <- unname(split(stratas, stratas$strata))

          stratas <-
            purrr::map_dfr(stratas, function(x, prop, times) {
              n <- nrow(x)

              idx <-
                map(rep(n, times), sample, replace = T)

              out <-
                purrr::map_df(idx, function(ind, x) {
                  x[sort(ind), "idx"]
                }, x = x)
              out$rs_id <-
                rep(1:times, each = floor(n * prop))
              out
            }, times = times, prop = prop)

          indices <- unname(split(stratas$idx, stratas$rs_id))
        }

        boot_complement <- function(ind, n, times) {
          out <- unique(sample(ind, floor(length(ind) / times)))

          list(analysis = ind[!ind %in% out], assessment = out)
        }

        indices <-
          lapply(indices, boot_complement, n = n, times = times)
        split_objs <-
          map(indices,
              rsample::make_splits,
              data = dataset,
              class = "boot_split"
          )
        list(
          splits = split_objs,
          id = names0(length(split_objs), "Boots")
        )
      }

    split_objs <- eval(bcall)
    rsample::manual_rset(splits = split_objs$splits, ids = split_objs$id)
  }


# random -----------------------------------------------------------------------

#' @rdname  custom_resamples
#' @export

random_rset <-
  function(dataset,
           times = 10,
           rep = 3,
           outcome = NULL,
           prop = 1) {
    if (is_empty(match.call()$outcome)) {
      outcome <- NULL
    } else {
      if (!is_chr(match.call()$outcome)) {
        outcome <-
          do_try(
            substitute_q(outcome, parent.frame()),
            rlang::enexpr(outcome)
          )
      }
    }

    if (is_true(is.name(outcome) & !is_chr(outcome))) {
      outcome <- rlang::as_label(outcome)
    }

    fn_samp <- c("folds", "manual", "sld", "boots")
    fn_samp <- sample(fn_samp, rep, TRUE)

    res <- list()

    for (i in seq_len(rep)) {
      fns <- fn_samp[i]

      res[[i]] <- switch(fns,
                         folds = rlang::expr(
                           vfolds(
                             dataset = !!dataset,
                             times = !!times,
                             outcome = !!outcome
                           )
                         ),
                         manual = rlang::expr(
                           manual_resamples(
                             dataset = !!dataset,
                             times = !!times,
                             outcome = !!outcome
                           )
                         ),
                         sld = rlang::expr(sliding_resamples(
                           dataset = !!dataset, times = !!times
                         )),
                         boots = rlang::expr(
                           bootsplits(
                             dataset = !!dataset,
                             times = !!times,
                             outcome = !!outcome,
                             prop = !!prop
                           )
                         )
      )
    }
    purrr::map_dfr(res, ~ eval(.x), .id = "id2") %>%
      mutate(id2 = as_num(id2)) %>%
      dplyr::relocate(id2, .after = id)
  }



# split_resamples -----------------------------------------------------------------------------


#' @rdname  custom_resamples
#' @export
split_resamples <- function(dataset, times = 10, split_col) {
  #args_fn <- rlang::call_args(match.call())
  split_col <- do_try(force(split_col), rlang::enexpr(split_col))
  if (is_false(is.character(split_col))) {
    split_col <- deparse(split_col)
  }

  data_split <-
    data.frame(r = dataset[[split_col]], id = seq_len(nrow(dataset)))

  if (is_dblint(data_split$r)) {
    nn <- min(c(nsplits(unique(data_split$r)), 6))
    if (nn > 3) {
      data_split$r <-
        breaktize_vec(
          var = data_split$r,
          num_breaks = nn,
          algorithm = "quantile"
        )
    }
  }


  data_split$r <- factor_clean(as_int(as_fct(data_split$r)))


  ids_splits <-
    map(
      .x = split(data_split, data_split$r),
      .f = ~ kfolds_vec(
        x = .x$id,
        times = times,
        rep = 1
      )
    )

  ids_splits <- purrr::flatten(.x = purrr::imap(
    .x = ids_splits,
    .f = ~ purrr::set_names(
      x = .x,
      nm = paste(.y, seq_along(.x), sep = "_")
    )
  ))
  pos_id <- seq_row(dataset)

  ids_splits <-
    map(
      .x = ids_splits,
      .f = ~ list(analysis = .x, assessment = pos_id[!pos_id %in% .x])
    )

  ids_splits <-
    purrr::map_dfr(
      .x = ids_splits,
      .f = ~ tibble(splits = list(
        rsample::make_splits(.x,
                             data = dataset,
                             class = "manual_splits"
        )
      )),
      .id = "id"
    )

  ids_splits <-
    rsample::manual_rset(
      splits = ids_splits$splits,
      ids = paste0("Split", ids_splits$id)
    )

  ids_splits
}
