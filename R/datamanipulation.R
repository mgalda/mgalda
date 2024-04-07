# !diagnostics off
# !diagnostics suppress=y
#' @include util_env.R
NULL

#' herramientas
#'
#'
#' @name datamanipulation
#' @rdname datamanipulation
#' @keywords internal
#'
#' @examples
#'
#' library(ggplot2)
#' library(tidyverse)
#'
#'
#' set.seed(1)
#' dat <- tibble(x = sort(runif(30)), y = sort(runif(30)))
#' dsrb_max(dat$x[1:10], 0.1, 0.75)
#'
#' dat %>%
#'   mutate(d_x = dsrb_max(x, 0.1, 0.75))
#'
#' set.seed(2)
#' tibble(z = sort(runif(100))) %>%
#'   mutate(
#'     no_scale = dsrb_max(z, 0.1, 0.75),
#'     easier   = dsrb_max(z, 0.1, 0.75, scale = 1/2)
#'   ) %>%
#'   ggplot(aes(x = z)) +
#'   geom_point(aes(y = no_scale)) +
#'   geom_line(aes(y = no_scale), alpha = .5) +
#'   geom_point(aes(y = easier), col = "blue") +
#'   geom_line(aes(y = easier), col = "blue", alpha = .5) +
#'   lims(x = 0:1, y = 0:1) +
#'   coord_fixed() +
#'   ylab("Desirability")
#'
#'
#' # Target example
#'
#' dat %>%
#'   mutate(
#'     triangle = dsrb_target(x, 0.1, 0.5, 0.9,
#'     scale_low = 2, scale_high = 1/2)
#'   ) %>%
#'   ggplot(aes(x = x, y = triangle)) +
#'   geom_point() +
#'   geom_line(alpha = .5) +
#'   lims(x = 0:1, y = 0:1) +
#'   coord_fixed() +
#'   ylab("Desirability")
#'
#'
#' # Box constraints
#'
#' dat %>%
#'   mutate(box = dsrb_box(x, 1/4, 3/4)) %>%
#'   ggplot(aes(x = x, y = box)) +
#'   geom_point() +
#'   geom_line(alpha = .5) +
#'   lims(x = 0:1, y = 0:1) +
#'   coord_fixed() +
#'   ylab("Desirability")
#'
#'
#' # Custom function
#'
#' v_x <- seq(0, 1, length.out = 20)
#' v_d <- 1 - exp(-10 * abs(v_x - .5))
#'
#' dat %>%
#'   mutate(v = dsrb_custom(x, v_x, v_d)) %>%
#'   ggplot(aes(x = x, y = v)) +
#'   geom_point() +
#'   geom_line(alpha = .5) +
#'   lims(x = 0:1, y = 0:1) +
#'   coord_fixed() +
#'   ylab("Desirability")
#'
#'
#' # Qualitative data
#'
#' set.seed(3)
#' groups <- sort(runif(10))
#' names(groups) <- letters[1:10]
#'
#' tibble(x = letters[1:7]) %>%
#'   mutate(d = dsrb_category(x, groups)) %>%
#'   ggplot(aes(x = x, y = d)) +
#'   geom_bar(stat = "identity") +
#'   lims(y = 0:1) +
#'   ylab("Desirability")
#'
#'
#' # Apply the same function to many columns at once (dplyr > 1.0)
#'
#' dat %>%
#'   mutate(across(c(everything()), ~ dsrb_min(., .2, .6), .names = "d_{col}"))
#'
#'
#' # Choose model tuning parameters that minimize the number of predictors used
#' # while maximizing the area under the ROC curve.
#'
#' datalearn$classification_results %>%
#'   mutate(
#'     d_feat = dsrb_min(num_features, 1, 200),
#'     d_roc  = dsrb_max(roc_auc, 0.5, 0.9),
#'     d_all  = dsrb_overall(across(starts_with("d_")))
#'   ) %>%
#'   arrange(desc(d_all))
#'
#' # Bias the ranking toward minimizing features by using a larger scale.
#'
#' datalearn$classification_results %>%
#'   mutate(
#'     d_feat = dsrb_min(num_features, 1, 200, scale = 3),
#'     d_roc  = dsrb_max(roc_auc, 0.5, 0.9),
#'     d_all  = dsrb_overall(across(starts_with("d_")))
#'   ) %>%
#'   arrange(desc(d_all))
#'
#'
NULL

### select help
#' @rdname datamanipulation
#' @export
where <- function(fn) {
  predicate <- as_fn(fn)
  function(x, ...) {
    out <- predicate(x, ...)
    if (!rlang::is_bool(out)) {
      cat_stop("`where()` debe ser usado cuando el resultado es `TRUE` or `FALSE`.")
    }
    out
  }
}
#' @rdname datamanipulation
#' @export
wherenot <- function(...) {
  neg <- function(.p) {
    purrr::compose(`!`, purrr::as_mapper(.p))
  }
  neg(where(...))
}
#' @rdname datamanipulation
#' @export
all_of <- function(x) {
  if (is.function(x)) {
    vctrs::vec_as_location(x, 0L)
    cat_stop("`all_of()` should have failed sooner")
  }
  x
}


#### dplyr compl

#' @rdname datamanipulation
#' @export
group_summarise <- function(.data, .groups, ...) {
  call <- match.call(expand.dots = T)
  call[[1]] <- quote(dplyr::with_groups)
  call$.f <- quote(summarise)
  eval(call, env_call())
}
#' @rdname datamanipulation
#' @export
group_summarise_all <- function(.data, .groups, ...) {
  call <- match.call(expand.dots = T)
  call[[1]] <- quote(dplyr::with_groups)
  call$.f <- quote(summarise_all)
  eval(call, env_call())
}
#' @rdname datamanipulation
#' @export
group_summarise_at <-  function(.data, .groups,.vars,...) {
  call <-match.call(expand.dots = T)
  call[[1]] <-quote(dplyr::with_groups)
  call$.f<- quote(summarise_at)
  eval(call,env_call())
}
#' @rdname datamanipulation
#' @export
group_mutate <- function(.data, .groups, ...) {
  call <- match.call(expand.dots = T)
  call[[1]] <- quote(dplyr::with_groups)
  call$.f <- quote(mutate)
  eval(call, env_call())
}
#' @rdname datamanipulation
#' @export
group_mutate_all <- function(.data, .groups, ...) {
  call <- match.call(expand.dots = T)
  call[[1]] <- quote(dplyr::with_groups)
  call$.f <- quote(mutate_all)
  eval(call, env_call())
}
#' @rdname datamanipulation
#' @export
group_filter <- function(.data, .groups, ...) {
  call <- match.call(expand.dots = T)
  call[[1]] <- quote(dplyr::with_groups)
  call$.f <- quote(filter)
  eval(call, env_call())
}
#' @rdname datamanipulation
#' @export
group_indx <- function(.data, ...) {
  .grp_indx <-
    dplyr::group_indices(.data = group_by(.data = .data, ...))

  .data %>% mutate(.grp_indx = .grp_indx)
}

#' @rdname datamanipulation
#' @export
pull_summarise <- function(.data, .groups, ...) {
  call <-match.call(expand.dots = T)
  call[[1]] <-quote(dplyr::with_groups)
  call$.f<- quote(summarise)
  .data <-eval(call,env_call())
  nm_var <- map_chr(call$.groups,lang2str)
  nm_var <- nm_var[nm_var %in% names(.data)]
  .data <-unite_str(.data = .data,col = "'.groups'",nm_var)
  pull_str(.data = .data,var = names(.data)[2],name = ".groups")
}
#' @rdname datamanipulation
#' @export
pull_mutate <- function(.data, .name_col, ...) {
  call <- match.call(expand.dots = T)
  call[[1]] <- quote(dplyr::mutate)
  .data <- eval(call, env_call())
  nm_var <- lang2str(call$.name_col)
  nm_var <- nm_var[nm_var %in% names(.data)]
  .data <- unite_str(.data = .data, col = "'.groups'", nm_var)
  pull_str(
    .data = .data,
    var = names(.data)[2],
    name = ".groups"
  )
}
#' @rdname datamanipulation
#' @export
add_denserank <- function(.data, ..., .groups = NULL) {
  if (!is_empty(enexprs(.groups))) {
    call <- match.call(expand.dots = F)
    call$... <- NULL
    call[[1]] <- quote(dplyr::group_by)
    names(call)[names(call) == ".groups"] <- ""
    .data <- eval(call, env_call())
  }
  .data <- nest(.data = .data, data = !c(...))
  .data <- mutate(arrange(.data, ...), .rank = row_number())
  .data
  unnest(ungroup(.data), cols = c(data))
}
#' @rdname datamanipulation
#' @export
filter_invalid <- function(.data, ..., exclude = TRUE) {
  .data_f <- select(.data,...)

  if (exclude) {
    bool <-!complete.cases(.data_f)
  } else {
    bool <-complete.cases(.data_f)
  }
  .data[bool,]
}
#' @rdname datamanipulation
#' @export
filter_freq <- function(.data, .count, .groups = NULL) {
  .count <- substitute(.count)
  .count <- lang2str(.count)
  count_call <- call <- match.call(expand.dots = T)
  count_call[[1]] <- quote(dplyr::count)
  count_call$.groups <- NULL
  names(count_call)[names(count_call) == ".count"] <- ""
  names(count_call)[names(count_call) == ".data"] <- "x"
  .count_data <- eval(count_call, env_call())
  call[[1]] <- quote(dplyr::with_groups)
  call$.f <- quote(filter)
  call[[".count"]] <- quote(n == max(n))
  names(call)[names(call) == ".count"] <- ""
  call$.data <- inner_join(.data, .count_data, by = .count)

  eval(call, env_call()) %>% dplyr::select(!n)
}
#' @rdname datamanipulation
#' @export
rowindex <- function(.data, random_name = F) {
  if (!is.data.frame(.data)) {
    cat_stop("`x` should be a data frame.")
  }
  if (nrow(.data) > 0) {
    if (random_name) {
      candidates <- c(letters, LETTERS, paste(0:9))
      rname <-
        paste0("r", sample(candidates, 5, replace = TRUE), collapse = "")
      .data <- .data %>% mutate(!!rname := 1:dplyr::n())
    } else {
      .data <- .data %>% mutate(row = 1:dplyr::n())
    }
  }
  .data
}
#' @rdname datamanipulation
#' @export
rename_prefix <- function(.data, .prefix, .cols = everything()) {
  cols <- tidyselect::eval_select(enquo(.cols), .data)

  names <- names(.data)
  names[cols] <- paste(.prefix, names[cols], sep = "")
  names <- vctrs::vec_as_names(names, repair = "check_unique")

  set_names(.data, names)
}
#' @rdname datamanipulation
#' @export
by_row <- function(...) {
  .data <- dplyr::cur_data_all()

  .expr <- dots(...)[[1]]

  args<-as_l(call_args(.expr))

  data_list <- as_l(.data)

  fun <- make_function(
    args = map(args,~quote(expr = )),
    body = .expr,
    env = env_call()
  )
  simplify(pmap(data_list[names(args)],fun))

}

#' @rdname datamanipulation
#' @export
select_pctloss <- function(x,
                           ...,
                           metric,
                           maximize = TRUE,
                           limit = 2) {
  metric <- enexpr(metric)
  if (is.character(metric)) {
    metric <- str2lang(metric)
  }
  dots_arr <- match.call(expand.dots = F)$...
  dots <- map(dots_arr, ~do_try(call_args(.x),.x))
  if (length(dots) == 0) {
    cat_stop("Please choose at least one parameter to sort in `...`.")
  }
  res <-
    select_str(x, c(lang2str(metric), map_chr(dots, lang2str))) %>%
    drop_na()
  if (nrow(res) == 0) {
    cat_stop("No results are available. Please check the value of `metric`.")
  }
  if (maximize) {
    best_metric <- max(res[[lang2str(metric)]], na.rm = TRUE)
    res <-
      res %>% dplyr::mutate(
        .best = best_metric,
        .loss = (best_metric - !!metric) / best_metric * 100
      )
  } else {
    best_metric <- min(res[[lang2str(metric)]], na.rm = TRUE)
    res <-
      res %>% dplyr::mutate(
        .best = best_metric,
        .loss = (!!metric - best_metric) / best_metric * 100
      )
  }

  res <- do_try(dplyr::arrange(res, !!!dots_arr))
  best_index <- which(res$.loss == 0)
  res %>%
    as_tbl() %>%
    dplyr::slice(1:best_index) %>%
    dplyr::filter(.loss <
                    limit) %>%
    dplyr::arrange(desc(.loss)) %>%
    dplyr::slice(1)

}


#' @rdname datamanipulation
#' @export
filter_str = function(.data, ...) {
  eval_string_dplyr(.data,"filter", ...)
}
#' @rdname datamanipulation
#' @export
select_str = function(.data, ...) {
  eval_string_dplyr(.data,"select", ...)
}
#' @rdname datamanipulation
#' @export
arrange_str = function(.data, ...) {
  eval_string_dplyr(.data,"arrange", ...)
}
#' @rdname datamanipulation
#' @export
mutate_str = function(.data, ...) {
  eval_string_dplyr(.data,"mutate", ...)
}
#' @rdname datamanipulation
#' @export
summarise_str = function(.data, ...) {
  eval_string_dplyr(.data,"summarise", ...)
}
#' @rdname datamanipulation
#' @export
group_by_str = function(.data, ...) {
  eval_string_dplyr(.data,"group_by", ...)
}
#' @rdname datamanipulation
#' @export
unite_str <-
  function(.data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE) {
    .args <- match.call(expand.dots = F)
    .args <- flatten(as_l(.args[c("col", "...")]))
    eval_string_tbl(.data = .data, .data_name = "data", .fun.name = "tidyr::unite", .args)
  }
pull_str <- function(.data, ...) {
  eval_string_dplyr(.data,"pull", ...)
}

eval_string_dplyr <- function(.data, .fun.name, ...) {
  args <- list(...)
  args <- unlist(args)
  code <- paste0(.fun.name, "(.data,", paste0(args, collapse = ","), ")")
  eval(parse(text = code, srcfile = NULL))

}
eval_string_tbl <- function(.data, .data_name = "x", .fun.name, .args) {
  str_args <- function(.x, .y) {
    if (.y == "") {
      paste0(unlist(.x), collapse = ", ")
    } else if (.y == "...") {
      paste0(unlist(.x), collapse = ", ")
    } else {
      paste0(.y, " = ", .x)
    }
  }
  .args <- purrr::imap_chr(.x = .args, .f = str_args)
  code <- paste0(.fun.name, "(", .data_name, " = .data,", paste0(.args, collapse = ","), ")")
  eval(parse(text = code, srcfile = NULL))
}





#' @rdname datamanipulation
#' @export
dsrb_max <- function(x, low, high, scale = 1, missing = NA_real_, use_data = FALSE) {
  .comp_max <- function(x, low, high, scale, missing) {
    assert_unit_range(missing,xmin = 0,xmax = 1,severity = "stop")
    assert_dblint(x,severity = "stop")
    assert_value_order(low = low,high =  high)

    out <- rep(missing, length(x))
    out[x < low & !is.na(x)] <- 0
    out[x > high & !is.na(x)] <- 1
    middle <- x <= high & x >= low & !is.na(x)
    out[middle] <- ((x[middle] - low)/ (high - low))^scale
    out
  }
  low <- dsrb_assign_agrs(low, x, use_data, fn = "dsrb_max")
  high <- dsrb_assign_agrs(high, x, use_data, fn = "dsrb_max", type = "high")

  .comp_max(x, low, high, scale, missing)
}
#' @rdname datamanipulation
#' @export
dsrb_min <- function(x, low, high, scale = 1, missing = NA_real_, use_data = FALSE) {
  .comp_min <- function(x, low, high, scale, missing) {
    assert_unit_range(missing,xmin = 0,xmax = 1,severity = "stop")
    assert_dblint(x,severity = "stop")
    assert_value_order(low = low,high =  high)

    out <- rep(missing, length(x))
    out[x < low & !is.na(x)] <- 1
    out[x > high & !is.na(x)] <- 0
    middle <- x <= high & x >= low & !is.na(x)
    out[middle] <- ((x[middle] - high)/ (low - high))^scale
    out
  }
  low <- dsrb_assign_agrs(low, x, use_data, fn = "dsrb_min")
  high <- dsrb_assign_agrs(high, x, use_data, fn = "dsrb_min", type = "high")
  .comp_min(x, low, high, scale, missing)
}
#' @rdname datamanipulation
#' @export
dsrb_target <- function(x, low, target, high, scale_low = 1, scale_high = 1,
                        missing = NA_real_, use_data = FALSE) {
  .comp_target <- function(x, low, target, high, scale_low, scale_high, missing) {
    assert_unit_range(missing,xmin = 0,xmax = 1,severity = "stop")
    assert_dblint(x,severity = "stop")
    assert_value_order(low, high, target)

    out <- rep(missing, length(x))

    out[(x < low | x > high) &  !is.na(x)] <- 0
    lower <- x <= target & x >= low & !is.na(x)
    out[lower] <- ((x[lower] - low) / (target - low))^scale_low
    upper <- x <= high & x >= target & !is.na(x)
    out[upper] <- ((x[upper] - high) / (target - high))^scale_high

    out
  }
  low <- dsrb_assign_agrs(low, x, use_data, fn = "dsrb_target")
  high <- dsrb_assign_agrs(high, x, use_data, fn = "dsrb_target", type = "high")
  target <- dsrb_assign_agrs(target, x, use_data, fn = "dsrb_target", type = "target")

  .comp_target(x, low, target, high, scale_low, scale_high, missing)
}

#' @rdname datamanipulation
#' @export
dsrb_box <- function(x, low, high, missing = NA_real_, use_data = FALSE) {
  .comp_box <- function(x, low, high, missing) {
    assert_dblint(x,severity = "stop")
    assert_unit_range(missing,xmin = 0,xmax = 1,severity = "stop")
    assert_value_order(low = low,high =  high)

    out <- rep(missing, length(x))
    out[x < low | x > high & !is.na(x)] <- 0
    out[x >= low & x <= high & !is.na(x)] <- 1

    out
  }
  low <- dsrb_assign_agrs(low, x, use_data, fn = "dsrb_box")
  high <- dsrb_assign_agrs(high, x, use_data, fn = "dsrb_box", type = "high")
  .comp_box(x, low, high, missing)

}
#' @rdname datamanipulation
#' @export
dsrb_custom <- function(x, x_vals, desirability, missing = NA_real_) {
  .comp_custom <- function(x, values, d, missing) {
    assert_unit_range(missing,xmin = 0,xmax = 1,severity = "stop")
    assert_unit_range(d)
    assert_dblint(x,severity = "stop")
    assert_vectors_nums(values, d)

    ord <- order(values)
    values <- values[ord]
    d <- d[ord]

    out <- rep(missing, length(x))
    out[x < min(values) & !is.na(x)] <- 0
    out[x > max(values) & !is.na(x)] <- 1

    middle <- x <= max(values) & x >= min(values) & !is.na(x)
    x_mid <- x[middle]
    out[middle] <- stats::approx(values, d, xout = x_mid)$y

    out
  }
  .comp_custom(x, x_vals, desirability, missing)
}
#' @rdname datamanipulation
#' @export
dsrb_category <- function(x, categories, missing = NA_real_) {
  .comp_category <- function(x, values, missing) {
    assert_fctchr(x)
    assert_unit_range(missing,xmin = 0,xmax = 1,severity = "stop")
    assert_unit_range(values)

    # make consistent factors when needed, check names, better missing handling

    values <- tibble::tibble(value = names(values), d = unname(values))
    dat <- tibble::tibble(value = x, order = seq_along(x))
    out <- dplyr::left_join(dat, values, by = "value")

    out$d[out$order]
  }

  .comp_category(x, categories, missing)
}
#' @rdname datamanipulation
#' @export
dsrb_overall <- function(..., geometric = TRUE, tolerance = 0) {
  check_dsrb_inputs <- function(x) {
    assert_across_df(
      .data = x,
      everything(),
      predicate = is_dblint(x),
      severity = "stop"
    )
    assert_across_df(
      .data = x,
      everything(),
      predicate = is_inrange(x, xmin = 0, xmax = 1),
      severity = "stop"
    )

    size <- map_int(x, length)

    assert_engine(
      length(unique(size)) == 1,
      env = env_curr(),
      msg = "All desirability inputs should have the same length.",
      severity = "stop"
    )
    invisible(TRUE)
  }
  maybe_name <- function(x) {
    # The selector can return vectors (unnamed) and data frames.
    # Binding unnamed things generates a warning so add names here when needed.
    is_tbl <- map_lgl(x, is.data.frame)
    if (all(is_tbl)) {
      return(x)
    }
    if (any(!is_tbl)) {
      if (any(is_tbl)) {
        df_x <- x[is_tbl]
      }
      x <- x[!is_tbl]
      names(x) <- paste0("dsrb_", which(!is_tbl))
      if (any(is_tbl)) {
        x <- c(x, df_x)
      }
    }
    x
  }

  dsrb_lst <- list(...)
  dsrb_lst <- maybe_name(dsrb_lst)
  vals <- dplyr::bind_cols(dsrb_lst)
  check_dsrb_inputs(vals)
  if (ncol(vals) == 1) {
    return(vals[[1]])
  }
  vals <- as.matrix(vals)
  if (tolerance > 0) {
    vals[vals < tolerance] <- tolerance
  }
  if (geometric) {
    res <- apply(vals, 1, geo_mean)
  } else {
    res <- apply(vals, 1, mean, na.rm = TRUE)
  }
  res
}

dsrb_assign_agrs <- function(arg, x, use_data, fn, type = "low") {
  if (is_empty(arg)) {
    if (use_data) {
      type <- rlang::arg_match0(type, c("low", "high", "target"))
      .fn <- switch(type,
                    low = min,
                    high = max,
                    target = stats::median
      )
      arg <- .fn(x, na.rm = TRUE)
    } else {
      cat_stop("In `{fn}()`, argument '{type}' is required when 'new_data = FALSE'.")
    }
  }
  arg
}

### random data








#' @export
xrandom <- function(n = 100,
                    mean = 0,
                    sd = 1,
                    discrete = FALSE) {
  x <-
    list(
      runif(n, 0, 1),
      rcauchy(n, 1, 1),
      rnorm(n, 0, 1),
      rpois(n, 1),
      rchisq(n, 1)
    )

  x <- map(x, safe_vec)
  x <- .un(map(x, center_scale))

  x <- sample(x * sd + mean, n, replace = T)
  if (discrete) {
    x <- round(x)
  }

  x
}

#' @export
xrandom_cat <- function(n = 100,
                        lev = 3,
                        tab = F,
                        factor = T) {
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
  if (factor) {
    x <- factor(x)
  }
  x
}
#' @export
prandom <- function(n) {
  rlog2unif <- function (n, min = 0, max = 1) {
    min_sign <- sign(min)
    max_sign <- sign(max)
    r_d <- max - min
    r_d_log <- log2(r_d)
    x_l <- runif(n, min = 1+.Machine$double.eps, max = r_d_log + 1+.Machine$double.eps)

    (2^(x_l-1)) *sample(x = c(min_sign, max_sign), n, T)
  }

  over_ratio <- runif(1)

  xlog<-rlog2unif(n = n,min = 1,max = 10)

  xlin <- runif(n = n, min = 1, max = 10)

  x <- xlog*over_ratio + xlin*(1-over_ratio)

  x / sum(x)
}


#' @export
random_range <- function(lower = 10, upper = 200) {
  if (lower > upper) {
    tmp <- lower
    upper <- lower
    lower <- tmp
  }

  xcut <- diff(c(lower, upper)) * runif(1, .1, .9)

  c(runif(1, lower, lower + xcut), runif(1, lower + xcut, upper))
}


#' @export
simulate_num <- function(n,
                         xmean = 1,
                         xsd = abs(xmean) * .5) {
  simulate_num_impl <- function(n,
                                xmean = 1,
                                xsd = abs(xmean) * .5) {
    c_probs <- function(n) {
      p <- runif(n, 1, sample(1:20, 1))
      p / sum(p)
    }
    p <- sample(0:2, n, replace = T, prob = c_probs(3))

    truth <-
      list(
        runif = runif(n = n, min = 0, max = 1),
        rnorm = rnorm(n = n, mean = 1, sd = 1),
        rchisq = rchisq(n, sample(0:2, 1), sample(2 * 0:3, 1))
      )

    truth <- lapply(truth, function(x) {
      x <- (x - mean(x)) / sd(x)
      x * xsd + xmean + xsd
    })

    truth <-
      sapply(seq_along(p), function(x) {
        truth[[p[x] + 1]][x]
      })

    s <- sample(1:3, n, replace = T, prob = c_probs(3))
    estimate <- truth

    estimate[s == 1] <-
      estimate[s == 1] + rnorm(sum(s == 1), xsd * runif(1, .2, 1), xsd * runif(1, 0, 1))

    estimate[s == 2] <-
      estimate[s == 2] * runif(sum(s == 2), min = .4, max = 1.6)

    estimate[s == 3] <-
      estimate[s == 3] + runif(sum(s == 3), min = .8, max = 1.2) * xsd

    g_probs <- function() {
      p <-
        c(
          runif(1, 1, sample(4:15, 1)),
          runif(1, 1, sample(4:15, 1)),
          runif(1, 80, sample(80:100, 1))
        )
      p / sum(p)
    }

    s <- sample(0:2, n, replace = T, prob = g_probs())
    estimate[s == 0] <-
      sample(x = estimate[s == 0], size = length(estimate[s == 0]))
    estimate[s == 1] <-
      runif(
        n = length(estimate[s == 1]),
        min = min(estimate),
        max = max(estimate)
      )

    estimate <- safe_vec(x = estimate,ops = "random")
    truth <- safe_vec(x = truth,ops = "random")
    list(
      truth = truth,
      estimate = estimate
    )
  }

  suppressWarnings(simulate_num_impl(n, xmean, xsd))
}
#' @export
simulate_probclass <-
  function(n = 100,
           n_lvs = 2,
           random = trunc(.25 * n)) {
    truth <- xrandom_cat(n, n_lvs)
    lvs <- unique(truth)
    lvs <- lvs[order(lvs)]
    truth <- factor(truth, lvs)
    pred <- truth

    s <- sample(seq_along(pred), random)

    if (n_lvs > 1) {
      for (i in s) {
        ls <- pred[i]
        pred[i] <-
          sample(unique(pred)[unique(pred) != ls], 1, replace = T)
      }

      if (sample(1:10, 1) == 1) {
        lt <- sample(lvs, 1)
        nlt <- length(pred[pred == lt])
        pred[pred == lt] <-
          sample(unique(truth)[unique(truth) != lt], nlt, replace = T)
      }
    }

    estimate <-
      purrr::map_dfr(
        pred,
        .f = function(x) {
          mat <- matrix(rep(0, length(lvs)), nrow = 1)
          nmax <- which(lvs == x)
          mat[, nmax] <- runif(1, 50, 100)
          mat[, -nmax] <- runif(length(lvs) - 1, 10, 40)
          colnames(mat) <- lvs
          as_df(mat / sum(mat[1, ]))
        }
      )
    x <- table(truth, pred)

    list(
      truth = tibble(truth = truth),
      estimate = tibble(estimate = pred),
      prob = estimate,
      x = x
    )
  }
#' @export
xrandomdata <- function(nrows = 400,
                        numcols = 3,
                        factcols = 3,
                        na = FALSE,
                        output = c("cat", "num")) {
  randna <- function(x, na) {
    runrand <- function() {
      sample(1:5, 1) == sample(1:5, 1)
    }
    if (na & runrand()) {
      nn <- sample(seq_len(length(x) * 0.1), 1)
      idx <- sample(seq_along(x), nn)
      x[idx] <- NA
    }
    x
  }
  outnum <- function(datacols) {
    datadmy <- do_getdummy(datacols, where(is_fctchr))
    nm_dmy <- suppressall(do.call(rbind, strsplit(
      x = names(datadmy),
      split = "_",
      fixed = T
    )))
    nm_dmy <- split(x = nm_dmy[, 2], f = nm_dmy[, 1])
    nm_dmy[lengths(nm_dmy) > 1] <-
      purrr::imap(.x = nm_dmy[lengths(nm_dmy) >
                                1], ~ paste(.y, .x, sep = "_"))
    per_dmy <- as_l(prandom(n = length(nm_dmy)))
    per_dmy <- unlist(lapply(seq_along(nm_dmy), function(n) {
      nm <- nm_dmy[[n]]
      x <- length(nm_dmy[[n]])
      y <- per_dmy[[n]]
      if (x == 1) {
        setNames(y, nm)
      } else {
        setNames(prandom(n = x) * y, nm)
      }
    }))
    rng <- range(sample(c(-1, 1), 2) * (10^runif(
      2, 0.001,
      3
    )))
    datadmy <- do.call(cbind, lapply(datadmy, function(x) {
      roll_noise(rescalar(x))
    }))
    datacols$output <- rescalar(rowSums(datadmy * per_dmy),
                                to = rng
    )
    datacols
  }
  outcat <- function(datacols) {
    datadmy <- do_getdummy(datacols, where(is_fctchr))
    kclus <-
      suppressall(kmeans(x = datadmy, centers = sample(
        2:6,
        1
      )))
    datacols$output <-
      factor_clean(x = kclus$cluster, prefix = "cat")
    datacols
  }
  if (numcols > 0) {
    num_mean <- rlogunif(
      n = numcols,
      min = -1000,
      max = 1000
    )
    num_sd <- runif(n = numcols, min = .1, max = 3)
    num_sd <- abs(num_sd * num_mean)
    numcols <-
      set_names(seq_len(numcols), paste("num", seq_len(numcols), sep = "_"))
    num_rand <-
      map_dfc(
        .x = numcols,
        ~ xrandom(
          n = nrows,
          mean = num_mean[.x],
          sd = num_sd[.x],
          discrete = sample(c(T, F), numcols, replace = T)
        )
      )
  } else {
    num_rand <- tibble()
  }

  if (factcols > 0) {
    factcols <-
      set_names(seq_len(factcols), paste("cat", seq_len(factcols), sep = "_"))
    nlev <- 2:6

    cat_rand <-
      map_dfc(
        .x = factcols,
        .f = ~ xrandom_cat(n = nrows, lev = sample(nlev,1,FALSE,1/nlev))
      )
  } else {
    cat_rand <- tibble()
  }

  datacols <- bind_cols(num_rand, cat_rand)

  output <- match.arg(output)
  datacols <- as_tbl(datacols)
  if (output == "cat") {
    datacols <- outcat(datacols)
  } else if (output == "num") {
    datacols <- outnum(datacols)
  }
  if (na) {
    datacols[, -ncol(datacols)] <- purrr::map_dfc(datacols[
      ,
      -ncol(datacols)
    ], ~
      randna(x = .x, na = na))
  }
  datacols
}

### cum & maths
#' @export
cumweight <- function(x) {

  cumsum(x)/ sum(x)
}
#' @export
cumcount <- function(x) {
  x <- as.character(x)
  counts <- new.env(parent = emptyenv())
  setNames(vapply(
    x, function(w) {
      counts[[w]] <- 1L + mget(w, counts, ifnotfound = 0L)[[1]]
    },
    integer(1)
  ), x)
}
#' @export
rank_percent <- function(x) {
  min_rank <-
    function(x) {
      rank(x, ties.method = "min", na.last = "keep")
    }

  (min_rank(x) - 1) / (sum(!is.na(x)) - 1)
}
#' @export
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
#' @export
mcm <- function(...) {
  numeros <- c(...)

  if (prod(numeros) != 0) {
    resultado <- prod(numeros) / mcd(numeros)^(length(numeros) - 1)
  } else {
    resultado <- NaN
  }
  abs(resultado)
}
#' @export
make_iterator <- function(f, n = 1) {
  fun_call <- function(f, ...) {
    f(...)
  }
  function(x) {
    Reduce(fun_call, rep.int(list(f), n), x, right = TRUE)
  }
}
### inputs_series

#' inputs_series
#'
#' @rdname datamanipulation
#' @export
inputs_series <- function(x, ...) {
  UseMethod("inputs_series")
}
#' @export
inputs_series.default <- function(x, ...) {
  generic_default()
}
#' @export
inputs_series.numeric <- function(x, ...) {
  .metrics_summary <- list(
    mean = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          mean(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    median = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          median(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    length = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          length(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    sd = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          sd(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    var = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          var(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    IQR = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          IQR(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    qsr = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          qsr(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    entropy = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          entropy(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    gini = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          gini(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    geo_mean = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          geo_mean(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    skewness = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          skewness(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    kurtosis = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          kurtosis(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    min = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          min(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    max = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          max(x)
        },
        error = function(e) {
          NA
        }
      )
    },
    range = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          diff(range(x))
        },
        error = function(e) {
          NA
        }
      )
    },
    moment01 = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          moment(x, order = 1)
        },
        error = function(e) {
          NA
        }
      )
    },
    moment02 = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          moment(x, order = 2)
        },
        error = function(e) {
          NA
        }
      )
    },
    moment03 = function(x) {
      x <- x[!is_nonnum(x)]
      tryCatch(
        {
          moment(x, order = 3)
        },
        error = function(e) {
          NA
        }
      )
    }
  )

  .info <- map(.metrics_summary, ~ .x(x))

  attributes(x) <- c(attributes(x), .info)
  class(x) <- c("inputs_series", class(x))
  x
}
inputs_series.list <- function(x, ...) {
  .info <- table(vapply(x, function(x) class(x)[1], character(1)))
  attributes(x) <- c(attributes(x), .info)
  class(x) <- c("inputs_series", class(x))
  x
}
#' @export
print.inputs_series <- function(x) {
  if (any(class(x) %in% "numeric")) {
    x <- nullattr(nullclass(x))
    cat(paste(vctrs::vec_ptype_abbr(x), "vector\n"))
  } else if (any(class(x) %in% "list")) {
    x <-
      unlist(map(x, ~ vctrs::vec_ptype_abbr(nullattr(
        nullclass(.x)
      )[1])))
    cat_subtitle1("Lista de inputs con las siguientes categorias:\n")

    tx <- table(x)
    cli::cli_bullets(purrr::set_names(
      x = paste(names(tx), ": ", tx, "\n", sep = ""),
      nm = rep("*", length(tx))
    ))
  }
  invisible(x)
}

### bech

#' inputs_series
#'
#' @rdname datamanipulation
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' x <- runif(1000)
#' time_bnch(
#' min = f(min(x, na.rm = TRUE) > 0),
#' all_rm = f(all(safe_vec(x, ops = "rm") > 0)),
#' all_mean = f(all(safe_vec(x, ops = "mean") > 0)),
#' all_random =f(all(safe_vec(x, ops = "random") > 0)),x = x
#' )
#'
time_bnch <- function(...){
  UseMethod("time_bnch")
}
#' @export
time_bnch.default <- function(..., x, sample_size = runif(rep, .2, .8), rep = 10) {
  if (length(sample_size) == 1) {
    sample_size <- rep(sample_size, rep)
  } else if (length(sample_size) < rep) {
    sample_size <- sample(sample_size, rep, T)
  } else if (length(sample_size) > rep) {
    sample_size <- sample(sample_size, rep, F)
  }
  exprs <- dots_named(...)

  x <- map(
    .x = sample_size,
    .f = ~ sample_n(
      x = x,
      size = .x,
      replace = TRUE
    )
  )

  res <- list()
  for (j in names(exprs)) {
    .f <- exprs[[j]]
    .f <- eval(.f, envir = env_call())
    if (!is.function(.f)) {
      cat_stop("'se debe ingresar funciones para evaluar'")
    }
    if (!is.list(x)) {
      cat_stop("'sample' debe ser una lista")
    }
    n <- length(x)
    ftime <- numeric(n + 1)
    tmp <- list()
    ftime[1] <- microbenchmark::get_nanotime()
    for (i in 1:n) {
      tmp[[i]] <- do_try_ier(.f(x[[i]]))

      ftime[1 + i] <- microbenchmark::get_nanotime()
    }
    nmsym <- rlang::sym(j)
    ftime <- diff(ftime) / 1000000
    ftime[map_lgl(tmp, is_try)]
    res[[j]] <- tibble(
      id = 1:n,
      n = size_n(x),
      `:=`(!!nmsym, ftime)
    )
  }
  out <- purrr::reduce(res, inner_join, by = c("id", "n"))
  q <-
    out %>%
    dplyr::select(!id) %>%
    group_by(n) %>%
    summarise_all(mean,
                  na.rm = T
    ) %>%
    ungroup() %>%
    pivot_longer(!n,
                 names_to = "caso",
                 values_to = "tiempo"
    ) %>%
    mutate(caso = factor(caso))
  if (nrow(distinct(dplyr::select(q, !c(tiempo, n)))) == nrow(q)) {
    q <-
      grf_bar(tiempo~caso,data = q) +
      suppressall(ggplot2::ylim(0, max(q$tiempo)))
  } else {
    q <- grf_line(gformula = tiempo~n,data = q,fill = ~caso,colour = ~caso)
  }
  caso_lvls <- levels(q$data$caso)
  out <- list(summary = out)
  gplot <- q + gr_current_theme()
  l_plot <- list(tiempo = q)

  print(gplot)
  out$plot <- l_plot
  class(out) <- "time_bnch"
  out
}
#' @export
print.time_bnch <- function(x) {
  cat_title_head(text = "Benchmark")
  cat("\n")
  fn_names <- syms(nmdiff(x$summary, c("n", "id")))

  d_summ_n <- group_summarise_at(
    .data = x$summary,
    .groups = n,
    .vars = vars(!!!fn_names),
    ~ mean(.x, na.rm = TRUE)
  )
  d_summ <- t(apply(d_summ_n[, nmdiff(d_summ_n, "n")], 1, f(x / min(x, na.rm = TRUE))))
  cat_subtitle2("Relative Time")
  cat("\n")
  print(cbind(
    t(summarise_all(as_tbl(d_summ), min)),
    t(summarise_all(as_tbl(d_summ), mean)),
    t(summarise_all(as_tbl(d_summ), max))
  ) %colnm% c("min", "mean", "max"))
  cat("\n")
  nr <- nrow(d_summ_n)
  if (nr > 5) {
    ids <- sort(sort_filter_vec(seq_len(nr), 5))
    d_summ_n <- d_summ_n[ids, ]
    d_t <- paste(nrow(d_summ_n), " sample of ", nr, sep = "")
  } else {
    d_t <- ""
  }

  cat_subtitle2(text = "Absolute Time in seg.", sub_text = d_t)
  cat("\n")

  n_cols <- paste("n=", d_summ_n$n, sep = "")
  print(t(d_summ_n[, nmdiff(d_summ_n, "n")]) %colnm% n_cols)

  cat("\n")
  cat_endline()
}
#' @export
time_bnch2 <- function(..., x) {
  fn_smr <- function(x, y) {
    .fn_smr <- fn_each(
      .mean = mean,
      .median = median,
      .max = max,
      .min = min,
      .sd = sd
    )

    matrix(
      data = .fn_smr(x),
      nrow = 1,
      dimnames = list(
        c(y),
        names(.fn_smr)
      )
    )
  }
  exprs <- dots_named(...)
  res <- map(exprs, ~ list())
  out <- map(exprs, ~ list())
  n <- length(x)
  for (j in names(exprs)) {
    .f <- exprs[[j]]
    .f <- eval(.f, envir = env_call())
    if (!is.function(.f)) {
      # cat_stop("'se debe ingresar funciones para evaluar'")
      stop("'se debe ingresar funciones para evaluar'")
    }
    if (!is.list(x)) {
      # cat_stop("'sample' debe ser una lista")
      stop("'sample' debe ser una lista")
    }
    ftime <- numeric(n + 1)
    tmp <- list()
    ftime[1] <- microbenchmark::get_nanotime()
    for (i in seq_along(x)) {
      tmp[[i]] <- do.call(what = .f, args = as.list(x[[i]]))
      ftime[1 + i] <- microbenchmark::get_nanotime()
    }
    out[[j]] <- tmp
    res[[j]] <- diff(ftime) / 1000000
  }
  res_df <- as_df(do_call(rbind, imap(res, ~ fn_smr(.x, .y))))
  print(res_df)


  invisible(list(resultado = res_df, output = out))
}

# sample_n

#' @export
sample_n <- function(x, ...) {
  UseMethod("sample_n")
}
#' @export
sample_n.default <- function(x, ...) {
  generic_default()
}
#' @export
sample_n.data.frame <-
  function(x, size = runif(1, .1, .9), replace = FALSE, weight = NULL) {
    size <- eval(bquote_w(size))
    weight <- eval(bquote_w(weight))
    slice(x, local({
      size <- check_size(size, n = size_n(x), replace = replace)
      sample.int(n(), size, replace = replace, prob = weight)
    }))
  }
#' @export
sample_n.numeric <-
  function(x, size = runif(1, .1, .9), replace = FALSE, weight = NULL) {
    size <- eval(bquote_w(size))
    weight <- eval(bquote_w(weight))
    size <- check_size(size, n = size_n(x), replace = replace)
    sample(x = x, size = size, replace = replace, prob = weight)
  }
#' @export
sample_n.tbl_df <- sample_n.data.frame
#' @export
sample_n.tbl <- sample_n.data.frame
#' @export
sample_n.logical <-
  function(x, size = runif(1, .1, .9), replace = FALSE, weight = NULL) {
    size <- eval(bquote_w(size))
    weight <- eval(bquote_w(weight))
    size <- check_size(size, n = size_n(x), replace = replace)
    sample(x = x, size = size, replace = replace, prob = weight)
  }
#' @export
sample_n.factor <-
  function(x, size = runif(1, .1, .9), replace = FALSE, weight = NULL) {
    size <- eval(bquote_w(size))
    weight <- eval(bquote_w(weight))
    size <- check_size(size, n = size_n(x), replace = replace)
    sample(x = x, size = size, replace = replace, prob = weight)
  }
#' @export
sample_n.character <-
  function(x, size = runif(1, .1, .9), replace = FALSE, weight = NULL) {
    size <- eval(bquote_w(size))
    weight <- eval(bquote_w(weight))
    size <- check_size(size, n = size_n(x), replace = replace)
    sample(x = x, size = size, replace = replace, prob = weight)
  }
#' @export
sample_n.integer <-
  function(x, size = runif(1, .1, .9), replace = FALSE, weight = NULL) {
    size <- eval(bquote_w(size))
    weight <- eval(bquote_w(weight))
    size <- check_size(size, n = size_n(x), replace = replace)
    sample(x = x, size = size, replace = replace, prob = weight)
  }
#' @export
sample_n.list <-
  function(x, size = runif(1, .1, .9), replace = FALSE, weight = NULL) {
    size <- eval(bquote_w(size))
    weight <- eval(bquote_w(weight))
    size <- check_size(size, n = size_n(x,F), replace = replace)

    local({
      x[sample(seq_along(x),
               size,
               replace = replace,
               prob = weight
      )]
    })
  }
check_size <- function(size, n, replace = FALSE) {
  size <- eval(bquote_w(size), envir = env_call())
  if (is_prob(size)) {
    ret_invis(ceiling(size * n))
  } else if (size <= n || replace) {
    ret_invis(size)
  }
  bullets <-
    c(glue("`size` must be less than or equal to {n} (size of data)."),
      i = "set `replace = TRUE` to use sampling with replacement."
    )
  cat_stop(bullets)
}

#size_m

#' @export
size_n <- function(x,...) {
  UseMethod("size_n")
}
#' @export
size_n.default <- function(x) {
  generic_default()
}
#' @export
size_n.numeric <- function(x) {
  length(x)
}
#' @export
size_n.logical <- size_n.numeric
#' @export
size_n.factor <- size_n.numeric
#' @export
size_n.character <- size_n.numeric
#' @export
size_n.integer <- size_n.numeric
#' @export
size_n.data.frame <- function(x) {
  nrow(x)
}
#' @export
size_n.tbl_df <- size_n.data.frame
#' @export
size_n.tbl <- size_n.data.frame
#' @export
size_n.list <- function(x, by_element = TRUE) {
  if (by_element) {
    map_dbl(x, size_n)
  } else {
    length(x)
  }
}
