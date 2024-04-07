# !diagnostics off
# !diagnostics suppress=y


#' custom_step
#'
#'
#' @name custom_step
#' @rdname custom_step
#' @keywords internal
#'
#' @examples
#'
#' library(recipes)
#' library(mgalda)
#' library(tidyverse)
#'
#' data_num <<-
#'   xrandomdata(
#'     nrows = 500,
#'     numcols = 3,
#'     factcols = 2,
#'     output = "num"
#'   )
#' data_cat <-
#'   xrandomdata(
#'     nrows = 500,
#'     numcols = 3,
#'     factcols = 2,
#'     output = "cat"
#'   )
#' data_num_out <- mutate_if(data_num,is_dblint, create_outliers)
#'
#' rec_num <- recipe(output ~ ., data = data_num)
#' rec_cat <- recipe(output ~ ., data = data_cat)
#' rec_num_out <- recipe(output ~ ., data = data_num_out)
#'
#' plot_num_num <- function(new, old, perc = TRUE) {
#'   .s_num <- f(select(x, contains("num")))
#'   data <- bind_rows(
#'     new = .s_num(new) %>% rowindex(),
#'     old = .s_num(old) %>% rowindex(),
#'     .id = "group"
#'   )
#'
#'   if (is_true(perc)) {
#'     P <- data %>%
#'       tidyr::pivot_longer(contains("num"), names_to = "var") %>%
#'       mutate_if(is_fctchr, factor) %>%
#'       group_by(group, var) %>%
#'       arrange_all() %>%
#'       mutate(
#'         value = rescalar(value),
#'         percentil = rank_percent(x = value)
#'       ) %>%
#'       filter(percentil < .99 & percentil > .01) %>%
#'       mutate(value = rescalar(value)) %>%
#'       ungroup() %>%
#'       grf_line(value ~ percentil, colour = ~group) %>%
#'       grf_facet_wrap(var ~ .) +
#'       scale_pal_gr(scale = "discrete", aesthetics = "colour") +
#'       theme_greyscale(axis = "xy")
#'   } else if (is_false(perc)) {
#'     P <- data %>%
#'       tidyr::pivot_longer(contains("num")) %>%
#'       tidyr::pivot_wider(names_from = group, values_from = value) %>%
#'       grf_boxplot(old ~ factor(new), group = ~new) %>%
#'       grf_facet_wrap(name ~ ., scale = "free") +
#'       scale_pal_gr(scale = "discrete", aesthetics = "colour") +
#'       theme_greyscale(axis = "xy") +
#'       xlab("cortes") +
#'       ylab("valor original")
#'   } else {
#'     P <- data %>%
#'       tidyr::pivot_longer(contains("num"), names_to = "var") %>%
#'       tidyr::pivot_wider(names_from = group, values_from = value) %>%
#'       mutate_if(is_fctchr, factor) %>%
#'       grf_line(new ~ old, colour = ~var) %>%
#'       grf_facet_wrap(var ~ ., scales = "free") +
#'       scale_pal_gr(scale = "discrete", aesthetics = "colour") +
#'       theme_greyscale(axis = "xy")
#'   }
#'   P
#' }
#' plot_num_cat <- function(new, old) {
#'   inner_join(
#'     x = select(new, new = contains("cartd")) %>%
#'       rowindex(),
#'     y = select(old, contains("num")) %>%
#'       rowindex() %>%
#'       tidyr::pivot_longer(
#'         contains("num_"),
#'         values_to = "old",
#'         names_to = "var"
#'       ),
#'     by = "row"
#'   ) %>%
#'     mutate_if(is_fctchr, factor) %>%
#'     grf_boxplot(old ~ new, fill = ~new) %>%
#'     grf_facet_wrap(var ~ ., scales = "free_y") +
#'     scale_pal_gr(scale = "discrete", aesthetics = "fill") +
#'     theme_greyscale(axis = "xy")
#' }
#' plot_num_bin <- function(new, old) {
#'   .s_num <- function(x, nm) {
#'     select(x, contains("num")) %>%
#'       rowindex() %>%
#'       tidyr::pivot_longer(contains("num"),
#'                    names_to = "var",
#'                    values_to = nm
#'       )
#'   }
#'   inner_join(
#'     x = .s_num(new, "new"),
#'     y = .s_num(old, "old"),
#'     by = c("var", "row")
#'   ) %>%
#'     mutate_if(is_fctchr, factor) %>%
#'     grf_boxplot(old ~ factor(1), fill = ~new) %>%
#'     grf_facet_wrap(var ~ ., scales = "free_y") +
#'     scale_pal_gr(scale = "discrete", aesthetics = "fill") +
#'     theme_greyscale(axis = "xy")
#' }
#' plot_cat_cat <- function(new, old) {
#'   inner_join(
#'     x = select(new, contains("cat_")) %>%
#'       rowindex() %>%
#'       tidyr::pivot_longer(
#'         contains("cat_"),
#'         values_to = "new",
#'         names_to = "var"
#'       ),
#'     y = select(old, contains("cat_")) %>%
#'       rowindex() %>%
#'       tidyr::pivot_longer(
#'         contains("cat_"),
#'         values_to = "old",
#'         names_to = "var"
#'       ),
#'     by = c("var", "row")
#'   ) %>%
#'     mutate_if(is_fctchr, factor) %>%
#'     select(!row) %>%
#'     count(var, new, old) %>%
#'     grf_col(n ~ new, fill = ~old) +
#'     scale_pal_gr(scale = "discrete", aesthetics = "fill") +
#'     contraster_label_bw(label = n, position = position_stack(vjust = .5)) +
#'     facet_grid(~var, scale = "free") +
#'     theme_greyscale(axis = "xy")
#' }
#' plot_num_cat2 <- function(new, old) {
#'   inner_join(
#'     x = select(new, contains("num")) %>%
#'       rowindex() %>%
#'       tidyr::pivot_longer(
#'         contains("num_"),
#'         values_to = "new",
#'         names_to = "var"
#'       ),
#'     y = select(old, contains("num")) %>%
#'       rowindex() %>%
#'       tidyr::pivot_longer(
#'         contains("num_"),
#'         values_to = "old",
#'         names_to = "var"
#'       ),
#'     by = c("row","var")
#'   ) %>%
#'     mutate_if(is_fctchr, factor) %>%
#'     grf_boxplot(old ~ new, fill = ~new) %>%
#'     grf_facet_wrap(var ~ ., scales = "free") +
#'     scale_pal_gr(scale = "discrete", aesthetics = "fill") +
#'     theme_greyscale(axis = "xy") +
#'     gr_legend_acction(remove_legend = TRUE)
#' }
#' rsh1 <- function(.data,nm) {
#'   select(.data = rowindex(.data = .data), c(contains("num_"), .data$row)) %>%
#'     tidyr::pivot_longer(contains("num_"), names_to = "var", values_to = nm)
#' }
#'
#' juiced<-rec_num %>%
#'   step_bestnorm(all_numeric_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' plot_num_num(new = juiced,old = data_num)
#'
#' juiced<-rec_num %>%
#'   step_binarize(all_numeric_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' plot_num_bin(new = juiced,old = data_num)
#'
#' juiced<-rec_num %>%
#'   step_breaktize(all_numeric_predictors(),num_breaks = 5) %>%
#'   prep() %>%
#'   juice()
#'
#' plot_num_num(new = juiced,old = data_num,perc = FALSE)
#'
#' juiced<-rec_num %>%
#'   step_cartdiscrmulti(all_numeric_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' plot_num_cat(new = juiced,old = data_num)
#'
#' juiced<-rec_num %>%
#'   step_categoricaloptim(all_nominal_predictors()) %>%
#'   step_select(all_nominal_predictors(),all_outcomes()) %>%
#'   prep() %>%
#'   juice()
#'
#' plot_cat_cat(new = juiced,old = data_num)
#'
#' rec_num_out %>%
#'   step_clip_bounds(
#'     all_numeric_predictors(),
#'     cutoff = .8,
#'     fn = "side",
#'     method = "quantile",
#'     times = 10,
#'     rep = 5
#'   ) %>%
#'   prep() %>%
#'   juice() %>%
#'   rsh1(nm = "new") %>%
#'   inner_join(
#'     y = rsh1(.data = data_num_out, nm = "old"),
#'     by = c("row", "var")
#'   ) %>%
#'   tidyr::pivot_longer(new:old, names_to = "group") %>%
#'   group_by(var, group) %>%
#'   summarise(
#'     min = min(value),
#'     max = max(value)
#'   ) %>%
#'   ungroup() %>%
#'   split(f = .$var) %>%
#'   map(
#'     .f = ~ select(.x, !.data$var) %>%
#'       as.data.frame() %>%
#'       set_rownames(value = .$group) %>%
#'       select(!.data$group)
#'   )
#'
#' juiced<-rec_num %>%
#'   step_discrkknn(all_numeric_predictors(),type = "factor") %>%
#'   prep() %>%
#'   juice()
#'
#' plot_num_cat2(new = juiced,old = data_num)
#'
#' juiced<-rec_cat %>%
#'   step_discrmdlp(all_numeric_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' plot_num_cat2(new = select_if(juiced,is_fct),old = data_num)
#'
#' juiced<-rec_num %>%
#'   step_discrchi(all_numeric_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' plot_num_cat2(new = select_if(juiced,is_fct),old = data_num)
#'
#' juiced<-rec_num %>%
#'   step_discrtopdown(all_numeric_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' plot_num_cat2(new = select_if(juiced,is_fct),old = data_num)
#'
#' rec_num%>%
#'   step_efa(all_numeric_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#'
#' rec_num %>%
#'   step_limits_bounds(all_numeric_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' rec_num %>%
#'   step_mca(all_nominal_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' rec_num %>%
#'   step_xnls(all_numeric_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#'
#' rec_num %>%
#'   step_simplifycategorical(all_nominal_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' rec_num %>%
#'   step_lenc_glm(all_nominal_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' rec_num %>%
#'   step_lenc_mixed(all_nominal_predictors()) %>%
#'   prep() %>%
#'   juice()
#'
#' rec_num %>%
#'   step_xrossdummy(num_1, dummy_var = "cat_2") %>%
#'   prep() %>%
#'   juice()
#'
#'
NULL


# multicollin -----------------------------------------------------------------------



#' multicollin
#'
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
step_multicollin <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           object = NULL,
           max_steps = 10,
           min_var = 0.1,
           max_cor = 0.9,
           skip = FALSE,
           id = rand_id("multicollin")) {
    add_step(
      recipe,
      step_multicollin_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        object = object,
        max_steps = max_steps,
        min_var = min_var,
        max_cor = max_cor,
        skip = skip,
        id = id
      )
    )
  }

step_multicollin_new <-
  function(terms, role, object,trained, max_steps, min_var, max_cor, na_rm, skip, id) {
    recipes::step(
      subclass = "multicollin",
      terms = terms,
      role = role,
      trained = trained,
      object = object,
      max_steps = max_steps,
      min_var = min_var,
      max_cor = max_cor,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_multicollin <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )

  object <- multicollin(
    .data = training[, col_names], everything(),
    max_steps = x$max_steps,
    min_var = x$min_var,
    max_cor = x$max_cor
  )
  step_multicollin_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    object = object,
    max_steps = x$max_steps,
    min_var = x$min_var,
    max_cor = x$max_cor,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_multicollin <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_multicollin <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("multi collinearity for ", sep = "")
    printer(attr(x$object,"transformed"), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @export
tidy.step_multicollin <- function(x, ...) {
  term_names <- sel2char(x$terms)
  if (recipes::is_trained(x)) {
    res <- tibble(terms = term_names,value = lengths(x$object))
  } else {

    res <- tibble(terms = term_names,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}

# xcorrr ----------------------------------------------------------------------------


#' xcorrr
#'
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
step_xcorrr <-
  function(recipe,
           ...,
           object = NULL,
           role = NA,
           trained = FALSE,
           cutoff = 0.9,
           use = "pairwise.complete.obs",
           method = "pearson",
           skip = FALSE,
           id = rand_id("xcorrr")) {
    add_step(
      recipe,
      step_xcorrr_new(
        terms = ellipse_check(...),
        trained = trained,
        object = object,
        role = role,
        cutoff = cutoff,
        use = use,
        method = method,
        skip = skip,
        id = id
      )
    )
  }

step_xcorrr_new <-
  function(terms,object, role,cutoff,use,method,trained, na_rm, skip, id) {
    recipes::step(
      subclass = "xcorrr",
      terms = terms,
      object = object,
      role = role,
      trained = trained,
      cutoff = cutoff,
      use = use,
      method = method,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_xcorrr <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(quos = x$terms,
                        data = training,
                        info = info)

  object <-
    xcorrr(.data = training[, col_names],everything(),cutoff = x$cutoff,use = x$use,method = x$method)

  step_xcorrr_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    object = object,
    cutoff = x$cutoff,
    use = x$use,
    method = x$method,
    skip = x$skip,
    id = x$id
  )
}
#' @export
bake.step_xcorrr <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_xcorrr <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("<correlation all> ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @export
tidy.step_xcorrr <- function(x, ...) {
  term_names <- sel2char(x$terms)
  if (recipes::is_trained(x)) {
    res <- tibble(terms = term_names,value = lengths(x$object))
  } else {

    res <- tibble(terms = term_names,value = rlang::na_dbl)
  }
  res$id <- x$id
  res

}

# discrkknn -------------------------------------------------------------------------



#' discrkknn
#'
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export

step_discrkknn <-
  function(recipe,
           ...,
           type = c("factor","numeric"),
           role = NA,
           trained = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("discrkknn")) {
    type <- match.arg(type)
    add_step(
      recipe,
      step_discrkknn_new(
        terms = ellipse_check(...),
        type = type,
        trained = trained,
        role = role,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_discrkknn_new <-
  function(terms,type,trained, role, na_rm,object, skip, id) {
    recipes::step(
      subclass = "discrkknn",
      terms = terms,
      type =type,
      role = role,
      trained = trained,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_discrkknn <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )
  check_type(training[, col_names])

  training <- as_tbl(training[, col_names])
  object <- discrkknn(data = training, everything())

  step_discrkknn_new(
    terms = x$terms,
    type = x$type,
    role = x$role,
    trained = TRUE,
    object = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_discrkknn <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data,object$type))
}

#' @export
print.step_discrkknn <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("<discrete kknn> ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @export
tidy.step_discrkknn <- function(x, ...) {
  term_names <- sel2char(x$terms)
  if (recipes::is_trained(x)) {
    res <- tibble(terms = term_names,value = lengths(x$object))
  } else {

    res <- tibble(terms = term_names,value = rlang::na_dbl)
  }
  res$id <- x$id
  res
}

# discrmdlp ---------------------------------------------------------------

#'  discrmdlp step
#'
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_discrmdlp <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           objects = NULL,
           skip = FALSE,
           id = rand_id("discrmdlp")) {
    add_step(
      recipe,
      step_discrmdlp_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_discrmdlp_new <-
  function(terms,
           role,
           trained,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "discrmdlp",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_discrmdlp <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )

  check_type(training[, col_names])

  y_names <-
    recipes_eval_select(
      quos = recipes::all_outcomes(),
      info = info,
      data = training
    )

  training <- training[, c(y_names, col_names)]
  y_names <- rlang::sym(y_names)

  object <-
    discrmdlp(
      .data = training,
      y = !!y_names,
      everything()
    )

  step_discrmdlp_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    objects = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_discrmdlp <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_discrmdlp <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Discretice MDLP ", sep = "")
    if (length(names(x$object)) == 0) {
      "no discretizado"
    } else {
      printer(names(x$object), x$terms, x$trained, width = width)
    }

    invisible(x)
  }

# discrtopdown ------------------------------------------------------------

#'  discrtopdown step
#'
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_discrtopdown <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           method = "caim",
           objects = NULL,
           skip = FALSE,
           id = rand_id("discrtopdown")) {
    add_step(
      recipe,
      step_discrtopdown_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        method = method,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_discrtopdown_new <-
  function(terms,
           role,
           trained,
           method,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "discrtopdown",
      terms = terms,
      role = role,
      trained = trained,
      method = method,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_discrtopdown <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )

  check_type(training[, col_names])

  y_names <-
    recipes_eval_select(
      quos = recipes::all_outcomes(),
      info = info,
      data = training
    )

  training <- training[, c(y_names, col_names)]
  y_names <- rlang::sym(y_names)

  object <-
    eval_expr(discrtopdown(
      .data = !!training,
      y = !!y_names,
      everything(),
      method = !!x$method
    ))

  step_discrtopdown_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    method = x$method,
    objects = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_discrtopdown <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_discrtopdown <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Discretice Topdown ", sep = "")
    if (length(names(x$object)) == 0) {
      "no discretizado"
    } else {
      printer(names(x$object), x$terms, x$trained, width = width)
    }

    invisible(x)
  }


# discrchi ----------------------------------------------------------------


#'  discrchi step
#'
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_discrchi <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           objects = NULL,
           skip = FALSE,
           id = rand_id("discrchi")) {
    add_step(
      recipe,
      step_discrchi_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_discrchi_new <-
  function(terms,
           role,
           trained,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "discrchi",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_discrchi <- function(x, training, info = NULL, ...) {

  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )

  check_type(training[, col_names])

  y_names <-
    recipes_eval_select(
      quos = recipes::all_outcomes(),
      info = info,
      data = training
    )

  training <- training[, c(y_names, col_names)]
  y_names <- rlang::sym(y_names)

  object <-
    eval_expr(discrchi(
      .data = !!training,
      y = !!y_names,
      everything()
    ))

  step_discrchi_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    objects = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_discrchi <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_discrchi <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Discretice Chi ", sep = "")
    if (length(names(x$object)) == 0) {
      "no discretizado"
    } else {
      printer(names(x$object), x$terms, x$trained, width = width)
    }

    invisible(x)
  }


# limits_bounds -----------------------------------------------------------

#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_limits_bounds <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("limits_bounds")) {
    add_step(
      recipe,
      step_limits_bounds_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_limits_bounds_new <-
  function(terms, role, trained, object, skip, id) {
    recipes::step(
      subclass = "limits_bounds",
      terms = terms,
      role = role,
      trained = trained,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_limits_bounds <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )
  check_type(training[, col_names])

  training <- as_tbl(training)

  object <-
    limits_bounds(.data = training[, col_names], everything())

  step_limits_bounds_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    object = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_limits_bounds <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_limits_bounds <-
  function(x,
           width = max(20, options()$width - 30),
           info = NULL,
           ...) {
    cat("generar limites de las variables ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }


# clip_bounds -------------------------------------------------------------

#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_clip_bounds <-
  function(recipe,
           ...,
           role = NA,
           cutoff = .95,
           fn = "normal",
           limite = "ambos",
           method = "quantile",
           times = 5,
           rep = 1,
           trained = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("clip_bounds")) {
    add_step(
      recipe,
      step_clip_bounds_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        cutoff = cutoff,
        fn = fn,
        limite = limite,
        method = method,
        times = times,
        rep = rep,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_clip_bounds_new <-
  function(terms,
           role,
           cutoff,
           fn,
           limite,
           method,
           times,
           rep,
           trained,
           object,
           skip,
           id) {
    recipes::step(
      subclass = "clip_bounds",
      terms = terms,
      role = role,
      cutoff = cutoff,
      fn = fn,
      limite = limite,
      method = method,
      times = times,
      rep = rep,
      trained = trained,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_clip_bounds <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )
  check_type(training[, col_names])

  training <- as_tbl(training[, col_names])

  args_call <- x[names(x) %in% names(formals(mgalda::clip_bounds))]

  args_call <-
    c(.data = list(training), rlang::expr(where(~ is_dblint(.x))), args_call)

  object <-
    eval(rlang::expr(do.call(clip_bounds, !!as_l(args_call))))


  step_clip_bounds_new(
    terms = x$terms,
    role = x$role,
    cutoff = args_call$cutoff,
    fn = args_call$fn,
    limite = args_call$limite,
    method = args_call$method,
    times = args_call$times,
    rep = args_call$rep,
    trained = TRUE,
    object = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_clip_bounds <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_clip_bounds <-
  function(x,
           width = max(20, options()$width - 30),
           info = NULL,
           ...) {
    cat("generar limites de las variables ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }


# categoricaloptim -------------------------------------------------------

#' categoricaloptim step
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export

step_categoricaloptim <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("categoricaloptim")) {
    add_step(
      recipe,
      step_categoricaloptim_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_categoricaloptim_new <-
  function(terms,
           role,
           trained,
           object,
           skip,
           id) {
    recipes::step(
      subclass = "categoricaloptim",
      terms = terms,
      role = role,
      trained = trained,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_categoricaloptim <-
  function(x, training, info = NULL, ...) {
    col_names <-
      recipes_eval_select(
        quos = x$terms,
        data = training,
        info = info
      )
    training <- as_tbl(training)

    y_names <-
      recipes_eval_select(
        quos = recipes::all_outcomes(),
        info = info,
        data = training
      )

    training <- training[, c(y_names, col_names)]

    y_names <- rlang::sym(y_names)

    object <-
      eval_expr(categoricaloptim(
        .data = !!training,
        y = !!y_names,
        everything()
      ),
      envir = env_curr()
      )
    step_categoricaloptim_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      object = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_categoricaloptim <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_categoricaloptim <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("categoricaloptim ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }

# cartdiscrmulti ----------------------------------------------------------



#' cartdiscrmulti step
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export

step_cartdiscrmulti <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("cartdiscrmulti")) {
    add_step(
      recipe,
      step_cartdiscrmulti_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_cartdiscrmulti_new <-
  function(terms,
           role,
           trained,
           object,
           skip,
           id) {
    recipes::step(
      subclass = "cartdiscrmulti",
      terms = terms,
      role = role,
      trained = trained,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_cartdiscrmulti <-
  function(x, training, info = NULL, ...) {
    col_names <-
      recipes_eval_select(
        quos = x$terms,
        data = training,
        info = info
      )
    training <- as_tbl(training)
    y_names <-
      recipes_eval_select(
        quos = all_outcomes(),
        info = info,
        data = training
      )

    object_expr <-
      rlang::expr(cartdiscrmulti(
        .data = !!training,
        y = !!y_names,
        all_of(!!col_names)
      ))

    object <- eval(object_expr)

    step_cartdiscrmulti_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      object = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_cartdiscrmulti <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_cartdiscrmulti <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("cartdiscrmulti ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }


# breaktize ---------------------------------------------------------------

#'  breaktize step
#'
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_breaktize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           algorithm = "equalwidth",
           objects = NULL,
           num_breaks = NULL,
           skip = FALSE,
           id = rand_id("breaktize")) {
    add_step(
      recipe,
      step_breaktize_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        algorithm = algorithm,
        objects = objects,
        num_breaks = num_breaks,
        skip = skip,
        id = id
      )
    )
  }

step_breaktize_new <-
  function(terms,
           role,
           trained,
           algorithm,
           objects,
           num_breaks,
           skip,
           id) {
    recipes::step(
      subclass = "breaktize",
      terms = terms,
      role = role,
      trained = trained,
      algorithm = algorithm,
      objects = objects,
      num_breaks = num_breaks,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_breaktize <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )
  check_type(training[, col_names])

  training <- as_tbl(training)

  if (is_emptyna(x$num_breaks)) {
    x$num_breaks <-
      map_dbl(training[, col_names], ~ ncase(n_unique(.x)))
  }
  object <-
    breaktize(
      .data = training[, col_names],
      num_breaks = x$num_breaks,
      algorithm = x$algorithm,
      everything()
    )

  step_breaktize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    algorithm = x$algorithm,
    objects = object,
    num_breaks = x$num_breaks,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_breaktize <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
#'
print.step_breaktize <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Cut numeric for ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @export
tidy.step_breaktize <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- tibble(terms = recipes::sel2char(x$terms))
  } else {
    # term_names <- recipes::sel2char(x$terms)
    res <- tibble(terms = rlang::na_chr)
  }
  res$id <- x$id
  res
}

#' tunable methods
#' @keywords internal
#' @export
#' @rdname custom_step

tunable.step_breaktize <- function(x, ...) {
  tibble(
    name = c("num_breaks"),
    call_info = list(pkg = "dials", fun = "num_breaks"),
    source = "recipe",
    component = "breaktize",
    component_id = x$id
  )
}


# binarize ----------------------------------------------------------------

#'  binarize step
#'
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_binarize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           algorithm = "kmean",
           objects = NULL,
           skip = FALSE,
           id = rand_id("binarize")) {
    add_step(
      recipe,
      step_binarize_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        algorithm = algorithm,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_binarize_new <-
  function(terms,
           role,
           trained,
           algorithm,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "binarize",
      terms = terms,
      role = role,
      trained = trained,
      algorithm = algorithm,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_binarize <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )
  check_type(training[, col_names])

  training <- as_tbl(training)

  object <-
    binarize(
      .data = training[, col_names],
      algorithm = x$algorithm,
      everything()
    )

  step_binarize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    algorithm = x$algorithm,
    objects = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_binarize <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
#'
print.step_binarize <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Binarize numeric for ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @export
tidy.step_binarize <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- tibble(terms = recipes::sel2char(x$terms))
  } else {
    # term_names <- recipes::sel2char(x$terms)
    res <- tibble(terms = rlang::na_chr)
  }
  res$id <- x$id
  res
}

# bestnorm ----------------------------------------------------------------



#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_bestnorm <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("bestnorm")) {
    add_step(
      recipe,
      step_bestnorm_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_bestnorm_new <-
  function(terms,
           role,
           trained,
           object,
           skip,
           id) {
    recipes::step(
      subclass = "bestnorm",
      terms = terms,
      role = role,
      trained = trained,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_bestnorm <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )

  check_type(training[, col_names])

  training <- as_tbl(training)

  object <-
    bestnorm(.data = training[, col_names], everything())

  step_bestnorm_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    object = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_bestnorm <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_bestnorm <-
  function(x,
           width = max(20, options()$width - 30),
           info = NULL,
           ...) {
    cat("best normalize: ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }



# efa ---------------------------------------------------------------------

#' efa step
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export


step_efa <- function(recipe,
                     ...,
                     role = "predictor",
                     trained = FALSE,
                     num_comp = 2,
                     rotate = "oblimin",
                     res = NULL,
                     prefix = "MC",
                     skip = FALSE,
                     id = rand_id("efa")) {
  add_step(
    recipe,
    step_efa_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      num_comp = num_comp,
      rotate = rotate,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}


step_efa_new <- function(terms,
                         role,
                         trained,
                         num_comp,
                         rotate,
                         res,
                         prefix,
                         skip,
                         id) {
  recipes::step(
    subclass = "efa",
    terms = terms,
    role = role,
    trained = trained,
    num_comp = num_comp,
    rotate = rotate,
    res = res,
    prefix = prefix,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_efa <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )

  check_type(training[, col_names])

  max_length <- length(col_names)

  x$num_comp <- min(x$num_comp, max_length)

  efa_call <- dplyr::expr(psych::fa(
    rotate = x$rotate,
    nfactors = x$num_comp
  ))

  efa_call$r <- dplyr::expr(training[, col_names])

  efa_obj <-suppressall(eval(efa_call))
  step_efa_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    rotate = x$rotate,
    res = efa_obj,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}



#' @export
bake.step_efa <- function(object, new_data, ...) {
  efa_vars <- rownames(object$res$weights)


  comps <-
    stats::predict(eval(object$res), data = new_data[, efa_vars])

  colnames(comps) <- paste0("efa", 1:ncol(object$res$weights))

  new_data <- bind_cols(datadiff(new_data,efa_vars), as_tbl(comps))

  as_tbl(new_data)
}

#' @export
rotate <- function(values = values_rotate) {
  dials::new_quanl_param(
    type = "character",
    values = values,
    default = "oblimin",
    label = c(rotate = "Rotation"),
    finalize = NULL
  )
}

values_rotate <- c(
  "oblimin",
  "varimax",
  "quartimax",
  "bentlerT",
  "equamax",
  "varimin",
  "geominT",
  "bifactor",
  "Promax",
  "promax",
  "simplimax",
  "bentlerQ",
  "biquartmin",
  "cluster"
)

#' @rdname custom_step
#' @export
tunable.step_efa <- function(x, ...) {
  tibble(
    name = c("num_comp", "rotate"),
    call_info = list(
      list(
        pkg = "dials",
        fun = "num_comp",
        range = c(1L, 5L)
      ),
      list(
        pkg = "dials",
        fun = "rotate",
        values = values_rotate
      )
    ),
    source = "recipe",
    component = "step_efa",
    component_id = x$id
  )
}

#' @export
required_pkgs.step_efa <- function(x, ...) {
  c("psych", "GPArotation")
}

print.step_efa <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Factor Analysis Exploratory: ", sep = "")
    printer(rownames(x$res$residual), x$terms, x$trained, width = width)
    invisible(x)
  }


# mca ---------------------------------------------------------------------



#' mca step
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export


step_mca <- function(recipe,
                     ...,
                     role = "predictor",
                     trained = FALSE,
                     num_f = 2,
                     res = NULL,
                     prefix = "MC",
                     skip = FALSE,
                     id = rand_id("mca")) {
  add_step(
    recipe,
    step_mca_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      num_f = num_f,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}

step_mca_new <- function(terms,
                         role,
                         trained,
                         num_f,
                         res,
                         prefix,
                         skip,
                         id) {
  recipes::step(
    subclass = "mca",
    terms = terms,
    role = role,
    trained = trained,
    num_f = num_f,
    res = res,
    prefix = prefix,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_mca <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )

  check_type(training[, col_names], FALSE)

  max_length <- length(unique(unlist(training[, col_names]))) - 1

  x$num_f <- min(x$num_f, max_length)

  mca_call <- dplyr::expr(MASS::mca(x$num_f, abbrev = FALSE))

  mca_call$df <- dplyr::expr(training[, col_names])

  mca_obj <- eval(mca_call)

  step_mca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_f = x$num_f,
    res = mca_obj,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_mca <- function(object, new_data, ...) {
  mca_vars <- unique(gsub("\\..*", "", x = rownames(object$res$cs)))

  nfs <-
    stats::predict(eval(object$res), newdata = new_data[, mca_vars])

  colnames(nfs) <- paste0("MC", 1:ncol(nfs))

  as_tbl(bind_cols(datadiff(new_data, mca_vars), as_tbl(nfs)))
}

#' @export
num_f <- function(range = c(2, 5), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_f = "MCA Comps"),
    finalize = NULL
  )
}

#' @export
tunable.step_mca <- function(x, ...) {
  tibble(
    name = "num_f",
    call_info = list(list(
      pkg = "extradim",
      fun = "num_f",
      range = c(2L, 5L)
    )),
    source = "recipe",
    component = "step_mca",
    component_id = x$id
  )
}

#' @export
required_pkgs.step_mca <- function(x, ...) {
  c("MASS")
}

#' @export
print.step_mca <-
  function(x,
           width = max(20, options()$width - 30),
           info = NULL,
           ...) {
    col_names <-
      unique(gsub("\\..*", "", x = rownames(x$res$cs)))
    cat("Multiple Correspondence Analysis: ", sep = "")
    printer(col_names, x$terms, x$trained, width = width)
    invisible(x)
  }



# xnls --------------------------------------------------------------------

#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
#'

step_xnls <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("xnls")) {
    add_step(
      recipe,
      step_xnls_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_xnls_new <-
  function(terms, role, trained, object, skip, id) {
    recipes::step(
      subclass = "xnls",
      terms = terms,
      role = role,
      trained = trained,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_xnls <- function(x, training, info = NULL, ...) {
  col_names <-
    recipes_eval_select(
      quos = x$terms,
      data = training,
      info = info
    )
  training <- as_tbl(training)
  y_names <-
    recipes_eval_select(
      quos = all_outcomes(),
      info = info,
      data = training
    )

  object <-
    map(
      col_names,
      ~ xnls(
        .data = training,
        x = !!rlang::sym(.x),
        y = !!rlang::sym(y_names)
      )
    )

  names(object) <- col_names
  step_xnls_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    object = object,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_xnls <- function(object, new_data, ...) {
  vars_xnls <- names(object$object)

  for (nm in vars_xnls) {
    new_data <-
      mutate(new_data, {{ nm }} := rlang::eval_tidy(object$object[[nm]]))
  }
  new_data
}

#' @export
print.step_xnls <-
  function(x,
           width = max(20, options()$width - 30),
           info = NULL,
           ...) {
    cat("generar nueva variable en base a funciones no lineales ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }


# xrossdummy  --------------------------------------------------------------

#' xrossdummy step
#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export

step_xrossdummy <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           dummy_var = NA,
           object = NULL,
           skip = FALSE,
           id = rand_id("xrossdummy")) {
    add_step(
      recipe,
      step_xrossdummy_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        dummy_var = dummy_var,
        object = object,
        skip = skip,
        id = id
      )
    )
  }



step_xrossdummy_new <-
  function(terms,
           role,
           trained,
           dummy_var,
           object,
           skip,
           id) {
    recipes::step(
      subclass = "xrossdummy",
      terms = terms,
      role = role,
      trained = trained,
      dummy_var = dummy_var,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_xrossdummy <-
  function(x, training, info = NULL, ...) {
    col_names <-
      recipes_eval_select(
        quos = x$terms,
        data = training,
        info = info
      )
    check_type(training[, col_names])

    training <- as_tbl(training)

    dummy_var <- str2lang(x$dummy_var)
    syms_cols <- map(col_names,str2lang)

    object <-
      do_call(f = "xrossdummy", .args = c(
        .data = list(training),
        dummy_var = dummy_var,
        syms_cols
      ))

    step_xrossdummy_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      dummy_var = x$dummy_var,
      object = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_xrossdummy <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_xrossdummy <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("dummycross ", sep = "")
    printer(names(x$object$transf$target), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export


# categoricaloptim --------------------------------------------------------

step_categoricaloptim <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           objects = NULL,
           skip = FALSE,
           id = rand_id("categoricaloptim")) {
    add_step(
      recipe,
      step_categoricaloptim_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_categoricaloptim_new <-
  function(terms,
           role,
           trained,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "categoricaloptim",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_categoricaloptim <-
  function(x, training, info = NULL, ...) {
    col_names <-
      recipes_eval_select(
        quos = x$terms,
        data = training,
        info = info
      )
    y_names <-
      recipes_eval_select(
        quos = recipes::all_outcomes(),
        info = info,
        data = training
      )

    training <- training[, c(y_names, col_names)]
    y_names <- str2lang(y_names)
    col_names <- map(col_names,str2lang)
    object <-
      make_call(f = categoricaloptim,
                .args = c(list(.data = training, y = y_names),
                          unname(col_names)))
    object <-  eval(object)

    step_categoricaloptim_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      objects = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_categoricaloptim <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_categoricaloptim <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Categorical Optim: ", sep = "")
    printer(x$object$cols, x$terms, x$trained, width = width)
    invisible(x)
  }


# simplifycategorical -----------------------------------------------------



#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export


step_simplifycategorical <-
  function(recipe,
           ...,
           min_f = 4,
           role = NA,
           trained = FALSE,
           objects = NULL,
           skip = FALSE,
           id = rand_id("simplifycategorical")) {
    add_step(
      recipe,
      step_simplifycategorical_new(
        terms = ellipse_check(...),
        trained = trained,
        min_f = min_f,
        role = role,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_simplifycategorical_new <-
  function(terms,
           min_f,
           role,
           trained,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "simplifycategorical",
      terms = terms,
      min_f = min_f,
      role = role,
      trained = trained,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_simplifycategorical <-
  function(x, training, info = NULL, ...) {
    col_names <-
      recipes_eval_select(
        quos = x$terms,
        data = training,
        info = info
      )
    col_names <- map(col_names,str2lang)
    object <-
      do_call(f = "simplifycategorical",.args = c(
        .data = list(training),
        col_names,
        min_f = x$min_f
      ))

    step_simplifycategorical_new(
      terms = x$terms,
      role = x$role,
      min_f = x$min_f,
      trained = TRUE,
      objects = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_simplifycategorical <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_simplifycategorical <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Simplify Categorical: ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)

    invisible(x)
  }


# lenc_glm ----------------------------------------------------------------

#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
step_lenc_glm <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           objects = NULL,
           skip = FALSE,
           id = rand_id("lenc_glm")) {
    add_step(
      recipe,
      step_lenc_glm_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_lenc_glm_new <-
  function(terms,
           role,
           trained,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "lenc_glm",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lenc_glm <-
  function(x, training, info = NULL, ...) {
    col_names <-
      recipes_eval_select(
        quos = x$terms,
        data = training,
        info = info
      )
    y_names <-
      recipes_eval_select(
        quos = recipes::all_outcomes(),
        info = info,
        data = training
      )
    training <- training[, c(y_names, col_names)]
    y_names <- str2lang(y_names)
    col_names <- map(col_names,str2lang)

    object <-
      do_call(f = "lenc_glm",.args = c(
        .data = list(as_tbl(training)),
        y = y_names,
        col_names
      ))

    step_lenc_glm_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      objects = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_lenc_glm <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_lenc_glm <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Factor Conversions using Likelihood: ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }

# lenc_lm ----------------------------------------------------------------

#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
step_lenc_lm <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           objects = NULL,
           skip = FALSE,
           id = rand_id("lenc_lm")) {
    add_step(
      recipe,
      step_lenc_lm_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_lenc_lm_new <-
  function(terms,
           role,
           trained,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "lenc_lm",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lenc_lm <-
  function(x, training, info = NULL, ...) {
    col_names <-
      recipes_eval_select(
        quos = x$terms,
        data = training,
        info = info
      )
    y_names <-
      recipes_eval_select(
        quos = recipes::all_outcomes(),
        info = info,
        data = training
      )
    training <- training[, c(y_names, col_names)]
    y_names <- str2lang(y_names)
    col_names <- map(col_names,str2lang)

    object <-
      do_call(f = "lenc_lm",.args = c(
        .data = list(as_tbl(training)),
        y = y_names,
        col_names
      ))

    step_lenc_lm_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      objects = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_lenc_lm <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_lenc_lm <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Factor Conversions using linear regression: ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }

# lenc_mixed --------------------------------------------------------------

#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
step_lenc_mixed <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           objects = NULL,
           skip = FALSE,
           id = rand_id("lenc_mixed")) {
    add_step(
      recipe,
      step_lenc_mixed_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_lenc_mixed_new <-
  function(terms,
           role,
           trained,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "lenc_mixed",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lenc_mixed <-
  function(x, training, info = NULL, ...) {
    col_names <-
      recipes_eval_select(
        quos = x$terms,
        data = training,
        info = info
      )
    y_names <-
      recipes_eval_select(
        quos = recipes::all_outcomes(),
        info = info,
        data = training
      )

    training <- training[, c(y_names, col_names)]
    y_names <- str2lang(y_names)
    col_names <- map(col_names,str2lang)
    object <-
      do_call(f = "lenc_mixed",.args = c(
        .data = list(training),
        y = y_names,
        col_names
      ))

    step_lenc_mixed_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      objects = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_lenc_mixed <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_lenc_mixed <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Factor Conversions using Bayesian Likelihood: ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }


# discrete ----------------------------------------------------------------

#' @rdname custom_step
#' @keywords datagen
#' @concept preprocessing
#' @export
step_discrete <-
  function(recipe,
           ...,
           role = NA,
           threshold = .05,
           trained = FALSE,
           objects = NULL,
           skip = FALSE,
           id = rand_id("discrete")) {
    add_step(
      recipe,
      step_discrete_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        threshold = threshold,
        objects = objects,
        skip = skip,
        id = id
      )
    )
  }

step_discrete_new <-
  function(terms,
           role,
           threshold,
           trained,
           objects,
           skip,
           id) {
    recipes::step(
      subclass = "discrete",
      terms = terms,
      role = role,
      threshold = threshold,
      trained = trained,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_discrete <-
  function(x, training, info = NULL, ...) {
    col_names <-
      recipes_eval_select(
        quos = x$terms,
        data = training,
        info = info
      )
    y_names <-
      recipes_eval_select(
        quos = recipes::all_outcomes(),
        info = info,
        data = training
      )

    training <- training[, c(y_names, col_names)]
    y_names <- rlang::sym(y_names)
    object <-
      eval_expr(
        discrete(
          x = !!training,
          outcome = !!y_names,
          threshold = x$threshold
        ),
        env_curr()
      )

    step_discrete_new(
      terms = x$terms,
      role = x$role,
      threshold = x$threshold,
      trained = TRUE,
      objects = object,
      skip = x$skip,
      id = x$id
    )
  }

#' @export
bake.step_discrete <- function(object, new_data, ...) {
  as_tbl(predict(object$object, new_data))
}

#' @export
print.step_discrete <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Detect factor as integer : ", sep = "")
    printer(names(x$object), x$terms, x$trained, width = width)
    invisible(x)
  }




# helpers -----------------------------------------------------------------

#' @keywords internal
ellipse_check <-
  function(...) {
    terms <- rlang::quos(...)
    if (is_empty(terms)) {
      cat_stop(paste0(
        "Please supply at least one variable specification.",
        "See ?selections."
      ))
    }
    terms
  }
