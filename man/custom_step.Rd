% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_step.R
\name{custom_step}
\alias{custom_step}
\alias{step_multicollin}
\alias{step_xcorrr}
\alias{step_discrkknn}
\alias{step_discrmdlp}
\alias{step_discrtopdown}
\alias{step_discrchi}
\alias{step_limits_bounds}
\alias{step_clip_bounds}
\alias{step_categoricaloptim}
\alias{step_cartdiscrmulti}
\alias{step_breaktize}
\alias{tunable.step_breaktize}
\alias{step_binarize}
\alias{step_bestnorm}
\alias{step_efa}
\alias{tunable.step_efa}
\alias{step_mca}
\alias{step_xnls}
\alias{step_xrossdummy}
\alias{step_simplifycategorical}
\alias{step_lenc_glm}
\alias{step_lenc_lm}
\alias{step_lenc_mixed}
\alias{step_discrete}
\title{custom_step}
\usage{
step_multicollin(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  object = NULL,
  max_steps = 10,
  min_var = 0.1,
  max_cor = 0.9,
  skip = FALSE,
  id = rand_id("multicollin")
)

step_xcorrr(
  recipe,
  ...,
  object = NULL,
  role = NA,
  trained = FALSE,
  cutoff = 0.9,
  use = "pairwise.complete.obs",
  method = "pearson",
  skip = FALSE,
  id = rand_id("xcorrr")
)

step_discrkknn(
  recipe,
  ...,
  type = c("factor", "numeric"),
  role = NA,
  trained = FALSE,
  object = NULL,
  skip = FALSE,
  id = rand_id("discrkknn")
)

step_discrmdlp(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("discrmdlp")
)

step_discrtopdown(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  method = "caim",
  objects = NULL,
  skip = FALSE,
  id = rand_id("discrtopdown")
)

step_discrchi(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("discrchi")
)

step_limits_bounds(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  object = NULL,
  skip = FALSE,
  id = rand_id("limits_bounds")
)

step_clip_bounds(
  recipe,
  ...,
  role = NA,
  cutoff = 0.95,
  fn = "normal",
  limite = "ambos",
  method = "quantile",
  times = 5,
  rep = 1,
  trained = FALSE,
  object = NULL,
  skip = FALSE,
  id = rand_id("clip_bounds")
)

step_categoricaloptim(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("categoricaloptim")
)

step_cartdiscrmulti(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  object = NULL,
  skip = FALSE,
  id = rand_id("cartdiscrmulti")
)

step_breaktize(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  algorithm = "equalwidth",
  objects = NULL,
  num_breaks = NULL,
  skip = FALSE,
  id = rand_id("breaktize")
)

\method{tunable}{step_breaktize}(x, ...)

step_binarize(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  algorithm = "kmean",
  objects = NULL,
  skip = FALSE,
  id = rand_id("binarize")
)

step_bestnorm(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  object = NULL,
  skip = FALSE,
  id = rand_id("bestnorm")
)

step_efa(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  rotate = "oblimin",
  res = NULL,
  prefix = "MC",
  skip = FALSE,
  id = rand_id("efa")
)

\method{tunable}{step_efa}(x, ...)

step_mca(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_f = 2,
  res = NULL,
  prefix = "MC",
  skip = FALSE,
  id = rand_id("mca")
)

step_xnls(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  object = NULL,
  skip = FALSE,
  id = rand_id("xnls")
)

step_xrossdummy(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  dummy_var = NA,
  object = NULL,
  skip = FALSE,
  id = rand_id("xrossdummy")
)

step_categoricaloptim(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("categoricaloptim")
)

step_simplifycategorical(
  recipe,
  ...,
  min_f = 4,
  role = NA,
  trained = FALSE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("simplifycategorical")
)

step_lenc_glm(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("lenc_glm")
)

step_lenc_lm(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("lenc_lm")
)

step_lenc_mixed(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("lenc_mixed")
)

step_discrete(
  recipe,
  ...,
  role = NA,
  threshold = 0.05,
  trained = FALSE,
  objects = NULL,
  skip = FALSE,
  id = rand_id("discrete")
)
}
\description{
custom_step

multicollin

xcorrr

discrkknn

discrmdlp step

discrtopdown step

discrchi step

categoricaloptim step

cartdiscrmulti step

breaktize step

tunable methods

binarize step

efa step

mca step

xrossdummy step
}
\examples{

library(recipes)
library(mgalda)
library(tidyverse)

data_num <<-
  xrandomdata(
    nrows = 500,
    numcols = 3,
    factcols = 2,
    output = "num"
  )
data_cat <-
  xrandomdata(
    nrows = 500,
    numcols = 3,
    factcols = 2,
    output = "cat"
  )
data_num_out <- mutate_if(data_num,is_dblint, create_outliers)

rec_num <- recipe(output ~ ., data = data_num)
rec_cat <- recipe(output ~ ., data = data_cat)
rec_num_out <- recipe(output ~ ., data = data_num_out)

plot_num_num <- function(new, old, perc = TRUE) {
  .s_num <- f(select(x, contains("num")))
  data <- bind_rows(
    new = .s_num(new) \%>\% rowindex(),
    old = .s_num(old) \%>\% rowindex(),
    .id = "group"
  )

  if (is_true(perc)) {
    P <- data \%>\%
      tidyr::pivot_longer(contains("num"), names_to = "var") \%>\%
      mutate_if(is_fctchr, factor) \%>\%
      group_by(group, var) \%>\%
      arrange_all() \%>\%
      mutate(
        value = rescalar(value),
        percentil = rank_percent(x = value)
      ) \%>\%
      filter(percentil < .99 & percentil > .01) \%>\%
      mutate(value = rescalar(value)) \%>\%
      ungroup() \%>\%
      grf_line(value ~ percentil, colour = ~group) \%>\%
      grf_facet_wrap(var ~ .) +
      scale_pal_gr(scale = "discrete", aesthetics = "colour") +
      theme_greyscale(axis = "xy")
  } else if (is_false(perc)) {
    P <- data \%>\%
      tidyr::pivot_longer(contains("num")) \%>\%
      tidyr::pivot_wider(names_from = group, values_from = value) \%>\%
      grf_boxplot(old ~ factor(new), group = ~new) \%>\%
      grf_facet_wrap(name ~ ., scale = "free") +
      scale_pal_gr(scale = "discrete", aesthetics = "colour") +
      theme_greyscale(axis = "xy") +
      xlab("cortes") +
      ylab("valor original")
  } else {
    P <- data \%>\%
      tidyr::pivot_longer(contains("num"), names_to = "var") \%>\%
      tidyr::pivot_wider(names_from = group, values_from = value) \%>\%
      mutate_if(is_fctchr, factor) \%>\%
      grf_line(new ~ old, colour = ~var) \%>\%
      grf_facet_wrap(var ~ ., scales = "free") +
      scale_pal_gr(scale = "discrete", aesthetics = "colour") +
      theme_greyscale(axis = "xy")
  }
  P
}
plot_num_cat <- function(new, old) {
  inner_join(
    x = select(new, new = contains("cartd")) \%>\%
      rowindex(),
    y = select(old, contains("num")) \%>\%
      rowindex() \%>\%
      tidyr::pivot_longer(
        contains("num_"),
        values_to = "old",
        names_to = "var"
      ),
    by = "row"
  ) \%>\%
    mutate_if(is_fctchr, factor) \%>\%
    grf_boxplot(old ~ new, fill = ~new) \%>\%
    grf_facet_wrap(var ~ ., scales = "free_y") +
    scale_pal_gr(scale = "discrete", aesthetics = "fill") +
    theme_greyscale(axis = "xy")
}
plot_num_bin <- function(new, old) {
  .s_num <- function(x, nm) {
    select(x, contains("num")) \%>\%
      rowindex() \%>\%
      tidyr::pivot_longer(contains("num"),
                   names_to = "var",
                   values_to = nm
      )
  }
  inner_join(
    x = .s_num(new, "new"),
    y = .s_num(old, "old"),
    by = c("var", "row")
  ) \%>\%
    mutate_if(is_fctchr, factor) \%>\%
    grf_boxplot(old ~ factor(1), fill = ~new) \%>\%
    grf_facet_wrap(var ~ ., scales = "free_y") +
    scale_pal_gr(scale = "discrete", aesthetics = "fill") +
    theme_greyscale(axis = "xy")
}
plot_cat_cat <- function(new, old) {
  inner_join(
    x = select(new, contains("cat_")) \%>\%
      rowindex() \%>\%
      tidyr::pivot_longer(
        contains("cat_"),
        values_to = "new",
        names_to = "var"
      ),
    y = select(old, contains("cat_")) \%>\%
      rowindex() \%>\%
      tidyr::pivot_longer(
        contains("cat_"),
        values_to = "old",
        names_to = "var"
      ),
    by = c("var", "row")
  ) \%>\%
    mutate_if(is_fctchr, factor) \%>\%
    select(!row) \%>\%
    count(var, new, old) \%>\%
    grf_col(n ~ new, fill = ~old) +
    scale_pal_gr(scale = "discrete", aesthetics = "fill") +
    contraster_label_bw(label = n, position = position_stack(vjust = .5)) +
    facet_grid(~var, scale = "free") +
    theme_greyscale(axis = "xy")
}
plot_num_cat2 <- function(new, old) {
  inner_join(
    x = select(new, contains("num")) \%>\%
      rowindex() \%>\%
      tidyr::pivot_longer(
        contains("num_"),
        values_to = "new",
        names_to = "var"
      ),
    y = select(old, contains("num")) \%>\%
      rowindex() \%>\%
      tidyr::pivot_longer(
        contains("num_"),
        values_to = "old",
        names_to = "var"
      ),
    by = c("row","var")
  ) \%>\%
    mutate_if(is_fctchr, factor) \%>\%
    grf_boxplot(old ~ new, fill = ~new) \%>\%
    grf_facet_wrap(var ~ ., scales = "free") +
    scale_pal_gr(scale = "discrete", aesthetics = "fill") +
    theme_greyscale(axis = "xy") +
    gr_legend_acction(remove_legend = TRUE)
}
rsh1 <- function(.data,nm) {
  select(.data = rowindex(.data = .data), c(contains("num_"), .data$row)) \%>\%
    tidyr::pivot_longer(contains("num_"), names_to = "var", values_to = nm)
}

juiced<-rec_num \%>\%
  step_bestnorm(all_numeric_predictors()) \%>\%
  prep() \%>\%
  juice()

plot_num_num(new = juiced,old = data_num)

juiced<-rec_num \%>\%
  step_binarize(all_numeric_predictors()) \%>\%
  prep() \%>\%
  juice()

plot_num_bin(new = juiced,old = data_num)

juiced<-rec_num \%>\%
  step_breaktize(all_numeric_predictors(),num_breaks = 5) \%>\%
  prep() \%>\%
  juice()

plot_num_num(new = juiced,old = data_num,perc = FALSE)

juiced<-rec_num \%>\%
  step_cartdiscrmulti(all_numeric_predictors()) \%>\%
  prep() \%>\%
  juice()

plot_num_cat(new = juiced,old = data_num)

juiced<-rec_num \%>\%
  step_categoricaloptim(all_nominal_predictors()) \%>\%
  step_select(all_nominal_predictors(),all_outcomes()) \%>\%
  prep() \%>\%
  juice()

plot_cat_cat(new = juiced,old = data_num)

rec_num_out \%>\%
  step_clip_bounds(
    all_numeric_predictors(),
    cutoff = .8,
    fn = "side",
    method = "quantile",
    times = 10,
    rep = 5
  ) \%>\%
  prep() \%>\%
  juice() \%>\%
  rsh1(nm = "new") \%>\%
  inner_join(
    y = rsh1(.data = data_num_out, nm = "old"),
    by = c("row", "var")
  ) \%>\%
  tidyr::pivot_longer(new:old, names_to = "group") \%>\%
  group_by(var, group) \%>\%
  summarise(
    min = min(value),
    max = max(value)
  ) \%>\%
  ungroup() \%>\%
  split(f = .$var) \%>\%
  map(
    .f = ~ select(.x, !.data$var) \%>\%
      as.data.frame() \%>\%
      set_rownames(value = .$group) \%>\%
      select(!.data$group)
  )

juiced<-rec_num \%>\%
  step_discrkknn(all_numeric_predictors(),type = "factor") \%>\%
  prep() \%>\%
  juice()

plot_num_cat2(new = juiced,old = data_num)

juiced<-rec_cat \%>\%
  step_discrmdlp(all_numeric_predictors()) \%>\%
  prep() \%>\%
  juice()

plot_num_cat2(new = select_if(juiced,is_fct),old = data_num)

juiced<-rec_num \%>\%
  step_discrchi(all_numeric_predictors()) \%>\%
  prep() \%>\%
  juice()

plot_num_cat2(new = select_if(juiced,is_fct),old = data_num)

juiced<-rec_num \%>\%
  step_discrtopdown(all_numeric_predictors()) \%>\%
  prep() \%>\%
  juice()

plot_num_cat2(new = select_if(juiced,is_fct),old = data_num)

rec_num\%>\%
  step_efa(all_numeric_predictors()) \%>\%
  prep() \%>\%
  juice()


rec_num \%>\%
  step_limits_bounds(all_numeric_predictors()) \%>\%
  prep() \%>\%
  juice()

rec_num \%>\%
  step_mca(all_nominal_predictors()) \%>\%
  prep() \%>\%
  juice()

rec_num \%>\%
  step_xnls(all_numeric_predictors()) \%>\%
  prep() \%>\%
  juice()


rec_num \%>\%
  step_simplifycategorical(all_nominal_predictors()) \%>\%
  prep() \%>\%
  juice()

rec_num \%>\%
  step_lenc_glm(all_nominal_predictors()) \%>\%
  prep() \%>\%
  juice()

rec_num \%>\%
  step_lenc_mixed(all_nominal_predictors()) \%>\%
  prep() \%>\%
  juice()

rec_num \%>\%
  step_xrossdummy(num_1, dummy_var = "cat_2") \%>\%
  prep() \%>\%
  juice()


}
\concept{preprocessing}
\keyword{datagen}
\keyword{internal}
