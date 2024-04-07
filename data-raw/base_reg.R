# ------------------------------------------------------------------------------
# codigo de bd workflow para superlearner regression

# ------------------------------------------------------------------------------

library(scales, warn.conflicts = FALSE)
library(parsnip, warn.conflicts = FALSE)
library(dials, warn.conflicts = FALSE)
library(tune, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(plsmod, warn.conflicts = FALSE)

decision_treeXrpart <-
  parsnip::decision_tree(
    tree_depth = tune::tune("tree_depth"),
    min_n = tune::tune("min_n"),
    cost_complexity = tune::tune("cost_complexity")
  )
decision_treeXrpart <-
  parsnip::set_mode(object = decision_treeXrpart, mode = "regression")
decision_treeXrpart <-
  parsnip::set_engine(object = decision_treeXrpart, engine = "rpart")
decision_treeXrpart_param <-
  tune::parameters(x = decision_treeXrpart)
decision_treeXrpart_param <-
  stats::update(
    object = decision_treeXrpart_param,
    tree_depth = dials::value_set(
      object = dials::tree_depth(range = c(3L, 9L), trans = NULL),
      values = seq(3, 9, 2)
    ),
    min_n = dials::value_set(
      object = dials::min_n(range = c(25L, 100L), trans = NULL),
      values = seq(25, 100, 5)
    ),
    cost_complexity = dials::value_set(
      object = dials::cost_complexity(range = c(-3, -1), trans = scales::log10_trans()),
      values = c(-3:-1)
    )
  )
decision_treeXrpart_param <-
  dials::grid_regular(x = decision_treeXrpart_param, levels = 999)
decision_treeXrpart <-
  dplyr::rename(
    .data = merge(x = decision_treeXrpart, y = decision_treeXrpart_param),
    spec = x
  )
rm(decision_treeXrpart_param)
#-----------------------------------------------
linear_regXglmnet <-
  parsnip::linear_reg(
    penalty = tune::tune("penalty"),
    mixture = tune::tune("mixture")
  )
linear_regXglmnet <-
  parsnip::set_mode(object = linear_regXglmnet, mode = "regression")
linear_regXglmnet <-
  parsnip::set_engine(object = linear_regXglmnet, engine = "glmnet")
linear_regXglmnet_param <- tune::parameters(x = linear_regXglmnet)
linear_regXglmnet_param <-
  stats::update(
    object = linear_regXglmnet_param,
    penalty = dials::value_set(
      object = dials::penalty(range = c(-3, 0), trans = scales::log10_trans()),
      values = c(-3:-1)
    ),
    mixture = dials::value_set(
      object = dials::mixture(range = c(0, 1), trans = NULL),
      values = c(0, .3, .6, 1)
    )
  )
linear_regXglmnet_param <-
  dials::grid_regular(x = linear_regXglmnet_param, levels = 999)
linear_regXglmnet <-
  dplyr::rename(
    .data = merge(x = linear_regXglmnet, y = linear_regXglmnet_param),
    spec = x
  )
rm(linear_regXglmnet_param)
#-----------------------------------------------
linear_regXlm <- parsnip::linear_reg()
linear_regXlm <-
  parsnip::set_mode(object = linear_regXlm, mode = "regression")
linear_regXlm <-
  parsnip::set_engine(object = linear_regXlm, engine = "lm")

linear_regXlm <- tibble(spec = list(linear_regXlm))
#-----------------------------------------------
marsXearth <-
  parsnip::mars(prod_degree = tune::tune("prod_degree"))
marsXearth <-
  parsnip::set_mode(object = marsXearth, mode = "regression")
marsXearth <-
  parsnip::set_engine(object = marsXearth, engine = "earth")
marsXearth_param <- tune::parameters(x = marsXearth)
marsXearth_param <-
  stats::update(
    object = marsXearth_param,
    prod_degree = dials::value_set(
      object = dials::prod_degree(range = c(1L, 2L), trans = NULL),
      values = c(1, 2)
    )
  )
marsXearth_param <-
  dials::grid_regular(x = marsXearth_param, levels = 999)
marsXearth <-
  dplyr::rename(
    .data = merge(x = marsXearth, y = marsXearth_param),
    spec = x
  )
rm(marsXearth_param)
#-----------------------------------------------
mlpXnnet <-
  parsnip::mlp(
    hidden_units = tune::tune("hidden_units"),
    penalty = tune::tune("penalty"),
    epochs = tune::tune("epochs")
  )
mlpXnnet <-
  parsnip::set_mode(object = mlpXnnet, mode = "regression")
mlpXnnet <- parsnip::set_engine(object = mlpXnnet, engine = "nnet")
mlpXnnet_param <- tune::parameters(x = mlpXnnet)
mlpXnnet_param <-
  stats::update(
    object = mlpXnnet_param,
    hidden_units = dials::value_set(
      object = dials::hidden_units(range = c(1L, 10L), trans = NULL),
      values = c(1, seq(2, 10, 2))
    ),
    penalty = dials::value_set(
      object = dials::penalty(range = c(-3, 0), trans = scales::log10_trans()),
      values = c(-3:-1)
    ),
    epochs = dials::value_set(
      object = dials::epochs(range = c(10L, 30), trans = NULL),
      values = seq(10, 30, 5)
    )
  )
mlpXnnet_param <-
  dials::grid_regular(x = mlpXnnet_param, levels = 999)
mlpXnnet <-
  dplyr::rename(
    .data = merge(x = mlpXnnet, y = mlpXnnet_param),
    spec = x
  )
rm(mlpXnnet_param)
#-----------------------------------------------
nearest_neighborXkknn <-
  parsnip::nearest_neighbor(
    neighbors = tune::tune("neighbors"),
    weight_func = tune::tune("weight_func"),
    dist_power = tune::tune("dist_power")
  )
nearest_neighborXkknn <-
  parsnip::set_mode(object = nearest_neighborXkknn, mode = "regression")
nearest_neighborXkknn <-
  parsnip::set_engine(object = nearest_neighborXkknn, engine = "kknn")
nearest_neighborXkknn_param <-
  tune::parameters(x = nearest_neighborXkknn)
nearest_neighborXkknn_param <-
  stats::update(
    object = nearest_neighborXkknn_param,
    neighbors = dials::value_set(
      object = dials::neighbors(range = c(1L, 10L), trans = NULL),
      values = c(1, seq(2, 10, 2))
    ),
    weight_func = dials::weight_func(values = c("optimal", "gaussian", "inv")),
    dist_power = dials::value_set(
      object = dials::dist_power(range = c(1, 2), trans = NULL),
      values = seq(1, 2, .25)
    )
  )
nearest_neighborXkknn_param <-
  dials::grid_regular(x = nearest_neighborXkknn_param, levels = 999)
nearest_neighborXkknn <-
  dplyr::rename(
    .data = merge(x = nearest_neighborXkknn, y = nearest_neighborXkknn_param),
    spec = x
  )
rm(nearest_neighborXkknn_param)
#-----------------------------------------------
plsXmixOmics <-
  plsmod::pls(
    predictor_prop = tune::tune("predictor_prop"),
    num_comp = tune::tune("num_comp")
  )
plsXmixOmics <-
  parsnip::set_mode(object = plsXmixOmics, mode = "regression")
plsXmixOmics <-
  parsnip::set_engine(object = plsXmixOmics, engine = "mixOmics")
plsXmixOmics_param <- tune::parameters(x = plsXmixOmics)
plsXmixOmics_param <-
  stats::update(
    object = plsXmixOmics_param,
    predictor_prop = dials::value_set(
      object = dials::predictor_prop(range = c(0, 1), trans = NULL),
      values = seq(.5, 1, .25)
    ),
    num_comp = dials::value_set(
      object = dials::num_comp(range = c(1L, 7L), trans = NULL),
      values = seq(1, 7, 2)
    )
  )
plsXmixOmics_param <-
  dials::grid_regular(x = plsXmixOmics_param, levels = 999)
plsXmixOmics <-
  dplyr::rename(
    .data = merge(x = plsXmixOmics, y = plsXmixOmics_param),
    spec = x
  )
rm(plsXmixOmics_param)
#-----------------------------------------------
rand_forestXranger <-
  parsnip::rand_forest(
    mtry = tune::tune("mtry"),
    min_n = tune::tune("min_n")
  )
rand_forestXranger <-
  parsnip::set_mode(object = rand_forestXranger, mode = "regression")
rand_forestXranger <-
  parsnip::set_engine(object = rand_forestXranger, engine = "ranger")
rand_forestXranger_param <-
  tune::parameters(x = rand_forestXranger)
rand_forestXranger_param <-
  stats::update(
    object = rand_forestXranger_param,
    mtry = dials::value_set(
      object = dials::mtry(range = c(1L, 7L), trans = NULL),
      values = seq(1, 7, 2)
    ),
    min_n = dials::value_set(
      object = dials::min_n(range = c(2L, 40L), trans = NULL),
      values = c(2, seq(5, 40, 5))
    )
  )
rand_forestXranger_param <-
  dials::grid_regular(x = rand_forestXranger_param, levels = 999)
rand_forestXranger <-
  dplyr::rename(
    .data = merge(x = rand_forestXranger, y = rand_forestXranger_param),
    spec = x
  )
rm(rand_forestXranger_param)
#-----------------------------------------------
svm_polyXkernlab <-
  parsnip::svm_poly(
    cost = tune::tune("cost"),
    degree = tune::tune("degree"),
    scale_factor = tune::tune("scale_factor"),
    margin = tune::tune("margin")
  )
svm_polyXkernlab <-
  parsnip::set_mode(object = svm_polyXkernlab, mode = "regression")
svm_polyXkernlab <-
  parsnip::set_engine(object = svm_polyXkernlab, engine = "kernlab")
svm_polyXkernlab_param <- tune::parameters(x = svm_polyXkernlab)
svm_polyXkernlab_param <-
  stats::update(
    object = svm_polyXkernlab_param,
    cost = dials::value_set(
      object = dials::cost(range = c(-10, 5), trans = scales::log2_trans()),
      values = seq(-3, 3, 2)
    ),
    degree = dials::value_set(
      object = dials::degree(range = c(1, 3), trans = NULL),
      values = c(1:3)
    ),
    scale_factor = dials::value_set(
      object = dials::scale_factor(range = c(-10, -1), trans = scales::log10_trans()),
      values = c(-3:-1)
    ),
    margin = dials::value_set(
      object = dials::svm_margin(range = c(0, 0.2), trans = NULL),
      values = seq(0, .2, .05)
    )
  )
svm_polyXkernlab_param <-
  dials::grid_regular(x = svm_polyXkernlab_param, levels = 999)
svm_polyXkernlab <-
  dplyr::rename(
    .data = merge(x = svm_polyXkernlab, y = svm_polyXkernlab_param),
    spec = x
  )
rm(svm_polyXkernlab_param)
#-----------------------------------------------
svm_rbfXkernlab <-
  parsnip::svm_rbf(
    cost = tune::tune("cost"),
    rbf_sigma = tune::tune("rbf_sigma"),
    margin = tune::tune("margin")
  )
svm_rbfXkernlab <-
  parsnip::set_mode(object = svm_rbfXkernlab, mode = "regression")
svm_rbfXkernlab <-
  parsnip::set_engine(object = svm_rbfXkernlab, engine = "kernlab")
svm_rbfXkernlab_param <- tune::parameters(x = svm_rbfXkernlab)
svm_rbfXkernlab_param <-
  stats::update(
    object = svm_rbfXkernlab_param,
    cost = dials::value_set(
      object = dials::cost(range = c(-10, 5), trans = scales::log2_trans()),
      values = seq(-3, 3, 2)
    ),
    rbf_sigma = dials::value_set(
      object = dials::rbf_sigma(range = c(-10, 0), trans = scales::log10_trans()),
      values = c(-3:0)
    ),
    margin = dials::value_set(
      object = dials::svm_margin(range = c(0, 0.2), trans = NULL),
      values = seq(0, .2, .05)
    )
  )
svm_rbfXkernlab_param <-
  dials::grid_regular(x = svm_rbfXkernlab_param, levels = 999)
svm_rbfXkernlab <-
  dplyr::rename(
    .data = merge(x = svm_rbfXkernlab, y = svm_rbfXkernlab_param),
    spec = x
  )
rm(svm_rbfXkernlab_param)
#-----------------------------------------------

svm_linearXLiblineaR <-
  parsnip::svm_linear(
    cost = tune::tune("cost"),
    margin = tune::tune("margin")
  )
svm_linearXLiblineaR <-
  parsnip::set_mode(object = svm_linearXLiblineaR, mode = "regression")
svm_linearXLiblineaR <-
  parsnip::set_engine(object = svm_linearXLiblineaR, engine = "LiblineaR")
svm_linearXLiblineaR_param <-
  tune::parameters(x = svm_linearXLiblineaR)
svm_linearXLiblineaR_param <-
  stats::update(
    object = svm_linearXLiblineaR_param,
    cost = dials::value_set(
      object = dials::cost(range = c(-10, 5), trans = scales::log2_trans()),
      values = seq(-3, 3, 2)
    ),
    margin = dials::value_set(
      object = dials::svm_margin(range = c(0, 0.2), trans = NULL),
      values = seq(0, .2, .05)
    )
  )
svm_linearXLiblineaR_param <-
  dials::grid_regular(x = svm_linearXLiblineaR_param, levels = 999)
svm_linearXLiblineaR <-
  dplyr::rename(
    .data = merge(x = svm_linearXLiblineaR, y = svm_linearXLiblineaR_param),
    spec = x
  )
rm(svm_linearXLiblineaR_param)


#-----------------------------------------------


base_reg <-
  dplyr::tibble(
    parsnip =
      c(
        "decision_treeXrpart",
        "linear_regXglmnet",
        "linear_regXlm",
        "marsXearth",
        "mlpXnnet",
        "nearest_neighborXkknn",
        "plsXmixOmics",
        "rand_forestXranger",
        "svm_linearXLiblineaR",
        "svm_polyXkernlab",
        "svm_rbfXkernlab"
      ),
    engine =
      c(
        "decision_treeXrpart",
        "linear_regXglmnet",
        "linear_regXlm",
        "marsXearth",
        "mlpXnnet",
        "nearest_neighborXkknn",
        "plsXmixOmics",
        "rand_forestXranger",
        "svm_linearXLiblineaR",
        "svm_polyXkernlab",
        "svm_rbfXkernlab"
      ),
    spec =
      list(
        list(decision_treeXrpart),
        list(linear_regXglmnet),
        list(linear_regXlm),
        list(marsXearth),
        list(mlpXnnet),
        list(nearest_neighborXkknn),
        list(plsXmixOmics),
        list(rand_forestXranger),
        list(svm_linearXLiblineaR),
        list(svm_polyXkernlab),
        list(svm_rbfXkernlab)
      )
  ) %>% tidyr::unnest(spec)

rm(list = ls()[ls() != "base_reg"])

base_reg

base_reg$parsnip <-
  stringr::str_sub(
    string = base_reg$parsnip,
    start = 1,
    end = stringr::str_locate(string = base_reg$parsnip, pattern = "X")[, 1] -
      1
  )

base_reg$engine <-
  stringr::str_sub(
    string = base_reg$engine,
    start = stringr::str_locate(string = base_reg$engine, pattern = "X")[, 1] +
      1,
    end = stringr::str_length(base_reg$engine)
  )

base_reg <- base_reg[base_reg$engine != "mixOmics", ]

usethis::use_data(base_reg, overwrite = TRUE)
