# ------------------------------------------------------------------------------
# codigo de bd workflow para superlearner

# ------------------------------------------------------------------------------

# multinom_regXglmnet ----------------------------------------

library(scales, warn.conflicts = FALSE)
library(parsnip, warn.conflicts = FALSE)
library(dials, warn.conflicts = FALSE)
library(tune, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

multinom_regXglmnet <-
  parsnip::multinom_reg(
    penalty = tune::tune("penalty"),
    mixture = tune::tune("mixture")
  ) #
multinom_regXglmnet <-
  parsnip::set_mode(object = multinom_regXglmnet, mode = "classification")

multinom_regXglmnet <-
  parsnip::set_engine(object = multinom_regXglmnet, engine = "glmnet")

multinom_regXglmnet_param <-
  tune::parameters(x = multinom_regXglmnet)

multinom_regXglmnet_param <-
  stats::update(
    object = multinom_regXglmnet_param,
    penalty = dials::value_set(
      object = dials::penalty(range = c(-3, 0), trans = scales::log10_trans()),
      values = seq(-3, 0, .5)
    ),
    mixture = dials::value_set(
      object = dials::mixture(trans = NULL),
      values = c(0, .4, .6, 1)
    )
  )

multinom_regXglmnet_param <-
  dials::grid_regular(x = multinom_regXglmnet_param, levels = 999)

multinom_regXglmnet <-
  dplyr::rename(
    .data = merge(x = multinom_regXglmnet, y = multinom_regXglmnet_param),
    spec = x
  )

rm(multinom_regXglmnet_param)

# multinom_regXnnet ----------------------------------------

multinom_regXnnet <-
  parsnip::multinom_reg(penalty = tune::tune("penalty"))

multinom_regXnnet <-
  parsnip::set_mode(object = multinom_regXnnet, mode = "classification")

multinom_regXnnet <-
  parsnip::set_engine(object = multinom_regXnnet, engine = "nnet")

multinom_regXnnet_param <-
  tune::parameters(x = multinom_regXnnet)

multinom_regXnnet_param <-
  stats::update(
    object = multinom_regXnnet_param,
    penalty = dials::value_set(
      object = dials::penalty(range = c(-4, 0), trans = scales::log10_trans()),
      values = seq(-4, 0, .5)
    )
  )

multinom_regXnnet_param <-
  dials::grid_regular(x = multinom_regXnnet_param, levels = 999)

multinom_regXnnet <-
  dplyr::rename(
    .data = merge(x = multinom_regXnnet, y = multinom_regXnnet_param),
    spec = x
  )
rm(multinom_regXnnet_param)

# mlpXnnet ----------------------------------------

mlpXnnet <-
  parsnip::mlp(
    hidden_units = tune::tune("hidden_units"),
    penalty = tune::tune("penalty"),
    epochs = tune::tune("epochs")
  )

mlpXnnet <-
  parsnip::set_mode(object = mlpXnnet, mode = "classification")

mlpXnnet <-
  parsnip::set_engine(object = mlpXnnet, engine = "nnet")

mlpXnnet_param <-
  tune::parameters(mlpXnnet)

mlpXnnet_param <-
  stats::update(
    object = mlpXnnet_param,
    penalty = dials::value_set(
      dials::penalty(range = c(-3, 0), trans = scales::log10_trans()),
      values = c(-3:-0)
    ),
    hidden_units = dials::hidden_units(range = c(1, 5)),
    epochs = dials::value_set(dials::epochs(range = c(10, 50)),
      values = seq(10, 50, 10)
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

# decision_treeXrpart ----------------------------------------

decision_treeXrpart <-
  parsnip::decision_tree(
    cost_complexity = tune::tune("cost_complexity"),
    tree_depth = tune::tune("tree_depth"),
    min_n = tune::tune("min_n")
  )

decision_treeXrpart <-
  parsnip::set_mode(object = decision_treeXrpart, mode = "classification")

decision_treeXrpart <-
  parsnip::set_engine(object = decision_treeXrpart, engine = "rpart")

decision_treeXrpart_param <-
  tune::parameters(x = decision_treeXrpart)

decision_treeXrpart_param <-
  stats::update(
    object = decision_treeXrpart_param,
    cost_complexity = dials::value_set(
      object = dials::cost_complexity(range = c(-5, -1), trans = scales::log10_trans()),
      values = c(-5:-1)
    ),
    tree_depth = dials::tree_depth(range = c(3, 10)),
    min_n = dials::value_set(
      object = dials::min_n(range = c(1, 20)),
      values = c(1, seq(5, 20, 5))
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

# rand_forestXranger ----------------------------------------

rand_forestXranger <-
  parsnip::rand_forest(
    mtry = tune::tune("mtry"),
    trees = tune::tune("trees"),
    min_n = tune::tune("min_n")
  )

rand_forestXranger <-
  parsnip::set_mode(object = rand_forestXranger, mode = "classification")

rand_forestXranger <-
  parsnip::set_engine(
    object = rand_forestXranger,
    engine = "ranger",
    importance = "permutation"
  )

rand_forestXranger_param <-
  tune::parameters(x = rand_forestXranger)

rand_forestXranger_param <-
  stats::update(
    object = rand_forestXranger_param,
    trees = dials::value_set(
      object = dials::trees(range = c(10, 40)),
      values = c(10, 20, 40)
    ),
    mtry = dials::mtry(range = c(3, 7)),
    min_n = dials::value_set(dials::min_n(range = c(5, 20)), values = seq(5, 20, 5))
  )

rand_forestXranger_param <-
  dials::grid_regular(x = rand_forestXranger_param, levels = 999)

rand_forestXranger <-
  dplyr::rename(
    .data = merge(x = rand_forestXranger, y = rand_forestXranger_param),
    spec = x
  )
rm(rand_forestXranger_param)

# nearest_neighborXkknn ----------------------------------------

nearest_neighborXkknn <-
  parsnip::nearest_neighbor(
    neighbors = tune("neighbors"),
    weight_func = tune("weight_func"),
    dist_power = tune("dist_power")
  )

nearest_neighborXkknn <-
  parsnip::set_mode(object = nearest_neighborXkknn, mode = "classification")

nearest_neighborXkknn <-
  parsnip::set_engine(object = nearest_neighborXkknn, engine = "kknn")

nearest_neighborXkknn_param <-
  tune::parameters(x = nearest_neighborXkknn)

nearest_neighborXkknn_param <-
  stats::update(
    object = nearest_neighborXkknn_param,
    neighbors = dials::value_set(
      object = dials::neighbors(range = c(4, 20)),
      values = seq(4, 20, 4)
    ),
    weight_func = dials::weight_func(values = c("inv", "gaussian", "optimal")),
    dist_power = dials::value_set(dials::dist_power(range = c(.5, 1.5)), values = seq(.5, 1.5, .25))
  )

nearest_neighborXkknn_param <-
  dials::grid_regular(x = nearest_neighborXkknn_param, levels = 999)

nearest_neighborXkknn <-
  dplyr::rename(
    .data = merge(x = nearest_neighborXkknn, y = nearest_neighborXkknn_param),
    spec = x
  )
rm(nearest_neighborXkknn_param)


# crear base -----------------------------------------------------------------------------------------------------------

base_class <-
  dplyr::tibble(
    parsnip =
      c(
        "decision_treeXrpart",
        "mlpXnnet",
        "multinom_regXglmnet",
        "multinom_regXnnet",
        "rand_forestXranger",
        "nearest_neighborXkknn"
      ),
    engine =
      c(
        "decision_treeXrpart",
        "mlpXnnet",
        "multinom_regXglmnet",
        "multinom_regXnnet",
        "rand_forestXranger",
        "nearest_neighborXkknn"
      ),
    spec =
      list(
        list(decision_treeXrpart),
        list(mlpXnnet),
        list(multinom_regXglmnet),
        list(multinom_regXnnet),
        list(rand_forestXranger),
        list(nearest_neighborXkknn)
      )
  ) %>%
  tidyr::unnest(spec)

rm(list = ls()[ls() != "base_class"])

base_class

base_class$parsnip <-
  stringr::str_sub(
    string = base_class$parsnip,
    start = 1,
    end = stringr::str_locate(string = base_class$parsnip, pattern = "X")[, 1] -
      1
  )

base_class$engine <-
  stringr::str_sub(
    string = base_class$engine,
    start = stringr::str_locate(string = base_class$engine, pattern = "X")[, 1] +
      1,
    end = stringr::str_length(base_class$engine)
  )

base_class

usethis::use_data(base_class, overwrite = TRUE)
