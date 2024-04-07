# ------------------------------------------------------------------------------
# codigo de bd workflow

# ------------------------------------------------------------------------------

# multinom_regXglmnet ----------------------------------------

library(scales, warn.conflicts = FALSE)
library(parsnip, warn.conflicts = FALSE)
library(dials, warn.conflicts = FALSE)
library(tune, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(discrim, warn.conflicts = FALSE)
library(plsmod, warn.conflicts = FALSE)
library(rules, warn.conflicts = FALSE)

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

# discrim_linearXmda ----------------------------------------

discrim_linearXmda <-
  discrim::discrim_linear(penalty = tune::tune("penalty"))

discrim_linearXmda <-
  parsnip::set_mode(object = discrim_linearXmda, mode = "classification")

discrim_linearXmda <-
  parsnip::set_engine(object = discrim_linearXmda, engine = "mda")

discrim_linearXmda_param <-
  tune::parameters(x = discrim_linearXmda)

discrim_linearXmda_param <-
  stats::update(
    object = discrim_linearXmda_param,
    penalty = dials::value_set(
      dials::penalty(range = c(-4, 0), trans = scales::log10_trans()),
      values = seq(-4, 0, .5)
    )
  )

discrim_linearXmda_param <-
  dials::grid_regular(x = discrim_linearXmda_param, levels = 999)

discrim_linearXmda <-
  dplyr::rename(
    .data = merge(x = discrim_linearXmda, y = discrim_linearXmda_param),
    spec = x
  )
rm(discrim_linearXmda_param)

# marsXearth ----------------------------------------

marsXearth <-
  parsnip::mars(
    num_terms = tune::tune("num_terms"),
    prod_degree = tune::tune("prod_degree"),
    prune_method = tune::tune("prune_method")
  )

marsXearth <-
  parsnip::set_engine(object = marsXearth, engine = "earth")

marsXearth <-
  parsnip::set_mode(object = marsXearth, mode = "classification")

marsXearth_param <-
  tune::parameters(marsXearth)

marsXearth_param <-
  stats::update(
    object = marsXearth_param,
    num_terms = dials::num_terms(range = c(2, 7)),
    prod_degree = dials::prod_degree(range = c(1, 2)),
    prune_method = dials::prune_method(values = c("backward", "none", "seqrep", "forward"))
  )

marsXearth_param <-
  dials::grid_regular(x = marsXearth_param, levels = 999)

marsXearth <-
  dplyr::rename(
    .data = merge(x = marsXearth, y = marsXearth_param),
    spec = x
  )
rm(marsXearth_param)

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
      dials::penalty(range = c(-2, 0), trans = scales::log10_trans()),
      values = c(-2:-0)
    ),
    hidden_units = dials::hidden_units(range = c(1, 5)),
    epochs = dials::value_set(dials::epochs(range = c(10, 50)), values = seq(10, 50, 10))
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
    tree_depth = dials::tree_depth(range = c(1, 5)),
    min_n = dials::value_set(
      object = dials::min_n(range = c(5, 20)),
      values = seq(5, 20, 5)
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
  parsnip::set_engine(object = rand_forestXranger, engine = "ranger")

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


# naive_BayesXnaivebayes ----------------------------------------

naive_BayesXnaivebayes <-
  discrim::naive_Bayes(
    smoothness = tune::tune("smoothness"),
    Laplace = tune::tune("Laplace")
  )

naive_BayesXnaivebayes <-
  parsnip::set_mode(object = naive_BayesXnaivebayes, mode = "classification")

naive_BayesXnaivebayes <-
  parsnip::set_engine(object = naive_BayesXnaivebayes, engine = "naivebayes")

naive_BayesXnaivebayes_param <-
  tune::parameters(x = naive_BayesXnaivebayes)

naive_BayesXnaivebayes_param <-
  stats::update(
    object = naive_BayesXnaivebayes_param,
    smoothness = dials::value_set(dials::smoothness(), values = seq(.5, 1.5, .25)),
    Laplace = dials::value_set(dials::Laplace(), values = seq(0, 3, 1))
  )

naive_BayesXnaivebayes_param <-
  dials::grid_regular(x = naive_BayesXnaivebayes_param, levels = 999)

naive_BayesXnaivebayes <-
  dplyr::rename(
    .data = merge(x = naive_BayesXnaivebayes, y = naive_BayesXnaivebayes_param),
    spec = x
  )
rm(naive_BayesXnaivebayes_param)

# plsXmixOmics ----------------------------------------

plsXmixOmics <-
  plsmod::pls(
    num_comp = tune::tune(),
    predictor_prop = tune::tune()
  )

plsXmixOmics <-
  parsnip::set_engine(object = plsXmixOmics, engine = "mixOmics")

plsXmixOmics <-
  parsnip::set_mode(object = plsXmixOmics, mode = "classification")

plsXmixOmics_param <-
  tune::parameters(x = plsXmixOmics)

plsXmixOmics_param <-
  stats::update(
    object = plsXmixOmics_param,
    predictor_prop = dials::value_set(dials::predictor_prop(), values = seq(.25, 1, .25)),
    num_comp = dials::value_set(dials::num_comp(range = c(1, 5)), values = 1:5)
  )

plsXmixOmics_param <-
  dials::grid_regular(x = plsXmixOmics_param, levels = 999)

plsXmixOmics <-
  dplyr::rename(
    .data = merge(x = plsXmixOmics, y = plsXmixOmics_param),
    spec = x
  )

rm(plsXmixOmics_param)

# svm_rbfXkernlab ----------------------------------------

svm_rbfXkernlab <-
  parsnip::svm_rbf(
    cost = tune::tune(),
    rbf_sigma = tune::tune(),
    margin = tune::tune("svm_margin")
  )

svm_rbfXkernlab <-
  parsnip::set_engine(object = svm_rbfXkernlab, engine = "kernlab")

svm_rbfXkernlab <-
  parsnip::set_mode(object = svm_rbfXkernlab, mode = "classification")

svm_rbfXkernlab_param <-
  tune::parameters(x = svm_rbfXkernlab)

svm_rbfXkernlab_param <-
  stats::update(
    object = svm_rbfXkernlab_param,
    cost = dials::value_set(
      object = dials::cost(
        range = c(-2, 2),
        trans = scales::log10_trans()
      ),
      values = c(-2:2)
    ),
    rbf_sigma = dials::value_set(
      object = dials::rbf_sigma(
        range = c(-2, 0),
        trans = scales::log10_trans()
      ),
      values = c(-2:0)
    ),
    svm_margin = dials::value_set(object = dials::svm_margin(), values = c(0, .1, .2))
  )

svm_rbfXkernlab_param <-
  dials::grid_regular(x = svm_rbfXkernlab_param, levels = 999)

svm_rbfXkernlab <-
  dplyr::rename(
    .data = merge(x = svm_rbfXkernlab, y = svm_rbfXkernlab_param),
    spec = x
  )
rm(svm_rbfXkernlab_param)

# rule_fitXxrf ----------------------------------------

rule_fitXxrf <-
  rules::rule_fit(
    mtry = tune::tune("mtry_prop"),
    trees = tune::tune("trees"),
    min_n = 10,
    tree_depth = 7,
    learn_rate = tune::tune("learn_rate"),
    loss_reduction = tune::tune("loss_reduction"),
    sample_size = .75,
    penalty = .1
  )

rule_fitXxrf <-
  parsnip::set_engine(object = rule_fitXxrf, engine = "xrf")

rule_fitXxrf <-
  parsnip::set_mode(object = rule_fitXxrf, mode = "classification")

rule_fitXxrf_param <- tune::parameters(x = rule_fitXxrf)

rule_fitXxrf_param <-
  stats::update(
    object = rule_fitXxrf_param,
    mtry_prop = dials::value_set(object = rules::mtry_prop(), values = c(.5, .75, 1)),
    trees = dials::value_set(dials::trees(range = c(15, 30)), values = c(15, 20, 30)),
    learn_rate = dials::value_set(
      object = dials::learn_rate(range = c(-3, 0), trans = scales::log10_trans()),
      values = c(-3, -2, -1, 0)
    ),
    loss_reduction = dials::value_set(
      dials::loss_reduction(range = c(-3, 0), trans = scales::log10_trans()),
      values = c(-3, -2, -1, 0)
    )
  )

rule_fitXxrf_param <-
  dials::grid_regular(x = rule_fitXxrf_param, levels = 999)

rule_fitXxrf <-
  dplyr::rename(
    .data = merge(x = rule_fitXxrf, y = rule_fitXxrf_param),
    spec = x
  )
rm(rule_fitXxrf_param)

# crear base -----------------------------------------------------------------------------------------------------------

base_class_otras <-
  dplyr::tibble(
    parsnip =
      c(
        "decision_treeXrpart",
        "discrim_linearXmda",
        "marsXearth",
        "mlpXnnet",
        "multinom_regXglmnet",
        "multinom_regXnnet",
        "naive_BayesXnaivebayes",
        "plsXmixOmics",
        "rand_forestXranger",
        "rule_fitXxrf",
        "svm_rbfXkernlab"
      ),
    engine =
      c(
        "decision_treeXrpart",
        "discrim_linearXmda",
        "marsXearth",
        "mlpXnnet",
        "multinom_regXglmnet",
        "multinom_regXnnet",
        "naive_BayesXnaivebayes",
        "plsXmixOmics",
        "rand_forestXranger",
        "rule_fitXxrf",
        "svm_rbfXkernlab"
      ),
    spec =
      list(
        list(decision_treeXrpart),
        list(discrim_linearXmda),
        list(marsXearth),
        list(mlpXnnet),
        list(multinom_regXglmnet),
        list(multinom_regXnnet),
        list(naive_BayesXnaivebayes),
        list(plsXmixOmics),
        list(rand_forestXranger),
        list(rule_fitXxrf),
        list(svm_rbfXkernlab)
      )
  )

rm(list = ls()[ls() != "base_class_otras"])

base_class_otras

base_class_otras$parsnip <-
  stringr::str_sub(
    string = base_class_otras$parsnip,
    start = 1,
    end = stringr::str_locate(string = base_class_otras$parsnip, pattern = "X")[, 1] -
      1
  )

base_class_otras$engine <-
  stringr::str_sub(
    string = base_class_otras$engine,
    start = stringr::str_locate(string = base_class_otras$engine, pattern = "X")[, 1] +
      1,
    end = stringr::str_length(base_class_otras$engine)
  )

base_class_otras <- base_class_otras %>% tidyr::unnest(spec)

usethis::use_data(base_class_otras, overwrite = TRUE)
