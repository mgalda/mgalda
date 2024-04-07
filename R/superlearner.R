#' superlearner
#'
#' @rdname superlearner
#' @name  superlearner
#' @keywords internal
#'
#' @examples
#'
#'
#' \dontrun{
#' dataset_multiclass <- datalearn$penguins_lite %>% tidyr::drop_na()
#'
#' dataset_class <- datalearn$wa_churn %>% tidyr::drop_na()
#'
#' dataset_class <- nearmiss(dataset_class, class_var = churn)
#'
#' dataset_reg <- datalearn$housing_market %>% tidyr::drop_na()
#'
#' init_multiclass <-
#'   init_model(
#'     dataset = dataset_multiclass,
#'     outcome = species
#'   )
#'
#' init_class <-
#'   init_model(
#'     dataset = dataset_class,
#'     outcome = churn
#'   )
#'
#' init_reg <-
#'   init_model(
#'     dataset = dataset_reg,
#'     outcome = sales_price
#'   )
#'
#'
#' ## ensemble_models
#'
#' ensemble_multiclass <-
#'   ensemble_models(
#'     x = init_multiclass,
#'     times = 4,
#'     rep = 4,
#'     cross = F
#'   )
#'
#' ensemble_class <-
#'   ensemble_models(
#'     x = init_class,
#'     times = 4,
#'     cross = F,
#'     rep = 4
#'   )
#'
#' ensemble_reg <-
#'   ensemble_models(
#'     x = init_reg,
#'     cross = F,
#'     times = 4,
#'     rep = 4
#'   )
#'
#' ensemble_reg$full_fits
#' ensemble_multiclass$full_fits
#' ensemble_class$full_fits
#'
#'
#' fitensemble_multiclass <-
#'   fit_libraries(x = ensemble_multiclass)
#'
#' fitensemble_multiclass
#'
#' fitensemble_class <-
#'   fit_libraries(x = ensemble_class)
#'
#' fitensemble_class
#'
#' fitensemble_reg <-
#'   fit_libraries(x = ensemble_reg)
#'
#' fitensemble_reg
#'
#' stackingensemble_multiclass <-
#'   build_stacking(x = fitensemble_multiclass)
#'
#' stackingensemble_multiclass
#'
#' stackingensemble_class <-
#'   build_stacking(x = fitensemble_class)
#'
#' stackingensemble_class
#'
#' stackingensemble_reg <-
#'   build_stacking(x = fitensemble_reg, allow_par = T)
#'
#' stackingensemble_reg
#'
#' softvotingensemble_multiclass <-
#'   build_softvoting(x = fitensemble_multiclass)
#'
#' softvotingensemble_class <-
#'   build_softvoting(x = fitensemble_class)
#'
#' softvotingensemble_reg <-
#'   build_softvoting(x = fitensemble_reg)
#'
#' table(
#'   dataset_class$churn,
#'   predict(
#'     object = stackingensemble_class,
#'     new_data = dataset_class
#'   )$.pred_class
#' )
#'
#' table(
#'   dataset_multiclass$species,
#'   predict(object = stackingensemble_multiclass,
#'           new_data = dataset_multiclass)$.pred_class
#' )
#'
#' predict(object = softvotingensemble_reg, new_data = dataset_reg[1:10, ])
#'
#' table(
#'   dataset_class$churn,
#'   predict(object = softvotingensemble_class, new_data = dataset_class)$.pred_class
#' )
#'
#' table(
#'   dataset_multiclass$species,
#'   predict(object = softvotingensemble_multiclass,
#'           new_data = dataset_multiclass)$.pred_class
#' )
#'
#' rec_multiclass <-
#'   recipes::recipe(species ~ ., data = dataset_multiclass)
#'
#' rec_class <-
#'   recipes::recipe(churn ~ ., data = dataset_class)
#'
#' rec_reg <-
#'   recipes::recipe(sales_price ~ ., data = dataset_reg)
#'
#'
#' bagging_multiclass <-
#'   bagging(x = rec_multiclass, data = dataset_multiclass, times = 30)
#' bagging_reg <- bagging(x = rec_reg, data = dataset_reg)
#' bagging_class <- bagging(x = rec_class, data = dataset_class)
#'
#' bagging_multiclass
#' bagging_reg
#' bagging_class
#'
#'
#' table(
#'   dataset_multiclass$species,
#'   predict(bagging_multiclass, dataset_multiclass)$.pred_class
#' )
#' table(
#'   dataset_class$churn,
#'   predict(bagging_class, dataset_class)$.pred_class
#' )
#' yardstick::rsq_trad_vec(truth = dataset_reg$sales_price,
#'                         predict(bagging_reg, dataset_reg)$.pred)
#'
#' bagging_multiclass <-
#'   bagging(species ~ ., data = dataset_multiclass, times = 30)
#' bagging_class <-
#'   bagging(churn ~ ., data = dataset_class)
#' bagging_reg <-
#'   bagging(sales_price ~ .,data = dataset_reg)
#'
#' table(
#'   dataset_multiclass$species,
#'   predict(bagging_multiclass, dataset_multiclass)$.pred_class
#' )
#' table(
#'   dataset_class$churn,
#'   predict(bagging_class, dataset_class)$.pred_class
#' )
#' yardstick::rsq_trad_vec(truth = dataset_reg$sales_price,
#'                         predict(bagging_reg, dataset_reg)$.pred)
#'
#' rm(list = ls())
#'}
#'
#'
#'

NULL

# inicial -----------------------------------------------------------------

#' @export
new_ensemble_models <- function(dataset, ...) {
  UseMethod("new_ensemble_models")
}

#' @export
new_ensemble_models.default <-
  function(dataset, ...) {
    generic_default()
  }

#' @export
new_ensemble_models.data.frame <-
  function(dataset,
           outcome,
           formula,
           splits,
           model_mode) {
    new_ensemble_models <- structure(
      list(
        full_fits = structure(
          list(
            wflow = list(),
            wflow_id = character(0),
            parsnip = character(0),
            engine = character(0)
          ),
          class = c("tbl_df", "tbl", "data.frame"),
          row.names = integer(0)
        ),
        metricas = list(),
        base_data = list(
          outcome = outcome,
          formula = formula,
          dataset = dataset,
          splits =  splits
        ),
        model_mode = "init"
      ),
      class = c("new_ensemble_models")
    )


    purrr::modify_at(
      .x = new_ensemble_models,
      "base_data",
      ~ list(
        outcome = outcome,
        formula = formula,
        dataset = dataset,
        splits =  splits
      )
    ) %>%
      purrr::modify_at("model_mode", ~model_mode)
  }

#' @export
print.new_ensemble_models <- function(x) {
  cat_subtitle2("Iniciando modelo de { get_mode(x) } ensamblado")
  cat_subtitle1("Resumen de la base de datos")
  dataset <- x$base_data$dataset
  print(data_summary(dataset))
  invisible(NULL)
}


#' @export
init_model <- function(dataset, outcome) {
  reqlibs_impl()
  cli::cli_progress_step("Check data")
  outcome <- enexpr(outcome)
  outcome_name <- lang2str(outcome)
  check_inherits(
    x = outcome,
    what = "name",
    severity = "stop"
  )
  check_inherits(dataset, "data.frame", severity = "stop")
  # do_call(
  #   f = "assert_cols_exist",
  #   .args = c(data = list(dataset), outcome_name, severity = "stop"),
  #   .env = parent.frame()
  # )

  if (any(are_chr(dataset))) {
    dataset[, are_chr(dataset)] <-
      purrr::map_dfc(dataset[, are_chr(dataset)], as.factor)
  }
  model_mode <- set_mode_model(dataset, outcome_name)
  cli::cli_progress_step("model mode: {model_mode}")
  outcome <- as_lazy(outcome)
  if (model_mode[1] == "classification") {
    attr(outcome, "lvls") <- levels(dataset[[outcome_name]])
  }
  formula_outcome <- paste0(outcome_name, " ~ .")
  formula_outcome <- formula(formula_outcome, to_env(dataset))

  out <- new_ensemble_models(
    outcome = outcome,
    formula = formula_outcome,
    dataset = dataset,
    splits = do_call(
      f = rsample::initial_split,
      .args = list(data = dataset, strata = outcome_name)
    ),
    model_mode = model_mode
  )
  print(out)
  invisible(out)
}

# ensemble ----------------------------------------------------------------

#' @export
ensemble_models <-
  function(x,
           times = 4,
           rep = 4,
           cross = FALSE,
           add_step = NULL,
           small = FALSE) {
    UseMethod("ensemble_models")
  }

#' @export
ensemble_models.default <- function(x, ...) {
  generic_default()
}

#' @export
ensemble_models.new_ensemble_models <- function(x,
                                                times = 5,
                                                rep = 5,
                                                cross = FALSE,
                                                add_step = NULL,
                                                small = FALSE) {
  reqlibs_impl()
  curr_args <- match.call()
  cli::cli_progress_step("Validacion y setup")
  curr_args <- call_modify(call = curr_args, new_args = list(x = x))

  curr_args <-
    modify_args(x = ensemble_models, forms = curr_args)

  args_call <-
    modify_args(x = random_rset, forms = curr_args)

  args_call$outcome <- outcome_chr(x)

  call2specs <-
    modify_args(x = create_specs, forms = curr_args)

  cli::cli_progress_step("Crear Specs")
  call2specs$x <- x

  wflow_base <-
    try(eval_expr(do.call("create_specs", !!as_l(call2specs))))

  args_call$dataset <- get_train(x)

  cli::cli_progress_step("Crear resamples")

  rset_data <-
    make_call("random_rset", .args = as_l(args_call))
  rset_data <- eval(rset_data)

  if (cross) {
    wflow_base <- tidyr::expand_grid(wflow_base, rset_data)
  } else {
    wflow_base <- bind_cols(wflow_base, rset_data)
  }

  x$full_fits <-
    bind_rows(
      x$full_fits,
      wflow_base %>%
        dplyr::transmute(
          wflow = spec,
          wflow_id = paste(stringr::str_pad(row_number(), 3, pad = "0"), id, sep = "_"),
          parsnip,
          engine,
          splits
        )
    )
  cli::cli_progress_done()
  cli::cli_progress_step("Configurando preprocesos")
  f_envir <- env_curr()
  x <- models_frm(x = x, envir = f_envir)

  x02 <- x01 <- x

  if (small) {
    f <- sample(1:0, nrow(x02$full_fits), replace = T, c(.3, .7))
  } else {
    f <- sample(1:0, nrow(x02$full_fits), replace = T, c(.7, .3))
  }

  x01$full_fits <- x01$full_fits[f == 1, ]
  x02$full_fits <- x02$full_fits[f == 0, ]
  nn <- 1
  cli::cli_progress_step("Preprocesos { nn }")
  x01 <- build_preprocess(x01, small = small, add_step = add_step)
  nn <- nn + 1
  cli::cli_progress_update()
  x02 <- simple_rec(x02)

  x$full_fits <-
    rbind(
      x01$full_fits[!map_lgl(x01$full_fits$wflow, ~ inherits(.x, "try-error")), ],
      x02$full_fits[!map_lgl(x02$full_fits$wflow, ~ inherits(.x, "try-error")), ]
    )

  x01$full_fits <-
    x01$full_fits[map_lgl(x01$full_fits$wflow, ~ inherits(.x, "try-error")), ]

  x02$full_fits <-
    x02$full_fits[map_lgl(x02$full_fits$wflow, ~ inherits(.x, "try-error")), ]

  nn <- nn + 1
  cli::cli_progress_update()

  x <- repro_prepros(x, x01, x02)


  if (get_mode(x)[1] == "classification") {
    nlvss <-
      try(map_dbl(
        x$full_fits$wflow,
        ~ map_dbl(.x$pre$mold$outcomes, n_unique)
      ))

    x$full_fits <-
      x$full_fits[nlvss == length(attr(outcome_quo(x), "lvls")), ]
  }

  cli::cli_progress_done()
  if (small) {
    attr(x$full_fits, "preprocess") <- "small"
  } else {
    attr(x$full_fits, "preprocess") <- "standard"
  }
  class(x) <- c("ensemble_models")
  x$trained <- FALSE
  cli::cli_alert_success("{ nrow(x$full_fits) } modelos ensamblados.")
  x
}

#' @export
print.ensemble_models <- function(x) {
  mods <-
    x$full_fits %>%
    count(engine, parsnip) %>%
    as_tbl() %>%
    arrange(engine, parsnip) %>%
    mutate(parsnip = paste(parsnip, " (", n, ")", sep = "")) %>%
    group_by(engine) %>%
    summarise(parsnip = paste(parsnip, collapse = ", ")) %>%
    dplyr::transmute(unit = paste0(cli::style_bold(engine), ", ", parsnip)) %>%
    pull(unit)

  if (x$trained) {
    .trained <- "TRUE"
  } else {
    .trained <- "FALSE"
  }

  enf <- function(x) {
    cli::col_yellow(cli::style_bold(x))
  }

  cat_title_head("{ paste0(class(x),collapse = '-') } ")
  cli::cli_inform(paste0("Entrenado: ", enf(" { .trained } ")))
  cli::cli_inform(paste0("Modo: ", enf("{ x$model_mode }")))
  cli::cli_inform(paste0("Total Modelos: ", enf("{ nrow(x$full_fits) }")))
  cli::cli_bullets(text = purrr::set_names(mods, rep("*", length(mods))))

  cat_subtitle1("Data Summary")
  print(data_summary(x$base_data$dataset))
  invisible(NULL)
}

#' @export
manual_ensemble_wflows <- function(preproc, models, cross = TRUE) {
  wflows <- workflowsets::workflow_set(preproc, models, cross) %>%
    select(wflow_id, info) %>%
    unnest(info) %>%
    pull(workflow)

  wflows <-
    map(.x = wflows, .f = ~ finalize_blueprint(.x))
  map(
    .x = wflows,
    .f = ~ try(workflows::.fit_pre(
      workflow = .x,
      data = extract_preprocessor(.x)$template
    ))
  )
}

#' @export
manual_ensemble_models <-
  function(dataset, outcome, workflows, resamples) {
    .f_parsnip <- function(x) {
      map_chr(x, ~ class(workflows::extract_spec_parsnip(.x))[1])
    }
    .f_engine <- function(x) {
      map_chr(x, ~ workflows::extract_spec_parsnip(.x)$engine)
    }
    .f_formulas <- function(x) {
      map(x, function(.x) {
        drec <- workflows::extract_preprocessor(.x)$var_info
        r_drec <-
          paste0(drec$variable[drec$role == "predictor"], collapse = " + ")
        as.formula(paste(drec$variable[drec$role == "outcome"], "~", r_drec))
      })
    }

    reqlibs_impl()
    cli::cli_progress_step("Check data")
    outcome <- enexpr(outcome)
    outcome_name <- lang2str(outcome)
    check_inherits(
      x = outcome,
      what = "name",
      severity = "stop"
    )
    check_inherits(dataset, "data.frame", severity = "stop")
    if (any(are_chr(dataset))) {
      dataset[, are_chr(dataset)] <- purrr::map_dfc(dataset[
        ,
        are_chr(dataset)
      ], as.factor)
    }
    model_mode <- set_mode_model(dataset, outcome_name)
    cli::cli_progress_step("model mode: {model_mode}")
    outcome <- as_lazy(outcome)
    if (model_mode[1] == "classification") {
      attr(outcome, "lvls") <- levels(dataset[[outcome_name]])
    }
    formula_outcome <- paste0(outcome_name, " ~ .")
    formula_outcome <- formula(formula_outcome, to_env(dataset))
    out <-
      new_ensemble_models(
        outcome = outcome,
        formula = formula_outcome,
        dataset = dataset,
        splits = do_call(
          f = rsample::initial_split,
          .args = list(data = dataset, strata = outcome_name)
        ),
        model_mode = model_mode
      )
    out[["trained"]] <- FALSE
    out[["full_fits"]] <-
      structure(
        list(
          wflow = workflows,
          wflow_id = resamples$id,
          parsnip = .f_parsnip(workflows),
          engine = .f_engine(workflows),
          splits = resamples$splits,
          formulas = .f_formulas(workflows)
        ),
        row.names = integer(0),
        class = c(
          "tbl_df",
          "tbl", "data.frame"
        ),
        preprocess = "standard"
      )
    out[["full_fits"]] <- as_tbl(as.list(out[["full_fits"]]))
    class(out) <- "ensemble_models"
    invisible(out)
  }



# fit_libs ----------------------------------------------------------------

#' @export
fit_libraries <- function(x, ...) {
  UseMethod("fit_libraries")
}

#' @export
fit_libraries.default <- function(x, ...) {
  generic_default()
}

#' @export
fit_libraries.ensemble_models <- function(x) {
  allow_par <- F
  reqlibs_impl()

  members <- x$full_fits

  cli::cli_progress_step("Fit members", spinner = T)

  if (allow_par) {
    future::plan(future::multisession, workers = 5)
    libspacks_ch <- libs_packs()

    members$wflow <-
      furrr::future_map2(
        .x = members$wflow,
        .y = members$splits,
        .f = ~ fit_safe(
          object = .x,
          data = .y
        ),
        .options = furrr::furrr_options(seed = T, packages = libspacks_ch),
        .env_globals = env_curr()
      )
  } else {
    members$wflow <-
      purrr::map2(
        .x = members$wflow,
        .y = members$splits,
        .f = ~ fit_safe(
          object = .x,
          data = .y
        )
      )
  }

  members <-
    members[!map_lgl(members$wflow, ~ class(.x)[1] == "try-error"), ]

  members <-
    members[!map_lgl(members$wflow, is.null), ]

  cli::cli_progress_step("obtener metricas por miembros", spinner = T)

  if (allow_par) {
    mets <-
      furrr::future_map2(
        .x = members$wflow,
        .y = members$wflow_id,
        .f = ~ tryCatch(
          expr = eval_models(
            object = .x,
            id = .y,
            test_data = get_test(x)
          ),
          error = function(e) {
            NULL
          }
        ),
        .options = furrr::furrr_options(seed = T, packages = libspacks_ch),
        .env_globals = env_curr(),
        .id = "row"
      )
    future::plan(future::sequential)
  } else {
    mets <-
      purrr::map2(
        .x = members$wflow,
        .y = members$wflow_id,
        .f = ~ tryCatch(
          expr = eval_models(
            object = .x,
            id = .y,
            test_data = get_test(x)
          ),
          error = function(e) {
            NULL
          }
        )
      )
  }

  mets <- purrr::compact(mets)

  ninm <- purrr::map_dbl(mets, ~ sum(is_nonnum(.x[[3]])))

  mets <- mets[ifelse(min(ninm) < ninm, F, T)]


  mets <-
    purrr::reduce(mets,
                  .f = full_join,
                  by = c("metric", "dir")
    ) %>%
    drop_na() %>%
    dplyr::select(.data$metric, .data$dir, where(~ sum(is_nonnum(.x)) == 0))

  cli::cli_progress_step("filtrar por metricas")

  if (get_mode(x)[1] == "classification") {
    wflow_id_vec <-
      mets %>%
      dplyr::select(!dir) %>%
      pivot_longer(2:ncol(.), names_to = "wflow_id", values_to = "score") %>%
      pivot_wider(names_from = metric, values_from = score) %>%
      filter(roc_auc > .55) %>%
      pull(wflow_id)

    members <-
      members %>%
      mutate(row = row_number()) %>%
      filter(wflow_id %in% wflow_id_vec) %>%
      mutate(
        row = NULL,
        preds = map(
          .x = wflow,
          .f = ~ standardized_predict(
            object = .x,
            get_train(x),
            lvls = attr(outcome_quo(x), "lvls")
          )
        ),
        preds_test = map(
          .x = wflow,
          .f = ~ standardized_predict(
            object = .x,
            get_test(x),
            lvls = attr(outcome_quo(x), "lvls")
          )
        )
      ) %>%
      filter(map_lgl(preds, ~ inherits(.x, "tbl"))) %>%
      filter(map_lgl(preds_test, ~ inherits(.x, "tbl"))) %>%
      mutate(
        preds = map(
          .x = preds,
          .f = ~ try(bind_cols(.x, get_train(x) %>%
                                 dplyr::select(!!outcome_expr(x))) %>%
                       mutate(obs = row_number()),
                     silent = T
          )
        ),
        preds_test = map(
          .x = preds_test,
          .f = ~ try(bind_cols(.x, get_test(x) %>%
                                 dplyr::select(!!outcome_expr(x))) %>%
                       mutate(obs = row_number()),
                     silent = T
          )
        )
      ) %>%
      filter(!map_lgl(preds, ~ inherits(.x, "try-error"))) %>%
      filter(!map_lgl(preds_test, ~ inherits(.x, "try-error"))) %>%
      mutate(wflow_id = stringr::str_c(
        "wflow",
        stringr::str_pad(row_number(), stringr::str_length(nrow(.)), pad = "0"),
        sep = ""
      )) %>%
      dplyr::select(
        wflow,
        wflow_id,
        parsnip,
        engine,
        preds,
        preds_test
      )
  } else if (get_mode(x) == "regression") {
    wflow_id_vec <-
      mets %>%
      dplyr::select(!dir) %>%
      pivot_longer(2:ncol(.), names_to = "wflow_id", values_to = "score") %>%
      pivot_wider(names_from = metric, values_from = score) %>%
      filter(nrmse < 1.5) %>%
      pull(wflow_id)

    members <-
      members %>%
      mutate(row = row_number()) %>%
      filter(wflow_id %in% wflow_id_vec) %>%
      mutate(
        row = NULL,
        preds = map(
          .x = wflow,
          .f = ~ predict_misleading(object = .x, get_train(x))
        ),
        preds_test = map(
          .x = wflow,
          .f = ~ predict_misleading(object = .x, get_test(x))
        )
      ) %>%
      filter(map_lgl(preds, ~ inherits(.x, "tbl"))) %>%
      filter(map_lgl(preds_test, ~ inherits(.x, "tbl"))) %>%
      mutate(
        preds = map(
          .x = preds,
          .f = ~ try(bind_cols(.x, get_train(x) %>%
                                 dplyr::select(!!outcome_expr(x))) %>%
                       mutate(obs = row_number()),
                     silent = T
          )
        ),
        preds_test = map(
          .x = preds_test,
          .f = ~ try(bind_cols(.x, get_test(x) %>%
                                 dplyr::select(!!outcome_expr(x))) %>%
                       mutate(obs = row_number()),
                     silent = T
          )
        )
      ) %>%
      filter(!map_lgl(preds, ~ inherits(.x, "try-error"))) %>%
      filter(!map_lgl(preds_test, ~ inherits(.x, "try-error"))) %>%
      mutate(wflow_id = stringr::str_c(
        "wflow",
        stringr::str_pad(row_number(), stringr::str_length(nrow(.)), pad = "0"),
        sep = ""
      )) %>%
      dplyr::select(
        wflow,
        wflow_id,
        parsnip,
        engine,
        preds,
        preds_test
      )
  }
  cli::cli_progress_done()
  x$full_fits <- members

  x$trained <- TRUE
  cli::cli_alert_success("{ nrow(x$full_fits) } miembros filtrados")
  x
}

# build_mods --------------------------------------------------------------

#' @export
build_stacking <- function(x, allow_par = F) {
  reqlibs_impl()
  full_fits <- x$full_fits

  cli::cli_progress_step("Setup model")

  preds_sl <- suppressWarnings(nested_preds(x))
  split_sl <-
    initial_split(preds_sl, strata = outcome_chr(x))
  premeta_workflow <-
    meta_wflow(x, training(split_sl))
  if (allow_par) {
    libspacks_ch <- libs_packs()

    control <- tune::control_grid(
      save_pred = TRUE,
      save_workflow = TRUE,
      allow_par = TRUE,
      pkgs = libspacks_ch,
      extract = function(x) {
        x %>%
          workflows::extract_fit_parsnip() %>%
          purrr::pluck("fit")
      }
    )
  } else {
    control <- tune::control_grid(
      save_pred = TRUE,
      save_workflow = TRUE,
      allow_par = FALSE,
      extract = function(x) {
        x %>%
          workflows::extract_fit_parsnip() %>%
          purrr::pluck("fit")
      }
    )
  }

  if (x$model_mode[1] == "classification") {
    metric <-
      yardstick::metric_set(yardstick::accuracy)
  } else if (x$model_mode[1] == "regression") {
    metric <-
      yardstick::metric_set(yardstick::rmse)
  }

  cli::cli_progress_step("Fit candidatos", spinner = T)
  stack_model <-
    tune::tune_grid(
      object = premeta_workflow,
      resamples = kfolds(
        dataset = training(split_sl),
        times = 3,
        rep = 5,
        outcome = !!outcome_expr(x),
        reverse = F
      ),
      metrics = metric,
      grid = purrr::cross_df(list(
        penalty = 10^(-4:-1),
        mixture = c(1, .6, 0)
      )),
      control = control
    )

  metric <- tune::.get_tune_metric_names(stack_model)[1]

  cli::cli_progress_step("seleccionar mejor candidato")

  best_param <- tune::select_best(stack_model, metric = metric)

  coefs <-
    premeta_workflow %>%
    workflows::extract_spec_parsnip() %>%
    tune::finalize_model(best_param) %>%
    generics::fit(
      formula = x$base_data$formula,
      data = training(split_sl)
    )

  x[["coefs"]] <- coefs

  x[["penalty"]] <- list(
    penalty = best_param$penalty,
    mixture = best_param$mixture,
    metric = metric
  )

  x[["equations"]] <-
    get_expressions(coefs, x$model_mode)

  x[["data_stack"]] <-
    list(data = preds_sl, split = split_sl)

  x[["cols_map"]] <- names(preds_sl)
  x[["wflow_id"]] <-
    unique(stringr::str_sub(
      x$cols_map[x$cols_map != outcome_chr(x)],
      stringr::str_locate(x$cols_map[x$cols_map != outcome_chr(x)], "wflow")[, 1],
      stringr::str_length(x$cols_map[x$cols_map != outcome_chr(x)])
    ))

  x$full_fits <-
    x$full_fits[x$full_fits$wflow_id %in% x$wflow_id, ]

  class(x) <- c("stacking", class(x))
  x[["metricas"]] <- list(
    fit_metrics = glmnet_metrics(stack_model),
    test_eval = eval_models(
      object = x,
      test_data = get_test(x)
    )
  )
  cli::cli_progress_done()
  cli::cli_alert_success("Fit modelo con { nrow(x$full_fits) } miembros")
  x
}

#' @export
build_softvoting <- function(x, rango = c(3, 8)) {
  reqlibs_impl()
  cli::cli_progress_step("Setup model")
  members <- x$full_fits

  cli::cli_progress_step("obtener metricas por miembros", spinner = T)
  mets <-
    map2(
      .x = members$wflow,
      .y = members$wflow_id,
      .f = ~ tryCatch(
        expr = eval_models(
          object = .x,
          id = .y,
          test_data = get_test(x)
        ),
        error = function(e) {
          NULL
        }
      )
    ) %>%
    map2(
      .y = members$wflow_id,
      .f = ~ try(.x %>% mutate({{ .y }} := case_when(
        dir == "minimize" ~ !!str2lang(.y) * -1,
        dir == "zero" ~ abs(!!str2lang(.y)) * -1,
        T ~ !!str2lang(.y)
      )), silent = T)
    ) %>%
    compact()

  ninm <- map_dbl(mets, ~ sum(is_nonnum(.x[[3]])))

  mets <- mets[ifelse(min(ninm) < ninm, F, T)]

  cli::cli_progress_step("Metricas de posibles modelos", spinner = T)
  mets <-
    purrr::reduce(mets,
                  .f = full_join,
                  by = c("metric", "dir")
    ) %>%
    drop_na() %>%
    dplyr::select(metric, dir, where(~ sum(is_nonnum(.x)) == 0))


  if (get_mode(x)[1] == "classification") {
    mets <- mets %>%
      filter(metric == "roc_auc") %>%
      dplyr::select(!c(metric, dir))
  } else if (get_mode(x) == "regression") {
    mets <- mets %>%
      filter(metric == "nrmse") %>%
      dplyr::select(!c(metric, dir))
  }

  mets <-
    structure(rescalar(as_num(mets), to = c(.5, 1)), names = names(mets))

  x$full_fits <- members[members$wflow_id %in% names(mets), ]

  preds_sl <- x$full_fits$preds_test

  names(preds_sl) <- x$full_fits$wflow_id

  nsamples <-
    unique(purrr::rerun(
      .n = length(preds_sl)^2,
      sort(
        sample(
          names(mets),
          size = sample(x = rango[1]:rango[2], size = 1),
          replace = F,
          prob = mets
        )
      )
    ))


  preds_sl <-
    map(nsamples, ~ bind_rows(preds_sl[.x], .id = "wflow_id"))

  preds_sl <-
    map(
      preds_sl,
      ~ .x %>%
        group_by(obs) %>%
        summarise_if(is_dblint, mean) %>%
        ungroup()
    )
  cli::cli_progress_step("Filtar modelos")

  if (get_mode(x)[1] == "classification") {
    preds_sl <-
      map(
        preds_sl,
        ~ .x %>%
          pivot_longer(
            cols = c(dplyr::starts_with(".pred")),
            names_to = "class",
            values_to = "prob"
          ) %>%
          group_by(obs) %>%
          arrange(desc(prob)) %>%
          slice(1) %>%
          mutate(
            .pred_class = gsub(".pred_", "", class, fixed = TRUE),
            .pred_class = factor(
              x = .pred_class,
              levels = attr(outcome_quo(x), "lvls")
            )
          ) %>%
          ungroup() %>%
          arrange(obs) %>%
          dplyr::select(.pred_class) %>%
          bind_cols(.x) %>%
          dplyr::select(!obs)
      )


    if (get_mode(x)[2] == "class") {
      nmc <- paste0(".pred_", attr(outcome_quo(x), "lvls")[1])

      classmax <- map_dbl(
        .x = preds_sl,
        .f = ~ yardstick::roc_auc_vec(
          truth = get_test(x)[[outcome_chr(x)]],
          estimate = .x[[nmc]]
        )
      )
    } else if (get_mode(x)[2] == "multiclass") {
      classmax <- map_dbl(
        .x = preds_sl,
        .f = ~ yardstick::roc_auc_vec(
          truth = get_test(x)[[outcome_chr(x)]],
          estimate = as.matrix(dplyr::select(.x, !.pred_class))
        )
      )
    }

    if (sum(classmax == max(classmax)) == 1) {
      preds_sl <- which.max(classmax)
    } else if (sum(classmax == max(classmax)) > 1) {
      if (get_mode(x)[2] == "class") {
        lvls <- attr(outcome_quo(x), "lvls")
        y <- as_chr(get_test(x)[[outcome_chr(x)]])
        y <- ifelse(y == lvls[1], 1, 0)

        preds_sl <-
          which.min(map_dbl(
            .x = preds_sl,
            .f = ~ sqrt(sum((.x[[nmc]] - y)^2)) / length(.x)
          ))
      } else if (get_mode(x)[2] == "multiclass") {
        classmax <-
          map(
            preds_sl,
            ~ bind_cols(get_test(x)[, outcome_chr(x)], .x) %>%
              pivot_longer(!c(
                .pred_class, outcome_expr(x)
              )) %>%
              mutate(name = stringr::str_remove(name, ".pred_")) %>%
              filter(!!outcome_expr(x) == name) %>%
              mutate(value = (1 - value)^2) %>%
              pull(value)
          )

        preds_sl <-
          which.min(map_dbl(classmax, ~ sqrt(sum(.x)) / length(.x)))
      }
    }
  } else if (get_mode(x)[1] == "regression") {
    preds_sl <-
      which.min(map_dbl(
        .x = preds_sl,
        .f = ~ mgalda::nrmse_vec(truth = .x[[outcome_chr(x)]], estimate = .x$.pred)
      ))
  }
  x$full_fits <-
    x$full_fits[x$full_fits$wflow_id %in% nsamples[[preds_sl]], ]

  x[["cols_map"]] <- x$full_fits$wflow_id

  class(x) <- c("softvoting", class(x))
  x[["metricas"]] <-
    eval_models(object = x, test_data = get_test(x))
  cli::cli_progress_done()
  cli::cli_alert_success("Fit modelo con { nrow(x$full_fits) } miembros")
  x
}

# bagging -----------------------------------------------------------------

#' Fit a bagging
#'
#' @rdname superlearner
#' @export
#'

bagging <- function(x, ...) {
  UseMethod("bagging")
}

#' @export
#' @rdname superlearner
bagging.default <- function(x, ...) {
  generic_default()
}

# XY method - data frame

#' @export
#' @rdname superlearner
bagging.data.frame <-
  function(x,
           y,
           times = 11L,
           control = list(var_imp = TRUE, sampling = "none"),
           num_terms = NULL,
           prod_degree = 1,
           prune_method = "none",
           tree_depth = 4,
           min_n = ncase(nrow(x)),
           cost_complexity = 0.1) {
    processed <- hardhat::mold(x, y)
    bagging_bridge(
      processed = processed,
      times = times,
      control = control,
      num_terms = num_terms,
      prod_degree = prod_degree,
      prune_method = prune_method,
      tree_depth = tree_depth,
      min_n = min_n,
      cost_complexity = cost_complexity
    )
  }

# XY method - matrix

#' @export
#' @rdname superlearner
bagging.matrix <-
  function(x,
           y,
           times = 11L,
           control = list(var_imp = TRUE, sampling = "none"),
           num_terms = NULL,
           prod_degree = 1,
           prune_method = "none",
           tree_depth = 4,
           min_n = ncase(nrow(x)),
           cost_complexity = 0.1) {
    processed <- hardhat::mold(x, y)
    bagging_bridge(
      processed = processed,
      times = times,
      control = control,
      num_terms = num_terms,
      prod_degree = prod_degree,
      prune_method = prune_method,
      tree_depth = tree_depth,
      min_n = min_n,
      cost_complexity = cost_complexity
    )
  }

# Formula method

#' @export
#' @rdname superlearner
bagging.formula <-
  function(formula,
           data,
           times = 11L,
           control = list(var_imp = TRUE, sampling = "none"),
           num_terms = NULL,
           prod_degree = 1,
           prune_method = "none",
           tree_depth = 4,
           min_n = ncase(nrow(data)),
           cost_complexity = 0.1) {
    processed <- hardhat::mold(formula, data)
    bagging_bridge(
      processed = processed,
      times = times,
      control = control,
      num_terms = num_terms,
      prod_degree = prod_degree,
      prune_method = prune_method,
      tree_depth = tree_depth,
      min_n = min_n,
      cost_complexity = cost_complexity
    )
  }

# Recipe method

#' @export
#' @rdname superlearner
bagging.recipe <-
  function(x,
           data,
           times = 11L,
           control = list(var_imp = TRUE, sampling = "none"),
           num_terms = NULL,
           prod_degree = 1,
           prune_method = "none",
           tree_depth = 4,
           min_n = ncase(nrow(data)),
           cost_complexity = 0.1) {
    processed <- hardhat::mold(x, data)
    bagging_bridge(
      processed = processed,
      times = times,
      control = control,
      num_terms = num_terms,
      prod_degree = prod_degree,
      prune_method = prune_method,
      tree_depth = tree_depth,
      min_n = min_n,
      cost_complexity = cost_complexity
    )
  }

# Bridge

bagging_bridge <-
  function(processed,
           times,
           control,
           num_terms,
           prod_degree,
           prune_method,
           tree_depth,
           min_n,
           cost_complexity) {
    fit <-
      bagging_impl(processed = processed,
                   times  =  times,
                   control  =  control,
                   num_terms  =  num_terms,
                   prod_degree  =  prod_degree,
                   prune_method  =  prune_method,
                   tree_depth  =  tree_depth,
                   min_n  =  min_n,
                   cost_complexity  =  cost_complexity
      )

    new_bagging(fit = fit, blueprint = processed$blueprint)
  }


# Implementation

bagging_impl <-
  function(processed,
           times,
           control,
           num_terms,
           prod_degree,
           prune_method,
           tree_depth,
           min_n,
           cost_complexity) {
    reqlibs_impl()
    modelmod <-
      set_mode_model(
        x = processed$outcomes,
        outcome = names(processed$outcomes)
      )
    times <- as_int(times)
    seed <- sample.int(10^5, 1)
    dat <- as_df(processed$predictors)
    dat$.outcome <- processed$outcomes[[1]]
    set.seed(seed)
    rs <-
      rsample::bootstraps(dat, times = times) %>%
      mutate(fit_seed = sample.int(10^5,times))
    opt_cart <- list(
      tree_depth = tree_depth,
      min_n = min_n,
      cost_complexity = cost_complexity
    )
    mod_spec_cart <-
      make_cart_spec(
        is.factor(rs$splits[[1]]$data$.outcome),
        opt_cart
      )
    opt_mars <-
      list(
        num_terms = num_terms,
        prod_degree = prod_degree,
        prune_method = prune_method
      )
    mod_spec_mars <-
      make_mars_spec(
        is.factor(rs$splits[[1]]$data$.outcome),
        opt_mars
      )
    bmars <- bagger_mars(rs, control, processed, mod_spec_mars)
    bcart <- bagger_cart(rs, control, processed, mod_spec_cart)
    structure(
      list(
        model_df = list(mars = bmars$model_df, cart = bcart$model_df),
        imp_cart = bcart$imp,
        imp_mars = bmars$imp,
        blueprint = processed$blueprint,
        mode = modelmod
      ),
      class = "bagging"
    )
  }


new_bagging <- function(fit = fit, blueprint) {
  hardhat::new_model(
    fit = fit,
    blueprint = blueprint,
    class = "bagging"
  )
}



#' @export
print.bagging <- function(x, ...) {
  x <- x$fit
  w <-
    list(
      mars = list(
        model_df = x$model_df[["mars"]],
        imp = x$imp_mars
      ),
      cart = list(
        model_df = x$model_df[["cart"]],
        imp = x$imp_cart
      )
    )

  for (i in names(w)) {
    z <- w[[i]]

    # mod_name <- paste("Cost-Sensitive", u)

    cat(
      "Bagged ",
      i,
      " (",
      paste0(x$mode, collapse = ", "),
      " with ",
      nrow(z$model_df),
      " members)\n",
      sep = ""
    )

    cat("\nVariable importance scores include:\n\n")
    print(z$imp)

    cat("\n")
  }
  invisible(x)
}




# prediccion --------------------------------------------------------------

#' @keywords internal
members_classification <- function(object, new_data) {
  new_preds <- map2_dfc(
    object$full_fits$wflow,
    object$full_fits$wflow_id,
    ~ standardized_predict(.x, new_data, lvls = attr(object$base_data$outcome, "lvls")) %>%
      rename_with(function(w) {
        paste(str_remove(string = w, pattern = ".pred_"), .y,
              sep = "_"
        )
      })
  )
  cero_cols <-
    setdiff(object$cols_map[object$cols_map != outcome_chr(object)], names(new_preds))

  if (length(cero_cols) > 0) {
    new_preds <-
      bind_cols(
        new_preds,
        purrr::map_dfc(.x = cero_cols, .f = ~ tibble(!!.x := 0))
      )
  }

  new_preds
}

#' @keywords internal
members_regression <- function(object, new_data) {
  new_preds <-
    map2_dfc(
      .x = object$full_fits$wflow,
      .y = object$full_fits$wflow_id,
      .f = ~ predict(object = .x, new_data) %>%
        rename({{.y}} := .pred)
    )

  cero_cols <-
    setdiff(object$cols_map[object$cols_map != outcome_chr(object)], names(new_preds))

  if (length(cero_cols) > 0) {
    new_preds <-
      bind_cols(
        new_preds,
        purrr::map_dfc(.x = cero_cols, .f = ~ tibble(!!.x := 0))
      )
  }

  new_preds
}

#' @export

predict.stacking <- function(object, new_data) {
  reqlibs_impl()
  if (get_mode(object)[1] == "classification") {
    new_preds <- members_classification(object, new_data)
    new_preds <-
      bind_cols(
        stacking_predict(x = object$equations$class, data = new_preds),
        stacking_predict(x = object$equations$prob, data = new_preds)
      )
  } else if (get_mode(object)[1] == "regression") {
    new_preds <- members_regression(object, new_data)
    new_preds <-
      stacking_predict(x = object$equations$numeric, data = new_preds)
  }
  new_preds
}

#' @export
stacking_predict <- function(x, ...) {
  UseMethod("stacking_predict")
}

#' @export
stacking_predict.elnet_numeric <- function(x, data, ...) {
  tibble(.pred = rlang::eval_tidy(x$.pred, data))
}

#' @export
stacking_predict.lognet_class <- function(x, data, ...) {
  tibble(.pred_class = rlang::eval_tidy(x$.pred_class, data))
}

#' @export
stacking_predict.lognet_prob <- function(x, data, ...) {
  purrr::map_dfc(x, rlang::eval_tidy, data = data)
}

#' @keywords internal
multi_net_engine <- function(x, data, ...) {
  res <-
    purrr::map_dfc(x$.pred, rlang::eval_tidy, data = data) %>%
    multi_net_helper()
}

#' @keywords internal
multi_net_helper <- function(data, ...) {
  data %>%
    dplyr::rowwise() %>%
    mutate(.sum = sum(dplyr::c_across(dplyr::starts_with(".pred_")))) %>%
    mutate(dplyr::across(dplyr::starts_with(".pred_"), ~ .x / .sum),
           idx = which.max(dplyr::c_across(dplyr::starts_with(".pred_")))
    ) %>%
    ungroup()
}

#' @export
stacking_predict.multnet_class <- function(x, data, ...) {
  lvls <- attr(x, "levels")
  res <-
    mgalda:::multi_net_engine(x, data)

  res %>%
    mutate(
      .pred_class = names(res)[stringr::str_detect(names(res),".pred_")][idx],
      .pred_class = stringr::str_remove(string = .pred_class,pattern = ".pred_"),
      .pred_class = factor(.pred_class, levels = lvls)
    ) %>%
    dplyr::select(.pred_class)
}

#' @export
stacking_predict.multnet_prob <- function(x, data, ...) {
  multi_net_engine(x, data) %>%
    dplyr::select(dplyr::starts_with(".pred_"))
}

#' @export
predict.softvoting <- function(object, new_data) {
  reqlibs_impl()
  suppressWarnings(expr = predict_softvoting_impl(object, new_data))
}

#' @keywords internal
predict_softvoting_impl <- function(object, new_data) {
  if (get_mode(object)[1] == "classification") {
    preds_prob <-
      purrr::map_df(
        .x = object$full_fits$wflow,
        .f = ~ standardized_predict(
          object = .x,
          new_data = new_data,
          lvls = attr(object$base_data$outcome, "lvls")
        ) %>%
          mutate(.row = row_number())
      ) %>%
      group_by(.row) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      arrange(.row)

    preds_class <-
      preds_prob %>%
      pivot_longer(
        cols = c(dplyr::starts_with(".pred")),
        names_to = "class",
        values_to = "prob"
      ) %>%
      group_by(.row) %>%
      arrange(desc(prob)) %>%
      slice(1) %>%
      mutate(
        .pred_class = gsub(".pred_", "", class, fixed = TRUE),
        .pred_class = factor(
          x = .pred_class,
          levels = attr(object$base_data$outcome, "lvls")
        )
      ) %>%
      ungroup() %>%
      arrange(.row) %>%
      dplyr::select(.pred_class)

    preds <- bind_cols(preds_prob, preds_class) %>%
      dplyr::select(!.row) %>%
      dplyr::relocate(.pred_class, .before = everything())

    nm <- paste0(".pred_", attr(object$base_data$outcome, "lvls"))
    preds <- preds[, c(".pred_class", nm)]
  } else if (get_mode(object)[1] == "regression") {
    preds <-
      purrr::map_df(
        .x = object$full_fits$wflow,
        .f = ~ predict(
          object = .x,
          new_data = new_data
        ) %>%
          mutate(.row = row_number())
      ) %>%
      group_by(.row) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      arrange(.row) %>%
      dplyr::select(!.row)
  }


  preds
}


#' @export
predict.bagging <- function(object, new_data) {
  new_data <- hardhat::forge(new_data, object$blueprint)$predictors
  if (object$fit$mode[1] == "regression") {
    models <- bind_rows(object$fit$model_df)$model

    res <-
      purrr::map_dfr(models, ~ predict(.x, new_data = new_data) %>% rowindex()) %>%
      group_by(row) %>%
      summarise_all(mean) %>%
      ungroup() %>%
      dplyr::select(!row)
  } else {
    lvl <- levels(object$blueprint$ptypes$outcomes[[1]])
    if (object$fit$mode[2] == "class") {
      models <- bind_rows(object$fit$model_df)$model
      prob_pred <-
        purrr::map_dfr(
          models,
          ~ predict(.x, new_data = new_data, type = "prob") %>% rowindex()
        ) %>%
        group_by(row) %>%
        summarise_all(mean,.groups = "drop") %>%
        ungroup() %>%
        arrange(row)
    } else {
      models <- map(.x = object$fit$model_df, "model")
      cart_pred <-
        purrr::map_dfr(
          models$cart,
          ~ predict(.x, new_data, type = "prob") %>% rowindex()
        ) %>%
        group_by(row) %>%
        summarise_all(mean,.groups = "drop") %>%
        ungroup() %>%
        arrange(row)


      mars_pred <- purrr::map_dfr(
        .x = models$mars,
        .f = ~ tibble(.pred_class = predict(.x$fit, new_data, type = "class")[, 1]) %>% rowindex()
      ) %>%
        group_by(row, .pred_class) %>%
        summarise(n = dplyr::n(),.groups = "drop") %>%
        ungroup() %>%
        group_by(row) %>%
        mutate(n = n / sum(n)) %>%
        ungroup() %>%
        pivot_wider(
          names_from = .pred_class,
          values_from = n,
          values_fill = 0,
          names_prefix = ".pred_"
        ) %>%
        arrange(row)

      prob_pred <- bind_rows(cart_pred,mars_pred) %>%
        group_by(row) %>%
        summarise_all(mean,.groups = "drop") %>%
        ungroup() %>%
        arrange(row)
    }
    class_pred <-
      prob_pred %>%
      pivot_longer(
        cols = c(dplyr::starts_with(".pred")),
        names_to = "class",
        values_to = "prob"
      ) %>%
      group_by(row) %>%
      arrange(desc(prob)) %>%
      slice(1) %>%
      mutate(
        .pred_class = gsub(".pred_", "", class, fixed = TRUE),
        .pred_class = factor(.pred_class, levels = lvl)
      ) %>%
      ungroup() %>%
      dplyr::select(.pred_class)


    res <-
      bind_cols(
        class_pred,
        dplyr::select(prob_pred, !row)
      )
  }
  res
}

