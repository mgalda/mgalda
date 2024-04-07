#' superlearner
#'
#' @rdname superlearner_preprocess_helpers
#' @name  superlearner_preprocess_helpers
#' @keywords internal
NULL

# helpers -----------------------------------------------------------------

#' @keywords internal
get_colstyperec <- function(x) {
  x <- x %>%
    prep() %>%
    pluck("term_info") %>%
    filter(.data$role == "predictor")

  set_names(x$type, x$variable)
}
#' @keywords internal
clean_categorical <- function(x) {
  x %>%
    step_novel(all_nominal_predictors(), new_level = "newlvl") %>%
    textrecipes::step_clean_levels(all_nominal_predictors()) %>%
    step_other(all_nominal_predictors(),
               other = "newlvl",
               threshold = 0.05
    )
}
#' @keywords internal
finalize_blueprint <- function(workflow) {
  has_blueprint <- function(x) {
    if (has_preprocessor_formula(x)) {
      !is.null(x$pre$actions$formula$blueprint)
    } else if (has_preprocessor_recipe(x)) {
      !is.null(x$pre$actions$recipe$blueprint)
    } else if (has_preprocessor_variables(x)) {
      !is.null(x$pre$actions$variables$blueprint)
    } else {
      cat_stop("Internal error: `x` must have a preprocessor to check for a blueprint.")
    }
  }


  #### 01

  has_preprocessor_recipe <- function(x) {
    "recipe" %in% names(x$pre$actions)
  }

  # Use user supplied blueprint if provided

  finalize_blueprint_recipe <- function(workflow) {
    blueprint <- hardhat::default_recipe_blueprint()
    recipe <- workflows::extract_preprocessor(workflow)
    workflows::update_recipe(workflow, recipe = recipe, blueprint = blueprint)
  }

  ## 02

  has_preprocessor_formula <- function(x) {
    "formula" %in% names(x$pre$actions)
  }
  finalize_blueprint_formula <- function(workflow) {
    tbl_encodings <- pull_workflow_spec_encoding_tbl(workflow)

    indicators <- tbl_encodings$predictor_indicators
    intercept <- tbl_encodings$compute_intercept

    if (!is_string(indicators)) {
      cat_stop("Internal error: `indicators` encoding from parsnip should be a string.")
    }
    if (!is_bool(intercept)) {
      cat_stop("Internal error: `intercept` encoding from parsnip should be a bool.")
    }

    # Use model specific information to construct the blueprint
    blueprint <-
      hardhat::default_formula_blueprint(
        indicators = indicators,
        intercept = intercept
      )

    formula <- workflows::extract_preprocessor(workflow)

    workflows::update_formula(workflow, formula = formula, blueprint = blueprint)
  }



  ## 03
  ##

  has_preprocessor_variables <- function(x) {
    "variables" %in% names(x$pre$actions)
  }
  finalize_blueprint_variables <- function(workflow) {
    # Use the default blueprint, no parsnip model encoding info is used here
    blueprint <- hardhat::default_xy_blueprint()

    variables <- workflows::extract_preprocessor(workflow)

    workflows::update_variables(workflow,
                                blueprint = blueprint,
                                variables = variables
    )
  }

  # 0


  pull_workflow_spec_encoding_tbl <- function(workflow) {
    spec <- workflows::extract_spec_parsnip(workflow)
    spec_cls <- class(spec)[[1]]

    tbl_encodings <- parsnip::get_encoding(spec_cls)

    indicator_engine <- tbl_encodings$engine == spec$engine
    indicator_mode <- tbl_encodings$mode == spec$mode
    indicator_spec <- indicator_engine & indicator_mode

    out <- tbl_encodings[indicator_spec, , drop = FALSE]

    if (nrow(out) != 1L) {
      cat_stop("Internal error: Exactly 1 model/engine/mode combination must be located.")
    }

    out
  }


  if (has_blueprint(workflow)) {
    return(workflow)
  }


  if (has_preprocessor_recipe(workflow)) {
    finalize_blueprint_recipe(workflow)
  } else if (has_preprocessor_formula(workflow)) {
    finalize_blueprint_formula(workflow)
  } else if (has_preprocessor_variables(workflow)) {
    finalize_blueprint_variables(workflow)
  } else {
    cat_stop("Internal error: `workflow` should have a preprocessor at this point.")
  }
}

#' @keywords internal
check_recs <- function(x, data) {
  preper <- try(workflows::.fit_pre(x, data), silent = T)
  res <- try(recipes::juice(preper), silent = T)

  if (inherits(res, "try-error")) {
    preper <- NULL
  } else {
    if (ncol(res) > 10) {
      preper_pca <- x %>%
        step_pca(all_numeric_predictors())

      preper_pca <- workflows::.fit_pre(x, data)

      res <- try(recipes::juice(preper), silent = T)

      if (!inherits(res, "try-error")) {
        preper <- preper_pca
      }
    }
  }
  preper
}

#' @keywords internal
remove_unbalance <-
  function(dataset, outcome, threshold = .9) {
    unbalance <-
      map_lgl(
        .x = dplyr::select(dataset, -!!rlang::enquo(outcome)),
        .f = ~ max(table(.x)) / length(.x) > threshold
      )

    unbalance <- names(unbalance)[unbalance]

    dataset %>% dplyr::select(-tidyselect::matches(unbalance))
  }

#' @export
select_step <- function(x, ...) {
  UseMethod("select_step")
}
#' @export
select_step.default <- function(x, ...) {
  cat_message(
    "No method for { fn } is defined",
    "for { paste0(class(x), collapse = ',') }.",
    .sep = "",
    fn = stringr::str_remove(
      string = deparse(match.call()[[1]]),
      pattern = stringr::fixed(".default")
    )
  )
}

#' @export
select_step.recipe <- function(x, small, ...) {
  dataset <- x %>%
    step_rm(recipes::all_outcomes()) %>%
    prep() %>%
    recipes::juice()


  mode <- set_mode_model(x)
  if (small) {
    num_ops <- c(
      "f_binarize",
      "f_breaktize",
      "f_ns",
      "f_poly",
      "f_xnls",
      "f_discrchi",
      "f_discrtopdown",
      "f_discrmdlp",
      "f_normalize"
    )
    fct_ops <- c(
      "f_dummy",
      "f_lencode_mixed"
    )
  } else {
    num_ops <- c(
      "f_bestnorm",
      "f_binarize",
      "f_breaktize",
      "f_bs",
      "f_discretize_cart",
      "f_ns",
      "f_poly",
      "f_xnls",
      "f_discrchi",
      "f_discrtopdown",
      "f_discrmdlp",
      "f_normalize"
    )
    fct_ops <- c(
      "f_dummy",
      "f_lencode_glm",
      "f_lencode_mixed"
    )
  }

  # sale por el momento ya que debe tener la condicion de 2 factores  "f_woe"


  def_fn <-
    function(class, mode, n) {
      if (mode[1] == "regression") {
        fct_ops <- fct_ops[fct_ops != "f_woe"]
      }

      if (mode[1] == "classification") {
        num_ops <- num_ops[num_ops != "f_xnls"]
        num_ops <- num_ops[num_ops != "f_discrchi"]
        num_ops <- num_ops[num_ops != "f_discrtopdown"]
        if (mode[2] == "multiclass") {
          fct_ops <- fct_ops[fct_ops != "f_lencode_mixed"]
          fct_ops <- fct_ops[fct_ops != "f_woe"]
        } else {
          fct_ops <- fct_ops[fct_ops != "f_lencode_glm"]
        }
      }

      if (n <= 10) {
        num_ops <- num_ops[num_ops != "f_poly"]
        num_ops <- num_ops[num_ops != "f_ns"]
      }

      ifelse(class == "numeric", sample(num_ops, 1), sample(fct_ops, 1))
    }
  class_dataset <-
    map_chr(dataset, ~ ifelse(class(.x) == "integer", "numeric", class(.x)))

  n_uniques <-
    map_dbl(dataset, n_unique)

  tibble(
    var = names(class_dataset),
    class = class_dataset,
    uniques = n_uniques
  ) %>%
    mutate(fn = purrr::map2_chr(
      .x = class,
      .y = uniques,
      .f = ~ def_fn(.x, mode = mode, n = .y)
    )) %>%
    dplyr::select(!uniques)
}
#' @keywords internal
getcols_num <-
  function(predictors,x,outcome) {
    predictors <- rlang::enexpr(predictors)
    outcome <- rlang::enquo(outcome)
    tryCatch(
      expr = {
        dataset %>%
          dplyr::select(-!!outcome) %>%
          dplyr::select(tidyselect::matches(match = !!predictors)) %>%
          select_if(is_dblint) %>%
          names()
      },
      error = function(e) {
        NULL
      }
    )
  }

#' @keywords internal
custom_steps <- function(x, add_step) {
  recs <- x$full_fits$recs

  recs_add <- map(recs, ~ add_step(rec = .x))

  check_rec <-
    map_lgl(recs_add, ~ is_try(try(recipes::juice(prep(.x)), silent = T)))

  for (i in seq_along(check_rec)) {
    if (!check_rec[i]) {
      recs[[i]] <- recs_add[[i]]
    }
  }
  x$full_fits$recs <- recs
  x
}

#' @keywords internal
custom_steps_recipe_list <- function(x, add_step) {
  recs <- x$recs

  recs_add <- map(recs, ~ add_step(rec = .x))

  check_rec <-
    map_lgl(recs_add, ~ is_try(try(juice(prep(.x)), silent = T)))

  for (i in seq_along(check_rec)) {
    if (!check_rec[i]) {
      recs[[i]] <- recs_add[[i]]
    }
  }
  x$recs <- recs
  x
}

#' @keywords internal
fit_recipes_list <-
  function(workflow, data) {
    fit_action_recipe <- function(object, workflow, data) {
      recipe <- object$recipe
      blueprint <- object$blueprint

      workflow$pre$mold <-
        hardhat::mold(recipe, data, blueprint = blueprint)

      # All pre steps return the `workflow` and `data`
      list(workflow = workflow, data = data)
    }


    n <- length(workflow[["pre"]]$actions)
    for (i in seq_len(n)) {
      action <- workflow[["pre"]]$actions[[i]]
      result <-
        fit_action_recipe(action, workflow = workflow, data = data)
      workflow <- result$workflow
      data <- result$data
    }
    workflow
  }

# cat to num --------------------------------------------------------------


#' @keywords internal
f_dummy <- function(rec, var) {
  var <- rlang::sym(var)
  rlang::expr(recipes::step_dummy(!!var, one_hot = T))
}

#' @keywords internal
f_lencode_glm <- function(rec, var) {
  var <- rlang::sym(var)

  rlang::expr(step_lenc_glm(!!var))
}

#' @keywords internal
f_lencode_mixed <- function(rec, var) {
  var <- rlang::sym(var)

  rlang::expr(step_lenc_mixed(!!var))
}

#' @keywords internal
f_woe <- function(rec, var) {
  var <- rlang::sym(var)

  outcome <-
    rec$var_info$variable[rec$var_info$role ==
                            "outcome"]
  outcome <- rlang::sym(outcome)

  dict <-
    embed::dictionary(
      .data = !!rec$template,
      !!var,
      outcome = !!deparse(outcome)
    )
  rlang::expr(embed::step_woe(
    !!var,
    outcome = vars(!!outcome),
    dictionary = !!dict
  ))
}
# num to cat --------------------------------------------------------------

#' @keywords internal
f_discretize_cart <-
  function(rec, var) {
    var <- rlang::sym(var)

    outcome_name <-
      rec$var_info$variable[rec$var_info$role == "outcome"]

    rlang::expr(embed::step_discretize_cart(!!var, outcome = !!outcome_name))
  }

#' @keywords internal
f_binarize <-
  function(rec, var) {
    var <- rlang::sym(var)

    algth <-
      sample(c("kmean", "mean", "median", "dens"), 1)

    rlang::expr(step_binarize(!!var, algorithm = !!algth))
  }

#' @keywords internal
f_discrchi <-
  function(rec, var) {
    var <- rlang::sym(var)

    rlang::expr(step_discrchi(!!var))
  }

#' @keywords internal
f_discrtopdown <-
  function(rec, var) {
    var <- rlang::sym(var)

    algth <-
      sample(c("caim", "cacc", "ameva"), 1)
    rlang::expr(step_discrtopdown(!!var, method = !!algth))
  }

#' @keywords internal
f_discrmdlp <-
  function(rec, var) {
    var <- rlang::sym(var)

    rlang::expr(step_discrmdlp(!!var))
  }



# nun to num --------------------------------------------------------------

#' @keywords internal
f_breaktize <- function(rec, var) {
  nn_br <- nsplits(unique(rec$template[[var]]))

  var <- rlang::sym(var)

  algth <-
    sample(c("quantile", "trees", "kmean", "equalwidth"), 1)

  rlang::expr(step_breaktize(
    !!var,
    num_breaks = !!nn_br,
    algorithm = !!algth
  ))
}

#' @keywords internal
f_bs <- function(rec, var) {
  var <- rlang::sym(var)
  df <- base::sample(2:3, size = 1)

  rlang::expr(recipes::step_bs(!!var, deg_free = !!df))
}

#' @keywords internal
f_xnls <- function(rec, var) {
  var <- rlang::sym(var)


  rlang::expr(step_xnls(!!var))
}

#' @keywords internal
f_poly <- function(rec, var) {
  var <- rlang::sym(var)
  df <- base::sample(2:3, size = 1)

  rlang::expr(recipes::step_poly(!!var, degree = !!df))
}

#' @keywords internal
f_ns <- function(rec, var) {
  var <- rlang::sym(var)
  df <- base::sample(2:3, size = 1)

  rlang::expr(recipes::step_ns(!!var, deg_free = !!df))
}

#' @keywords internal
f_bestnorm <- function(rec, var) {
  var <- rlang::sym(var)

  rlang::expr(step_bestnorm(!!var))
}

#' @keywords internal
f_normalize <- function(rec, var) {
  var <- rlang::sym(var)

  rlang::expr(recipes::step_normalize(!!var))
}
