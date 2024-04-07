mgalda::hfrec_libraries()

# alldata -----------------------------------------------------------------

datalearn <- read_rds("data-raw/datalearn.rds")

usethis::use_data(datalearn, overwrite = TRUE)

nm <- rep(letters, length.out = length(datalearn))

nm <- paste0(letters[cumcount(nm)], nm)

names(datalearn) <- nm

data_numeric <-
  compact(map(
    .x = datalearn,
    .f = ~ as_tibble(.x[, are_dblint(.x)])
  ))

data_factor <-
  compact(map(
    .x = datalearn,
    .f = ~ as_tibble(.x[, are_fctchr(.x)])
  ))


data_factor <-
  map(
    .x = data_factor,
    .f = ~ .x[, sapply(.x, function(x) {
      n_unique(x) / length(x)
    }) < .1]
  )


data_factor <-
  map(
    .x = data_factor,
    .f = ~ .x[, sapply(.x, function(x) {
      n_unique(x[!is_nonnum(x) | !is_emptyna(x)])
    }) > 1]
  )

data_numeric<-
  map(
    .x = data_numeric,
    .f = ~ .x[, sapply(.x, function(x) {
      n_unique(x) / length(x)
    }) > .1]
  )


data_numeric<-
  compact(map(.x = data_numeric, ~map_dfc(.x,as.double)))

data_numeric <-
  purrr::flatten(map2(
    .x = data_numeric,
    .y = names(data_numeric),
    .f = function(x, y) {
      as.list(dplyr::rename_with(
        .data = x,
        .fn = ~ paste0("num_", y, stringr::str_pad(seq_len(ncol(
          x
        )), 3, pad = "0")),
        .cols = dplyr::everything()
      ))
    }
  ))

#data_numeric <- map(data_numeric, ~ .x[!is_numinvalid(.x)])

data_factor <-
  purrr::flatten(map2(
    .x = data_factor,
    .y = names(data_factor),
    .f = function(x, y) {
      as.list(dplyr::rename_with(
        .data = x,
        .fn = ~ paste0("fct_", y, stringr::str_pad(seq_len(ncol(
          x
        )), 3, pad = "0")),
        .cols = dplyr::everything()
      ))
    }
  ))


bigletters <- c(letters,LETTERS)

nm2 <-
  rep(bigletters, length.out = max(map_dbl(data_factor,~length(levels(.x)))))

nm <-
  paste0(bigletters[cumcount(nm2)], nm2)

recod_fct <- function(x){
  x<- x[x!=""]
  x<-fct_drop(x)
  l <- levels(x)
  w <- nm[1:length(l)]
  names(w) <- l
  dplyr::recode_factor(.x = x,!!!w)

}
data_factor <- map(data_factor,as.factor)

for(i in seq_along(data_factor)){
  data_factor[[i]] <- recod_fct(data_factor[[i]])
}

data_factor <- map(data_factor, ~ .x[!is_empty(.x)])

# orig <- map(data_factor, levels)
# cleaned <- map(orig, janitor::make_clean_names)
# clean <- map2(cleaned, orig, rlang::set_names)
# clean<-
# map(clean,~.x[names(.x)!=""])

# data_factor <-
#   map2(data_factor, clean, ~ recode_factor(.x, !!!.y))

data_numeric <-
  data_numeric[map_dbl(data_numeric,  ~ n_unique(.x[!is_nonnum(.x)]) /
                         length(.x)) < .8]

list_sample <- list(factor = data_factor, numeric = data_numeric)

rm(list = ls()[ls() != "list_sample"])

usethis::use_data(list_sample, overwrite = TRUE)

rm(list = ls())

# geocodigos --------------------------------------------------------------

geocodigos <-
  read_excel("data-raw/COD_GEO.xlsx")

name_geocodigos <-
  map_dbl(geocodigos, n_distinct)

name_geocodigos <- rev(name_geocodigos[order(name_geocodigos)])

geocodigos <- geocodigos[, names(name_geocodigos)]

unchr <- map(geocodigos, unique)

unchr <-
  map(unchr, .f = ~ unique(str_remove_all(string = .x, pattern = "[A-Za-z0-9]")))

unchr <- unique(unlist(unchr))

unchr <- str_split(
  string = unchr,
  pattern = "",
  n = 10
)

unchr <- map(unchr, unique)

unchr <- unique(unlist(unchr))

unchr

geocodigos %<>%
  dplyr::mutate_all(str_to_lower) %>%
  dplyr::mutate_all(~ str_replace_all(string = .x, pattern = "-", "_")) %>%
  dplyr::mutate_all(~ str_remove_all(string = .x, pattern = " ")) %>%
  janitor::clean_names() %>%
  dplyr::mutate_all(factor)

rm(name_geocodigos, unchr)

usethis::use_data(geocodigos, overwrite = TRUE)
rm(list = ls())

# sl models ---------------------------------------------------------------
rm(list = ls())

  base_superlearner <-
  readr::read_rds("data-raw/base_superlearner.rds")

usethis::use_data(base_superlearner, overwrite = TRUE)

rm(list = ls())

# curvas ------------------------------------------------------------------
mgalda::hfrec_libraries()

curv_data <- parse(file = "data-raw/curv_data.R")
curv_data<-eval(curv_data,envir = baseenv())

usethis::use_data(curv_data, overwrite = TRUE)

rm(list = ls())

# metricas ----------------------------------------------------------------

yardsticks_metrics_info <-
  tibble::tribble(
    ~metrica,
    ~class,
    ~direction,
    ~fn,
    "accuracy",
    "class_metric",
    "maximize",
    "yardstick::accuracy",
    "average_precision",
    "prob_metric",
    "maximize",
    "yardstick::average_precision",
    "bal_accuracy",
    "class_metric",
    "maximize",
    "yardstick::bal_accuracy",
    "ccc",
    "numeric_metric",
    "maximize",
    "yardstick::ccc",
    "classification_cost",
    "prob_metric",
    "minimize",
    "yardstick::classification_cost",
    "detection_prevalence",
    "class_metric",
    "maximize",
    "yardstick::detection_prevalence",
    "f_meas",
    "class_metric",
    "maximize",
    "yardstick::f_meas",
    "gain_capture",
    "prob_metric",
    "maximize",
    "yardstick::gain_capture",
    "huber_loss_pseudo",
    "numeric_metric",
    "minimize",
    "yardstick::huber_loss_pseudo",
    "huber_loss",
    "numeric_metric",
    "minimize",
    "yardstick::huber_loss",
    "iic",
    "numeric_metric",
    "maximize",
    "yardstick::iic",
    "j_index",
    "class_metric",
    "maximize",
    "yardstick::j_index",
    "kap",
    "class_metric",
    "maximize",
    "yardstick::kap",
    "mae",
    "numeric_metric",
    "minimize",
    "yardstick::mae",
    "mape",
    "numeric_metric",
    "minimize",
    "yardstick::mape",
    "mase",
    "numeric_metric",
    "minimize",
    "yardstick::mase",
    "mcc",
    "class_metric",
    "maximize",
    "yardstick::mcc",
    "mn_log_loss",
    "prob_metric",
    "minimize",
    "yardstick::mn_log_loss",
    "mpe",
    "numeric_metric",
    "zero",
    "yardstick::mpe",
    "msd",
    "numeric_metric",
    "zero",
    "yardstick::msd",
    "npv",
    "class_metric",
    "maximize",
    "yardstick::npv",
    "ppv",
    "class_metric",
    "maximize",
    "yardstick::ppv",
    "pr_auc",
    "prob_metric",
    "maximize",
    "yardstick::pr_auc",
    "precision",
    "class_metric",
    "maximize",
    "yardstick::precision",
    "recall",
    "class_metric",
    "maximize",
    "yardstick::recall",
    "rmse",
    "numeric_metric",
    "minimize",
    "yardstick::rmse",
    "roc_auc",
    "prob_metric",
    "maximize",
    "yardstick::roc_auc",
    "roc_aunp",
    "prob_metric",
    "maximize",
    "yardstick::roc_aunp",
    "roc_aunu",
    "prob_metric",
    "maximize",
    "yardstick::roc_aunu",
    "rpd",
    "numeric_metric",
    "maximize",
    "yardstick::rpd",
    "rpiq",
    "numeric_metric",
    "maximize",
    "yardstick::rpiq",
    "rsq_trad",
    "numeric_metric",
    "maximize",
    "yardstick::rsq_trad",
    "rsq",
    "numeric_metric",
    "maximize",
    "yardstick::rsq",
    "sens",
    "class_metric",
    "maximize",
    "yardstick::sens",
    "sensitivity",
    "class_metric",
    "maximize",
    "yardstick::sensitivity",
    "smape",
    "numeric_metric",
    "minimize",
    "yardstick::smape",
    "spec",
    "class_metric",
    "maximize",
    "yardstick::spec",
    "specificity",
    "class_metric",
    "maximize",
    "yardstick::specificity"
  )

yardsticks_metrics_info <- dplyr::mutate(
  .data = yardsticks_metrics_info,
  fn = map(
    .x = fn,
    .f = ~ eval(parse(text = .x))
  )
)

usethis::use_data(yardsticks_metrics_info, overwrite = TRUE)

rm(list = ls())


# info_distr --------------------------------------------------------------

info_distribucion <- parse("data-raw/info_distribucion.R")
info_distribucion <- eval(info_distribucion)
info_distribucion <- deparse(info_distribucion)
#info_distribucion <-  eval(info_distribucion)

usethis::use_data(info_distribucion, overwrite = TRUE)

rm(list = ls())

# stocks_data -------------------------------------------------------------

stocks_data <- read_rds("data-raw/stocks_data.rds")

usethis::use_data(stocks_data, overwrite = TRUE)

rm(list = ls())

streets <- read_rds("data-raw/streets_mts.rds")

usethis::use_data(streets, overwrite = TRUE)

rm(list = ls())

# dictionary --------------------------------------------------------------

dictionary <- list(
  rm_abbreviation = structure(
    "([A-Za-z][\\.]\\s*){1,}([A-Za-z][\\.])",
    class = c(
      "regexr",
      "character"
    ),
    subs = list(
      let_per_1 = structure(
        "([A-Za-z][\\.]\\s*){1,}",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Letter folowed by period and optional space (1 or more times)"
      ),
      let_per_2 = structure(
        "([A-Za-z][\\.])",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Ending letter folowed by period"
      )
    ),
    comments = list(
      let_per_1 = "Letter folowed by period and optional space (1 or more times)",
      let_per_2 = "Ending letter folowed by period"
    )
  ),
  rm_between = "(%s)(.*?)(%s)",
  rm_between2 = "(?<=%s).*?(?=%s)",
  rm_caps = "(\\b[A-Z]{2,}\\b)",
  rm_caps_phrase = structure(
    "(([A-Z'-]+\\b\\s)*(\\b[A-Z'-]{2,}\\b))|(([A-Z'-]+\\b\\s)([A-Z'-]+\\b))|(([A-Z]\\b\\s)(\\b[A-Z]\\b))",
    class = c(
      "regexr",
      "character"
    ),
    subs = list(
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 2 START"
      ),
      opt_caps = structure(
        "([A-Z'-]+\\b\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "2 or more caps/-/' & space (group optional)"
      ),
      req_caps = structure(
        "(\\b[A-Z'-]{2,}\\b)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "2 or more caps/-/' & space (required)"
      ),
      structure(
        ")",
        class = c("subcom", "character"),
        comment = "GROUP 2 END"
      ),
      or = "|",
      structure(
        "(",
        class = c("subcom", "character"),
        comment = "GROUP 1 START"
      ),
      req_caps1_sp = structure(
        "([A-Z'-]+\\b\\s)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "1 or more caps/-/' & space"
      ),
      req_caps2 = structure(
        "([A-Z'-]+\\b)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "1 or more caps/-/'"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1 END"
      ),
      or = "|",
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 3 START"
      ),
      cap_1_sp = structure(
        "([A-Z]\\b\\s)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "1 cap & space"
      ),
      cap_2 = structure(
        "(\\b[A-Z]\\b)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "1 cap"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 3 END"
      )
    ),
    comments = list(
      "GROUP 2 START",
      opt_caps = "2 or more caps/-/' & space (group optional)",
      req_caps = "2 or more caps/-/' & space (required)",
      "GROUP 2 END",
      or = NULL,
      "GROUP 1 START",
      req_caps1_sp = "1 or more caps/-/' & space",
      req_caps2 = "1 or more caps/-/'",
      "GROUP 1 END",
      or = NULL,
      "GROUP 3 START",
      cap_1_sp = "1 cap & space",
      cap_2 = "1 cap",
      "GROUP 3 END"
    )
  ),
  rm_citation = "(((((((\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+,\\s)*(\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+,*\\s(\\&|and))\\s)*([A-Z]\\.\\s)*(\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-'s]+\\s(et\\sal\\.('s??)??\\s){0,1})|((\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z][A-Za-z'-]*\\s)+)\\((((\\d{4}[a-z]{0,1})|(n\\.d\\.)|(in press))(,\\s*)*)+\\))|(((?<=(\\(|(;\\s))|(\\(((e\\.g\\.)|(cf\\.)|(viz\\.)),{0,1}\\s))((((\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+,\\s)*(\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+,*\\s(\\&|and))\\s)*([A-Z]\\.\\s)*(\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+\\s{0,1}(et\\sal\\.){0,1})|((\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z][A-Za-z'-]*\\s)+),\\s(((\\d{4}[a-z]{0,1})|(n\\.d\\.)|(in press))(,*\\s*)*(?!(\\s{0,1}p{1,2}(ara){0,1}\\.)))+(?=[)]{0,1})",
  rm_citation2 = structure(
    "(((((((\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+,\\s)*(\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+,*\\s(\\&|and))\\s)*([A-Z]\\.\\s)*(\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-'s]+\\s(et\\sal\\.('s??)??\\s){0,1})|((\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z][A-Za-z'-]*\\s)+)\\((((\\d{4}[a-z]{0,1})|(n\\.d\\.)|(in press))(,\\s*)*)+\\))",
    class = c(
      "regexr",
      "character"
    ),
    subs = list(
      "(",
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1 START"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1A START"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1A.1 START"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1A.1a START"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1A.1a_i START"
      ),
      von_prefix = structure(
        "(\\b([Vv][oa]n|[dD][eua])\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Find a von/van/de/du/da followed by..."
      ),
      last_name = structure(
        "[A-Z]{1}[a-zA-Z-']+",
        class = c(
          "subcom",
          "character"
        ),
        comment = "A last name (capital followed by one or mor letters (1 or more times)"
      ),
      comma_space = structure(
        ",\\s",
        class = c("subcom", "character"),
        comment = "Comma and a space"
      ),
      structure(
        ")*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1A.1a_i END (zero or more times)"
      ),
      von_prefix = structure(
        "(\\b([Vv][oa]n|[dD][eua])\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Find a von/van/de/du/da followed by space"
      ),
      last_name = structure(
        "[A-Z]{1}[a-zA-Z-']+",
        class = c(
          "subcom",
          "character"
        ),
        comment = "A last name (capital followed by one or more letters (1 or more times)"
      ),
      comma_space = structure(
        ",*\\s",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Comma (0 or more) and a space"
      ),
      and = structure(
        "(\\&|and)",
        class = c("subcom", "character"),
        comment = "And and sign"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1A.1a END"
      ),
      space = "\\s",
      structure(
        ")*",
        class = c("subcom", "character"),
        comment = "GROUP 1A.1 END (zero or more times)"
      ),
      letter_period = structure(
        "([A-Z]\\.\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Single capital letter followed by a period (0 or more times)"
      ),
      von_prefix = structure(
        "(\\b([Vv][oa]n|[dD][eua])\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Find a von/van/de/du/da followed by..."
      ),
      last_name = structure(
        "[A-Z]{1}[a-zA-Z-'s]+",
        class = c(
          "subcom",
          "character"
        ),
        comment = "A last name (capital followed by one or more letters (1 or more times)"
      ),
      space = "\\s",
      et_al = structure(
        "(et\\sal\\.('s??)??\\s){0,1}",
        class = c(
          "subcom",
          "character"
        ),
        comment = "et al. 0 or 1 times)"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1A END"
      ),
      or = structure(
        "|",
        class = c(
          "subcom",
          "character"
        ),
        comment = "OR"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "START GROUP 1B"
      ),
      von_prefix = structure(
        "(\\b([Vv][oa]n|[dD][eua])\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Find a von/van/de/du/da followed by..."
      ),
      last_name = structure(
        "[A-Z][A-Za-z'-]*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "A last name (capital followed by one or more letters (1 or more times)"
      ),
      space = structure(
        "\\s",
        class = c("subcom", "character"),
        comment = "Space"
      ),
      structure(
        ")+",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1B END"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1 END"
      ),
      left_par = structure(
        "\\(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "open parenthesis"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "YEAR: GROUP 2 START"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 2A START"
      ),
      year = structure(
        "(\\d{4}[a-z]{0,1})",
        class = c(
          "subcom",
          "character"
        ),
        comment = "4 digit year optionally followed by 1 lower case letter"
      ),
      or = structure(
        "|",
        class = c("subcom", "character"),
        comment = "OR"
      ),
      no_date = structure(
        "(n\\.d\\.)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "No date"
      ),
      or = structure(
        "|",
        class = c(
          "subcom",
          "character"
        ),
        comment = "OR"
      ),
      in_press = structure(
        "(in press)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "In press"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 2A END"
      ),
      comma_space_optiona = structure(
        "(,\\s*)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Comma + optional space (whole group optional"
      ),
      structure(
        ")+",
        class = c("subcom", "character"),
        comment = "YEAR: GROUP 2 END (1 or more times)"
      ),
      right_par = structure(
        "\\)",
        class = c("subcom", "character"),
        comment = "closed parenthesis"
      ),
      ")"
    ),
    comments = list(
      NULL,
      "GROUP 1 START",
      "GROUP 1A START",
      "GROUP 1A.1 START",
      "GROUP 1A.1a START",
      "GROUP 1A.1a_i START",
      von_prefix = "Find a von/van/de/du/da followed by...",
      last_name = "A last name (capital followed by one or mor letters (1 or more times)",
      comma_space = "Comma and a space",
      "GROUP 1A.1a_i END (zero or more times)",
      von_prefix = "Find a von/van/de/du/da followed by space",
      last_name = "A last name (capital followed by one or more letters (1 or more times)",
      comma_space = "Comma (0 or more) and a space",
      and = "And and sign",
      "GROUP 1A.1a END",
      space = NULL,
      "GROUP 1A.1 END (zero or more times)",
      letter_period = "Single capital letter followed by a period (0 or more times)",
      von_prefix = "Find a von/van/de/du/da followed by...",
      last_name = "A last name (capital followed by one or more letters (1 or more times)",
      space = NULL,
      et_al = "et al. 0 or 1 times)",
      "GROUP 1A END",
      or = "OR",
      "START GROUP 1B",
      von_prefix = "Find a von/van/de/du/da followed by...",
      last_name = "A last name (capital followed by one or more letters (1 or more times)",
      space = "Space",
      "GROUP 1B END",
      "GROUP 1 END",
      left_par = "open parenthesis",
      "YEAR: GROUP 2 START",
      "GROUP 2A START",
      year = "4 digit year optionally followed by 1 lower case letter",
      or = "OR",
      no_date = "No date",
      or = "OR",
      in_press = "In press",
      "GROUP 2A END",
      comma_space_optiona = "Comma + optional space (whole group optional",
      "YEAR: GROUP 2 END (1 or more times)",
      right_par = "closed parenthesis",
      NULL
    )
  ),
  rm_citation3 = structure(
    "(((?<=(\\(|(;\\s))|(\\(((e\\.g\\.)|(cf\\.)|(viz\\.)),{0,1}\\s))((((\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+,\\s)*(\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+,*\\s(\\&|and))\\s)*([A-Z]\\.\\s)*(\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z]{1}[a-zA-Z-']+\\s{0,1}(et\\sal\\.){0,1})|((\\b([Vv][oa]n|[dD][eua])\\s)*[A-Z][A-Za-z'-]*\\s)+),\\s(((\\d{4}[a-z]{0,1})|(n\\.d\\.)|(in press))(,*\\s*)*(?!(\\s{0,1}p{1,2}(ara){0,1}\\.)))+(?=[)]{0,1})",
    class = c(
      "regexr",
      "character"
    ),
    subs = list(
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "LAST NAME GROUP START"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1 START (MULTIPLE AUTHORS)"
      ),
      ensure_start = structure(
        "(?<=",
        class = c(
          "subcom",
          "character"
        ),
        comment = "BEGIN ENSURE"
      ),
      para_or_semi = structure(
        "(\\(|(;\\s))",
        class = c(
          "subcom",
          "character"
        ),
        comment = "string is lead by an open parenthesis or semicolon + space"
      ),
      or = structure(
        "|",
        class = c("subcom", "character"),
        comment = "OR"
      ),
      structure(
        "(",
        class = c("subcom", "character"),
        comment = "GROUP ENSURE.1 START"
      ),
      left_par = structure(
        "\\(",
        class = c("subcom", "character"),
        comment = "open parenthesis"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP ENSURE.1a START"
      ),
      eg = structure(
        "(e\\.g\\.)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "e.g."
      ),
      or = structure(
        "|",
        class = c(
          "subcom",
          "character"
        ),
        comment = "OR"
      ),
      cf = structure(
        "(cf\\.)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "cf."
      ),
      or = structure(
        "|",
        class = c(
          "subcom",
          "character"
        ),
        comment = "OR"
      ),
      viz = structure(
        "(viz\\.)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "viz."
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP ENSURE.1a END"
      ),
      coma_space = structure(
        ",{0,1}\\s",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Optional comma followed by a space"
      ),
      structure(
        ")",
        class = c("subcom", "character"),
        comment = "END GROUP ENSURE.1"
      ),
      ensure_end = structure(
        ")",
        class = c("subcom", "character"),
        comment = "END ENSURE"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1.A START"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1.A.1 START"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1.A.1.a START"
      ),
      von_prefix = structure(
        "(\\b([Vv][oa]n|[dD][eua])\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Find a von/van/de/du/da followed by space (0 or more)"
      ),
      last_name = structure(
        "[A-Z]{1}[a-zA-Z-']+",
        class = c(
          "subcom",
          "character"
        ),
        comment = "A last name (capital followed by one or more letters (1 or more times)"
      ),
      comma_space = structure(
        ",\\s",
        class = c("subcom", "character"),
        comment = "Comma and a space"
      ),
      structure(
        ")*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1.A.1.a END (0 or more)"
      ),
      von_prefix = structure(
        "(\\b([Vv][oa]n|[dD][eua])\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Find a von/van/de/du/da followed by space (0 or more)"
      ),
      last_name = structure(
        "[A-Z]{1}[a-zA-Z-']+",
        class = c(
          "subcom",
          "character"
        ),
        comment = "A last name (capital followed by one or more letters (1 or more times)"
      ),
      comma_space = structure(
        ",*\\s",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Optiona comma and a space"
      ),
      and = structure(
        "(\\&|and)",
        class = c("subcom", "character"),
        comment = "And and sign"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1.A.1 END"
      ),
      space = "\\s",
      structure(
        ")*",
        class = c("subcom", "character"),
        comment = "GROUP 1.A END"
      ),
      letter_period = structure(
        "([A-Z]\\.\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Single capital letter followed by a period (0 or more times)"
      ),
      von_prefix = structure(
        "(\\b([Vv][oa]n|[dD][eua])\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Find a von/van/de/du/da followed by space (0 or more)"
      ),
      last_name = structure(
        "[A-Z]{1}[a-zA-Z-']+",
        class = c(
          "subcom",
          "character"
        ),
        comment = "A last name (capital followed by one or more letters (1 or more times)"
      ),
      space = structure(
        "\\s{0,1}",
        class = c("subcom", "character"),
        comment = "0 or more spaces"
      ),
      et_al = structure(
        "(et\\sal\\.){0,1}",
        class = c(
          "subcom",
          "character"
        ),
        comment = "et al. 0 or 1 times)"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 1 END"
      ),
      or = structure(
        "|",
        class = c(
          "subcom",
          "character"
        ),
        comment = "OR"
      ),
      structure(
        "(",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP 2 START"
      ),
      von_prefix = structure(
        "(\\b([Vv][oa]n|[dD][eua])\\s)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Find a von/van/de/du/da followed by space (0 or more)"
      ),
      last_name = structure(
        "[A-Z][A-Za-z'-]*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "A last name (capital followed by one or more letters (0 or more times)"
      ),
      space = "\\s",
      structure(
        ")+",
        class = c("subcom", "character"),
        comment = "GROUP 2 END (1 or more times)"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "LAST NAME GROUP END"
      ),
      comma_space = ",\\s",
      structure(
        "(",
        class = c("subcom", "character"),
        comment = "YEAR GROUP START"
      ),
      structure(
        "(",
        class = c("subcom", "character"),
        comment = "GROUP  START"
      ),
      year = structure(
        "(\\d{4}[a-z]{0,1})",
        class = c(
          "subcom",
          "character"
        ),
        comment = "4 digit year optionally followed by 1 lower case letter"
      ),
      or = structure(
        "|",
        class = c("subcom", "character"),
        comment = "OR"
      ),
      no_date = structure(
        "(n\\.d\\.)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "No date"
      ),
      or = structure(
        "|",
        class = c(
          "subcom",
          "character"
        ),
        comment = "OR"
      ),
      in_press = structure(
        "(in press)",
        class = c(
          "subcom",
          "character"
        ),
        comment = "In press"
      ),
      structure(
        ")",
        class = c(
          "subcom",
          "character"
        ),
        comment = "GROUP  END"
      ),
      comma_space = structure(
        "(,*\\s*)*",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Optional comma and optional space (0 or more times)"
      ),
      not_pages = structure(
        "(?!(\\s{0,1}p{1,2}(ara){0,1}\\.))",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Don't pull out the pages (including para) as a year part"
      ),
      structure(
        ")+",
        class = c("subcom", "character"),
        comment = "YEAR GROUP END"
      ),
      left_para = structure(
        "(?=[)]{0,1})",
        class = c(
          "subcom",
          "character"
        ),
        comment = "Optional ending closed parenthesis (0 or 1 times)"
      )
    ),
    comments = list(
      "LAST NAME GROUP START",
      "GROUP 1 START (MULTIPLE AUTHORS)",
      ensure_start = "BEGIN ENSURE",
      para_or_semi = "string is lead by an open parenthesis or semicolon + space",
      or = "OR",
      "GROUP ENSURE.1 START",
      left_par = "open parenthesis",
      "GROUP ENSURE.1a START",
      eg = "e.g.",
      or = "OR",
      cf = "cf.",
      or = "OR",
      viz = "viz.",
      "GROUP ENSURE.1a END",
      coma_space = "Optional comma followed by a space",
      "END GROUP ENSURE.1",
      ensure_end = "END ENSURE",
      "GROUP 1.A START",
      "GROUP 1.A.1 START",
      "GROUP 1.A.1.a START",
      von_prefix = "Find a von/van/de/du/da followed by space (0 or more)",
      last_name = "A last name (capital followed by one or more letters (1 or more times)",
      comma_space = "Comma and a space",
      "GROUP 1.A.1.a END (0 or more)",
      von_prefix = "Find a von/van/de/du/da followed by space (0 or more)",
      last_name = "A last name (capital followed by one or more letters (1 or more times)",
      comma_space = "Optiona comma and a space",
      and = "And and sign",
      "GROUP 1.A.1 END",
      space = NULL,
      "GROUP 1.A END",
      letter_period = "Single capital letter followed by a period (0 or more times)",
      von_prefix = "Find a von/van/de/du/da followed by space (0 or more)",
      last_name = "A last name (capital followed by one or more letters (1 or more times)",
      space = "0 or more spaces",
      et_al = "et al. 0 or 1 times)",
      "GROUP 1 END",
      or = "OR",
      "GROUP 2 START",
      von_prefix = "Find a von/van/de/du/da followed by space (0 or more)",
      last_name = "A last name (capital followed by one or more letters (0 or more times)",
      space = NULL,
      "GROUP 2 END (1 or more times)",
      "LAST NAME GROUP END",
      comma_space = NULL,
      "YEAR GROUP START",
      "GROUP  START",
      year = "4 digit year optionally followed by 1 lower case letter",
      or = "OR",
      no_date = "No date",
      or = "OR",
      in_press = "In press",
      "GROUP  END",
      comma_space = "Optional comma and optional space (0 or more times)",
      not_pages = "Don't pull out the pages (including para) as a year part",
      "YEAR GROUP END",
      left_para = "Optional ending closed parenthesis (0 or 1 times)"
    )
  ),
  rm_citation_tex = "\\\\[a-zA-Z0-9]{0,}cite[a-zA-Z0-9]{0,}(\\[([^]]+)\\]){0,2}\\**\\{([a-zA-Z0-9 ,]+)\\}",
  rm_city_state = "([A-Z][\\w-]*(\\s+[A-Z][\\w-]*)+),\\s*([A-Z]{2})\\b|(\\b[A-Za-z]+),\\s*([A-Z]{2})\\b",
  rm_city_state_zip = "([A-Z][\\w-]*(\\s+[A-Z][\\w-]*)+),\\s*([A-Z]{2})\\s*(?<!\\d)\\d{5}(?:[ -]\\d{4})?\\b|(\\b[A-Za-z]+),\\s*([A-Z]{2})\\s*(?<!\\d)\\d{5}(?:[ -]\\d{4})?\\b",
  rm_date = "\\d{0,2}/\\d{2}/(?:\\d{4}|\\d{2})?|\\d{0,2}-\\d{2}-(?:\\d{4}|\\d{2})?|\\d{0,2}\\.\\d{2}\\.(?:\\d{4}|\\d{2})?",
  rm_date2 = "(\\b)([A-Za-z]{3,9})(\\s+)([0-9][0-9]*)(,)(\\s+)([0-9]{4})",
  rm_date3 = "[0-9]{4}-[0-9]{2}-[0-9]{2}",
  rm_date4 = "\\d{0,2}/\\d{2}/(?:\\d{4}|\\d{2})?|\\d{0,2}-\\d{2}-(?:\\d{4}|\\d{2})?|\\d{0,2}\\.\\d{2}\\.(?:\\d{4}|\\d{2})?|(\\b)([A-Za-z]{3,9})(\\s+)([0-9][0-9]*)(,)(\\s+)([0-9]{4})|[0-9]{4}-[0-9]{2}-[0-9]{2}",
  rm_dollar = "\\$\\(?[0-9.,]+\\)?",
  rm_email = "([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))",
  rm_emoticon = ">?[:;=8XB]{1}[-~+o^]?[|\")(&gt;DO>{pP3/]+|</?3|XD+|D:<|x[-~+o^]?[|\")(&gt;DO>{pP3/]+",
  rm_endmark = "[\\!\\*\\.\\?\\|]+$",
  rm_endmark2 = "[\\!\\.\\?]+$",
  rm_endmark3 = "[\\!\\*\\.\\;\\?\\|\\:]+$",
  rm_hash = "(?<!/)((#)(\\w+))",
  rm_nchar_words = "(?<![\\w'])(?:'?\\w'?){%s}(?![\\w'])",
  rm_nchar_words2 = "(?<![\\w'])([\\w']){%s}(?![\\w'])",
  rm_non_ascii = "(<[0-9a-f]{2}>)",
  rm_non_words = "[^\\p{L}']+",
  rm_number = "(?<=^| )[-.]*\\d+(?:\\.\\d+)?(?= |\\.?$)|\\d+(?:,\\d{3})+(\\.\\d+)*",
  rm_percent = "\\(?[0-9.]+\\)?%",
  rm_phone = "(?:(?:\\+?1\\s*(?:[.-]\\s*)?)?(?:\\(\\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])\\s*\\)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\\s*(?:[.-]\\s*)?)?([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\\s*(?:[.-]\\s*)?([0-9]{4})(?:\\s*(?:#|x\\.?|ext\\.?|extension)\\s*(\\d+))?",
  rm_postal_code = "((A[LKZR])|(C[AOT])|(D[EC])|(FL)|(GA)|(HI)|(I[DLNA])|(K[SY])|(LA)|(M[EDAINSOT])|(N[EVHJMYCD])|(O[HKR])|(PA)|(RI)|(S[CD])|(T[NX])|(UT)|(V[TA])|(W[AVIY]))",
  rm_repeated_characters = "\\b(\\S+?)\\1\\S*\\b",
  rm_repeated_phrases = "(?i)\\b(\\w.*)((?:\\s|\\.{3}|,)+\\1)+\\b",
  rm_repeated_words = "(?i)\\b(\\w+)\\s+\\1\\b",
  rm_tag = "(?<![@\\w])(@)(([a-z0-9_]+)\\b)",
  rm_tag2 = "(?<![@\\w])(@)(([a-z0-9_]{1,15})\\b)",
  rm_title_name = "(((Dr|Mr|Mrs|Ms|dr|mr|mrs|ms)(\\.))|(Miss|Mizz|mizz))(\\s+)([A-Za-z]+)(\\s[A-Z][A-Za-z]*\\b)*",
  rm_time = "\\d{0,2}:\\d{2}(?:[:.]\\d+)?",
  rm_time2 = "(\\d{0,2}:\\d{2}(?:[:.]\\d+)?)(\\s+(([AP]\\.{0,1}M\\.{0,1})|([ap]\\.{0,1}m\\.{0,1})))",
  rm_transcript_time = "(#)?([0-9]){1,2}:?([0-9]){1,2}[:.]([0-9]){2}([:,.-][0-9]+)?#??|\\d{1}:\\d{2}",
  rm_twitter_url = "(https?://t\\.co[^ ]*)|(t\\.co[^ ]*)",
  rm_url = "(http[^ ]*)|(ftp[^ ]*)|(www\\.[^ ]*)",
  rm_url2 = "(((https?|ftps?)://)|(www\\.))(-\\.)?([^\\s/?\\.#-]+\\.?)+(/[^\\s]*)?",
  rm_url3 = "(https?|ftps?)://(-\\.)?([^\\s/?\\.#-]+\\.?)+(/[^\\s]*)?",
  rm_white = "^\\s+|\\s+$|\\s+(?=[.](?:\\D|$))|(\\s+)(?=[,]|[;:?!\\]\\}\\)]+)|(?<=[\\(\\[\\{])(\\s+)|(\\s+)(?=[\\s])",
  rm_white_bracket = "(\\s+)(?=[\\)\\]\\}])|(?<=[\\(\\[\\{])(\\s+)",
  rm_white_colon = "(\\s+)(?=[\\:\\;]+)",
  rm_white_comma = "(\\s+)(?=,)",
  rm_white_endmark = "\\s+(?=[.?!](?:\\D|$))",
  rm_white_lead = "^\\s+",
  rm_white_lead_trail = "^\\s+|\\s+$",
  rm_white_multiple = "(\\s+)(?=[\\s])",
  rm_white_punctuation = "\\s+(?=[.](?:\\D|$))|(\\s+)(?=[,]|[;:?!]+)",
  rm_white_trail = "\\s+$",
  rm_zip = "(?<!\\d)\\d{5}(?:[ -]\\d{4})?\\b"
)

usethis::use_data(dictionary, overwrite = TRUE)

rm(list = ls())

# maps ----------------------------------------------------------------------------------------

library(sf)
library(stars)
library(sp)
library(raster)

london_gpkg <-
  sf::st_read(dsn = "data-raw/london_shapefile.gpkg", as_tibble = TRUE)

map_fromto <- read_rds("data-raw/map_fromto.rds")

map_grid <- read_rds("data-raw/map_grid.rds")

map_hex <- read_rds("data-raw/map_hex.rds")

map_pois <- read_rds("data-raw/map_pois.rds")

map_pois <- list(pois = map_pois)

hexbin_data <- read_rds("data-raw/hexbin_data.rds")

usethis::use_data(london_gpkg, overwrite = TRUE)
usethis::use_data(map_fromto, overwrite = TRUE)
usethis::use_data(map_grid, overwrite = TRUE)
usethis::use_data(map_hex, overwrite = TRUE)
usethis::use_data(hexbin_data, overwrite = TRUE)
usethis::use_data(map_pois, overwrite = TRUE)

rm(list = ls())
mgalda::update_namespace()

