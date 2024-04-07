#' modelo en dos pasos
#'
#' @param first_step df
#' @param second_step submodelos
#' @param first_class numero de resamples
#'
#' @return modelo de clasificacion en dos pasos
#' @export
#' @rdname twostep

twostep <-
  function(first_step, second_step, first_class) {
    UseMethod("twostep")
  }

#' crear modelo
#'
#' @rdname make_twostep
#' @export

make_twostep <-
  function(first_step, second_step, first_class) {
    model <-
      list(
        first_step = first_step,
        second_step = second_step,
        first_class = first_class
      )
    class(model) <- "twostep"
    model
  }

#' @export
predict.twostep <-
  function(object,
           new_data) {
    preds_first <-
      predict(object = object$first_step, new_data = new_data) %>%
      dplyr::select(tidyselect::contains(object$first_class)) %>%
      mutate(.row = row_number())

    preds_second <-
      predict(object = object$second_step, new_data = new_data) %>%
      mutate(.row = row_number(), .pred_class = NULL)

    names_first <- names(preds_first)[1]
    names_first <- rlang::sym(names_first)
    names_first <- rlang::enquo(names_first)
    names_second <- names(preds_second)[1:2]

    preds_first01 <- preds_first[preds_first[[1]] > .5, ]

    preds_first01 <-
      preds_first01 %>%
      inner_join(preds_second, by = ".row") %>%
      mutate_at(
        .vars = vars(tidyselect::matches(names_second)),
        .funs = ~ (1 - !!names_first) * .x
      )

    preds_first02 <- preds_first[preds_first[[1]] <= .5, ]

    preds_first02 <-
      preds_first02 %>%
      inner_join(preds_second, by = ".row") %>%
      mutate(!!rlang::as_label(names_first) := !!names_first * .33 / .5) %>%
      mutate_at(
        .vars = vars(tidyselect::matches(names_second)),
        .funs = ~ (1 - !!names_first) * .x
      )

    preds <-
      bind_rows(preds_first01, preds_first02) %>%
      arrange(.row) %>%
      dplyr::relocate(.row, .before = everything())

    preds_class <-
      preds %>%
      pivot_longer(!.row, names_to = ".pred_class", names_prefix = ".pred_") %>%
      group_by(.row) %>%
      filter(value == max(value)) %>%
      ungroup() %>%
      dplyr::select(!value)

    inner_join(preds_class, preds, by = ".row") %>% dplyr::select(!.row)
  }
