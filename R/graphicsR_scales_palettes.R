#' @include graphicsR_utils.R

#' Apply func
#'
#' @name gr_graphics
#' @rdname gr_graphics
#' @keywords internal
NULL

# edit scale

#' grf_scale
#'
#' @param .scale axis scale. Allowed values are
#' one of c("none", "log2", "log10",
#' "sqrt", "percent", "dollar", "scientific",
#' "numeric,"millones,"miles); e.g.: .scale="log2".
#' @param .format ogical value. If TRUE, axis tick
#' mark labels will be formatted
#'  when .scale  = "log2" or "log10".
#' @export
grf_xscale <- function(object, .scale, .format = FALSE) {
  object + .gr_scale(
    .scale = .scale,
    .format = .format,
    .fn = ggplot2::scale_x_continuous
  )
}

#' @export
grf_yscale <- function(object, .scale, .format = FALSE) {
  object + .gr_scale(
    .scale = .scale,
    .format = .format,
    .fn = ggplot2::scale_y_continuous
  )
}

.gr_scale <- function(.scale, .format = FALSE, .fn) {
  .x <- NULL
  if (.format &
    .scale %in% c(
      "percent",
      "dollar",
      "scientific",
      "numeric",
      "miles",
      "millones"
    )) {
    .format <- FALSE
  }
  if (.format) {
    if (.scale == "log2") {
      .fn(
        trans = scales::log2_trans(),
        breaks = scales::trans_breaks("log2", function(x) {
          2^x
        }),
        labels = scales::trans_format("log2", scales::math_format(2^.x))
      )
    } else if (.scale == "log10") {
      .fn(
        trans = scales::log10_trans(),
        breaks = scales::trans_breaks("log10", function(x) {
          10^x
        }),
        labels = scales::trans_format("log10", scales::math_format(10^
          .x))
      )
    }
  } else if (.scale %in% c("log2", "log10")) {
    .fn(trans = .scale)
  } else {
    .f_format <- switch(.scale,
      percent = scales::percent,
      dollar = scales::dollar,
      scientific = scales::scientific,
      numeric = num_format,
      miles = .miles_format,
      millones = .millones_format
    )

    .fn(labels = .f_format)
  }
}
.miles_format <- function(x) {
  scales::number_format(
    big.mark = ".",
    decimal.mark = ",",
    accuracy = 1,
    scale = .001,
    suffix = "k"
  )(x)
}
.millones_format <- function(x) {
  scales::number_format(
    big.mark = ".",
    decimal.mark = ",",
    accuracy = 1,
    scale = .000001,
    suffix = "m"
  )(x)
}

## scale_breacks & scale_cuts

#' @export
scale_x_break <-
  function(breaks,
           scales = "fixed",
           ticklabels = NULL,
           expand = TRUE,
           space = 0.1) {
    require(patchwork)
    scale_break("x", breaks, scales, ticklabels, expand, space)
  }
#' @export
scale_y_break <-
  function(breaks,
           scales = "fixed",
           ticklabels = NULL,
           expand = TRUE,
           space = .1) {
    require(patchwork)
    scale_break("y", breaks, scales, ticklabels, expand, space)
  }

#' @export
scale_wrap <- function(n) {
  require(patchwork)
  structure(list(n = n, expand = FALSE),
    class = "wrap_params"
  )
}

#' @export
scale_x_cut <-
  function(breaks,
           which = NULL,
           scales = NULL,
           expand = FALSE,
           space = .1) {
    require(patchwork)
    scale_cut(
      axis = "x",
      breaks = breaks,
      which = which,
      scales = scales,
      expand = expand,
      space = space
    )
  }
#' @export
scale_y_cut <-
  function(breaks,
           which = NULL,
           scales = NULL,
           expand = FALSE,
           space = .1) {
    require(patchwork)
    scale_cut(
      axis = "y",
      breaks = breaks,
      which = which,
      scales = scales,
      expand = expand,
      space = space
    )
  }

#' @keywords internal
scale_cut <-
  function(axis,
           breaks,
           which = NULL,
           scales = NULL,
           expand = FALSE,
           space = .1) {
    structure(
      list(
        axis = axis,
        breaks = breaks,
        which = which,
        scales = scales,
        expand = expand,
        space = space
      ),
      class = "ggcut_params"
    )
  }
#' @keywords internal
scale_break <-
  function(axis,
           breaks,
           scales,
           ticklabels = NULL,
           expand = TRUE,
           space = .1) {
    structure(
      list(
        axis = axis,
        breaks = breaks,
        scales = scales,
        ticklabels = ticklabels,
        expand = expand,
        space = space
      ),
      class = "ggbreak_params"
    )
  }

## scale focus

#' @export
scale_size_focus <- function(...,
                             size_focus = 3,
                             size_other = 1) {
  focus_levels <- eval(substitute(alist(...)))
  structure(
    list(
      focus_levels = focus_levels,
      size_focus = size_focus,
      size_other = size_other
    ),
    class = "ggfocus_size"
  )
}
#' @export
ggplot_add.ggfocus_size <- function(object, plot, object_name) {
  p1 <- plot
  focus_levels <- object$focus_levels
  size_focus <- object$size_focus
  size_other <- object$size_other
  var <- p1$mapping$size

  if (is.null(var)) {
    cat_stop(
      "'size' isn't mapped by any variable. Use '+ aes(size = ...)' before setting the focus scale."
    )
  }
  res <- rep(TRUE, nrow(p1$data))
  for (.levels in focus_levels) {
    res <- do_try(eval(.levels, df2env(p1$data))) & res
  }
  p1$data <- p1$data %>%
    mutate(.marker_size = ifelse(res, as.character(!!var), "Other"))

  if (sum(p1$data$.marker_size == "Other") == 0) {
    cat_stop("Every observation is focused. Use less values in 'focus_levels'.")
  }

  if (sum(p1$data$.marker_size != "Other") == 0) {
    cat_message(
      "There are no observations selected. Are the levels misspelled? Is the correct variable mapped to 'alpha'?"
    )
  }

  n_levels <- p1$data$.marker_size %>%
    unique() %>%
    length()

  size_values <- rep(size_focus, n_levels)
  names(size_values) <- p1$data$.marker_size %>% unique()
  size_values["Other"] <- size_other

  p1 +
    aes(size = .marker_size) +
    scale_size_manual(
      values = size_values,
      breaks = NULL
    )
}

#' @export
scale_color_focus <-
  function(...,
           color_focus = NULL,
           color_other = "gray",
           palette_focus = "Set1") {
    focus_levels <- eval(substitute(alist(...)))

    structure(
      list(
        focus_levels = focus_levels,
        color_focus = color_focus,
        color_other = color_other,
        palette_focus = palette_focus
      ),
      class = "ggfocus_color"
    )
  }
#' @export
ggplot_add.ggfocus_color <- function(object, plot, object_name) {
  p1 <- plot
  focus_levels <- object$focus_levels
  color_focus <- object$color_focus
  color_other <- object$color_other
  palette_focus <- object$palette_focus
  var <-
    ifelse(
      !is.null(p1$mapping$colour),
      chr_cllp(focus_levels, lang2str, .collapse = " & "),
      NULL
    )
  var <- gsub('\"', "'", var)

  if (is.null(var)) {
    cat_message(
      "'color' isn't mapped in any variable. Use 'aes(color = ...)' before setting the focus scale."
    )
    return(plot)
  }

  res <- rep(TRUE, nrow(p1$data))
  for (.levels in focus_levels) {
    res <- do_try(eval(.levels, df2env(p1$data))) & res
  }
  p1$data <- p1$data %>%
    mutate(.marker_color = ifelse(res, var, "Other"))

  if (sum(p1$data$.marker_color == "Other") == 0) {
    cat_stop("Every observation is focused. Use less values in 'focus_levels'.")
  }

  if (sum(p1$data$.marker_color != "Other") == 0) {
    cat_message(
      "There are no observations selected. Are the levels misspelled? Is the correct variable mapped to 'color'?"
    )
  }

  n_levels <- p1$data$.marker_color %>%
    unique() %>%
    length()

  if (is.null(color_focus)) {
    color_focus <-
      suppressWarnings(RColorBrewer::brewer.pal(n_levels - 1, palette_focus)[1:(n_levels - 1)])
  }

  if (length(color_focus) != 1 &
    length(color_focus) != length(focus_levels)) {
    cat_stop("color_focus must be of length 1 or same length as focus_levels.")
  }
  color_values <- rep(color_other, n_levels)
  names(color_values) <- p1$data$.marker_color %>% unique()
  color_values[var] <- color_focus

  p1 +
    aes(color = .marker_color) +
    scale_color_manual(values = color_values, name = NULL)
}

#' @export
scale_fill_focus <-
  function(...,
           color_focus = NULL,
           color_other = "gray",
           palette_focus = "Set1") {
    focus_levels <- eval(substitute(alist(...)))

    structure(
      list(
        focus_levels = focus_levels,
        color_focus = color_focus,
        color_other = color_other,
        palette_focus = palette_focus
      ),
      class = "ggfocus_fill"
    )
  }
#' @export
ggplot_add.ggfocus_fill <- function(object, plot, object_name) {
  p1 <- plot
  focus_levels <- object$focus_levels
  color_focus <- object$color_focus
  color_other <- object$color_other
  palette_focus <- object$palette_focus
  var <-
    ifelse(!is.null(p1$mapping$fill),
      chr_cllp(focus_levels, lang2str, .collapse = " & "),
      NULL
    )
  var <- gsub('\"', "'", var)

  if (is.null(var)) {
    cat_message(
      "'fill' isn't mapped in any variable. Use 'aes(fill = ...)' before setting the focus scale."
    )
    return(plot)
  }

  res <- rep(TRUE, nrow(p1$data))
  for (.levels in focus_levels) {
    res <- do_try(eval(.levels, df2env(p1$data))) & res
  }
  p1$data <- p1$data %>%
    mutate(.marker_fill = ifelse(res, var, "Other"))

  if (sum(p1$data$.marker_fill == "Other") == 0) {
    cat_stop("Every observation is focused. Use less values in 'focus_levels'.")
  }

  if (sum(p1$data$.marker_fill != "Other") == 0) {
    cat_message(
      "There are no observations selected. Are the levels misspelled? Is the correct variable mapped to 'color'?"
    )
  }

  n_levels <- p1$data$.marker_fill %>%
    unique() %>%
    length()

  if (is.null(color_focus)) {
    color_focus <-
      suppressWarnings(RColorBrewer::brewer.pal(n_levels - 1, palette_focus)[1:(n_levels - 1)])
  }

  if (length(color_focus) != 1 &
    length(color_focus) != length(focus_levels)) {
    cat_stop("color_focus must be of length 1 or same length as focus_levels.")
  }
  fill_values <- rep(color_other, n_levels)
  names(fill_values) <- p1$data$.marker_fill %>% unique()

  fill_values[var] <- color_focus
  p1 +
    aes(fill = .marker_fill) +
    scale_fill_manual(values = fill_values, breaks = var)
}

#' @export
scale_alpha_focus <-
  function(...,
           alpha_focus = 3,
           alpha_other = 1) {
    focus_levels <- eval(substitute(alist(...)))
    structure(
      list(
        focus_levels = focus_levels,
        alpha_focus = alpha_focus,
        alpha_other = alpha_other
      ),
      class = "ggfocus_alpha"
    )
  }
#' @export
ggplot_add.ggfocus_alpha <- function(object, plot, object_name) {
  p1 <- plot
  focus_levels <- object$focus_levels
  alpha_focus <- object$alpha_focus
  alpha_other <- object$alpha_other
  var <- p1$mapping$alpha

  if (is.null(var)) {
    cat_stop(
      "'alpha' isn't mapped by any variable. Use '+ aes(alpha = ...)' before setting the focus scale."
    )
  }
  res <- rep(TRUE, nrow(p1$data))
  for (.levels in focus_levels) {
    res <- do_try(eval(.levels, df2env(p1$data))) & res
  }
  p1$data <- p1$data %>%
    mutate(.marker_alpha = ifelse(res, as.character(!!var), "Other"))

  if (sum(p1$data$.marker_alpha == "Other") == 0) {
    cat_stop("Every observation is focused. Use less values in 'focus_levels'.")
  }

  if (sum(p1$data$.marker_alpha != "Other") == 0) {
    cat_message(
      "There are no observations selected. Are the levels misspelled? Is the correct variable mapped to 'alpha'?"
    )
  }

  n_levels <- p1$data$.marker_alpha %>%
    unique() %>%
    length()

  alpha_values <- rep(alpha_focus, n_levels)
  names(alpha_values) <- p1$data$.marker_alpha %>% unique()
  alpha_values["Other"] <- alpha_other

  p1 +
    aes(alpha = .marker_alpha) +
    scale_alpha_manual(
      values = alpha_values,
      breaks = NULL
    )
}


## lims

#' @export
xlim_tree <- function(xlim) {
  xlim_expand(xlim, panel = "Tree")
}
#' @export
xlim_expand <- function(xlim, panel) {
  structure(list(x = xlim, panel = panel), class = "facet_xlim")
}
#' @export
scale_x_range <- function() {
  structure(list(), class = "range_xaxis")
}


#' @export
xlim2 <- function(gg, limits = NULL) {
  axis_align(
    gg = gg,
    limits = limits,
    axis = "x"
  )
}
#' @export
ylim2 <- function(gg, limits = NULL) {
  axis_align(
    gg = gg,
    limits = limits,
    axis = "y"
  )
}
#' @export
yrange <- function(gg) {
  ggrange(gg, "y")
}
#' @export
xrange <- function(gg) {
  ggrange(gg, "x")
}
#' @keywords internal
ggrange <- function(gg, var) {
  res <- layer_scales(gg)[[var]]$limits
  if (is.null(res)) {
    res <- layer_scales(gg)[[var]]$range$range
  }
  if (is.character(res)) {
    return(res)
  }

  var <- paste0(var, ".range")
  ggplot_build(gg)$layout$panel_params[[1]][[var]]
}
#' @keywords internal
ggrange2 <- function(plot, var) {
  var <- paste0("panel_scales_", var)
  gb <- ggplot_build(plot)
  limits <- gb$layout[[var]][[1]]$limits
  if (!is.null(limits)) {
    axis_range <- limits
  } else {
    axis_range <- gb$layout[[var]][[1]]$range$range
  }
  flagrev <- gb$layout[[var]][[1]]$trans$name
  transfun <- gb$layout[[var]][[1]]$trans$transform
  inversefun <- gb$layout[[var]][[1]]$trans$inverse
  list(
    axis_range = axis_range,
    flagrev = flagrev,
    transfun = transfun,
    inversefun = inversefun
  )
}
#' @keywords internal
axis_align <- function(gg, limits = NULL, axis) {
  if (is.null(limits)) {
    if (axis == "x") {
      limits <- xrange(gg)
    } else {
      limits <- yrange(gg)
    }
  }
  structure(list(limits = limits, axis = axis),
    class = "axisAlign"
  )
}


## layout

#' @export
facet_set <- function(label, side = "t", angle = NULL) {
  side <- match.arg(side, c("top", "right"))

  structure(list(
    label = label,
    side  = side,
    angle = angle
  ),
  class = "facet_set"
  )
}

#' @export
contraster_label_bw <- function(label = NULL,
                                stat = "identity",
                                position = "identity",
                                ...,
                                nudge_x = 0,
                                nudge_y = 0,
                                check_overlap = FALSE,
                                na.rm = FALSE) {
  ggcall <- match.call()

  if (is_empty(ggcall$label)) {
    cat_stop("label es requerido")
  }

  ggcall[[1L]] <- quote(geom_text)

  aes_call <-
    interp(quote(aes(label = label)), label = ggcall$label)

  ggcall[["label"]] <- NULL
  ggcall[["mapping"]] <- aes_call

  class(ggcall) <- c("contraster_label", class(ggcall))
  ggcall
}
#' @export
insert_axis_plot <-
  function(.data,
           plot,
           side = c("left", "right", "bottom", "top"),
           width_height = 1) {
    get_aes_var <- function(mapping, var) {
      res <- rlang::quo_text(mapping[[var]])
      tail(res, 1)
    }
    insert_left <- function(.data, plot, width = 1) {
      insert_lr(
        .data = .data,
        plot = plot,
        width = width,
        side = "left"
      )
    }
    insert_right <- function(.data, plot, width = 1) {
      insert_lr(
        .data = .data,
        plot = plot,
        width = width,
        side = "right"
      )
    }
    insert_top <- function(.data, plot, height = 1) {
      insert_tb(
        .data = .data,
        plot = plot,
        height = height,
        side = "top"
      )
    }
    insert_bottom <- function(.data, plot, height = 1) {
      insert_tb(
        .data = .data,
        plot = plot,
        height = height,
        side = "bottom"
      )
    }

    insert_tb <- function(.data, plot, height, side) {
      side <- match.arg(side, c("top", "bottom"))
      .data <- as_aplot(.data)
      .data$n <- .data$n + 1

      new_row <- matrix(nrow = 1, ncol = ncol(.data$layout))
      new_row[.data$main_col] <- .data$n

      if (side == "top") {
        .data$height <- c(height, .data$height)
        .data$layout <- rbind(new_row, .data$layout)
        .data$main_row <- .data$main_row + 1
      } else {
        .data$height <- c(.data$height, height)
        .data$layout <- rbind(.data$layout, new_row)
      }


      if (is_ggtree(plot)) {
        selected <- .data$layout[, .data$main_col]
        selected <- selected[!is.na(selected)]
        selected <- selected[selected != .data$n]

        for (i in selected) {
          if (is_coord_flip(.data$plotlist[[i]])) {
            yvar <- get_aes_var(.data$plotlist[[i]]$mapping, "y")
            lvs <- rev(get_taxa_order(plot))

            axis_trans <- list(
              aes(y = factor(.data[[yvar]],
                levels = lvs
              )),
              ylab(.data$plotlist[[i]]$labels$y)
            )
          } else {
            xvar <- get_aes_var(.data$plotlist[[i]]$mapping, "x")
            lvs <- rev(get_taxa_order(plot))

            axis_trans <- list(
              aes(x = factor(.data[[xvar]],
                levels = lvs
              )),
              xlab(.data$plotlist[[i]]$labels$x)
            )
          }
          .data$plotlist[[i]] <- .data$plotlist[[i]] + axis_trans
        }
      }

      .data$plotlist[[.data$n]] <- plot
      .data
    }
    insert_lr <- function(.data, plot, width, side) {
      side <- match.arg(side, c("left", "right"))
      .data <- as_aplot(.data)
      .data$n <- .data$n + 1

      new_col <- matrix(nrow = nrow(.data$layout), ncol = 1)
      new_col[.data$main_row] <- .data$n

      if (side == "left") {
        .data$width <- c(width, .data$width)
        .data$layout <- cbind(new_col, .data$layout)
        .data$main_col <- .data$main_col + 1
      } else {
        .data$width <- c(.data$width, width)
        .data$layout <- cbind(.data$layout, new_col)
      }

      if (is_ggtree(plot)) {
        ## re-order based on the tree
        selected <- .data$layout[.data$main_row, ]
        selected <- selected[!is.na(selected)]
        selected <- selected[selected != .data$n]
        for (i in selected) {
          if (is_coord_flip(.data$plotlist[[i]])) {
            xvar <- get_aes_var(.data$plotlist[[i]]$mapping, "x")
            lvs <- rev(get_taxa_order(plot))

            axis_trans <- list(
              aes(x = factor(.data[[xvar]],
                levels = lvs
              )), ## c(.data[[xvar]][!.data[[xvar]] %in% lvs], lvs))),
              xlab(.data$plotlist[[i]]$labels$x)
            )
          } else {
            yvar <- get_aes_var(.data$plotlist[[i]]$mapping, "y")
            lvs <- rev(get_taxa_order(plot))

            axis_trans <- list(
              aes(y = factor(.data[[yvar]],
                levels = lvs
              )), ## c(.data[[yvar]][!.data[[yvar]] %in% lvs], lvs))),
              ylab(.data$plotlist[[i]]$labels$y)
            )
          }
          .data$plotlist[[i]] <- .data$plotlist[[i]] + axis_trans
        }
      }

      .data$plotlist[[.data$n]] <- plot
      .data
    }


    if (side %in% c("bottom", "top")) {
      insert_tb(
        .data = .data,
        plot = plot,
        height = width_height,
        side = side
      )
    } else if (side %in% c("left", "right")) {
      insert_lr(
        .data = .data,
        plot = plot,
        width = width_height,
        side = side
      )
    }
  }

#' @export
tree_layout <-
  function(treeview,
           layout = c(
             "rectangular",
             "circular",
             "dendrogram",
             "fan",
             "inward_circular",
             "open",
             "rotate"
           ),
           angle = 100,
           xlim = NULL) {
    switch(layout,
      circular = layout_circular(),
      dendrogram = layout_dendrogram(),
      fan = layout_fan(angle = angle),
      inward_circular = layout_inward_circular(xlim = xlim),
      rectangular = layout_rectangular(),
      open = open_tree(treeview = treeview, angle = angle),
      rotate = rotate_tree(treeview = treeview, angle = angle)
    )
  }

#' @export
gr_grid <-
  function(...,
           gglist = NULL,
           ncol = NULL,
           nrow = NULL,
           byrow = NULL,
           widths = NULL,
           heights = NULL,
           guides = NULL,
           labels = NULL,
           tag_levels = NULL,
           tag_size = 14,
           design = NULL) {
    gglist <- c(list(...), gglist)
    name <- names(gglist)

    for (i in seq_along(gglist)) {
      if (is_ggbreak(gglist[[i]])) {
        gglist[[i]] <- as.ggplot.gg(grid.draw(gglist[[i]]))
      }
    }

    if (!is.null(name)) {
      for (i in seq_along(gglist)) {
        if (name[i] != "") {
          if (inherits(gglist[[i]], "patchwork")) {
            gglist[[i]] <- as.ggplot.gg(gglist[[i]])
          }

          gglist[[i]] <-
            gglist[[i]] + facet_set(label = name[i])
        }
      }
    }

    if (!is.null(labels)) {
      tag_levels <- NULL
      n <- min(length(labels), length(gglist))
      for (i in seq_len(n)) {
        if (labels[i] == "") {
          next
        }
        gglist[[i]] <- gglist[[i]] + labs(tag = labels[i])
      }
    }

    p <- suppressall(
      Reduce(`+`, gglist, init = plot_filler()) +
        patchwork::plot_layout(
          ncol = ncol,
          nrow = nrow,
          byrow = byrow,
          widths = widths,
          heights = heights,
          guides = guides,
          design = design
        )
    )

    if (!is.null(tag_levels) || !is.null(labels)) {
      pt <- p$theme$plot.tag
      if (is.null(pt)) {
        pt <- ggplot2::element_text()
      }
      pt <- modifyList(pt, list(size = tag_size))
      p <- p + patchwork::plot_annotation(tag_levels = tag_levels) &
        theme(plot.tag = pt)
    }
    return(p)
  }

#' @keywords internal
plot_filler <- utils::getFromNamespace("plot_filler", "patchwork")

# paletas

#' @keywords internal
d_colours <- function(colours, order) {
  nmax <- length(colours)
  function(n) {
    if (n > nmax) {
      cat_warn(
        "Insufficient values in scale_pal_gr. ",
        n,
        " needed but only ",
        nmax,
        " provided."
      )
    }

    if (is_empty(order)) {
      order <- seq(0, 1, length.out = n)
    }

    if (max(order) > 1 | min(order) < 0) {
      order <- rescalar(x = order, to = c(0, 1))
    }

    vct_pal <- colour_ramp(colours)
    vct_pal(x = order)
  }
}

#' @rdname gr_graphics
#' @export
scale_pal_gr <- function(palette = NULL,
                         scale = c("continuous", "discrete", "binned"),
                         alpha = 1,
                         rev = FALSE,
                         mid = 0,
                         begin = NULL,
                         end = NULL,
                         order = NULL,
                         n_bin = NULL,
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = c("colour", "color", "fill"),
                         ...) {
  scale <- match.arg(scale)
  aesthetics <- match.arg(aesthetics)
  if (is_empty(palette)) {
    palette <- "Safe"
  }
  need_rescaler <- TRUE

  if (is_empty(alpha)) {
    alpha <- 1
  }
  if (is_empty(rev)) {
    rev <- FALSE
  }
  if (is_empty(mid)) {
    mid <- 0
    need_rescaler <- FALSE
  }
  if (is_empty(begin)) {
    begin <- 0
    need_rescaler <- FALSE
  }
  if (is_empty(end)) {
    end <- 1
    need_rescaler <- FALSE
  }

  l_palette <- metacolours$plot_colour(
    x = palette,
    alpha = alpha,
    rev = rev,
    mid = mid,
    begin = begin,
    end = end
  )

  fn_scale <-
    switch(scale,
      continuous = ggplot2::continuous_scale,
      discrete = ggplot2::discrete_scale,
      binned = ggplot2::binned_scale
    )

  if (scale == "discrete") {
    pal <- d_colours(l_palette$colours, order)
    if (is_nonempty(n_bin) && is_dblint(n_bin)) {
      pal <- pal(n_bin)
    }
    args_list <-
      list(
        aesthetics = aesthetics,
        scale_name = tolower(paste(scale, l_palette$type, sep = "_")),
        palette = pal,
        na.value = na.value,
        ...
      )
  } else {
    pal <-
      scales::gradient_n_pal(
        colours = l_palette$colours,
        space = "Lab"
      )

    if (need_rescaler) {
      args_list <-
        list(
          aesthetics = aesthetics,
          scale_name = tolower(paste(scale, l_palette$type, sep = "_")),
          palette = pal,
          na.value = na.value,
          guide = guide,
          rescaler = l_palette$rescale_fn,
          ...
        )
    } else {
      args_list <-
        list(
          aesthetics = aesthetics,
          scale_name = tolower(paste(scale, l_palette$type, sep = "_")),
          palette = pal,
          na.value = na.value,
          guide = guide,
          ...
        )
    }
  }

  do_call(f = fn_scale, .args = args_list)
}
#' @keywords internal
colour_ramp <- function(colors,
                        na.color = NA,
                        alpha = TRUE) {
  if (length(colors) == 0) {
    cat_stop("Must provide at least one colour to create a colour ramp")
  }

  if (length(colors) == 1) {
    return(structure(function(x) {
      ifelse(is.na(x), na.color, colors)
    },
    safe_palette_func = TRUE
    ))
  }

  # farver is not currently case insensitive, but col2rgb() is
  colors <- tolower(colors)
  lab_in <- farver::decode_colour(colors,
    alpha = TRUE,
    to = "lab",
    na_value = "transparent"
  )

  x_in <- seq(0, 1, length.out = length(colors))
  l_interp <- stats::approxfun(x_in, lab_in[, 1])
  u_interp <- stats::approxfun(x_in, lab_in[, 2])
  v_interp <- stats::approxfun(x_in, lab_in[, 3])

  if (!alpha || all(lab_in[, 4] == 1)) {
    alpha_interp <- function(x) {
      NULL
    }
  } else {
    alpha_interp <- stats::approxfun(x_in, lab_in[, 4])
  }

  structure(function(x) {
    lab_out <- cbind(l_interp(x), u_interp(x), v_interp(x))
    out <-
      farver::encode_colour(lab_out, alpha = alpha_interp(x), from = "lab")
    out[is.na(out)] <- na.color
    out
  },
  safe_palette_func = TRUE
  )
}

# lengeds

#' @export
gr_legend_acction <-
  function(...,
           position = c("noaction", "right", "none", "left", "bottom", "top"),
           direction = c("noaction", "vertical", "horizontal"),
           justification = c("noaction", "left", "right", "center"),
           remove_legend = FALSE,
           remove_tittle = FALSE,
           teach = FALSE) {
    position <- match.arg(position)
    direction <- match.arg(direction)
    justification <- match.arg(justification)

    if (remove_legend) {
      return(gr_legend_remove(..., teach = teach))
    }

    tgg <- theme()

    if (remove_tittle) {
      tgg <- tgg + gr_legend_remove_title(teach = teach)
    }

    if (position != "noaction") {
      tgg <- tgg + gr_legend_move(to = position, teach = teach)
    }

    if (direction != "noaction") {
      tgg <- tgg + gr_legend_rotate(to = direction, teach = teach)
    }

    if (justification != "noaction") {
      tgg <- tgg + gr_legend_adjust(to = justification, teach = teach)
    }

    tgg
  }

#' @export
gr_legend_add_title <- function(..., teach = FALSE) {
  .all_legend_aes <- c(
    "alpha",
    "cex",
    "col",
    "color",
    "colour",
    "fill",
    "linetype",
    "lty",
    "lwd",
    "pch",
    "radius",
    "shape",
    "size",
    "weight",
    "width"
  )

  dots <- rlang::dots_list(...)

  length(dots) > 0L || cat_stop("No title provided.")

  if (length(dots) == 1L && names(dots) == "") {
    orig_dots <- dots
    dots <-
      set_names(rep(dots, length(.all_legend_aes)), .all_legend_aes)
    if (teach) {
      cat_message(paste(
        'gr_legend_add_title("',
        orig_dots,
        '") call can be substituted with:'
      ))
      cat_message('labs(YOUR_AES = "', orig_dots, '")')
    }
    return(suppressall(do.call(labs, dots)))
  }

  if (teach) {
    gr_fun_args <-
      paste0(names(dots), ' = "', dots, '"', collapse = ", ")
    gr_fun <- paste0("gr_legend_add_title(", gr_fun_args, ")")
    cat_message(paste0(gr_fun, " call can be substituted with:"))
    cat_message(strwrap(
      paste0("labs(", gr_fun_args, ")"),
      width = 80,
      exdent = 2,
      prefix = "\n",
      initial = ""
    ))
  }

  do.call(labs, dots)
}

#' @keywords internal
gr_legend_remove <- function(..., teach = FALSE) {
  vars <- rlang::exprs(...)
  if (length(vars) == 0) {
    if (teach) {
      cat_message("gr_legend_remove call can be substituted with:")
      cat_message('theme(legend.position = "none")')
    }
    theme(legend.position = "none")
  } else {
    inputs <- lapply(vars, function(x) {
      "none"
    })
    names(inputs) <- vars

    if (teach) {
      cat_message("gr_legend_remove call can be substituted with:")
      false_strings <- lapply(vars, function(x) {
        " = FALSE"
      })
      args <- paste0(vars, false_strings, collapse = ", ")
      cat_message(strwrap(
        paste0("guides(", args, ")"),
        width = 80,
        exdent = 2,
        prefix = "\n",
        initial = ""
      ))
    }
    do.call(guides, inputs)
  }
}
#' @keywords internal
gr_legend_remove_title <- function(teach = FALSE) {
  if (teach) {
    cat_message("gr_legend_remove_title call can be substituted with:")
    cat_message(
      strwrap(
        "theme(legend.title = element_blank())",
        width = 80,
        exdent = 2,
        prefix = "\n",
        initial = ""
      )
    )
  }

  theme(legend.title = element_blank())
}
#' @keywords internal
gr_legend_move <-
  function(to = c("right", "none", "left", "bottom", "top"),
           teach = FALSE) {
    to <- match.arg(to, several.ok = FALSE)
    gr_legend_change(what = "position", to = to, teach = teach)
  }
#' @keywords internal
gr_legend_rotate <-
  function(to = c("vertical", "horizontal"),
           teach = FALSE) {
    to <- match.arg(to, several.ok = FALSE)
    gr_legend_change(what = "direction", to = to, teach = teach)
  }
#' @keywords internal
gr_legend_adjust <-
  function(to = c("left", "right", "center"),
           teach = FALSE) {
    to <- match.arg(to, several.ok = FALSE)
    gr_legend_change(what = "justification", to = to, teach = teach)
  }
#' @keywords internal
gr_legend_change <-
  function(what = c("position", "direction", "justification"),
           to,
           teach = FALSE) {
    what <- match.arg(what, several.ok = FALSE)

    theme_args <- setNames(to, paste0("legend.", what))

    ## attempt to determine which function was actually called
    callingFun <-
      tryCatch(
        as_l(sys.call(-1))[[1]],
        error = function(e) {
          e
        }
      )
    gr_fun <- if (inherits(callingFun, "simpleError")) {
      ## the call came from inside the house!
      paste0("gr_legend_change(", what, ' = "', to, '")') # nocov
    } else {
      ## called from a helper
      paste0(callingFun, '("', to, '")')
    }

    if (teach) {
      cat_message(paste0(gr_fun, " call can be substituted with:"))
      cat_message(strwrap(
        paste0("theme(", names(theme_args), ' = "', theme_args, '")'),
        width = 80,
        exdent = 2,
        prefix = "\n",
        initial = ""
      ))
    }

    do.call(theme, as_l(theme_args))
  }

# metacolours

#' @keywords internal
metacolors <- parse("data-raw/metacolors.R")
#' @keywords internal
metacolors <- eval(metacolors)
#' @keywords internal
ex_ggplot <- ggplot2::ggplot(
  data = tibble::tibble(
    x = runif(20),
    y = runif(20),
    cat = sample(letters[1:4], 20, T),
    bin = rdunif(20, 1, 10)
  ),
  ggplot2::aes(x, y)
)

#' @export
metacolours <-
  R6::R6Class(
    classname = "metacolours",
    public =
      list(
        metacolors = metacolors,
        ppal_nm = metacolors$plot$name,
        tpal_rm = names(metacolors$theme),
        type = NULL,
        show_palettes_plot = function(ex_type) {
          p <- metacolors$plot

          p <- p[p$type == ex_type, ]
          p <-
            split(p[, 3:6], p$name)

          p <- imap(p, function(x, y) {
            x <- as_l(x)
            x[["n05"]][[1]]
          })
          p <-
            imap(p, function(x, y) {
              as.ggplot.gg(gr_show_plot_color(x, label = FALSE)) + facet_set(y)
            })
          gr_grid(p)
        },
        show_palettes_theme = function() {
          merge_dpl <- function(x) {
            if (any(is_unique_scalar(x))) {
              x <- map_chr(split(names(x), x), ~ paste0(.x, collapse = "_"))
              x <- set_names(names(x), x)
            }
            x
          }
          p <- self$metacolors$theme
          p <- map(p, ~ merge_dpl(unlist(.x)))
          p <- map(p, ~ .x[names(.x) %!in% "type"])

          p <-
            imap(
              .x = p,
              .f = ~ gr_show_plot_color(
                colour_vector = unname(.x),
                text_vector = names(.x)
              )
            )
          gr_grid(gglist = p)
        },
        tclean = function(x) {
          gsub("[^a-z0-9]", "", tolower(x))
        },
        alpha_rev_colours = function(colours, rev, alpha) {
          if (rev) {
            colours <- rev(colours)
          }
          if (!missing(alpha)) {
            alpha <- pmax(pmin(alpha, 1), 0)
            alpha <-
              format(as.hexmode(round(alpha * 255 + 0.0001)),
                width = 2L,
                upper.case = TRUE
              )
            colours <-
              ifelse(is.na(colours), NA, paste(colours, alpha, sep = ""))
          }
          colours <- as.character(colours)
        },
        rescale_fn = function(x, mid, begin, end) {
          to_rescaler <- function(begin, end) {
            function(x,
                     to = c(begin, end),
                     from = range(x, na.rm = TRUE)) {
              scales::rescale(x, to, from)
            }
          }

          mid_rescaler <- function(mid) {
            function(x,
                     to = c(0, 1),
                     from = range(x, na.rm = TRUE)) {
              scales::rescale_mid(x, to, from, mid)
            }
          }

          pos <- which(self$tclean(x) == self$tclean(self$ppal_nm))
          if (is_empty(pos)) {
            cat_stop(
              "Palette 'type' should be one of: ",
              paste(levels(self$metacolors$plot$type), collapse = ", ")
            )
          }
          type <- self$metacolors$plot$type[pos]

          colours <- switch(type,
            Diverging = mid_rescaler(mid = mid),
            Qualitative = to_rescaler(begin = begin, end = end),
            `Sequential (single-hue)` = to_rescaler(begin = begin, end = end),
            `Sequential (multi-hue)` = to_rescaler(begin = begin, end = end)
          )

          list(colours = colours, type = type)
        },
        set_pallete = function(x, rev, alpha) {
          pos <- which(self$tclean(x) == self$tclean(self$ppal_nm))
          if (is_empty(pos)) {
            cat_stop(
              "Palette 'type' should be one of: ",
              paste(levels(self$metacolors$plot$type), collapse = ", ")
            )
          }
          plot_palettes <- self$metacolors$plot[pos, ][1, ]

          plot_palettes <-
            cmp_map(plot_palettes[, 3:6], ~ .x[[1]])
          colours <-
            plot_palettes[[which.max(lengths(plot_palettes))]]
          self$alpha_rev_colours(colours, rev, alpha)
        },
        gg_ex = ex_ggplot,
        plot_colour = function(x, alpha, rev, mid, begin, end) {
          res <- self$rescale_fn(x, mid, begin, alpha)

          list(
            colours = self$set_pallete(x, rev, alpha),
            rescale_fn = res$colours,
            type = res$type
          )
        },
        theme_colour = function(x) {
          self$metacolors$theme[[x]]
        },
        colour_ex = function(x) {
          ggp <- self$gg_ex + ggplot2::theme_minimal()
          continuous <- ggp +
            ggplot2::geom_point(mapping = aes(colour = y), size = 2) +
            scale_pal_gr(
              palette = x,
              scale = "continuous",
              aesthetics = "colour"
            ) + facet_set(label = "continuous")
          discrete <-
            ggp +
            ggplot2::geom_point(mapping = ggplot2::aes(colour = cat), size = 2) +
            scale_pal_gr(
              palette = x,
              scale = "discrete",
              aesthetics = "colour"
            ) + facet_set(label = "discrete")
          binned <-
            ggp + ggplot2::geom_point(mapping = ggplot2::aes(colour = bin), size = 2) +
            scale_pal_gr(
              palette = x,
              scale = "binned",
              aesthetics = "colour"
            ) + facet_set(label = "binned")
          gr_grid(continuous, binned, discrete)
        },
        theme_ex = function(x, all = TRUE) {
          col <- gr_global$default_colors[[paste0("theme_", x)]]
          if (all) {
            self$gg_ex +
              ggplot2::geom_point(colour = col, show.legend = TRUE) +
              new_gr_theme(
                name = x,
                axis = "XY",
                ticks = "XY",
                grid = "XY"
              )
          } else {
            self$gg_ex + ggplot2::geom_point(colour = col) + new_gr_theme(name = x)
          }
        },
        print = function(...) {
          cat_class("colour metadata")
          invisible()
        }
      )
  )
