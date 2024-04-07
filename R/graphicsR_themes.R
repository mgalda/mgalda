#' @include graphicsR_utils.R

#' Apply func
#'
#' @name gr_graphics
#' @rdname gr_graphics
#' @keywords internal
NULL

#' @export
gr_get_theme <- function() {
  gr_global$theme
}

#' @export
gr_set_theme <- function(theme, ...) {
  if (is_gr_theme(theme)) {
    theme <- attr(theme, "name")
  } else if (is.function(theme) || is.theme(theme)) {
    theme <- deparse(substitute(theme))
  }
  gr_themes <- gr_list_themes()

  if (!theme %in% gr_themes) {
    err_msg <- paste0(
      "`theme` must one of ",
      enumeration(gr_themes, "'", "or"),
      " but is '",
      theme,
      "'."
    )
    cat_stop(err_msg)
  }

  ellipsis <- list(...)
  args <- names(ellipsis)
  supported_args <- c("base_size", "base_family")
  if (length(args) && length(setdiff(args, supported_args))) {
    err_msg <-
      paste0(
        "Only ",
        enumeration(supported_args),
        " may be used in `...`."
      )
    cat_stop(err_msg)
  }

  old_theme <- gr_global$theme
  gr_global$theme <- theme
  gr_global$theme_args <- ellipsis
  invisible(old_theme)
}

#' @export
theme_gr <- function(base_size = 13,
                     base_family = "Helvetica",
                     header_family = "Helvetica",
                     axis = "",
                     ticks = "",
                     grid = "",
                     legend_positions = c(
                       "bottom", "none",
                       "left", "right", "top", "XY"
                     ),
                     legend_directions = c(
                       "horizontal",
                       "vedrtical"
                     ),
                     spacing = 1,
                     ytitle_hjust = 0.5,
                     xtitle_hjust = 0.5,
                     line_weight = 0.5,
                     aspect_ratio = NULL) {
  new_gr_theme(
    name = "gr",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    legend_positions = legend_positions,
    legend_directions = legend_directions,
    spacing = spacing,
    ytitle_hjust = ytitle_hjust,
    xtitle_hjust = xtitle_hjust,
    line_weight = line_weight,
    aspect_ratio = aspect_ratio

  )
}

#' @export
theme_hermit <- function(base_size = 13,
                         base_family = "Helvetica",
                         header_family = "Helvetica",
                         axis = "",
                         ticks = "",
                         grid = "",
                         legend_positions = c(
                           "bottom", "none",
                           "left", "right", "top", "XY"
                         ),
                         legend_directions = c(
                           "horizontal",
                           "vedrtical"
                         ),
                         spacing = 1,
                         ytitle_hjust = 0.5,
                         xtitle_hjust = 0.5,
                         line_weight = 0.5,
                         aspect_ratio = NULL) {
  new_gr_theme(
    name = "hermit",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    legend_positions = legend_positions,
    legend_directions = legend_directions,
    spacing = spacing,
    ytitle_hjust = ytitle_hjust,
    xtitle_hjust = xtitle_hjust,
    line_weight = line_weight,
    aspect_ratio = aspect_ratio

  )
}

#' @export
theme_ng <- function(base_size = 13,
                     base_family = "Helvetica",
                     header_family = "Helvetica",
                     axis = "",
                     ticks = "",
                     grid = "",
                     legend_positions = c(
                       "bottom", "none",
                       "left", "right", "top", "XY"
                     ),
                     legend_directions = c(
                       "horizontal",
                       "vedrtical"
                     ),
                     spacing = 1,
                     ytitle_hjust = 0.5,
                     xtitle_hjust = 0.5,
                     line_weight = 0.5,
                     aspect_ratio = NULL) {
  new_gr_theme(
    name = "ng",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    legend_positions = legend_positions,
    legend_directions = legend_directions,
    spacing = spacing,
    ytitle_hjust = ytitle_hjust,
    xtitle_hjust = xtitle_hjust,
    line_weight = line_weight,
    aspect_ratio = aspect_ratio

  )
}

#' @export
theme_nightblue <- function(base_size = 13,
                            base_family = "Helvetica",
                            header_family = "Helvetica",
                            axis = "",
                            ticks = "",
                            grid = "",
                            legend_positions = c(
                              "bottom", "none",
                              "left", "right", "top", "XY"
                            ),
                            legend_directions = c(
                              "horizontal",
                              "vedrtical"
                            ),
                            spacing = 1,
                            ytitle_hjust = 0.5,
                            xtitle_hjust = 0.5,
                            line_weight = 0.5,
                            aspect_ratio = NULL) {
  new_gr_theme(
    name = "nightblue",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    legend_positions = legend_positions,
    legend_directions = legend_directions,
    spacing = spacing,
    ytitle_hjust = ytitle_hjust,
    xtitle_hjust = xtitle_hjust,
    line_weight = line_weight,
    aspect_ratio = aspect_ratio

  )
}

#' @export
theme_coffee <- function(base_size = 13,
                         base_family = "Helvetica",
                         header_family = "Helvetica",
                         axis = "",
                         ticks = "",
                         grid = "",
                         legend_positions = c(
                           "bottom", "none",
                           "left", "right", "top", "XY"
                         ),
                         legend_directions = c(
                           "horizontal",
                           "vedrtical"
                         ),
                         spacing = 1,
                         ytitle_hjust = 0.5,
                         xtitle_hjust = 0.5,
                         line_weight = 0.5,
                         aspect_ratio = NULL) {
  new_gr_theme(
    name = "coffee",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    legend_positions = legend_positions,
    legend_directions = legend_directions,
    spacing = spacing,
    ytitle_hjust = ytitle_hjust,
    xtitle_hjust = xtitle_hjust,
    line_weight = line_weight,
    aspect_ratio = aspect_ratio

  )
}

#' @export
theme_flat <- function(base_size = 13,
                       base_family = "Helvetica",
                       header_family = "Helvetica",
                       axis = "",
                       ticks = "",
                       grid = "",
                       legend_positions = c(
                         "bottom", "none",
                         "left", "right", "top", "XY"
                       ),
                       legend_directions = c(
                         "horizontal",
                         "vedrtical"
                       ),
                       spacing = 1,
                       ytitle_hjust = 0.5,
                       xtitle_hjust = 0.5,
                       line_weight = 0.5,
                       aspect_ratio = NULL) {
  new_gr_theme(
    name = "flat",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    legend_positions = legend_positions,
    legend_directions = legend_directions,
    spacing = spacing,
    ytitle_hjust = ytitle_hjust,
    xtitle_hjust = xtitle_hjust,
    line_weight = line_weight,
    aspect_ratio = aspect_ratio
  )
}

#' @export
theme_flat_dark <- function(base_size = 13,
                            base_family = "Helvetica",
                            header_family = "Helvetica",
                            axis = "",
                            ticks = "",
                            grid = "",
                            legend_positions = c(
                              "bottom", "none",
                              "left", "right", "top", "XY"
                            ),
                            legend_directions = c(
                              "horizontal",
                              "vedrtical"
                            ),
                            spacing = 1,
                            ytitle_hjust = 0.5,
                            xtitle_hjust = 0.5,
                            line_weight = 0.5,
                            aspect_ratio = NULL) {
  new_gr_theme(
    name = "flat_dark",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    legend_positions = legend_positions,
    legend_directions = legend_directions,
    spacing = spacing,
    ytitle_hjust = ytitle_hjust,
    xtitle_hjust = xtitle_hjust,
    line_weight = line_weight,
    aspect_ratio = aspect_ratio
  )
}

#' @export
theme_earth <- function(base_size = 13,
                        base_family = "Helvetica",
                        header_family = "Helvetica",
                        axis = "",
                        ticks = "",
                        grid = "",
                        legend_positions = c(
                          "bottom", "none",
                          "left", "right", "top", "XY"
                        ),
                        legend_directions = c(
                          "horizontal",
                          "vedrtical"
                        ),
                        spacing = 1,
                        ytitle_hjust = 0.5,
                        xtitle_hjust = 0.5,
                        line_weight = 0.5,
                        aspect_ratio = NULL) {
  new_gr_theme(
    name = "earth",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    legend_positions = legend_positions,
    legend_directions = legend_directions,
    spacing = spacing,
    ytitle_hjust = ytitle_hjust,
    xtitle_hjust = xtitle_hjust,
    line_weight = line_weight,
    aspect_ratio = aspect_ratio
  )
}

#' @export
theme_greyscale <- function(base_size = 13,
                            base_family = "Helvetica",
                            header_family = "Helvetica",
                            axis = "",
                            ticks = "",
                            grid = "",
                            legend_positions = c(
                              "bottom", "none",
                              "left", "right", "top", "XY"
                            ),
                            legend_directions = c(
                              "horizontal",
                              "vedrtical"
                            ),
                            spacing = 1,
                            ytitle_hjust = 0.5,
                            xtitle_hjust = 0.5,
                            line_weight = 0.5,
                            aspect_ratio = NULL) {
  new_gr_theme(
    name = "greyscale",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    legend_positions = legend_positions,
    legend_directions = legend_directions,
    spacing = spacing,
    ytitle_hjust = ytitle_hjust,
    xtitle_hjust = xtitle_hjust,
    line_weight = line_weight,
    aspect_ratio = aspect_ratio
  )
}


#' @keywords internal
new_gr_theme <- function(name,
                         base_size = 13,
                         base_family = "",
                         header_family = "",
                         axis = "",
                         ticks = "",
                         grid = "",
                         legend_positions = c(
                           "bottom", "none",
                           "left", "right", "top", "XY"
                         ),
                         legend_directions = c(
                           "horizontal",
                           "vedrtical"
                         ),
                         spacing = 1,
                         ytitle_hjust = 0.5,
                         xtitle_hjust = 0.5,
                         line_weight = 0.5,
                         aspect_ratio = NULL) {
  if (is_empty(base_family)) {
    cat_warn("'base_family' is empty, fill with 'Helvetica'")
    base_family <- "Helvetica"
  }

  if (is_empty(header_family)) {
    header_family <- base_family
    cat_warn("'header_family' is empty, fill with '{ base_family }'")
  }

  legend_positions <- match.arg(legend_positions)

  legend_directions <- match.arg(legend_directions)

  half_line <- base_size / 2
  hl_spac <- half_line * spacing

  palette <- metacolours$theme_colour(x = name)
  fg_col <- palette$line
  bg_col <- palette$background
  txt_col <- palette$text
  grid_col <- palette$grid
  type <- palette$type

  base_family <- check_base_family(base_family)
  header_family <- check_base_family(header_family)

  name <- paste0("theme_", name)

  if (axis != "") {
    axis <- match.arg(tolower(axis), c("x", "y", "xy", "yx"))
  }
  if (ticks != "") {
    ticks <- match.arg(tolower(ticks), c("x", "y", "xy", "yx"))
  }
  if (grid != "" && grepl("[^xyXY]", grid)) {
    cat_stop("`grid` must only contain combinations of 'x', 'y', 'X', 'Y'.")
  }

  elm_gmajor <-
    element_line(color = grid_col, size = line_weight / 2)
  elm_gminor <-
    element_line(color = grid_col, size = line_weight / 4)
  elm_laxis <- element_line(color = fg_col, size = line_weight)
  elm_ltick <- elm_laxis

  new_theme <-
    theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.border = element_blank(),
      line = element_line(
        colour = fg_col,
        size = line_weight,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = "white",
        colour = txt_col,
        size = 0.5,
        linetype = 1
      ),
      text = element_text(
        family = base_family,
        face = "plain",
        colour = txt_col,
        size = base_size,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      )
    ) +
    theme(
      axis.line.x = if (grepl("x", axis)) {
        elm_laxis
      } else {
        gg_bnk
      },
      axis.line.y = if (grepl("y", axis)) {
        elm_laxis
      } else {
        gg_bnk
      },
      axis.ticks.x = if (grepl("x", ticks)) {
        elm_ltick
      } else {
        gg_bnk
      },
      axis.ticks.y = if (grepl("y", ticks)) {
        elm_ltick
      } else {
        gg_bnk
      },
      panel.grid.major.x = if (grepl("Y", grid)) {
        elm_gmajor
      } else {
        gg_bnk
      },
      panel.grid.major.y = if (grepl("X", grid)) {
        elm_gmajor
      } else {
        gg_bnk
      },
      panel.grid.minor.x = if (grepl("x", grid)) {
        elm_gminor
      } else {
        gg_bnk
      },
      panel.grid.minor.y = if (grepl("y", grid)) {
        elm_gminor
      } else {
        gg_bnk
      },
      axis.text = element_text(
        size = ggplot2::rel(0.8),
        colour = txt_col
      ),
      axis.text.x = element_text(
        margin = margin(t = 0.4 * hl_spac),
        vjust = 1
      ),
      axis.text.x.top = element_text(
        margin = margin(b = 0.4 * hl_spac),
        vjust = 0
      ),
      axis.text.y = element_text(
        margin = margin(r = 0.4 * hl_spac),
        hjust = 1
      ),
      axis.text.y.right = element_text(
        margin = margin(l = 0.4 * hl_spac),
        hjust = 0
      ),
      axis.ticks.length = unit(
        hl_spac / 2,
        "pt"
      ),
      axis.title.x = element_text(
        margin = margin(t = hl_spac),
        vjust = 1,
        hjust = xtitle_hjust,
      ),
      axis.title.x.top = element_text(
        margin = margin(b = hl_spac),
        vjust = 0
      ),
      axis.title.y = element_text(
        angle = 90,
        margin = margin(r = hl_spac),
        vjust = 1,
        hjust = ytitle_hjust
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = hl_spac),
        vjust = 0
      ),
      legend.background = element_rect(colour = bg_col, fill = bg_col),
      legend.spacing = unit(0.4 * spacing, "cm"),
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.margin = margin(0, 0, 0, 0, "cm"),
      legend.key = element_rect(colour = bg_col, fill = bg_col),
      legend.key.size = grid::unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = ggplot2::rel(0.8)),
      legend.text.align = NULL,
      legend.title = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position = legend_positions,
      legend.direction = legend_directions,
      legend.justification = "center",
      legend.box = NULL,
      legend.box.margin = margin(
        0,
        0, 0, 0, "cm"
      ),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(
        0.4 * spacing,
        "cm"
      ),
      panel.background = element_rect(
        fill = bg_col,
        colour = NA
      ),
      panel.grid = element_line(colour = "white"),
      panel.spacing = unit(hl_spac, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,
      strip.background = element_rect(colour = bg_col, fill = NA),
      strip.text = element_text(
        colour = txt_col,
        size = ggplot2::rel(0.8),
        margin = margin(
          hl_spac,
          hl_spac,
          hl_spac,
          hl_spac
        )
      ),
      strip.text.x = NULL,
      strip.text.y = element_text(angle = -90),
      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = unit(0.1, "cm"),
      strip.switch.pad.wrap = unit(0.1, "cm"),
      plot.background = element_rect(colour = bg_col, fill = bg_col),
      plot.title = element_text(
        size = ggplot2::rel(1.2),
        margin = margin(b = hl_spac * 1.2),
        hjust = 0,
        vjust = spacing,
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = ggplot2::rel(0.9),
        margin = margin(b = hl_spac * .9),
        hjust = 0,
        vjust = spacing
      ),
      plot.caption = element_text(
        size = ggplot2::rel(0.9),
        margin = margin(t = hl_spac * .9),
        hjust = spacing,
        vjust = spacing
      ),
      plot.tag = element_text(
        size = ggplot2::rel(1.2),
        hjust = 0.5,
        vjust = 0.5
      ),
      plot.tag.position = "topleft",
      plot.margin = margin(
        2 * hl_spac,
        hl_spac,
        hl_spac,
        hl_spac
      ),
      aspect.ratio = aspect_ratio
    )

  if (utils::packageVersion("ggplot2") >= "3.3.0") {
    new_theme <- new_theme + theme(plot.title.position = "plot")
  }

  structure(
    new_theme,
    class = c("theme_gr", class(new_theme)),
    name = name,
    type = type
  )
}

#' @keywords internal
is_gr_theme <- function(theme) {
  inherits(theme, "theme_gr")
}

#' @keywords internal
pyramid_theme <- function(side = c("left", "right")) {
  side <- match.arg(side)
  if (side == "left") {
    axis_text_y <- element_blank()
    plot_margin <- margin(5, 0, 5, 5)
    plot_title_hjust <- 1
  } else {
    axis_text_y <- element_text(
      hjust = .5,
      color = "black",
      margin = margin(l = 10, r = 10)
    )
    plot_margin <- margin(5, 5, 5, 0)
    plot_title_hjust <- 0
  }

  theme_minimal(base_size = 13) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = axis_text_y,
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = plot_margin,
      strip.text = element_text(
        hjust = 0,
        size = 14,
        face = "bold"
      ),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = plot_title_hjust, margin = margin()),
      axis.ticks = element_line(color = "darkgray"),
      axis.line.x = element_line(color = "darkgray")
    )
}

#' @export
theme_tree <- function(bgcolor = "white", legend_position = "right", ...) {
  list(
    xlab(NULL),
    ylab(NULL),
    theme_bw() +
      theme(
        legend.position = legend_position,
        panel.grid.minor = gg_bnk,
        panel.grid.major = gg_bnk,
        panel.background = element_rect(fill = bgcolor, colour = bgcolor),
        panel.border = gg_bnk,
        axis.line.y = gg_bnk,
        axis.text.y = gg_bnk,
        axis.ticks.y = gg_bnk,
        axis.line.x = gg_bnk,
        axis.text.x = gg_bnk,
        axis.ticks.x = gg_bnk,
        ...
      )
  )
}

#' @export
theme_tree2 <- function(palette = "greyscale", ...) {
  palette <- metacolours$theme_colour(x = palette)
  fgcolor <- palette$line
  bgcolor <- palette$background

  theme_tree2_internal <-
    function(bgcolor = "white",
             fgcolor = "black",
             legend.position = "right",
             panel.grid.minor = gg_bnk,
             panel.grid.major = gg_bnk,
             panel.border = gg_bnk,
             axis.line.y = gg_bnk,
             axis.ticks.y = gg_bnk,
             axis.text.y = gg_bnk,
             ...) {
      theme_bw() +
        theme(
          legend.position = legend.position,
          panel.grid.minor = panel.grid.minor,
          panel.grid.major = panel.grid.major,
          panel.background = element_rect(
            fill = bgcolor, colour =
              bgcolor
          ),
          panel.border = panel.border,
          axis.line = element_line(color = fgcolor),
          ## axis.line.x=element_line(color=fgcolor),
          axis.line.y = axis.line.y,
          axis.ticks.y = axis.ticks.y,
          axis.text.y = axis.text.y,
          ...
        )
    }

  list(
    xlab(NULL),
    ylab(NULL),
    theme_tree2_internal(bgcolor, fgcolor, ...)
  )
}

#' @export
theme_dendrogram <- function(palette = "greyscale", legend_position = "right", ...) {
  palette <- metacolours$theme_colour(x = palette)
  fgcolor <- palette$line
  bgcolor <- palette$background

  list(
    xlab(NULL),
    ylab(NULL),
    theme_bw() +
      theme(
        legend.position = legend_position,
        panel.grid.minor = gg_bnk,
        panel.grid.major = gg_bnk,
        panel.background = element_rect(fill = bgcolor, colour = bgcolor),
        panel.border = gg_bnk,
        axis.line.x = gg_bnk,
        axis.text.x = gg_bnk,
        axis.ticks.x = gg_bnk,
        axis.line = element_line(color = fgcolor),
        axis.line.y = element_line(color = fgcolor),
        axis.text.y = element_text(color = fgcolor),
        axis.ticks.y = element_line(color = fgcolor),
        ...
      )
  )
}

#' @export
theme_dotdensity <- function(legend = TRUE, legend_size = 10, palette = "greyscale") {
  basic_theme <- ggplot2::theme_void()

  palette <- metacolours$theme_colour(x = palette)
  bgcolor <- palette$background
  txtcolor <- palette$text

  # if including a legend
  if (legend == FALSE) {
    basic_theme <- basic_theme +
      theme(legend.position = "none")
  } else {
    basic_theme <- basic_theme +
      theme(legend.text = element_text(size = 12, colour = txtcolor)) +
      theme(legend.background = element_rect(fill = bgcolor, color = NA))
  }

  # to colour the background
  basic_theme <- basic_theme +
    theme(
      plot.background = element_rect(fill = bgcolor, color = NA),
      panel.background = element_rect(fill = bgcolor, color = NA),
      text = element_text(color = txtcolor, size = 20),
      title = element_text(color = txtcolor, size = 16)
    )

  basic_theme
}

#' @keywords internal
auto_color <- function() {
  gr_get_default_color(gr_get_theme())
}
#' @keywords internal
enumeration <- function(x, quote = "`", last = "&") {
  n <- length(x)
  quoted <- paste0(quote, x, quote)
  if (n == 1) {
    return(quoted)
  }
  paste(paste(quoted[-n], collapse = ", "), last, quoted[n])
}
#' @keywords internal
gr_current_theme <- function(...) {
  do.call(gr_get_theme(), c(gr_global$theme_args, list(...)))
}
#' @keywords internal
gr_list_themes <- function() {
  gr_exports <- c(getNamespaceExports("mgalda"), names(globalenv()))
  ggplot2_exports <- getNamespaceExports("ggplot2")
  grep("^theme_",
       setdiff(gr_exports, ggplot2_exports),
       value = TRUE
  )
}
#' @keywords internal
gr_get_default_color <- function(theme) {
  if (!is.character(theme)) {
    cat_stop("`theme` must be a string.")
  }
  if (!theme %in% gr_list_themes()) {
    err_msg <- paste0("'", theme, "' hasm't a default colot")
    cat_stop(err_msg)
  }
  gr_global$default_colors[theme]
}
