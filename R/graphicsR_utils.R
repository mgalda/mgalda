#' Apply func
#'
#' @name gr_graphics
#' @rdname gr_graphics
#' @keywords internal
NULL

gr_global <- new.env(parent = emptyenv())

gr_global$theme <- "theme_gr"
gr_global$theme_args <- list()

gr_global$default_colors <- c(
  "theme_coffee" = "#F4C95D",
  "theme_gr" = "#1F77B4",
  "theme_hermit" = "#94C1E0",
  "theme_nightblue" = "#D81E5B",
  "theme_ng" = "darkorange",
  "theme_flat" = "#7D0112",
  "theme_flat_dark" = "#FDE333",
  "theme_earth" = "#FDE333",
  "theme_greyscale" = "#7D0112"
)



gr_global$color_palettes <- list(
  "theme_gr" = c("#1F77B4", "#ECA400", "#483D3F", "#B8B8D1", "#A41623"),
  "theme_hermit" = c("#94C1E0", "#FFC09F", "#F06449", "#82A7A6", "#805D93"),
  "theme_nightblue" = c("#D81E5B", "#848C8E", "#ECA400", "#FAF0CA", "#7E2E84"),
  "theme_ng" = c("darkorange", "#6B9080", "#FFDDA1", "#FFFFFC", "#3E7CB1"),
  "okabe_ito" = c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#000000"
  ),
  "theme_greyscale" = c(
    "#68001D",
    "#A6231D",
    "#D45624",
    "#E59069",
    "#A5A5A5",
    "#7E7E7E",
    "#565656",
    "#303030"
  ),
  "theme_flat" = c(
    "#68001D",
    "#A6231D",
    "#D45624",
    "#E59069",
    "#A5A5A5",
    "#7E7E7E",
    "#565656",
    "#303030"
  ),
  "theme_flat_dark" = c("#68001D", "#A6231D", "#D45624", "#E59069", "#F0C1A8", "#A5A5A5"),
  "theme_earth" = c("#68001D", "#A6231D", "#D45624", "#E59069", "#F0C1A8", "#A5A5A5"),
  "theme_coffee" = c("#7D0112", "#AA592C", "#CE9758", "#E8CE8F", "#F2F1E4")
)

# objetos

#' @export
gg_bnk <- structure(list(), class = c("element_blank", "element"))

#' @export
gg_lgnd_null <- function() {
  theme(
    legend.background = gg_bnk,
    legend.margin = margin(),
    legend.box = NULL,
    legend.key = gg_bnk,
    legend.text = gg_bnk,
    legend.title = gg_bnk
  )
}

#' @importFrom  ggplot2 ggproto
#' @export
ggplot2::ggproto


## diplyr verbs

#' @export
gg_filter <- function(..., .f = NULL) {
  dots <- rlang::quos(...)
  function(.data) {
    if (!is.null(.f)) .data <- .f(.data)
    dplyr::filter(.data, !!!dots)
  }
}
#' @export
gg_unnest <- function(cols, ..., .f = NULL) {
  function(.data) {
    if (!is.null(.f)) .data <- .f(.data)
    tidyr::unnest(.data, {{ cols }}, ...)
  }
}
#' @export
gg_mutate <- function(..., .f = NULL) {
  function(.data) {
    if (!is.null(.f)) .data <- .f(.data)
    dplyr::mutate(.data, ...)
  }
}

## fortify

#' @importFrom  ggplot2 fortify
#' @keywords internal
ggplot2::fortify
#' @method fortify phylo
#' @export
fortify.phylo <- function(model,
                          data,
                          layout = "rectangular",
                          ladderize = TRUE,
                          right = FALSE,
                          branch.length = "branch.length",
                          mrsd = NULL,
                          as.Date = FALSE,
                          yscale = "none",
                          root.position = 0,
                          ...) {
  x <- as.phylo(model) ## reorder.phylo(get.tree(model), "postorder")
  if (ladderize == TRUE) {
    x <- ape::ladderize(x, right = right)
  }

  if (!is.null(x$edge.length)) {
    if (anyNA(x$edge.length)) {
      cat_warn(
        "'edge.length' contains NA values...\n## setting 'edge.length' to NULL automatically when plotting the tree..."
      )
      x$edge.length <- NULL
    }
  }

  if (layout %in% c("equal_angle", "daylight", "ape")) {
    res <-
      layout.unrooted(model,
        layout.method = layout,
        branch.length = branch.length,
        ...
      )
  } else {
    if (is.null(x$edge.length) || branch.length == "none") {
      xpos <- get_x_coord_no_length(x)
    } else {
      xpos <- get_x_coord(x)
    }

    ypos <- get_y_coord(x)
    N <- Nnode(x, internal.only = FALSE)
    xypos <-
      tibble::tibble(
        node = 1:N,
        x = xpos + root.position,
        y = ypos
      )

    df <- as_tibble(model) %>%
      mutate(isTip = !.data$node %in% .data$parent)

    res <- full_join(df, xypos, by = "node")
  }

  ## add branch mid position
  res <- calculate_branch_mid(res, layout = layout)

  if (!is.null(mrsd)) {
    res <- scale_x_by_time_from_mrsd(res, mrsd, as.Date)
  }

  if (layout == "slanted") {
    res <- add_angle_slanted(res)
  } else {
    ## angle for all layout, if 'rectangular', user use coord_polar, can still use angle
    res <- calculate_angle(res)
  }
  res <- scale_y(as.phylo(model), res, yscale, layout, ...)
  class(res) <- c("tbl_tree", class(res))
  attr(res, "layout") <- layout
  return(res)
}
#' @method fortify multiPhylo
#' @export
fortify.multiPhylo <- function(model,
                               data,
                               layout = "rectangular",
                               ladderize = TRUE,
                               right = FALSE,
                               mrsd = NULL,
                               ...) {
  df.list <-
    lapply(model, function(x) {
      fortify(
        x,
        layout = layout,
        ladderize = ladderize,
        right = right,
        mrsd = mrsd,
        ...
      )
    })
  if (is.null(names(model))) {
    names(df.list) <- paste0("Tree ", "#", seq_along(model))
  } else {
    names(df.list) <- names(model)
  }
  df <- do.call("rbind", df.list)
  df$.id <- rep(names(df.list), times = sapply(df.list, nrow))
  df$.id <- factor(df$.id, levels = names(df.list))
  attr(df, "layout") <- layout
  return(df)
}

#' @keywords internal
add_angle_slanted <- function(res) {
  x <- res[["x"]]
  y <- res[["y"]]
  dy <- (y - y[match(res$parent, res$node)]) / diff(range(y))
  dx <- (x - x[match(res$parent, res$node)]) / diff(range(x))
  theta <- atan(dy / dx)
  theta[is.na(theta)] <- 0 ## root node
  res$angle <- theta / pi * 180

  branch.y <- (y[match(res$parent, res$node)] + y) / 2
  idx <- is.na(branch.y)
  branch.y[idx] <- y[idx]
  res[, "branch.y"] <- branch.y
  return(res)
}
#' @method fortify treedataList
#' @export
fortify.treedataList <- fortify.multiPhylo
#' @importFrom ggplot2 fortify
#' @method fortify treedata
#' @export
fortify.treedata <- function(model,
                             data,
                             layout = "rectangular",
                             yscale = "none",
                             ladderize = TRUE,
                             right = FALSE,
                             branch.length = "branch.length",
                             mrsd = NULL,
                             as.Date = FALSE,
                             ...) {
  model <- set_branch_length(model, branch.length)

  fortify.phylo(
    model,
    data,
    layout        = layout,
    yscale        = yscale,
    ladderize     = ladderize,
    right         = right,
    branch.length = branch.length,
    mrsd          = mrsd,
    as.Date       = as.Date,
    ...
  )
}
#' @method fortify phylo4
#' @export
fortify.phylo4 <- function(model,
                           data,
                           layout = "rectangular",
                           yscale = "none",
                           ladderize = TRUE,
                           right = FALSE,
                           mrsd = NULL,
                           ...) {
  if (class(model) %in% c("dendrogram", "agnes", "diana", "twins")) {
    model <- stats::as.hclust(model)
  }


  phylo <- as.phylo(model)
  df <- fortify.phylo(phylo, data,
    layout, ladderize, right,
    mrsd = mrsd, ...
  )
  scale_y(phylo, df, yscale, layout, ...)
}
#' @method fortify hclust
#' @export
fortify.hclust <- fortify.phylo4
#' @method fortify dendrogram
#' @export
fortify.dendrogram <- fortify.phylo4
#' @method fortify agnes
#' @export
fortify.agnes <- fortify.phylo4
#' @method fortify diana
#' @export
fortify.diana <- fortify.phylo4
#' @method fortify twins
#' @export
fortify.twins <- fortify.phylo4
#' @method fortify phylog
#' @export
fortify.phylog <- fortify.phylo4
#' @method fortify igraph
#' @export
fortify.igraph <- fortify.phylo4
#' @method fortify phylo4d
#' @export
fortify.phylo4d <- function(model,
                            data,
                            layout = "rectangular",
                            yscale = "none",
                            ladderize = TRUE,
                            right = FALSE,
                            branch.length = "branch.length",
                            mrsd = NULL,
                            ...) {
  .as.phylo.phylo4 <- function(x, ...) {
    edge <- x@edge
    edge.filter <- edge[, 1] != 0
    edge <- edge[edge.filter, ]
    edge.length <- x@edge.length
    edge.length <- edge.length[edge.filter]
    tip.id <- sort(setdiff(edge[, 2], edge[, 1]))
    tip.label <- x@label[tip.id]
    phylo <-
      list(
        edge = edge,
        edge.length = edge.length,
        tip.label = tip.label
      )
    node.id <- sort(unique(edge[, 1]))
    node.id <- node.id[node.id != 0]
    node.label <- x@label[node.id]
    if (!all(is.na(node.label))) {
      phylo$node.label <- node.label
    }
    phylo$Nnode <- length(node.id)
    class(phylo) <- "phylo"
    return(phylo)
  }

  .as.treedata <- function(model) {
    d <- as_tibble(model@data)
    d$node <- as.numeric(rownames(tree@data))
    model_tree_data <-
      new("treedata", phylo = .as.phylo.phylo4(tree), data = d)
  }
  fortify(
    .as.treedata(model),
    data,
    layout,
    yscale,
    ladderize,
    right,
    branch.length,
    mrsd,
    ...
  )
}

#' @method fortify pvclust
#' @export
fortify.pvclust <- fortify.phylo4d
#' @method fortify obkData
#' @export
fortify.obkData <- function(model,
                            data,
                            layout = "rectangular",
                            ladderize = TRUE,
                            right = FALSE,
                            mrsd = NULL,
                            ...) {
  df <-
    fortify(
      model@trees[[1]],
      layout = layout,
      ladderize = ladderize,
      right = right,
      mrsd = mrsd,
      ...
    )

  meta.df <- model@dna@meta
  meta.df <- data.frame(taxa = rownames(meta.df), meta.df)
  loc <- model@individuals
  loc <- data.frame(individualID = rownames(loc), loc)
  meta_loc <- merge(meta.df, loc, by = "individualID")
  meta_loc <- meta_loc[, -1]

  df <-
    merge(df,
      meta_loc,
      by.x = "label",
      by.y = "taxa",
      all.x = TRUE
    )
  df <- df[order(df$node, decreasing = FALSE), ]
  df
}

## ggplot_add

#' @export
ggplot2::ggplot_add
#' @export
ggplot_add.layout_ggtree <- function(object, plot, object_name) {
  if (object$layout == "fan") {
    return(open_tree(plot, object$angle))
  }

  if (object$layout == "dendrogram") {
    plot <- revts(plot)
    obj <- list(
      scale_x_reverse(labels = abs),
      coord_flip(clip = "off")
    )
  } else if (object$layout == "circular" || object$layout == "inward_circular") {
    ## refer to: https://github.com/GuangchuangYu/ggtree/issues/6
    ## and also have some space for tree scale (legend)
    obj <- list(
      coord_polar(theta = "y", start = -pi / 2, -1, clip = "off"),
      scale_y_continuous(limits = c(0, NA), expand = expansion(0, 0.6))
    )
    if (object$layout == "inward_circular") {
      obj[[3]] <- scale_x_reverse(limits = object$xlim)
    }
  } else { ## rectangular
    obj <- coord_cartesian(clip = "off")
  }
  assign("layout", object$layout, envir = plot$plot_env)
  ggplot_add(obj, plot, object_name)
}
#' @export
ggplot_add.scale_ggtree <- function(object, plot, object_name) {
  mrsd <- get("mrsd", envir = plot$plot_env)
  if (!is.null(mrsd) && class(plot$data$x) == "Date") {
    x <- date2decimal(plot$data$x)
  } else {
    x <- plot$data$x
  }

  breaks <- object$breaks
  labels <- object$labels

  if (length(breaks) == 0) {
    breaks <- graphics::hist(x, breaks = 5, plot = FALSE)$breaks
  }
  m <- attr(plot, "mapping")

  if (!is.null(mrsd) && class(m$to) == "Date") {
    to <- date2decimal(m$to)
  } else {
    to <- m$to
  }

  idx <- which(sapply(breaks, function(x) {
    any(x > m$to)
  }))
  if (length(idx)) {
    breaks <- breaks[-idx]
  }

  if (length(labels) == 0) {
    labels <- breaks
  }

  if (length(breaks) != length(labels)) {
    cat_stop("breaks and labels should be in equal length.")
  }

  breaks <- c(breaks, to)
  labels <- c(labels, gsub("\\.", "", as.character(m$from)))

  if (!is.null(mrsd) && class(plot$data$x) == "Date") {
    obj <- scale_x_date(breaks = decimal2date(breaks), labels)
  } else {
    obj <- scale_x_continuous(breaks = breaks, labels = labels)
  }
  ggplot_add(obj, plot, object_name)
}
#' @export
ggplot_add.ggbreak_params <- function(object, plot, object_name) {
  if (inherits(plot, "ggbreak")) {
    origin_axis_break <- attr(plot, "axis_break")
    origin_axis <-
      ifelse(
        inherits(origin_axis_break, "ggbreak_params"),
        origin_axis_break$axis,
        origin_axis_break[[1]]$axis
      )
    if (origin_axis != object$axis) {
      cat_stop(
        "The truncation of different axis is not be supported simultaneously.
                         The ",
        origin_axis_break$axis,
        " axis of original plot has been
                         truncated."
      )
    } else {
      object <- list.add(origin_axis_break, object)
    }
  }
  attr(plot, "axis_break") <- object
  class(plot) <- unique(c("ggbreak", class(plot)))
  return(plot)
}
#' @export
ggplot_add.wrap_params <- function(object, plot, object_name) {
  attr(plot, "axis_wrap") <- object
  class(plot) <- c("ggwrap", class(plot))
  return(plot)
}
#' @export
ggplot_add.ggcut_params <- function(object, plot, object_name) {
  attr(plot, "axis_cut") <- object
  class(plot) <- c("ggcut", class(plot))
  return(plot)
}
#' @export
ggplot_add.ggbreak <- function(object, plot, object_name) {
  if (is_ggbreak(plot)) {
    ggplot_add(
      as.ggplot.ggbreak(object),
      as.ggplot.gg(plot),
      object_name
    )
  } else {
    ggplot_add(
      as.ggplot.gg(grid.draw(object, recording = FALSE)),
      as.ggplot.gg(plot),
      object_name
    )
  }
}
#' @export
ggplot_add.ggwrap <- function(object, plot, object_name) {
  if (is_ggbreak(plot)) {
    ggplot_add(
      as.ggplot.ggwrap(object),
      as.ggplot.gg(plot),
      object_name
    )
  } else {
    ggplot_add(
      as.ggplot.gg(grid.draw(object, recording = FALSE)),
      as.ggplot.gg(plot),
      object_name
    )
  }
}
#' @export
ggplot_add.ggcut <- function(object, plot, object_name) {
  if (is_ggbreak(plot)) {
    ggplot_add(
      as.ggplot.ggcut(object),
      as.ggplot.gg(plot),
      object_name
    )
  } else {
    ggplot_add(
      as.ggplot.gg(grid.draw(object, recording = FALSE)),
      as.ggplot.gg(plot),
      object_name
    )
  }
}
#' @export
ggplot_add.ggtree <- function(object, plot, object_name) {
  p <- ggplot_add(as.ggplot(object), as.ggplot(plot), object_name)
  class(p) <- c("ggtree", class(p))
  p
}
#' @export
ggplot_add.gg <- function(object, plot, object_name) {
  if (is_ggbreak(plot)) {
    ggplot_add(as.ggplot(object), as.ggplot(plot), object_name)
  } else {
    NextMethod()
  }
}
#' @export
ggplot_add.axisAlign <- function(object, plot, object_name) {
  limits <- object$limits
  if (is.numeric(limits)) {
    lim_x <- scale_x_continuous(limits = limits, expand = c(0, 0))
    lim_y <- scale_y_continuous(limits = limits, expand = c(0, 0))
  } else {
    lim_x <- scale_x_discrete(limits = limits, expand = c(0, 0.6))
    lim_y <- scale_y_discrete(limits = limits, expand = c(0, 0.6))
  }

  if (object$axis == "x") {
    if (is(plot$coordinates, "CoordFlip")) {
      cat_message("the plot was flipped and the x limits will be applied to y-axis")
      scale_lim <- lim_y
    } else {
      scale_lim <- lim_x
    }
  } else {
    if (is(plot$coordinates, "CoordFlip")) {
      cat_message("the plot was flipped and the y limits will be applied to x-axis")
      scale_lim <- lim_x
    } else {
      scale_lim <- lim_y
    }
  }
  ggplot_add(scale_lim, plot, object_name)
}
#' @export
ggplot_add.facet_set <- function(object, plot, object_name) {
  if (object$side == "right" && is.null(object$angle)) {
    object$angle <- -90
  }
  build_new_plot(object = object, plot = plot)
}
build_new_plot <- function(object, plot) {
  flag.params <- TRUE
  if (!inherits(plot$facet, "FacetNull")) {
    if (inherits(object$label, "labeller") || !is.null(names(object$label))) {
      facet.fun <- eval(parse(text = class(plot$facet)[1]))
      facet.obj <- ggplot2::ggproto(NULL,
        facet.fun,
        shrink = plot$facet$shrink,
        params = plot$facet$params
      )
      if (!is.null(plot$facet$strip)) {
        facet.obj$strip <- plot$facet$strip
      }
      strip.labels <- extract_strip_label(facet = facet.fun, plot = plot)
      if (inherits(object$label, "labeller")) {
        tmp.label <- extract_strip_label(facet = facet.fun, plot = plot, labeller = object$label)
        names(tmp.label) <- names(strip.labels)
        object$label <- tmp.label[!is.na(tmp.label)]
      }
      newnm <- intersect(names(object$label), names(strip.labels))
      if (length(newnm) > 0) {
        strip.labels[match(newnm, names(strip.labels))] <- object$label[match(newnm, names(object$label))]
      }
      facet.obj$params$labeller <- ggplot2::as_labeller(strip.labels)
      flag.params <- FALSE
    }
  }
  if (flag.params) {
    lb <- paste0("'", eval(object$label[1]), "'")
    if (object$side == "top") {
      params <- list(paste0("~", lb))
    } else {
      params <- list(paste0(lb, "~."))
    }
  } else {
    params <- NULL
  }
  if (!is.null(params)) {
    facet.layer <- do.call("facet_grid", params)
    th <- theme(
      strip.background = element_rect(fill = "grey85", colour = NA),
      strip.text = element_text(
        colour = "grey10",
        size = rel(0.8),
        angle = object$angle,
        margin = margin(4.4, 4.4, 4.4, 4.4)
      )
    )
    plot <- plot + facet.layer + th
  } else {
    plot <- plot + facet.obj
  }
  return(plot)
}
#' @export
ggplot_add.plot_layout <-
  utils::getFromNamespace("ggplot_add.plot_layout", "patchwork")
#' @export
ggplot_add.plot_annotation <-
  utils::getFromNamespace("ggplot_add.plot_annotation", "patchwork")
#' @export
ggplot_add.facet_plot <- function(object, plot, object_name) {
  `%+>%` <- function(p, data) {
    df <- p$data
    lv <- levels(df$.panel)
    if (inherits(data, "GRanges") || inherits(data, "GRangesList")) {
      names(data) <- df$y[match(names(data), df$label)]
      res <- data[order(as.numeric(names(data)))]
      mcols <- get_fun_from_pkg("GenomicRanges", "mcols")
      `mcols<-` <- get_fun_from_pkg("GenomicRanges", "`mcols<-`")
      mcols(res)$.panel <- factor(lv[length(lv)], levels = lv)
    } else if (is(data, "data.frame") || is(data, "tbl_df")) {
      data <- as.data.frame(data)
      ## res <- merge(df[, c('label', 'y')], data, by.x='label', by.y=1) ## , all.x=TRUE)
      res <- merge(df[, !names(df) %in% c("node", "parent", "x", "branch", "angle")], data, by.x = "label", by.y = 1)
      res[[".panel"]] <- factor(lv[length(lv)], levels = lv)
      res <- res[order(res$y), ]
    } else if (is.function(data)) {
      res <- data(df)
      if (!is.data.frame(res)) {
        cat_stop("Data function must return a data.frame")
      }
      res[[".panel"]] <- factor(lv[length(lv)], levels = lv)
      res %<>% dplyr::filter(.data$isTip)
      res <- res[order(res$y), ]
    } else {
      cat_stop("input 'data' is not supported...")
    }

    return(res)
  }
  plot <- add_panel(plot, object$panel)
  df <- plot %+>% object$data
  params <- c(
    list(data = df, mapping = object$mapping),
    object$params
  )
  obj <- do.call(object$geom, params)
  ggplot_add(obj, plot, object_name)
}
add_panel <- function(p, panel) {
  df <- p$data
  if (is.null(df[[".panel"]])) {
    df[[".panel"]] <- factor("Tree")
  }
  levels(df$.panel) %<>% c(., panel)
  p$data <- df
  # p + facet_grid(.~.panel, scales="free_x")
  p + facet_grid(
    cols = vars(factor(.data$.panel, levels = levels(df$.panel))),
    scales = "free_x"
  )
}
#' @export
ggplot_add.facet_xlim <- function(object, plot, object_name) {
  var <- panel_col_var(plot)
  free_x <- plot$facet$params$free$x
  if (!is.null(free_x)) {
    if (!free_x) {
      cat_message(
        "If you want to adjust xlim for specific panel, ",
        'you need to set `scales = "free_x"`'
      )
    }
  }

  dummy <- data.frame(x = object$x, .panel = object$panel)
  if (!is.null(var)) {
    names(dummy)[2] <- var
  }

  obj <- geom_blank(aes_(x = ~x), dummy, inherit.aes = FALSE)
  ggplot_add(obj, plot, object_name)
}
panel_col_var <- function(p) {
  m <- p$facet$params$cols[[1]]
  if (is.null(m)) {
    return(m)
  }

  ## rlang::quo_name(m)
  rlang::quo_text(m) %>%
    sub(",.*", "", .) %>%
    sub(".*\\(", "", .) %>%
    sub(".data\\$", "", .)
}
#' @export
ggplot_add.contraster_label <- function(object, plot, object_name) {
  if (is_empty(plot$labels$fill)) {
    cat_stop("no fill aes to contrasts")
  }

  colours <- ggplot_build(plot = plot)$data[[1]]$fill

  object[["color"]] <- contraster(colours)


  object <- eval(object)

  ggplot_add(object, plot, object_name)
}
contraster <- function(.colours) {
  .colurs_hcl <- farver::decode_colour(.colours, "rgb", "hcl")

  ifelse(.colurs_hcl[, "l"] > 50, "black", "white")
}
#' @export
ggplot_add.tiplab_ylab <- function(object, plot, object_name) {
  layout <- get_layout(plot)
  if (is.null(object$position)) {
    if (layout == "rectangular") {
      object$position <- "right"
    } else if (layout == "dendrogram") {
      object$position <- "left"
    }
  }
  df <- plot$data
  df <- df[df$isTip, ]
  yscale <- scale_y_continuous(
    breaks = df$y,
    labels = df$label,
    position = object$position,
    expand = expansion(0, 0.6)
  )
  object$position <- NULL
  object$node <- NULL
  ytext <- do.call(element_text, object)
  if (is.null(object$position)) {
    if (layout == "rectangular") {
      thm <- theme(axis.text.y = ytext)
    } else if (layout == "dendrogram") {
      thm <- theme(axis.text.x = ytext)
    }
  }
  plot + yscale + thm
}
#' @export
ggplot_add.tiplab <- function(object, plot, object_name) {
  layout <- get_layout(plot)
  if (object$as_ylab) {
    if (layout != "rectangular" && layout != "dendrogram") {
      cat_stop("displaying tiplab as y labels only supports rectangular layout")
    }
    object$mapping <- NULL
    object$align <- NULL
    object$linetype <- NULL
    object$linesize <- NULL
    object$geom <- NULL
    object$offset <- NULL
    object$nodelab <- NULL
    object$as_ylab <- NULL
    res <- ggplot_add.tiplab_ylab(object, plot, object_name)
    return(res)
  }
  object$as_ylab <- NULL
  if (layout == "circular" || layout == "fan" || layout ==
      "unrooted" ||
      layout == "equal_angle" || layout == "daylight" ||
      layout == "ape" || layout == "inward_circular") {
    ly <- do.call(geom_tiplab_circular, object)
  } else {
    ly <- do.call(geom_tiplab_rectangular, object)
  }
  ggplot_add(ly, plot, object_name)
}


## as.ggplot

#' @export
as.ggplot <- function(plot, ...) {
  UseMethod("as.ggplot")
}
#' @export
as.ggplot.default <-
  function(plot,
           scale = 1,
           hjust = 0,
           vjust = 0,
           angle = 0,
           ...) {
    as.ggplot_internal <-
      function(plot,
               scale = 1,
               hjust = 0,
               vjust = 0,
               ...) {
        ymin <- xmin <- 1 - scale
        xmax <- ymax <- scale

        ggplot(data.frame(x = 0:1, y = 0:1), aes_(x = ~x, y = ~y)) +
          geom_blank() +
          scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
          scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
          annotation_custom(
            as.grob(plot, ...),
            xmin = xmin + hjust,
            xmax = xmax + hjust,
            ymin = ymin + vjust,
            ymax = ymax + vjust
          ) +
          theme_void()
      }

    if (angle == 0) {
      return(as.ggplot_internal(
        plot = plot,
        scale = scale,
        hjust = hjust,
        vjust = vjust,
        ...
      ))
    }

    g <- grid2grob(print(
      as.ggplot_internal(plot, ...),
      newpage = TRUE,
      vp = viewport(
        x = .5 + hjust,
        y = .5 + vjust,
        angle = angle,
        width = scale,
        height = scale
      )
    ))
    as.ggplot(g)
  }

#' @export
as.ggplot.gg <- as.ggplot.default
#' @export
as.ggplot.ggplot <- as.ggplot.default
#' @export
as.ggplot.ggbreak <- function(plot) {
  as.ggplot.gg(grid.draw.ggbreak(plot, recording = FALSE))
}
#' @export
as.ggplot.ggcut <- function(plot) {
  as.ggplot.gg(grid.draw.ggcut(plot, recording = FALSE))
}
#' @export
as.ggplot.ggwrap <- function(plot) {
  as.ggplot.gg(grid.draw.ggwrap(plot, recording = FALSE))
}

## as.grob

#' @export
as.grob <- function(plot, ...) {
  UseMethod("as.grob")
}
#' @export
as.grob.aplot <- function(plot, ...) {
  res <- as_patchwork.aplot(x)
  patchwork::patchworkGrob(res)
}
#' @export
as.grob.patchwork <- function(plot, ...) {
  patchworkGrob <- utils::getFromNamespace("patchworkGrob", "patchwork")
  patchworkGrob(plot)
}
#' @export
as.grob.ggplot <- function(plot, ...) {
  ggplotGrob(plot)
}
#' @export
as.grob.trellis <- function(plot, ...) {
  grid2grob(print(plot))
}
#' @export
as.grob.Heatmap <- as.grob.trellis
#' @export
as.grob.upset <- as.grob.trellis
#' @export
as.grob.pheatmap <- function(plot, ...) {
  plot$gtable
}
#' @export
as.grob.grob <- function(plot, ...) {
  plot
}
#' @export
as.grob.ggbreak <- function(plot) {
  as.grob(as.ggplot.gg(grid.draw(plot, recording = FALSE)))
}
#' @export
as.grob.ggcut <- as.grob.ggbreak
#' @export
as.grob.ggwrap <- as.grob.ggbreak


# 2grob

#' @export
grid2grob <- function(plot_call) {
  grid::grid.grabExpr(plot_call, warn = 0)
}

# grid.draw


#' @importFrom grid grid.draw
#' @keywords internal
grid.draw <- grid::grid.draw
#' @export
grid.draw.ggbreak <- function(x, recording = TRUE) {
  class(x) <- class(x)[class(x) != "ggbreak"]
  x <- check_xy_intercept(plot = x)
  axis_break <- attr(x, "axis_break")
  axis_breaks <- extract_axis_break(object = axis_break)
  axis <- axis_breaks$axis
  margin <- axis_breaks$space
  breaks <- axis_breaks$breaks
  expand <- axis_breaks$expand
  scales <- axis_breaks$scales
  ticklabs <- axis_breaks$ticklabs

  rng <- ggrange2(plot = x, var = axis)
  res <- combine_range(breaks, rng, scales, ticklabs)
  breaks <- res$breaks
  scales <- res$scales
  ticklabs <- res$ticklabs

  totallabs <- extract_totallabs(plot = x)
  if (length(totallabs) > 0) {
    x$labels[names(totallabs)] <- NULL
  }
  nbreaks <- length(breaks)
  subplottheme1 <-
    subplot_theme(
      plot = x,
      axis = axis,
      type = "first",
      margin = margin,
      rev = rng$flagrev
    )
  subplottheme2 <-
    subplot_theme(
      plot = x,
      axis = axis,
      type = "other",
      margin = margin,
      rev = rng$flagrev
    )
  subplottheme3 <-
    subplot_theme(
      plot = x,
      axis = axis,
      type = "last",
      margin = margin,
      rev = rng$flagrev
    )
  coord_fun <- check_coord_flip(plot = x)
  relrange <-
    compute_relative_range(
      breaks = breaks,
      scales = scales,
      rng = rng
    )
  legendpos <- check_legend_position(plot = x)
  legeninfo <- extract_legend(p = x, position = legendpos$legend.position)
  if (!rng$flagrev %in% c("identity", "reverse")) {
    breaks <- lapply(breaks, function(i) {
      rng$inversefun(i)
    })
  }
  if (x$scales$has_scale(axis)) {
    scaleind <- which(x$scales$find(axis))
  } else {
    scaleind <- NULL
  }
  expand <- convert_expand(expand = expand)

  if (!is.null(scaleind)) {
    x$scales$scales[[scaleind]]$expand <- expand
    if (!inherits(x$scales$scales[[scaleind]]$name, "waiver")) {
      axis.title <- x$scales$scales[[scaleind]]$name
      x <- remove_axis_title(x, axis, coord_fun)
    } else {
      axis.title <- NULL
    }
    if (!inherits(x$scales$scales[[scaleind]]$secondary.axis$name, "waiver")) {
      axis.sec.title <- x$scales$scales[[scaleind]]$secondary.axis$name
      x <- remove_axis_title(x, axis, coord_fun, second = TRUE)
    } else {
      axis.sec.title <- NULL
    }
  } else {
    scale_axis <-
      switch(axis,
             x = scale_x_continuous,
             y = scale_y_continuous
      )
    x <-
      suppressall(x + do.call(scale_axis, list(expand = expand)))
    axis.title <- NULL
    axis.sec.title <- NULL
  }
  newxlab <-
    switch(coord_fun,
           coord_flip = totallabs$y,
           coord_cartesian = totallabs$x
    )
  newylab <-
    switch(coord_fun,
           coord_flip = totallabs$x,
           coord_cartesian = totallabs$y
    )


  if (axis == "x") {
    p1 <-
      suppressall(x + do.call(coord_fun, list(xlim = c(
        breaks[[1]][1], breaks[[1]][2]
      ))) + subplottheme1)

    pp1 <-
      suppressall(lapply(breaks[-c(1, nbreaks)], function(i) {
        x + do.call(coord_fun, list(xlim = c(i[1], i[2]))) +
          subplottheme2
      }))

    pp2 <-
      suppressall(x + do.call(coord_fun, list(xlim = c(
        breaks[[nbreaks]][1], breaks[[nbreaks]][2]
      ))) +
        subplottheme3)

    if (length(ticklabs) > 1) {
      newticklabs <- ticklabs[-length(ticklabs)]
      for (i in seq_len(length(newticklabs))) {
        if (!is.null(scaleind) && !is.null(newticklabs[[i]])) {
          pp1[[i]]$scales$scales[[scaleind]]$breaks <- newticklabs[[i]]
          pp1[[i]]$scales$scales[[scaleind]]$labels <-
            newticklabs[[i]]
        }
        if (is.null(scaleind) && !is.null(newticklabs[[i]])) {
          pp1[[i]] <-
            suppressall(
              pp1[[i]] + scale_x_continuous(
                breaks = newticklabs[[i]],
                labels = newticklabs[[i]],
                expand = expand
              )
            )
        }
      }
    }

    if (!is.null(scaleind) &&
        !is.null(ticklabs[[length(ticklabs)]]) &&
        rng$flagrev != "reverse") {
      pp2$scales$scales[[scaleind]]$breaks <- ticklabs[[length(ticklabs)]]
      pp2$scales$scales[[scaleind]]$labels <-
        ticklabs[[length(ticklabs)]]
    }
    if (is.null(scaleind) &&
        !is.null(ticklabs[[length(ticklabs)]]) &&
        rng$flagrev != "reverse") {
      pp2 <-
        suppressall(
          pp2 + scale_x_continuous(
            breaks = ticklabs[[length(ticklabs)]],
            labels = ticklabs[[length(ticklabs)]],
            expand = expand
          )
        )
    }
    if (!is.null(scaleind) &&
        !is.null(ticklabs[[length(ticklabs)]]) &&
        rng$flagrev == "reverse") {
      p1$scales$scales[[scaleind]]$breaks <- ticklabs[[length(ticklabs)]]
      p1$scales$scales[[scaleind]]$labels <-
        ticklabs[[length(ticklabs)]]
    }
    if (is.null(scaleind) &&
        !is.null(ticklabs[[length(ticklabs)]]) &&
        rng$flagrev == "reverse") {
      p1 <-
        suppressall(
          p1 + scale_x_continuous(
            breaks = ticklabs[[length(ticklabs)]],
            labels = ticklabs[[length(ticklabs)]],
            expand = expand
          )
        )
    }

    g <- switch(coord_fun,
                coord_flip = gr_grid(
                  gglist = set_names(c(list(pp2), rev(pp1), list(p1)), NULL),
                  ncol = 1,
                  heights = c(rev(relrange[-1]), relrange[1]),
                  guides = "collect"
                ) & theme(legend.position = "none"),
                coord_cartesian = gr_grid(
                  gglist = set_names(c(list(p1), pp1, list(pp2)), NULL),
                  nrow = 1,
                  widths = relrange,
                  guides = "collect"
                ) & theme(legend.position = "none")
    )
  } else {
    breaks <- rev(breaks)
    ticklabs <- rev(ticklabs)

    p1 <-
      suppressall(x + do.call(coord_fun, list(ylim = c(
        breaks[[nbreaks]][1], breaks[[nbreaks]][2]
      ))) + subplottheme1)

    pp1 <-
      suppressall(lapply(breaks[-c(1, nbreaks)], function(i) {
        x + do.call(coord_fun, list(ylim = c(i[1], i[2]))) +
          subplottheme2
      }))

    pp2 <-
      suppressall(x + do.call(coord_fun, list(ylim = c(
        breaks[[1]][1], breaks[[1]][2]
      ))) +
        subplottheme3)

    if (length(ticklabs) > 1) {
      newticklabs <- ticklabs[-1]
      for (i in seq_len(length(newticklabs))) {
        if (!is.null(scaleind) && !is.null(newticklabs[[i]])) {
          pp1[[i]]$scales$scales[[scaleind]]$breaks <- newticklabs[[i]]
          pp1[[i]]$scales$scales[[scaleind]]$labels <-
            newticklabs[[i]]
        }
        if (is.null(scaleind) && !is.null(newticklabs[[i]])) {
          pp1[[i]] <-
            suppressall(
              pp1[[i]] + scale_y_continuous(
                breaks = newticklabs[[i]],
                labels = newticklabs[[i]],
                expand = expand
              )
            )
        }
      }
    }

    if (!is.null(scaleind) &&
        !is.null(ticklabs[[1]]) && rng$flagrev != "reverse") {
      pp2$scales$scales[[scaleind]]$breaks <- ticklabs[[1]]
      pp2$scales$scales[[scaleind]]$labels <- ticklabs[[1]]
    }
    if (is.null(scaleind) &&
        !is.null(ticklabs[[1]]) && rng$flagrev != "reverse") {
      pp2 <-
        suppressall(
          pp2 + scale_y_continuous(
            breaks = ticklabs[[1]],
            labels = ticklabs[[1]],
            expand = expand
          )
        )
    }
    if (!is.null(scaleind) &&
        !is.null(ticklabs[[1]]) && rng$flagrev == "reverse") {
      p1$scales$scales[[scaleind]]$breaks <- ticklabs[[1]]
      p1$scales$scales[[scaleind]]$labels <- ticklabs[[1]]
    }
    if (is.null(scaleind) &&
        !is.null(ticklabs[[1]]) && rng$flagrev == "reverse") {
      p1 <-
        suppressall(
          p1 + scale_y_continuous(
            breaks = ticklabs[[1]],
            labels = ticklabs[[1]],
            expand = expand
          )
        )
    }

    g <- switch(coord_fun,
                coord_flip = gr_grid(
                  gglist = set_names(c(list(p1), rev(pp1), list(pp2)), NULL),
                  nrow = 1,
                  widths = relrange,
                  guides = "collect"
                ) & theme(legend.position = "none"),
                coord_cartesian = gr_grid(
                  gglist = set_names(c(list(pp2), pp1, list(p1)), NULL),
                  ncol = 1,
                  heights = c(rev(relrange[-1]), relrange[1]),
                  guides = "collect"
                ) & theme(legend.position = "none")
    )
  }

  totallabs$x <- NULL
  totallabs$y <- NULL
  g <- as.ggplot.gg(g) + xlab(newxlab) + ylab(newylab)

  g <- check_axis_title(
    plot = g,
    axis = axis,
    coord_fun = coord_fun,
    axis.title = axis.title,
    axis.sec.title = axis.sec.title
  )

  g <- set_label(g, totallabs = totallabs, p2 = x)

  attr(g, "legend_info") <- list(legendpos = legendpos, legeninfo = legeninfo)
  if (recording) {
    print(g)
  }
  invisible(g)
}
#' @export
grid.draw.ggwrap <- function(x, recording = TRUE) {
  class(x) <- class(x)[class(x) != "ggwrap"]
  x <- check_xy_intercept(plot = x)
  axis_wrap <- attr(x, "axis_wrap")
  totallabs <- extract_totallabs(plot = x)
  if (length(totallabs) > 0) {
    x$labels[names(totallabs)] <- NULL
  }
  nstep <- axis_wrap$n
  expand <- axis_wrap$expand
  rngrev <- ggrange2(plot = x, "x")
  rng <- rngrev$axis_range
  if (is.null(rngrev$flagrev)) {
    rng <- c(.5, length(rng))
  } else if (rngrev$flagrev == "reverse") {
    rng <- rev(-1 * (rng))
  }
  breaks <- seq(rng[1], rng[2], length.out = nstep + 1)
  if (is.null(rngrev$flagrev)) {
    breaks <- round(breaks, 0) + .5
  } else if (!rngrev$flagrev %in% c("identity", "reverse")) {
    breaks <- rngrev$inversefun(breaks)
  }
  x <- add_expand(
    plot = x,
    expand = expand,
    axis = "x"
  )
  legendpos <- check_legend_position(plot = x)
  legeninfo <- extract_legend(p = x, position = legendpos$legend.position)
  gg <-
    lapply(seq_len(length(breaks) - 1), function(i) {
      x + coord_cartesian(xlim = c(breaks[i], breaks[i + 1]))
    })
  pg <-
    gr_grid(
      gglist = set_names(gg, NULL),
      ncol = 1,
      guides = "collect"
    ) & theme(legend.position = "none")
  g <- set_label(as.ggplot.gg(pg), totallabs = totallabs, p2 = x)
  attr(g, "legend_info") <- list(legendpos = legendpos, legeninfo = legeninfo)
  if (recording) {
    print(g)
  }

  invisible(g)
}
#' @export
grid.draw.ggcut <- function(x, recording = TRUE) {
  class(x) <- class(x)[class(x) != "ggcut"]
  x <- check_xy_intercept(plot = x)
  axis_cut <- attr(x, "axis_cut")
  axis <- axis_cut$axis
  margin <- axis_cut$space
  expand <- axis_cut$expand
  totallabs <- extract_totallabs(plot = x)
  if (length(totallabs) > 0) {
    x$labels[names(totallabs)] <- NULL
  }
  rngrev <- ggrange2(plot = x, var = axis)
  breaks_relrange <-
    compute_ggcut_breaks_relrange(ggcut_params = axis_cut, rngrev = rngrev)
  breaks <- breaks_relrange$breaks
  relrange <- breaks_relrange$relrange

  nbreaks <- length(breaks)
  subplottheme1 <-
    subplot_theme(
      plot = x,
      axis = axis,
      type = "first",
      margin = margin,
      rev = rngrev$flagrev
    )
  subplottheme2 <-
    subplot_theme(
      plot = x,
      axis = axis,
      type = "other",
      margin = margin,
      rev = rngrev$flagrev
    )
  subplottheme3 <-
    subplot_theme(
      plot = x,
      axis = axis,
      type = "last",
      margin = margin,
      rev = rngrev$flagrev
    )
  coord_fun <- check_coord_flip(plot = x)
  newxlab <-
    switch(coord_fun,
           coord_flip = totallabs$y,
           coord_cartesian = totallabs$x
    )
  legendpos <- check_legend_position(plot = x)
  legeninfo <- extract_legend(p = x, position = legendpos$legend.position)
  newylab <-
    switch(coord_fun,
           coord_flip = totallabs$x,
           coord_cartesian = totallabs$y
    )
  if (!rngrev$flagrev %in% c("identity", "reverse")) {
    breaks <- rngrev$inversefun(breaks)
  }

  x <- add_expand(
    plot = x,
    expand = expand,
    axis = axis
  )
  if (axis == "x") {
    p1 <-
      suppressall(x + do.call(coord_fun, list(xlim = c(
        breaks[[1]][1], breaks[[1]][2]
      ))) + subplottheme1)
    pp1 <-
      suppressall(lapply(breaks[-c(1, nbreaks)], function(i) {
        x + do.call(coord_fun, list(xlim = c(i[1], i[2]))) +
          subplottheme2
      }))
    pp2 <-
      suppressall(x + do.call(coord_fun, list(xlim = c(
        breaks[[nbreaks]][1], breaks[[nbreaks]][2]
      ))) +
        subplottheme3)
    g <- switch(coord_fun,
                coord_flip = gr_grid(
                  gglist = set_names(c(list(pp2), rev(pp1), list(p1)), NULL),
                  ncol = 1,
                  heights = relrange,
                  guides = "collect"
                ) & theme(legend.position = "none"),
                coord_cartesian = gr_grid(
                  gglist = set_names(c(list(p1), pp1, list(pp2)), NULL),
                  nrow = 1,
                  widths = relrange,
                  guides = "collect"
                ) & theme(legend.position = "none")
    )
  } else {
    breaks <- rev(breaks)
    p1 <-
      suppressall(x + do.call(coord_fun, list(ylim = c(
        breaks[[nbreaks]][1], breaks[[nbreaks]][2]
      ))) + subplottheme1)
    pp1 <-
      suppressall(lapply(breaks[-c(1, nbreaks)], function(i) {
        x + do.call(coord_fun, list(ylim = c(i[1], i[2]))) +
          subplottheme2
      }))
    pp2 <-
      suppressall(x + do.call(coord_fun, list(ylim = c(
        breaks[[1]][1], breaks[[1]][2]
      ))) +
        subplottheme3)
    g <- switch(coord_fun,
                coord_flip = gr_grid(
                  gglist = set_names(c(list(p1), rev(pp1), list(pp2)), NULL),
                  nrow = 1,
                  widths = relrange,
                  guides = "collect"
                ) & theme(legend.position = "none"),
                coord_cartesian = gr_grid(
                  gglist = set_names(c(list(pp2), pp1, list(p1)), NULL),
                  ncol = 1,
                  heights = relrange,
                  guides = "collect"
                ) & theme(legend.position = "none")
    )
  }
  totallabs$x <- NULL
  totallabs$y <- NULL
  g <- as.ggplot.gg(g) + xlab(newxlab) + ylab(newylab)
  g <- set_label(g, totallabs = totallabs, p2 = x)
  attr(g, "legend_info") <- list(legendpos = legendpos, legeninfo = legeninfo)
  if (recording) {
    print(g)
  }

  invisible(g)
}
#' @export
grid.draw.aplot <- function(x, recoding = TRUE) {
  grid::grid.draw(as_patchwork.aplot(x))
}


# method-plot

#' @export
plot.ggbreak <- function(x, y, ...) {
  NextMethod()
}
#' @export
plot.ggwrap <- function(x, y, ...) {
  NextMethod()
}
#' @export
plot.ggcut <- function(x, y, ...) {
  NextMethod()
}

# method-print

#' @export
print.ggbreak <- function(x, ...) {
  p <- grid.draw(x, ...)
  build_print_ggbreak(p)
}
build_print_ggbreak <- function(p) {
  attr(p, "legend_info") -> len
  pos <- len$legendpos$legend.position
  legend <- len$legeninfo
  if (is_true(pos == "none") || is.null(pos) || is_empty(legend)) {
    print(p)
  } else {
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    combined <- switch(pos,
                       "bottom" = gridExtra::arrangeGrob(
                         ggplotGrob(p),
                         legend,
                         ncol = 1,
                         heights = grid::unit.c(unit(1, "npc") - lheight, lheight)
                       ),
                       "right" = gridExtra::arrangeGrob(
                         ggplotGrob(p),
                         legend,
                         ncol = 2,
                         widths = grid::unit.c(unit(1, "npc") - lwidth, lwidth)
                       ),
                       "top" = gridExtra::arrangeGrob(
                         legend,
                         ggplotGrob(p),
                         ncol = 1,
                         heights = grid::unit.c(lheight, unit(1, "npc") - lheight)
                       ),
                       "right" = gridExtra::arrangeGrob(
                         legend,
                         ggplotGrob(p),
                         ncol = 2,
                         widths = grid::unit.c(lwidth, unit(1, "npc") - lwidth)
                       )
    )
    grid::grid.newpage()
    grid::grid.draw(combined)

    invisible(combined)
  }
}
#' @export
print.ggwrap <- function(x, ...) {
  p <- grid.draw(x, ...)
  build_print_ggbreak(p)
}
#' @export
print.ggcut <- function(x, ...) {
  p <- grid.draw(x, ...)
  build_print_ggbreak(p)
}
#' @export
print.aplot <- function(x, ...) {
  grid.draw(x)
}


# treelayout

#' @keywords internal
rotate_tree <- function(treeview, angle) {
  treeview <- treeview + coord_polar(theta = "y", start = (angle - 90) / 180 * pi, -1)
  treeview$data$angle <- treeview$data$angle + angle
  assign("layout", "circular", envir = treeview$plot_env)
  return(treeview)
}
#' @keywords internal
open_tree <- function(treeview, angle) {
  p <- treeview + layout_circular()
  ymax <- max(range(p$data$y))
  p <- p + scale_y_continuous(limits = c(
    0,
    max(c(ymax * (1 + angle / (360 - angle)), ymax + 1))
  ))
  N <- nrow(p$data)
  idx <- match(1:N, order(p$data$y))
  NN <- N * (1 + angle / (360 - angle))
  angle <- 360 / (2 + NN) * (1:N + 1)
  angle <- angle[idx]
  p$data$angle <- angle
  assign("layout", "fan", envir = p$plot_env)
  return(p)
}
#' @keywords internal
layout_rectangular <- function() {
  layout_ggtree("rectangular")
}
#' @keywords internal
layout_circular <- function() {
  layout_ggtree("circular")
}
#' @keywords internal
layout_inward_circular <- function(xlim = NULL) {
  if (!is.null(xlim) && length(xlim) == 1) {
    xlim <- c(xlim, 0)
  }
  layout_ggtree(layout = "inward_circular", xlim = xlim)
}
#' @keywords internal
layout_fan <- function(angle = 180) {
  layout_ggtree("fan", angle = angle)
}
#' @keywords internal
layout_dendrogram <- function() {
  layout_ggtree("dendrogram")
}
#' @keywords internal
layout_ggtree <- function(layout = "rectangular",
                          angle = 180,
                          xlim = NULL) {
  structure(list(
    layout = layout,
    angle = angle,
    xlim = xlim
  ),
  class = "layout_ggtree"
  )
}


#' @keywords internal
as_patchwork <-
  utils::getFromNamespace("as_patchwork", "patchwork")
#' @keywords internal
as_patchwork.default <-
  utils::getFromNamespace("as_patchwork.default", "patchwork")
#' @keywords internal
as_patchwork.ggplot <-
  utils::getFromNamespace("as_patchwork.ggplot", "patchwork")
#' @keywords internal
as_patchwork.patchwork <-
  utils::getFromNamespace("as_patchwork.patchwork", "patchwork")
#' @keywords internal
as_patchwork.aplot <- function(x) {
  if (!inherits(x, "aplot")) {
    cat_stop("only aplot object supported")
  }

  mp <- x$plotlist[[1]]
  if (length(x$plotlist) == 1) {
    return(ggplotGrob(mp))
  }

  for (i in x$layout[, x$main_col]) {
    if (is.na(i)) next
    if (i == 1) next
    x$plotlist[[i]] <- suppressall(x$plotlist[[i]] + xlim2(mp))
  }
  for (i in x$layout[x$main_row, ]) {
    if (is.na(i)) next
    if (i == 1) next
    x$plotlist[[i]] <- suppressall(x$plotlist[[i]] + ylim2(mp))
  }

  idx <- as.vector(x$layout)
  idx[is.na(idx)] <- x$n + 1
  x$plotlist[[x$n + 1]] <- ggplot() +
    theme_void() # plot_spacer()
  plotlist <- x$plotlist[idx]

  pp <- plotlist[[1]] + no_margin_theme()
  for (i in 2:length(plotlist)) {
    pp <- pp + (plotlist[[i]] + no_margin_theme())
  }



  pp + patchwork::plot_layout(
    byrow = F,
    ncol = ncol(x$layout),
    widths = x$width,
    heights = x$height,
    guides = "collect"
  )
}


#' @keywords internal
is_ggtree <- function(x) {
  if (inherits(x, "ggtree")) {
    return(TRUE)
  }

  if (!inherits(x, "gg")) {
    return(FALSE)
  }

  ## to compatible with user using `ggplot(tree) + geom_tree()`

  tree_layer <- vapply(
    x$layers,
    function(y) {
      any(grepl("StatTree", class(y$stat)))
    },
    logical(1)
  )
  return(any(tree_layer))
}
#' @keywords internal
is_ggbreak <- function(plot) {
  if (inherits(plot, "ggbreak") ||
      inherits(plot, "ggwrap") ||
      inherits(plot, "ggcut")
  ) {
    return(TRUE)
  }

  return(FALSE)
}
#' @keywords internal
is_patchwork <-
  utils::getFromNamespace("is_patchwork", "patchwork")
#' @keywords internal
as_aplot <- function(plot) {
  if (inherits(plot, "aplot")) {
    return(plot)
  }

  if (!inherits(plot, "gg")) {
    cat_stop("input should be a 'gg' object.")
  }
  structure(
    list(
      plotlist = list(plot),
      width = 1,
      height = 1,
      layout = matrix(1),
      n = 1,
      main_col = 1,
      main_row = 1
    ),
    class = "aplot"
  )
}
#' @keywords internal
"&.gg" <- utils::getFromNamespace("&.gg", "patchwork")

#' @keywords internal
reorder_other <- function(x, by, other = FALSE) {
  if (other) {
    x2 <- x[x != "Other"]
    by2 <- by[x != "Other"]
    levels <- c("Other", x2[order(by2)])
    ordered(x, levels = levels)
  } else {
    factor(x, levels = x[order(by, na.last = FALSE)])
  }
}
#' @keywords internal
extract_pal_gr <- function(x) {
  as.list(environment(x$palette))$f
}
#' @keywords internal
lighten_darken <- function(x, action = c("lightup", "growdark"), factor = .5) {
  action <- match.arg(action)
  action <-
    switch(action,
           lightup = c("white", x),
           growdark = c("black", x)
    )
  color_f <- scales::gradient_n_pal(colours = c(action))
  color_f(factor)
}
#' @keywords internal
text_gradient <- function(text) {
  f_l <- c(0.757, 0.46, 0.276, 0.161)
  f_d <- c(0.087, 0.225, 0.362, 0.541)

  col_l <-
    map_chr(
      f_l,
      ~ lighten_darken(
        x = text,
        action = "lightup",
        factor = .x
      )
    )

  col_d <-
    map_chr(
      f_d,
      ~ lighten_darken(
        x = text,
        action = "growdark",
        factor = .x
      )
    )

  c(col_d, text, col_l)
}


#' @export
gr_show_plot_color <- function(colour_vector, text_vector = NULL, label = TRUE) {
  if (is_empty(text_vector)) {
    text_vector <- colour_vector
  }
  if (!is.factor(text_vector)) {
    text_vector <- factor(text_vector, rev(colour_vector))
  }
  if (!is.factor(colour_vector)) {
    colour_vector <- factor(colour_vector, rev(colour_vector))
  }

  data <- tibble(
    x = rep_along(1, factor(1)),
    y = rep_along(1, colour_vector),
    y2 = seq_along(colour_vector),
    fill = colour_vector,
    label = as.character(text_vector)
  )

  p <- ggplot(data = data) +
    geom_col(aes(
      x = x,
      y = y,
      fill = fill
    )) +
    scale_fill_identity() +
    theme_minimal() +
    theme(
      text = gg_bnk,
      line = gg_bnk,
      plot.background = gg_bnk
    ) +
    geom_text(aes(
      x = x,
      y = y2 - .5,
      label = label,
      colour = fill
    )) +
    scale_colour_identity()

  if (label) {
    p <- p + geom_text(
      aes(
        x = x,
        y = y2 - .4,
        label = label
      ),
      nudge_x = -.25,
      colour = "white",
      fontface = "bold"
    ) +
      geom_text(
        aes(
          x = x,
          y = y2 - .6,
          label = text_vector
        ),
        nudge_x = .25,
        colour = "black",
        fontface = "bold"
      )
  }
  p
}
#' @export
gr_show_linetypes <- function() {
  oldPar <- par()
  par(font = 2, mar = c(0, 0, 0, 0))
  plot(
    1,
    pch = "",
    ylim = c(0, 6),
    xlim = c(0, 0.7),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  for (i in 0:6) {
    lines(c(0.3, 0.7), c(i, i), lty = i, lwd = 3)
  }
  text(
    rep(0.1, 6),
    0:6,
    labels = c(
      "0.'blank'",
      "1.'solid'",
      "2.'dashed'",
      "3.'dotted'",
      "4.'dotdash'",
      "5.'longdash'",
      "6.'twodash'"
    )
  )
  par(mar = oldPar$mar, font = oldPar$font)
}
#' @export
gr_show_pointshapes <- function() {
  oldPar <- par()
  par(font = 2, mar = c(0.5, 0, 0, 0))
  y <- rev(c(rep(1, 6), rep(2, 5), rep(3, 5), rep(4, 5), rep(5, 5)))
  x <- c(rep(1:5, 5), 6)
  plot(
    x,
    y,
    pch = 0:25,
    cex = 1.5,
    ylim = c(1, 5.5),
    xlim = c(1, 6.5),
    axes = FALSE,
    xlab = "",
    ylab = "",
    bg = "blue"
  )
  text(x, y, labels = 0:25, pos = 3)
  par(mar = oldPar$mar, font = oldPar$font)
}
#' @export
gr_show_wheel <- function(color, num = 12, bg = "gray95", init.angle = 105, cex = 1) {
  if (!is.numeric(num) || any(is.na(num) | num < 0)) {
    cat_stop("\n'num' must be positive")
  }
  x <- rep(1, num)
  x <- c(0, cumsum(x) / sum(x))
  dx <- diff(x)
  nx <- length(dx)
  # set colors
  col <- setColors(color, num)
  labels <- col
  # labels color
  labcol <- ifelse(mean(col2rgb(bg)) > 127, "black", "white")
  # prepare plot window
  par(bg = bg)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) {
    xlim <- (pin[1L] / pin[2L]) * xlim
  } else {
    ylim <- (pin[2L] / pin[1L]) * ylim
  }
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  # get ready to plot
  border <- rep(bg, length.out = nx)
  border <- rep(border, length.out = nx)
  lty <- NULL

  angle <- rep(45, length.out = nx)
  radius <- seq(1, 0, by = -1 / num)[1:num]
  twopi <- -2 * pi
  t2xy <- function(t, rad) {
    t2p <- twopi * t + init.angle * pi / 180
    list(x = rad * cos(t2p), y = rad * sin(t2p))
  }
  # plot colored segments
  for (i in 1L:nx)
  {
    n <- max(2, floor(200 * dx[i]))
    P <-
      t2xy(seq.int(x[i], x[i + 1], length.out = n), rad = radius[1])
    polygon(
      c(P$x, 0),
      c(P$y, 0),
      angle = angle[i],
      border = border[i],
      col = col[i],
      lty = lty[i]
    )
    P <- t2xy(mean(x[i + 0:1]), rad = radius[1])
    lab <- labels[i]
    if (!is.na(lab) && nzchar(lab)) {
      adjs <- 0.5
      if (P$x > 1e-08) {
        adjs <- 0
      }
      if (P$x < -1e-08) {
        adjs <- 1
      }
      lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
      text(
        1.1 * P$x,
        1.1 * P$y,
        labels[i],
        xpd = TRUE,
        adj = adjs,
        cex = cex,
        col = labcol
      )
    }
  }
  # add title
  # return color names
  col
}

setColors <- function(color, num) {
  # convert to RGB
  rgb_col <- col2rgb(color)
  # convert to HSV
  hsv_col <- rgb2hsv(rgb_col)[, 1]
  # get degree
  hue <- hsv_col[1]
  sat <- hsv_col[2]
  val <- hsv_col[3]
  cols <- seq(hue, hue + 1, by = 1 / num)
  cols <- cols[1:num]
  cols[cols > 1] <- cols[cols > 1] - 1
  # get colors with hsv
  colors <- hsv(cols, sat, val)
  # transparency
  if (substr(color, 1, 1) == "#" && nchar(color) == 9) {
    alpha <- substr(color, 8, 9)
    colors <- paste(colors, alpha, sep = "")
  }
  colors
}
col2HSV <- function(color) {
  # convert to RGB
  rgb_col <- col2rgb(color)
  # convert to HSV
  hsv_col <- rgb2hsv(rgb_col)
  if (length(color) == 1) {
    # get degree
    hue <- hsv_col[1]
    sat <- hsv_col[2]
    val <- hsv_col[3]
    # get colors with hsv
    hex_col <- hsv(hue, sat, val)
  }
  if (length(color) > 1) {
    hex_col <- rep("", length(color))
    for (j in 1:length(color))
    {
      hex_col[j] <- hsv(hsv_col[1, j], hsv_col[2, j], hsv_col[3, j])
    }
  }
  hex_col
}
pizza <- function(colors, bg = "gray95", border = NA, init.angle = 105, cex = 0.8, lty = 1, labcol = NULL, ...) {
  n <- length(colors)
  x <- rep(1, n)
  x <- c(0, cumsum(x) / sum(x))
  dx <- diff(x)
  nx <- length(dx)
  # set colors
  labels <- colors
  #
  if (is.null(labcol)) {
    if (mean(col2rgb(bg)) > 127) {
      labcol <- rep("black", n)
    }
    if (mean(col2rgb(bg)) <= 127) {
      labcol <- rep("white", n)
    }
  }
  # prepare plot window
  par(bg = bg)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) {
    xlim <- (pin[1L] / pin[2L]) * xlim
  } else {
    ylim <- (pin[2L] / pin[1L]) * ylim
  }
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  # get ready to plot
  border <- rep(border, length.out = nx)
  if (is.null(border[1])) {
    border <- rep(bg, length.out = nx)
  }
  lty <- rep(lty, length.out = nx)
  angle <- rep(45, length.out = nx)
  radius <- seq(1, 0, by = -1 / n)[1:n]
  twopi <- -2 * pi
  t2xy <- function(t, rad) {
    t2p <- twopi * t + init.angle * pi / 180
    list(x = rad * cos(t2p), y = rad * sin(t2p))
  }
  # plot colored segments
  for (i in 1L:nx)
  {
    n <- max(2, floor(200 * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n), rad = radius[1])
    polygon(c(P$x, 0), c(P$y, 0),
            angle = angle[i],
            border = border[i], col = colors[i], lty = lty[i]
    )
    P <- t2xy(mean(x[i + 0:1]), rad = radius[1])
    lab <- labels[i]
    if (!is.na(lab) && nzchar(lab)) {
      adjs <- 0.5
      if (P$x > 1e-08) adjs <- 0
      if (P$x < -1e-08) adjs <- 1
      lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y, col = labcol[i])
      text(1.1 * P$x, 1.1 * P$y, labels[i],
           xpd = TRUE,
           adj = adjs, cex = cex, col = labcol[i], ...
      )
    }
  }
  invisible()
}

#' @export
gr_show_analogous <-
  function(color,
           type = c("adjacent", "complementary", "square", "triadic"),
           bg = "white",
           cex = 0.8) {
    type <- match.arg(type)
    switch(type,
           adjacent = gr_show_adjacent(color = color, bg = bg, cex = cex),
           complementary = gr_show_complementary(color = color, bg = bg, cex = cex),
           square = gr_show_square(color = color, bg = bg, cex = cex),
           triadic = gr_show_triadic(color = color, bg = bg, cex = cex)
    )
  }

gr_show_complementary <- function(color, bg = "white", cex = 0.8) {
  tmp_cols <- setColors(color, 12)
  comp_colors <- tmp_cols[c(1, 7)]

  # labels color
  lab_col <- rep("", 12)
  if (mean(col2rgb(bg)) > 127) {
    lab_col[c(1, 7)] <- "black"
    lab_col[c(2:6, 8:12)] <- col2HSV(bg)
  } else {
    lab_col[c(1, 7)] <- "white"
    lab_col[c(2:6, 8:12)] <- col2HSV(bg)
  }

  # hide non-adjacent colors
  tmp_cols[c(2:6, 8:12)] <-
    paste(substr(tmp_cols[c(2:6, 8:12)], 1, 7), "0D", sep = "")
  pizza(tmp_cols,
        labcol = lab_col,
        bg = bg,
        cex = cex
  )
  # title


  # result
  comp_colors
}
gr_show_adjacent <- function(color, bg = "white", cex = 0.8) {
  tmp_cols <- setColors(color, 12)
  adja_colors <- tmp_cols[c(1, 2, 12)]

  # plot

  # labels color
  lab_col <- rep("", 12)
  if (mean(col2rgb(bg)) > 127) {
    lab_col[c(1, 2, 12)] <- "black"
    lab_col[c(3:11)] <- col2HSV(bg)
  } else {
    lab_col[c(1, 2, 12)] <- "white"
    lab_col[c(3:11)] <- col2HSV(bg)
  }
  # hide non-adjacent colors
  tmp_cols[c(3:11)] <-
    paste(substr(tmp_cols[c(3:11)], 1, 7), "0D", sep = "")
  pizza(tmp_cols,
        labcol = lab_col,
        bg = bg,
        cex = cex
  )
  # title

  # result
  adja_colors
}
gr_show_square <- function(color, bg = "white", cex = 0.8) {
  tmp_cols <- setColors(color, 12)
  sqr_colors <- tmp_cols[c(1, 4, 7, 10)]

  # plot

  # labels color

  lab_col <- rep("", 12)
  if (mean(col2rgb(bg)) > 127) {
    lab_col[c(1, 4, 7, 10)] <- "black"
    lab_col[c(2, 3, 5, 6, 8, 9, 11, 12)] <- col2HSV(bg)
  } else {
    lab_col[c(1, 4, 7, 10)] <- "white"
    lab_col[c(2, 3, 5, 6, 8, 9, 11, 12)] <- col2HSV(bg)
  }
  # hide non-adjacent colors
  tmp_cols[c(2, 3, 5, 6, 8, 9, 11, 12)] <- paste(substr(tmp_cols[c(2, 3, 5, 6, 8, 9, 11, 12)], 1, 7), "0D", sep = "")
  pizza(tmp_cols, labcol = lab_col, bg = bg, cex = cex)
  # title


  # result
  sqr_colors
}
gr_show_triadic <- function(color, bg = "white", cex = 0.8) {
  tmp_cols <- setColors(color, 12)
  triad_colors <- tmp_cols[c(1, 5, 9)]

  lab_col <- rep("", 12)
  if (mean(col2rgb(bg)) > 127) {
    lab_col[c(1, 5, 9)] <- "black"
    lab_col[c(2, 3, 4, 6, 7, 8, 10, 11, 12)] <- col2HSV(bg)
  } else {
    lab_col[c(1, 5, 9)] <- "white"
    lab_col[c(2, 3, 4, 6, 7, 8, 10, 11, 12)] <- col2HSV(bg)
  }

  # hide non-adjacent colors
  tmp_cols[c(2, 3, 4, 6, 7, 8, 10, 11, 12)] <- paste(
    substr(tmp_cols[c(2, 3, 4, 6, 7, 8, 10, 11, 12)], 1, 7), "0D",
    sep = ""
  )
  pizza(tmp_cols, labcol = lab_col, bg = bg, cex = cex)
  # title

  # result
  triad_colors
}



## varios
##


#' @keywords internal
get_aes_var <- function(mapping, var) {
  res <- rlang::quo_text(mapping[[var]])
  utils::tail(res, 1)
}
#' @keywords internal
get_taxa_order <- function(tree_view) {
  df <- tree_view$data
  with(df, {
    i <- order(y, decreasing = T)
    label[i][isTip[i]]
  })
}
#' @keywords internal
check.graph.layout <- function(tr, trash, layout, layout.params) {
  if (inherits(trash, "try-error")) {
    gp <- ape::as.igraph.phylo(as.phylo(tr), use.labels = FALSE)
    # dd <- ggraph::create_layout(gp, layout = layout)
    if (is.function(layout)) {
      dd <- do.call(layout, c(list(gp), layout.params))
      if (!inherits(dd, "matrix")) {
        if ("xy" %in% names(dd)) {
          dd <- dd$xx
        } else if ("layout" %in% names(dd)) {
          dd <- dd$layout
        } else {
          cat_stop(trash)
        }
      }
      dd <- data.frame(dd)
      colnames(dd) <- c("x", "y")
      dd$node <- seq_len(nrow(dd))
    } else {
      cat_stop(trash)
    }
  } else {
    dd <- NULL
  }
  return(dd)
}
#' @keywords internal
revts <- function(treeview) {
  x <- treeview$data$x
  mx <- max(x, na.rm = TRUE)
  treeview$data$x <- x - mx
  treeview$data$branch <- treeview$data$branch - mx
  treeview
}
#' @keywords internal
setup_tree_data <- function(data) {
  if (nrow(data) == length(unique(data$node))) {
    return(data)
  }

  data[match(unique(data$node), data$node), ]
}
#' @keywords internal
get_layout <- function(p) {
  ggplot_build(p)$layout$layout
}


#' @keywords internal
combine_range <- function(breaks, rangeres, scales, ticklabs) {
  if (rangeres$flagrev == "reverse") {
    rangeres$axis_range <- rev(-1 * (rangeres$axis_range))
  }
  if (rangeres$flagrev == "date") {
    if (is.list(breaks)) {
      breaks <- lapply(breaks, function(i) {
        as.Date(i)
      })
    } else {
      breaks <- as.Date(breaks)
    }
  }
  if (!rangeres$flagrev %in% c("identity", "reverse")) {
    if (is.list(breaks)) {
      breaks <- lapply(breaks, function(i) {
        rangeres$transfun(i)
      })
    } else {
      breaks <- rangeres$transfun(breaks)
    }
  }
  res <- merge_intervals(breaks, scales, ticklabs)
  newbreaks <- res$breaks
  newscales <- res$scales
  newticklabs <- res$ticklabs
  newbreaks <-
    c(
      rangeres$axis_range[1],
      unlist(newbreaks),
      rangeres$axis_range[2]
    )
  newbreaks <-
    lapply(data.frame(matrix(newbreaks, nrow = 2)), function(i) {
      i
    })
  if (rangeres$flagrev == "reverse") {
    newbreaks <- lapply(newbreaks, function(i) {
      rev(i)
    })
    return(list(
      breaks = rev(newbreaks),
      scales = rev(newscales),
      ticklabs = c(rev(newticklabs[-length(newticklabs)]), newticklabs[length(newticklabs)])
    ))
  }
  return(list(
    breaks = newbreaks,
    scales = newscales,
    ticklabs = newticklabs
  ))
}
#' @keywords internal
check_xy_intercept <- function(plot) {
  confuse_xy_labs <- c(
    "xend",
    "xmax",
    "xmin",
    "xintercept",
    "yend",
    "ymax",
    "ymin",
    "yintercept"
  )
  index <- which(names(plot$labels) %in% confuse_xy_labs)
  plot$labels[index] <- NULL
  return(plot)
}
#' @keywords internal
check_strip_pos <- function(plot, type) {
  if (length(plot$facet$params) > 0) {
    flagwrap <- plot$facet$params$strip.position
    if ((!is.null(flagwrap) && flagwrap %in% c("bottom", "left")) ||
        !is.null(plot$facet$params$switch)) {
      type <- switch(type,
                     first = "internallast",
                     last = "other",
                     other = "other"
      )
    }
  }
  return(type)
}
#' @keywords internal
check_theme_coflip <- function(plot, axis) {
  if (inherits(plot, "gg") && inherits(plot$coordinates, "CoordFlip")) {
    axis <- switch(axis,
                   x = "y",
                   y = "x"
    )
  }
  return(axis)
}
#' @keywords internal
check_axis_title <-
  function(plot,
           axis,
           coord_fun,
           axis.title,
           axis.sec.title) {
    axis <- switch(coord_fun,
                   coord_flip = setdiff(c("x", "y"), axis),
                   coord_cartesian = intersect(c("x", "y"), axis)
    )
    if (!is.null(axis.sec.title)) {
      if (axis == "x") {
        plot <-
          plot + ggplot2::guides(x.sec = ggplot2::guide_axis(title = axis.sec.title))
      }
      if (axis == "y") {
        plot <-
          plot + ggplot2::guides(y.sec = ggplot2::guide_axis(title = axis.sec.title))
      }
    }
    if (!is.null(axis.title)) {
      if (axis == "x") {
        plot <-
          plot + ggplot2::guides(x = ggplot2::guide_axis(title = axis.title))
      }
      if (axis == "y") {
        plot <-
          plot + ggplot2::guides(y = ggplot2::guide_axis(title = axis.title))
      }
    }
    return(plot)
  }
#' @keywords internal
check_coord_flip <- function(plot) {
  if (inherits(plot, "gg") && inherits(plot$coordinates, "CoordFlip")) {
    return("coord_flip")
  }
  return("coord_cartesian")
}
#' @keywords internal
check_legend_position <- function(plot) {
  if (!is.null(plot$theme$legend.position)) {
    tm <- theme(legend.position = plot$theme$legend.position)
  } else {
    tm <- theme()
  }
  # tm <- theme(legend.position = "none")
  return(tm)
}
#' @keywords internal
check_base_family <- function(base_family) {
  if (is_empty(base_family)) {
    base_family <- "Helvetica"
    cat_warn("font is empty, replace whith 'Helvetica'")
  }

  if (!any(names(grDevices::windowsFonts()) == base_family)) {
    addfonts <- "grDevices::windowsFont"
    addfonts <- paste(addfonts,
                      "s('",
                      base_family,
                      "' = ",
                      addfonts,
                      "('",
                      base_family,
                      "'))",
                      sep = ""
    )
    addfonts <- str2lang(addfonts)
    eval(addfonts)
  }

  base_family
}


#' @keywords internal
merge_intervals <- function(breaks, scales, ticklabs) {
  if (!inherits(breaks, "list")) {
    breaks <- list(breaks)
  }
  if (!inherits(ticklabs, "list")) {
    ticklabs <- list(ticklabs)
  }
  newbreaks <- list()
  newscales <- list()
  newticklabs <- list()
  breaks <- lapply(breaks, function(i) {
    sort(i)
  })
  ind <- order(unlist(lapply(breaks, function(i) {
    i[1]
  })))
  scales <- scales[ind]
  breaks <- breaks[ind]
  ticklabs <- ticklabs[ind]
  for (i in seq_len(length(breaks))) {
    if (length(newbreaks) >= 1 &&
        breaks[[i]][1] <= newbreaks[[length(newbreaks)]][2]) {
      newbreaks[[length(newbreaks)]][2] <-
        max(newbreaks[[length(newbreaks)]][2], breaks[[i]][2])
      mergescales <-
        c(scales[[i]], newscales[[length(newscales)]])
      mergeticks <-
        c(list(ticklabs[[i]], list(newticklabs[[length(newticklabs)]])))
      if (any("fixed" %in% mergescales)) {
        newscales[[length(newscales)]] <- "fixed"
      }
      if ((!"fixed" %in% mergescales) &&
          any("free" %in% mergescales)) {
        newscales[[length(newscales)]] <- "free"
      }
      if (is.numeric(mergescales)) {
        newscales[[length(newscales)]] <- max(mergescales)
      }
      newticklabs[[length(newticklabs)]] <-
        mergeticks[[which.max(unlist(lapply(mergeticks, function(i) {
          length(i)
        })))]]
    } else {
      newbreaks <- c(newbreaks, list(breaks[[i]]))
      newscales <- c(newscales, list(scales[[i]]))
      newticklabs <- c(newticklabs, list(ticklabs[[i]]))
    }
  }
  return(list(
    breaks = newbreaks,
    scales = unlist(newscales),
    ticklabs = newticklabs
  ))
}


#' @keywords internal
compute_ggcut_breaks_relrange <- function(ggcut_params, rngrev) {
  if (rngrev$flagrev == "reverse") {
    rngrev$axis_range <- rev(-1 * (rngrev$axis_range))
  }
  breaks <- ggcut_params$breaks
  if (rngrev$flagrev == "date") {
    breaks <- as.Date(breaks)
  }
  if (!rngrev$flagrev %in% c("identity", "reverse")) {
    breaks <- rngrev$transfun(breaks)
  }
  if (any(breaks < rngrev$axis_range[1]) ||
      any(breaks > rngrev$axis_range[2])) {
    cat_stop("Some breaks are not in the plot range. Please check all breaks!")
  }
  if (length(breaks) > 1) {
    breaks <-
      c(rngrev$axis_range[1], sort(breaks), rngrev$axis_range[2])
    breaks <-
      lapply(seq_len(length(breaks) - 1), function(i) {
        c(breaks[i], breaks[i + 1])
      })
  } else {
    breaks <- list(
      c(rngrev$axis_range[1], breaks),
      c(breaks, rngrev$axis_range[2])
    )
  }
  relrange <- rep(1, length(breaks))
  if (!is.null(ggcut_params$which) &&
      !is.null(ggcut_params$scales)) {
    relrange[ggcut_params$which] <- ggcut_params$scales
  }
  if (rngrev$flagrev == "reverse") {
    breaks <- rev(lapply(breaks, function(i) {
      rev(i)
    }))
    # relrange <# rev(relrange)
  }
  return(list(breaks = breaks, relrange = relrange))
}
#' @keywords internal
compute_relative_range <- function(breaks, scales, rng) {
  if (rng$flagrev == "reverse") {
    baserange <- abs(diff(rev(breaks)[[1]]))
    otherbk <- breaks[-length(breaks)]
  } else {
    baserange <- abs(diff(breaks[[1]]))
    otherbk <- breaks[-1]
  }
  relranges <- unlist(mapply(compute_relative_range_,
                             breaks_ = otherbk,
                             scales_ = scales,
                             MoreArgs = list(baserange_ = baserange),
                             # baserange_ = baserange,
                             SIMPLIFY = FALSE
  ))
  if (rng$flagrev == "reverse") {
    return(c(relranges, baserange))
  } else {
    return(c(baserange, relranges))
  }
}
compute_relative_range_ <- function(breaks_, scales_, baserange_) {
  .is_numeric <- function(x) {
    !anyNA(suppressall(as.numeric(x)))
  }
  if (scales_ == "fixed") {
    return(abs(diff(breaks_)))
  }
  if (scales_ == "free") {
    scales_ <- 1
  }
  if (!.is_numeric(scales_) || length(scales_) > 1) {
    cat_stop("The scales must be a numeric or one of 'fixed', 'free' !")
  }
  relrange <- baserange_ * as.numeric(scales_)
  return(relrange)
}

#' @keywords internal
convert_expand <- function(expand) {
  if (!is.numeric(expand) && !expand) {
    expand <- c(0, 0)
  }
  if (!is.numeric(expand) && expand) {
    expand <- ggplot2::waiver()
  }
  return(expand)
}


#' @keywords internal
extract_params <- function(originparam, inputparam, defaultparam) {
  if (any(defaultparam %in% names(inputparam))) {
    args <- intersect(defaultparam, names(inputparam))
    originparam <-
      c(originparam, inputparam[names(inputparam) %in% args])
  }

  return(originparam)
}
#' @keywords internal
extract_axis_break <- function(object) {
  if (inherits(object, "ggbreak_params")) {
    axis <- object$axis
    breaks <- object$breaks
    expand <- object$expand
    scales <- object$scales
    ticklabs <- object$ticklabels
    space <- object$space
  } else {
    axis <- object[[1]]$axis
    space <- object[[1]]$space
    expand <- object[[length(object)]]$expand
    breaks <- lapply(object, function(i) {
      i$breaks
    })
    scales <- lapply(object, function(i) {
      i$scales
    })
    ticklabs <- lapply(object, function(i) {
      i$ticklabels
    })
  }
  return(
    list(
      axis = axis,
      space = space,
      breaks = breaks,
      expand = expand,
      scales = scales,
      ticklabs = ticklabs
    )
  )
}
#' @keywords internal
extract_totallabs <- function(plot) {
  alllabs <- plot$labels
  totallabs <-
    alllabs[names(alllabs) %in% c("x", "y", "title", "subtitle", "caption", "tag")]
  totallabs
}
#' @keywords internal
extract_legend <- function(p, position = NULL) {
  .get_legend <- function(p, position = NULL) {
    if (is.null(p)) {
      return(NULL)
    }
    if (!is.null(position)) {
      p <- p + theme(legend.position = position)
    }
    tmp <- ggplotGrob(p)
    leg <-
      which(sapply(tmp$grobs, function(x) {
        x$name
      }) == "guide-box")
    if (length(leg) > 0) {
      leg <- tmp$grobs[[leg]]
    } else {
      leg <- NULL
    }
    leg
  }

  if (is_l(p) & !is.ggplot(p)) {
    continue <- TRUE
    i <- 1
    while (i <= length(p) & continue) {
      leg <- do_try(.get_legend(p[[i]], position = position))
      if (!is.null(leg)) {
        continue <- FALSE
      }
      i <- i + 1
    }
  } else {
    leg <- .get_legend(p, position = position)
  }
  leg
}
#' @keywords internal
extract_pallete <- function(x, aes = c("fill", "colour"), var) {
  var <- rlang::enexpr(var)
  gg_build <- ggplot2::ggplot_build(x)
  aes <- match.arg(aes)
  data_aes <- cbind(gg_build$plot$data, gg_build$data[[1]][aes])

  scale_data <-
    distinct(dplyr::select(data_aes, !!sym(aes), !!var))

  scale_vct <-
    setNames(scale_data[[aes]], scale_data[[rlang::as_label(var)]])

  switch(aes,
         fill = ggplot2::scale_fill_manual(scale_vct),
         colour = ggplot2::scale_colour_manual(scale_vct)
  )
}
#' @keywords internal
extract_strip_label <- function(facet, plot, labeller = NULL) {
  layout <- facet$compute_layout(
    list(plot$data),
    c(plot$facet$params,
      list(.possible_columns = names(plot$data)),
      plot_env = plot$plot_env
    )
  )
  label_df <- layout[names(c(
    plot$facet$params$facet,
    plot$facet$params$cols,
    plot$facet$params$rows
  ))]
  if (is.null(labeller)) {
    labels <- lapply(plot$facet$params$labeller(label_df), cbind)
  } else {
    labels <- lapply(labeller(label_df), cbind)
  }
  labels <- do.call("cbind", labels)
  labels <- unique(as.vector(labels))
  names(labels) <- labels
  return(labels)
}

#' @keywords internal
subplot_theme <- function(plot, axis, type, margin = .2, rev) {
  type <- check_strip_pos(plot = plot, type = type)
  axis <- check_theme_coflip(plot = plot, axis = axis)
  te <- switch(type,
               first = strip_theme(plot = plot, axis = axis) +
                 first_margin_theme(
                   axis = axis,
                   margin = margin,
                   rev = rev
                 ),
               other = axis_theme(plot = plot, axis = axis) +
                 strip_theme(plot, axis = axis) +
                 other_margin_theme(axis = axis, margin = margin),
               last = axis_theme(plot = plot, axis = axis) +
                 last_margin_theme(
                   axis = axis,
                   margin = margin,
                   rev = rev
                 ),
               internallast = list()
  )

  te
}

#' @keywords internal
add_expand <- function(plot, expand, axis) {
  expand <- convert_expand(expand = expand)
  var <- paste0("panel_scales_", axis)
  gb <- ggplot_build(plot)
  scales_axis_obj <- gb$layout[[var]][[1]]
  scales_axis_obj$expand <- expand
  plot <- suppressall(plot + scales_axis_obj)
  return(plot)
}

#' @keywords internal
list.add <- function(obj, ...) {
  if (inherits(obj, "ggbreak_params")) {
    c(list(obj), list(...))
  } else {
    c(obj, list(...))
  }
}

#' @keywords internal
remove_axis_title <- function(plot, axis, coord_fun, second = FALSE) {
  axis <- switch(coord_fun,
                 coord_flip = setdiff(c("x", "y"), axis),
                 coord_cartesian = intersect(c("x", "y"), axis)
  )
  if (axis == "x") {
    if (second) {
      plot <-
        plot + ggplot2::guides(x.sec = ggplot2::guide_axis(title = NULL))
    } else {
      plot <-
        plot + ggplot2::guides(x = ggplot2::guide_axis(title = NULL))
    }
  } else if (axis == "y") {
    if (second) {
      plot <-
        plot + ggplot2::guides(y.sec = ggplot2::guide_axis(title = NULL))
    } else {
      plot <-
        plot + ggplot2::guides(y = ggplot2::guide_axis(title = NULL))
    }
  }
  return(plot)
}

#' @keywords internal
set_label <- function(p, totallabs, p2 = NULL) {
  p <- p +
    do.call(labs, totallabs)

  if (is.null(p2)) {
    has_theme <- FALSE
  } else {
    has_theme <- length(p2$theme) != 0
  }

  if (has_theme) {
    x <- p2
  } else {
    x <- NULL
  }
  labs_params <- c(
    "text",
    "title",
    "axis.title",
    "axis.title.x",
    "axis.title.x.top",
    "axis.title.x.bottom",
    "axis.title.y",
    "axis.title.y.left",
    "axis.title.y.right",
    "plot.title",
    "plot.title.position",
    "plot.subtitle",
    "plot.caption",
    "plot.caption.position",
    "plot.tag",
    "plot.tag.position"
  )
  p <- p +
    fp_theme(x = x, i = labs_params) +
    theme(
      axis.text = gg_bnk,
      axis.ticks = gg_bnk
    )
  return(p)
}

#' @keywords internal
get_theme_params <- function(x, i) {
  if (!inherits(x, "theme")) {
    x <- x$theme
  }
  if (length(x) == 0) {
    x <- ggplot2::theme_get()
  }
  x[i]
}

#' @keywords internal
fp_theme <- function(x, i) {
  params <- get_theme_params(x, i)
  params <- c(params, list(complete = TRUE))
  do.call(theme, params)
}
#' @keywords internal
no_margin_theme <- function(...) {
  ggplot2::theme(plot.margin = ggplot2::margin(), ...)
}
#' @keywords internal
axis_theme <- function(plot, axis) {
  axis_theme <- switch(axis,
                       x = theme(
                         axis.text.y = gg_bnk,
                         axis.ticks.y = gg_bnk,
                         axis.line.y = gg_bnk
                       ),
                       y = theme(
                         axis.text.x = gg_bnk,
                         axis.ticks.x = gg_bnk,
                         axis.line.x = gg_bnk
                       )
  )
  return(axis_theme)
}
#' @keywords internal
strip_theme <- function(plot, axis) {
  sp_theme <- switch(axis,
                     x = theme(
                       strip.background.y = gg_bnk,
                       strip.text.y = gg_bnk
                     ),
                     y = theme(
                       strip.background.x = gg_bnk,
                       strip.text.x = gg_bnk
                     )
  )
  return(sp_theme)
}
#' @keywords internal
first_margin_theme <- function(axis, margin, rev) {
  if (rev == "reverse") {
    fmg_theme <- switch(axis,
                        x = theme(plot.margin = margin(l = margin / 2, unit = "cm")),
                        y = theme(plot.margin = margin(b = margin / 2, unit = "cm")),
    )
  } else {
    fmg_theme <- switch(axis,
                        x = theme(plot.margin = margin(r = margin / 2, unit = "cm")),
                        y = theme(plot.margin = margin(t = margin / 2, unit = "cm"))
    )
  }
  return(fmg_theme)
}
#' @keywords internal
other_margin_theme <- function(axis, margin) {
  mg_theme <- switch(axis,
                     x = theme(plot.margin = margin(
                       r = margin / 2, l = margin / 2, unit = "cm"
                     )),
                     y = theme(plot.margin = margin(
                       t = margin / 2, b = margin / 2, unit = "cm"
                     ))
  )
}
#' @keywords internal
last_margin_theme <- function(axis, margin, rev) {
  if (rev == "reverse") {
    lmg_theme <- switch(axis,
                        x = theme(plot.margin = margin(r = margin / 2, unit = "cm")),
                        y = theme(plot.margin = margin(t = margin / 2, unit = "cm"))
    )
  } else {
    lmg_theme <- switch(axis,
                        x = theme(plot.margin = margin(l = margin / 2, unit = "cm")),
                        y = theme(plot.margin = margin(b = margin / 2, unit = "cm"))
    )
  }
}

#' @keywords internal
pre_dumbbell <- function(data, x, y, sort = TRUE, top_n = NULL, limit = NULL) {
  if (!is.null(limit)) {
    suppressWarnings(fun_name <- rlang::ctxt_frame(n = 4)$fn_name)
    what <- paste0(fun_name, "(limit=)")
    with <- paste0(fun_name, "(top_n=)")
    lifecycle::deprecate_warn("0.2.0", what, with, env = parent.frame())
    top_n <- limit
  }

  if (!is.null(top_n) && !sort) {
    cat_stop("`top_n` must not be set when `sort = FALSE`.")
  }
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  if (rlang::quo_is_missing(y)) {
    data <- dplyr::count(data, !!x)

    y <- rlang::sym("n")
  }

  if (sort) {
    if (!is.null(top_n)) {
      data <- dplyr::top_n(data, top_n, !!y)
    }

    data <- dplyr::ungroup(data)
  }

  data
}
#' @keywords internal
is_gr_palette_name <- function(x) {
  is_true(x %in% metacolours$ppal_nm)
}

## grf
#' @keywords internal
aes_from_qdots <- function(qdots, mapping = aes()) {
  if (length(qdots) > 0) {
    for (i in length(qdots):1L) {
      if (rlang::is_formula(f_rhs(qdots[[i]])) &&
          length(rlang::f_rhs(qdots[[i]])) == 2L) {
        mapping[[names(qdots)[i]]] <- rlang::f_rhs(qdots[[i]])[[2]]
        qdots[[i]] <- NULL
      }
    }
  }
  list(
    mapping = do.call(aes, mapping),
    qdots = qdots
  )
}

#' @keywords internal
grf_ingredients <-
  function(formula = NULL,
           data = NULL,
           extras = list(),
           aes_form = y ~ x,
           aesthetics = aes(),
           gg_object = NULL,
           envir = NULL) {
    if (is.null(envir)) {
      if (inherits(formula, "formula")) {
        envir <- environment(formula)
      }
    }
    fs <- formula_split(formula)

    var_names <-
      if (is.null(data)) {
        if (is.null(gg_object)) {
          character(0)
        } else {
          names(gg_object$data)
        }
      } else {
        names(data)
      }

    aes_df <-
      formula_to_df(fs[["formula"]], var_names, aes_form = aes_form)

    mapped_list <- as.list(aes_df[["expr"]][aes_df$map])
    names(mapped_list) <- aes_df[["role"]][aes_df$map]
    mapped_list[mapped_list == "."] <- NULL
    more_mapped_list <-
      lapply(aesthetics, function(x) {
        deparse(rlang::get_expr(x))
      }) %>%
      stats::setNames(names(aesthetics))
    mapped_list <- modifyList(more_mapped_list, mapped_list)

    set_list <- as.list(aes_df[["expr"]][!aes_df$map])
    names(set_list) <- aes_df[["role"]][!aes_df$map]
    set_list <- modifyList(extras, set_list)

    mapping <-
      modifyList(aesthetics, do.call(aes_string, mapped_list))
    mapping <- aes_env(mapping, envir)

    mapping <- remove_dot_from_mapping(mapping)

    res <-
      list(
        data = data,
        mapping = mapping,
        setting = set_list,
        facet =
          if (is.null(fs[["condition"]])) {
            NULL
          } else {
            do.call(fs[["facet_type"]], list(facets = fs[["condition"]]))
          },
        params = modifyList(set_list, extras)
      )
    if (identical(data, NA)) {
      res$data <-
        do.call(
          data.frame,
          c(
            lapply(res[["mapping"]], rlang::get_expr),
            res[["setting"]],
            list(stringsAsFactors = FALSE)
          )
        )
      res$params[names(res$mapping)] <-
        NULL # remove mapped attributes
      aes_list <-
        as.list(intersect(names(res$data), names(res$mapping)))
      names(aes_list) <- aes_list
      res$mapping <- do.call(aes_string, aes_list)
      res$setting <- as.list(res$data)[names(res$setting)]
      res$params[names(res$setting)] <- res$setting
    }
    res
  }

#' @keywords internal
response2explanatory <- function(formula, aes_form = NULL) {
  if (!is.null(aes_form) &&
      (!any(sapply(aes_form, function(f) {
        length(f) == 2L
      })) ||
      any(sapply(aes_form, function(f) {
        length(f) == 3L
      })))) {
    return(formula)
  }

  if (length(formula) == 3L && isTRUE(formula[[3]] == 1)) {
    formula[[3]] <- formula[[2]]
    formula[[2]] <- NULL
  } else if (length(formula) == 3L &&
             length(formula[[3]]) == 3L &&
             isTRUE(formula[[3]][[1]] == as.name("|")) &&
             isTRUE(formula[[3]][[2]] == 1L)) {
    formula[[3]][[2]] <- formula[[2]]
    formula[[2]] <- NULL
  } else if (length(formula) == 3L &&
             rlang::is_formula(formula[[2]])) {
    formula[[2]] <- response2explanatory(formula[[2]])
  }
  formula
}
#' @keywords internal
first_matching_formula <-
  function(gformula,
           aes_form,
           object,
           inherit,
           inherited.aes,
           function_name) {
    fmatches <- formula_match(gformula, aes_form = aes_form)

    if (!any(fmatches)) {
      if (inherits(object, "gg") &&
          (inherit || length(inherited.aes) > 0)) {
        return(NULL)
      } else {
        cat_stop("Invalid formula type for ", function_name, ".")
      }
    } else {
      return(aes_form[[which.max(fmatches)]])
    }
  }
#' @keywords internal
remove_dot_from_mapping <- function(mapping) {
  for (item in rev(seq_along(mapping))) {
    if (identical(rlang::get_expr(mapping[[item]]), quote(.))) {
      mapping[[item]] <- NULL
    }
  }
  mapping
}
#' @keywords internal
formula_match <- function(formula, aes_form = y ~ x, value = FALSE, unmatched = NULL) {
  if (!is.list(aes_form)) {
    aes_form <- list(aes_form)
  }
  user_shape <- formula_shape(formula_split(formula)$formula)
  shapes <- lapply(aes_form, formula_shape)
  bools <- sapply(shapes, function(s) {
    identical(s, user_shape)
  })
  if (value) {
    if (any(bools)) {
      aes_form[[which.max(bools)]]
    } else {
      unmatched
    }
  } else {
    bools
  }
}
#' @keywords internal
formula_shape0 <- function(x) {
  if (length(x) < 2) {
    return(0)
  }
  arity <- length(x) - 1
  if (as.character(x[[1]]) %in% c("(")) {
    return(0)
  }
  if (as.character(x[[1]]) %in% c(":", "(")) {
    return(0) # was -1 when supporting attribute:value
  }
  if (is.name(x[[1]]) && !as.character(x[[1]]) %in% c("+", "~")) {
    return(0)
  }


  if (arity == 1L) {
    right_shape0 <- formula_shape0(x[[2]])
    arity <- arity - (right_shape0[1] < 0)
    if (arity == 0) {
      return(arity)
    }
    return(right_shape0)
  }
  if (arity == 2L) {
    right_shape0 <- formula_shape0(x[[3]])
    left_shape0 <- formula_shape0(x[[2]])
    if (left_shape0[1] < 0 && right_shape0 < 0) {
      return(0)
    }
    if (left_shape0[1] < 0) {
      if (right_shape0[1] == 1L) {
        return(right_shape0[-1])
      }
      return(right_shape0)
    }
    if (right_shape0[1] < 0) {
      if (left_shape0[1] == 1L) {
        return(left_shape0[-1])
      }
      return(left_shape0)
    }
    return(c(2, left_shape0, right_shape0))
  }
  cat_stop("Bug: problems determining formula shape (0).")

  c(length(x) - 1, unlist(sapply(x[-1], formula_shape0)))
}
#' @keywords internal
formula_shape <- function(x) {
  if (is.null(x)) {
    return(integer(0))
  }
  if (length(x) == 1L) {
    return(0L)
  }
  if (x[[1]] == as.symbol("~")) {
    return(c(
      length(x) - 1,
      formula_shape(rlang::f_lhs(x)),
      formula_shape(rlang::f_rhs(x))
    ))
  }
  if (x[[1]] == as.symbol("(")) {
    return(0L)
  }

  if (length(x) == 3 && as.character(x[[1]]) %in% c("+")) {
    return(c(
      2L,
      formula_shape(rlang::f_lhs(x)),
      formula_shape(rlang::f_rhs(x))
    ))
  }

  return(0)
}
#' @keywords internal
formula_split <- function(formula) {
  fs <-
    stringr::str_split(deparse(formula), "\\|")[[1]]
  if ((length(fs) != 2) ||
      !tryCatch(
        {
          formula_string <- fs[1]
          condition_string <- fs[2]
          if (!grepl("~", condition_string)) {
            condition_string <- paste0("~", condition_string)
            condition <-
              as.formula(condition_string, env = environment(formula))
            facet_type <- "facet_wrap"
          } else {
            condition <-
              as.formula(condition_string, env = environment(formula))
            facet_type <- "facet_grid"
          }
          formula <-
            as.formula(formula_string, env = environment(formula))
          TRUE
        },
        error = function(e) {
          cat_warn(e)
          FALSE
        }
      )) {
    condition <- NULL
    facet_type <- "none"
  }
  list(
    formula = formula,
    condition = condition,
    facet_type = facet_type
  )
}
#' @keywords internal
formula_to_df <- function(formula = NULL, data_names = character(0), aes_form = y ~ x) {
  if (is.null(formula)) {
    return(data.frame(
      role = character(0),
      expr = character(0),
      map = logical(0)
    ))
  }
  parts <- formula_slots(formula) %>%
    rapply(deparse, how = "replace") %>%
    unlist()
  aes_names <- formula_slots(aes_form) %>%
    rapply(deparse, how = "replace") %>%
    unlist()

  parts <- gsub("^\\s+|\\s+$", "", parts)

  pairs <- parts[grepl(":+", parts)]
  nonpairs <- parts[!grepl(":+", parts)]

  pairs <- parts[FALSE]
  nonpairs <- parts[TRUE]

  pair_list <- list()
  mapped_pairs <- character(0)
  for (pair in pairs) {
    this_pair <- stringr::str_split(pair, ":+", n = 2)[[1]]
    pair_list[this_pair[1]] <- this_pair[2]
    if (stringr::str_match(pair, ":+") == "::") {
      mapped_pairs <- c(mapped_pairs, this_pair[1])
    }
  }

  nonpair_list <- nonpairs
  aes_names <- setdiff(all.vars(aes_form), names(pair_list))
  names(nonpair_list) <- head(aes_names, length(nonpair_list))

  if (length(nonpair_list) > length(aes_names)) {
    cat_stop("Formula too large.  I'm looking for ",
             format(aes_form),
             call. = FALSE
    )
  }
  if (length(nonpair_list) < length(aes_names)) {
    cat_stop("Formula too small.  I'm looking for ",
             format(aes_form),
             call. = FALSE
    )
  }

  res <- c(nonpair_list, pair_list)

  res <-
    tibble::tibble(
      role = names(res),
      expr = unlist(res),
      map = unlist(res) %in% c(data_names) |
        role %in% aes_names | role %in% mapped_pairs
    )
  row.names(res) <- NULL
  res
}

#' @keywords internal
create_extras_and_dots <-
  function(args,
           formals,
           stat_formals = list(),
           geom_formals = list(),
           extras = list(),
           env) {
    extras_and_dots <- modifyList(formals, args)
    extras_and_dots[["object"]] <- NULL
    extras_and_dots <-
      extras_and_dots[!sapply(
        extras_and_dots,
        function(x) {
          is.symbol(x) &&
            identical(as.character(x), "")
        }
      )]
    for (n in setdiff(
      names(formals),
      union(
        union(
          stat_formals,
          geom_formals
        ),
        names(extras)
      )
    )) {
      extras_and_dots[[n]] <- NULL
    }

    extras_and_dots <-
      lapply(extras_and_dots, function(x) {
        if (is.symbol(x) || is.call(x)) {
          eval(x, env)
        } else {
          x
        }
      })
    extras_and_dots
  }
#' @keywords internal
grab_formals <- function(f, type = "stat") {
  if (is.character(f) && !grepl(paste0("^", type), f)) {
    return(formals(paste0(type, "_", f)))
  } else if (is.function(f)) {
    return(formals(f))
  } else {
    return(list())
  }
}

#' @keywords internal
aes_env <- function(mapping, envir) {
  for (i in one_upto(length(mapping))) {
    if (!is.null(environment(mapping[[i]]))) {
      environment(mapping[[i]]) <- envir
    }
  }
  mapping
}
#' @keywords internal
uses_stat <- function(aes) {
  e <- rlang::get_expr(aes)
  length(e) > 1 && e[[1]] == as.name("stat")
}
#' @keywords internal
add_aes <- function(mapping, new, envir = parent.frame()) {
  if (length(new) > 0L) {
    for (i in 1L:length(new)) {
      if (rlang::is_formula(new[[i]]) && length(new[[i]] == 2L)) {
        new[[i]] <- new[[i]][[2]]
      }
    }
  }
  new <- do.call(aes, new) %>% aes_env(envir)
  res <- modifyList(mapping, new)
  res
}
#' @keywords internal
one_upto <- function(n) {
  if (n > 0) {
    1:n
  } else {
    integer(0)
  }
}


#' @keywords internal
formula_slots <- function(x, stop_binops = c(":", "::")) {
  if (length(x) == 2L && deparse(x[[1]]) == "~") {
    formula_slots(x[[2]])
  } else if (length(x) == 3L && deparse(x[[1]]) == "~") {
    list(formula_slots(x[[2]]), formula_slots(x[[3]]))
  } else if (length(x) > 1 &&
             is.name(x[[1]]) &&
             !deparse(x[[1]]) %in% c("+", "|")) {
    list(x)
  } else if (length(x) == 3L && deparse(x[[1]]) %in% stop_binops) {
    list(x)
  } else if (length(x) <= 2L) {
    list(x)
  } else {
    list(formula_slots(x[[2]]), formula_slots(x[[3]]))
  }
}
#' @keywords internal
f_formula_slots <- function(x, env = parent.frame()) {
  if (is.null(x)) {
    return(x)
  }
  if (length(x) == 1L) {
    return(as_frm(x, env))
  }
  if (x[[1]] == as.symbol("~")) {
    return(list(
      f_formula_slots(rlang::f_lhs(x), env),
      f_formula_slots(rlang::f_rhs(x), env)
    ))
  }
  if (x[[1]] == as.symbol("(")) {
    res <- ~x
    res[[2]] <- x[[2]] # strip parens
    environment(res) <- env
    return(res)
  }
  if (length(x) == 2L) {
    res <- ~x
    res[[2]] <- x # leave call as is
    environment(res) <- env
    return(res)
  }
  return(list(
    f_formula_slots(rlang::f_lhs(x), env),
    f_formula_slots(rlang::f_rhs(x), env)
  ))
}

#' @keywords internal
remove_from_list <- function(x, names) {
  for (n in names) {
    x[[n]] <- NULL
  }
  x
}
#' @keywords internal
cull_list <- function(x, names) {
  x[intersect(names(x), names)]
}

#' @keywords internal
unnamed <- function(l) {
  if (is.null(names(l))) {
    l
  } else {
    l[names(l) == ""]
  }
}
#' @keywords internal
named_among <- function(l, n) {
  l[intersect(names(l), n)]
}

#' @export
ggpptx <-
  function(plot,
           path = NULL,
           left = 1.5,
           top = 2.5,
           width = grDevices::dev.size("cm")[1],
           height = grDevices::dev.size("cm")[2],
           text_size = 20) {
    plot <- plot + theme(text = element_text(size = text_size))

    if (!is_true(file.exists(path))) {
      out <- officer::read_pptx()
      if (is_empty(path)) {
        path <- tempfile(fileext = ".pptx")
      }
    } else {
      out <- officer::read_pptx(path)
    }
    out %>%
      officer::add_slide() %>%
      officer::ph_with(
        as.ggplot(as.grob(plot)),
        location = officer::ph_location(
          width = width / 2.54,
          height = height / 2.54,
          left = left / 2.54,
          top = top / 2.54
        )
      ) %>%
      base::print(target = path)
  }
#' @export
ggsave_gr <- function(object,
                      filename = tempfile(fileext = ".png"),
                      width = grDevices::dev.size("cm")[1],
                      height = grDevices::dev.size("cm")[2],
                      text_size = 20) {
  if (is_empty(object)) {
    object <- ggplot2::last_plot()
  }

  object <- object + theme(text = element_text(size = text_size))

  ggplot2::ggsave(
    filename = filename,
    plot = as.ggplot(as.grob(object)),
    width = width,
    height = height,
    units = "cm"
  )
}
