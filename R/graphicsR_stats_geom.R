#' @include graphicsR_utils.R

#' Apply func
#'
#' @name gr_graphics
#' @rdname gr_graphics
#' @keywords internal
NULL

##' @importFrom  ggplot2 Geom
##' @keywords internal
Stat <- ggplot2::Stat

##' @importFrom  ggplot2 Stat
##' @keywords internal
Geom <- ggplot2::Geom

## trees
#' @export
ggtree <- function(tr,
                   mapping = NULL,
                   layout = "rectangular",
                   open.angle = 0,
                   mrsd = NULL,
                   as.Date = FALSE,
                   yscale = "none",
                   yscale_mapping = NULL,
                   ladderize = TRUE,
                   right = FALSE,
                   branch.length = "branch.length",
                   root.position = 0,
                   xlim = NULL,
                   layout.params = list(),
                   angle = 100,
                   ...) {
   # Check if layout string is valid.
   trash <- try(
      silent = TRUE,
      expr = {
         layout %<>% match.arg(
            c(
               "rectangular",
               "circular",
               "dendrogram",
               "fan",
               "inward_circular",
               "open",
               "rotate"
            )
         )
      }
   )

   dd <- check.graph.layout(tr, trash, layout, layout.params)
   if (inherits(trash, "try-error") && !is.null(dd)) {
      layout <- "rectangular"
   }

   if (is.null(mapping)) {
      mapping <- aes_(~x, ~y)
   } else {
      mapping <- modifyList(aes_(~x, ~y), mapping)
   }

   p <- ggplot(
      tr,
      mapping       = mapping,
      layout        = layout,
      mrsd          = mrsd,
      as.Date       = as.Date,
      yscale        = yscale,
      yscale_mapping = yscale_mapping,
      ladderize     = ladderize,
      right         = right,
      branch.length = branch.length,
      root.position = root.position,
      ...
   )

   if (!is.null(dd)) {
      p$data <- dplyr::left_join(p$data %>% select(-c("x", "y")),
                                 dd,
                                 by = "node"
      )
      layout <- "equal_angle"
   }

   if (is(tr, "multiPhylo")) {
      multiPhylo <- TRUE
   } else {
      multiPhylo <- FALSE
   }

   p <- p + geom_tree(layout = layout, multiPhylo = multiPhylo, ...)


   p <- p + theme_tree()


   p <- p + tree_layout(
      layout = layout,
      angle = angle,
      xlim = xlim
   )

   class(p) <- c("ggtree", class(p))

   return(p)
}


#' @export
geom_tree <-
   function(mapping = NULL,
            data = NULL,
            layout = "rectangular",
            multiPhylo = FALSE,
            continuous = "none",
            position = "identity",
            ...) {
      if (is.logical(continuous)) {
         warning_wrap(
            'The type of "continuous" argument was changed (v>=2.5.2). Now,
                       it should be one of "color" (or "colour"), "size", "all", and "none".'
         )
         ifelse(
            continuous,
            warning_wrap(
               'It was set to TRUE, it should be replaced with "color" (or "colour"),
                              this meaning the aesthethic of "color" (or "colour") is continuous.'
            ),
            warning_wrap(
               'It was set to FALSE, it should be replaced with "none",
                              this meaning the aesthethic of "color" (or "colour") or "size" will not be continuous.'
            )
         )
         continuous <- ifelse(continuous, "color", "none")
      }
      continuous <-
         match.arg(continuous, c("color", "colour", "size", "none", "all"))
      stat_tree(
         data = data,
         mapping = mapping,
         geom = "segment",
         position = position,
         layout = layout,
         multiPhylo = multiPhylo,
         continuous = continuous,
         ...
      )
   }


#' @export
geom_tippoint <- function(mapping = NULL,
                          data = NULL,
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
   self_mapping <- aes_(node = ~node, subset = ~isTip)
   if (is.null(mapping)) {
      mapping <- self_mapping
   } else {
      if (is.null(mapping$subset)) {
         mapping <- modifyList(self_mapping, mapping)
      } else {
         mapping <- modifyList(self_mapping, mapping)
         subset_mapping <- aes_string(subset = paste0(
            as.expression(get_aes_var(
               mapping, "subset"
            )),
            "&isTip"
         ))
         mapping <- modifyList(mapping, subset_mapping)
      }
   }
   geom_point2(mapping,
               data,
               position,
               na.rm,
               show.legend,
               inherit.aes,
               stat = StatTreeData,
               ...
   )
}

#' @export
geom_nodepoint <- function(mapping = NULL,
                           data = NULL,
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
   self_mapping <- aes_(node = ~node, subset = ~ (!isTip))
   if (is.null(mapping)) {
      mapping <- self_mapping
   } else {
      if (is.null(mapping$subset)) {
         mapping <- modifyList(self_mapping, mapping)
      } else {
         mapping <- modifyList(self_mapping, mapping)
         subset_mapping <- aes_string(subset = paste0(
            as.expression(get_aes_var(
               mapping, "subset"
            )),
            "&!isTip"
         ))
         mapping <- modifyList(mapping, subset_mapping)
      }
   }
   geom_point2(mapping,
               data,
               position,
               na.rm,
               show.legend,
               inherit.aes,
               stat = StatTreeData,
               ...
   )
}

#' @export
geom_rootpoint <- function(mapping = NULL,
                           data = NULL,
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
   isTip <- node <- parent <- NULL
   self_mapping <- aes(node = node, subset = (node == parent))
   if (is.null(mapping)) {
      mapping <- self_mapping
   } else {
      if (is.null(mapping$subset)) {
         mapping <- modifyList(self_mapping, mapping)
      } else {
         mapping <- modifyList(self_mapping, mapping)
         subset_mapping <- aes_string(subset = paste0(
            as.expression(get_aes_var(
               mapping, "subset"
            )),
            "&node==parent"
         ))
         mapping <- modifyList(mapping, subset_mapping)
      }
   }
   geom_point2(mapping,
               data,
               position,
               na.rm,
               show.legend,
               inherit.aes,
               stat = StatTreeData,
               ...
   )
}

#' @export
geom_point2 <-
   function(mapping = NULL,
            data = NULL,
            stat = "identity",
            position = "identity",
            na.rm = FALSE,
            show.legend = NA,
            inherit.aes = TRUE,
            ...) {
      default_aes <- aes_() # node=~node)
      if (is.null(mapping)) {
         mapping <- default_aes
      } else {
         mapping <- modifyList(mapping, default_aes)
      }

      layer(
         data = data,
         mapping = mapping,
         stat = stat,
         geom = GeomPointGGtree,
         position = position,
         show.legend = show.legend,
         inherit.aes = inherit.aes,
         params = list(
            na.rm = na.rm,
            ...
         ),
         check.aes = FALSE
      )
   }

#' @export
geom_text2 <- function(mapping = NULL,
                       data = NULL,
                       ...,
                       stat = "identity",
                       position = "identity",
                       family = "sans",
                       parse = FALSE,
                       na.rm = TRUE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       nudge_x = 0,
                       nudge_y = 0,
                       check_overlap = FALSE) {
   if (!missing(nudge_x) || !missing(nudge_y)) {
      if (!missing(position)) {
         cat_stop("Specify either `position` or `nudge_x`/`nudge_y`")
      }
      position <- position_nudge(nudge_x, nudge_y)
   }

   default_aes <- aes_() # node=~node)
   if (is.null(mapping)) {
      mapping <- default_aes
   } else {
      mapping <- modifyList(mapping, default_aes)
   }

   layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomTextGGtree,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
         parse = parse,
         family = family,
         check_overlap = check_overlap,
         na.rm = na.rm,
         ...
      ),
      check.aes = FALSE
   )
}

#' @export
geom_tiplab2 <- function(mapping = NULL,
                         hjust = 0,
                         ...) {
   params <- list(...)
   subset1 <- "(angle < 90 | angle > 270)"
   subset2 <- "(angle >= 90 & angle <=270)"
   m1 <-
      aes_string(
         subset = subset1,
         angle = "angle",
         node = "node"
      )
   m2 <-
      aes_string(
         subset = subset2,
         angle = "angle+180",
         node = "node"
      )

   if (!is.null(mapping)) {
      if (!is.null(mapping$subset)) {
         newsubset1 <-
            paste0(as.expression(get_aes_var(mapping, "subset")), "&", subset1)
         newsubset2 <-
            paste0(as.expression(get_aes_var(mapping, "subset")), "&", subset2)
         m1 <-
            aes_string(
               angle = "angle",
               node = "node",
               subset = newsubset1
            )
         m2 <-
            aes_string(
               angle = "angle+180",
               node = "node",
               subset = newsubset2
            )
      }
      m1 <- modifyList(mapping, m1)
      m2 <- modifyList(mapping, m2)
   }
   # params[["nodelab"]] <- NULL
   params1 <- params2 <- params
   params1[["mapping"]] <- m1
   params1[["hjust"]] <- hjust
   params2[["mapping"]] <- m2
   params2[["hjust"]] <- 1 - hjust
   list(
      do.call("geom_tiplab_rectangular", params1),
      do.call("geom_tiplab_rectangular", params2)
   )
}

#' @keywords internal
geom_tiplab_circular <- geom_tiplab2

##' @export
geom_nodelab <-
   function(mapping = NULL,
            nudge_x = 0,
            nudge_y = 0,
            geom = "text",
            hjust = 0.5,
            node = "internal",
            ...) {
      p <-
         geom_tiplab(
            mapping,
            offset = nudge_x,
            nudge_y = nudge_y,
            geom = geom,
            hjust = hjust,
            ...
         )
      p$node <- match.arg(node, c("internal", "external", "all"))
      return(p)
   }


#' @export
geom_tiplab <-
   function(mapping = NULL,
            hjust = 0,
            align = FALSE,
            linetype = "dotted",
            linesize = 0.5,
            geom = "text",
            offset = 0,
            as_ylab = FALSE,
            ...) {
      structure(
         list(
            mapping = mapping,
            hjust = hjust,
            align = align,
            linetype = linetype,
            linesize = linesize,
            geom = geom,
            offset = offset,
            as_ylab = as_ylab,
            node = "external",
            ...
         ),
         class = "tiplab"
      )
   }

#' @keywords internal
geom_tiplab_as_ylab <-
   function(hjust = 0,
            position = "right",
            ...) {
      structure(list(
         hjust = hjust,
         position = position,
         ...
      ),
      class = "tiplab_ylab"
      )
   }

#' @keywords internal
geom_tiplab_rectangular <-
   function(mapping = NULL,
            hjust = 0,
            align = FALSE,
            linetype = "dotted",
            linesize = 0.5,
            geom = "text",
            offset = 0,
            # family = "", fontface = "plain",
            node = "external",
            ...) {
      params <- list(...)
      if ("nudge_x" %in% names(params)) {
         if (offset != 0) {
            warning_wrap(
               "Both nudge_x and offset arguments are provided.
                               Because they all adjust the horizontal offset of labels,
                               and the 'nudge_x' is consistent with 'ggplot2'. The
                               'offset' will be deprecated here and only the 'nudge_x' will be used."
            )
         }
         offset <- params$nudge_x
         params$nudge_x <- NULL
      }
      geom <-
         match.arg(geom, c("text", "label", "shadowtext", "image", "phylopic"))
      if (geom == "text") {
         label_geom <- geom_text2
      } else if (geom == "label") {
         label_geom <- geom_label2
      } else if (geom == "shadowtext") {
         label_geom <- get_fun_from_pkg("shadowtext", "geom_shadowtext")
      } else if (geom == "image") {
         label_geom <- get_fun_from_pkg("ggimage", "geom_image")
      } else if (geom == "phylopic") {
         label_geom <- get_fun_from_pkg("ggimage", "geom_phylopic")
      }

      nodelab <- node
      x <- y <- label <- isTip <- node <- NULL
      if (align == TRUE) {
         self_mapping <- aes(
            x = max(x, na.rm = TRUE) + diff(range(x, na.rm = TRUE)) / 200,
            y = y,
            label = label,
            node = node
         ) # , subset = isTip)
      } else {
         self_mapping <- aes(
            x = x + diff(range(x, na.rm = TRUE)) / 200,
            y = y,
            label = label,
            node = node
         ) # , subset = isTip)
      }
      subset <- switch(nodelab,
                       internal = aes_string(subset = "!isTip"),
                       external = aes_string(subset = "isTip"),
                       all = aes_string(subset = NULL)
      )
      self_mapping <- modifyList(self_mapping, subset)
      if (is.null(mapping)) {
         text_mapping <- self_mapping
      } else {
         if (!is.null(mapping$subset) && nodelab != "all") {
            newsubset <- aes_string(subset = paste0(
               as.expression(get_aes_var(mapping, "subset")),
               "&",
               as.expression(get_aes_var(subset, "subset"))
            ))
            self_mapping <- modifyList(self_mapping, newsubset)
            mapping$subset <- NULL
         }
         text_mapping <- modifyList(self_mapping, mapping)
      }
      show_segment <- FALSE
      if (align && (!is.na(linetype) && !is.null(linetype))) {
         show_segment <- TRUE
         segment_mapping <- aes(
            x = max(x, na.rm = TRUE),
            xend = x + diff(range(x, na.rm = TRUE)) / 200,
            y = y,
            yend = y,
            node = node,
            label = label,
            subset = isTip
         )
         if (!is.null(text_mapping)) {
            segment_mapping <- modifyList(segment_mapping, text_mapping)
         }
      }
      imageparams <-
         list(
            mapping = text_mapping,
            hjust = hjust,
            nudge_x = offset,
            stat = StatTreeData
         )
      imageparams <- extract_params(
         imageparams,
         params,
         c(
            "data",
            "size",
            "alpha",
            "color",
            "colour",
            "image",
            "angle",
            "position",
            "inherit.aes",
            "by",
            "show.legend",
            "image_fun",
            ".fun",
            "asp",
            "nudge_y",
            "height",
            "na.rm"
         )
      )
      labelparams <-
         list(
            mapping = text_mapping,
            hjust = hjust,
            nudge_x = offset,
            stat = StatTreeData
         )
      labelparams <- extract_params(
         labelparams,
         params,
         c(
            "data",
            "size",
            "alpha",
            "vjust",
            "color",
            "colour",
            "angle",
            "alpha",
            "family",
            "fontface",
            "lineheight",
            "fill",
            "position",
            "nudge_y",
            "show.legend",
            "check_overlap",
            "parse",
            "inherit.aes",
            "na.rm",
            "label.r",
            "label.size",
            "label.padding",
            "bg.colour",
            "bg.r"
         )
      )
      list(
         if (show_segment) {
            lineparams <-
               list(
                  mapping = segment_mapping,
                  linetype = linetype,
                  nudge_x = offset,
                  size = linesize,
                  stat = StatTreeData
               )
            lineparams <- extract_params(
               lineparams,
               params,
               c(
                  "data",
                  "color",
                  "colour",
                  "alpha",
                  "show.legend",
                  "na.rm",
                  "inherit.aes",
                  "arrow",
                  "arrow.fill",
                  "lineend"
               )
            )
            do.call("geom_segment2", lineparams)
         },
         if (geom %in% c("image", "phylopic")) {
            do.call("label_geom", imageparams)
         } else {
            do.call("label_geom", labelparams)
         }
      )
   }


##' @keywords internal
get_fun_from_pkg <- function(pkg, fun) {
   utils::getFromNamespace(fun, pkg)
}
##' @export
geom_label2 <- function(mapping = NULL,
                        data = NULL,
                        ...,
                        stat = "identity",
                        position = "identity",
                        family = "sans",
                        parse = FALSE,
                        nudge_x = 0,
                        nudge_y = 0,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        label.size = 0.25,
                        na.rm = TRUE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
   if (!missing(nudge_x) || !missing(nudge_y)) {
      if (!missing(position)) {
         cat_stop("Specify either `position` or `nudge_x`/`nudge_y`")
      }

      position <- position_nudge(nudge_x, nudge_y)
   }

   default_aes <- aes_() # node=~node)
   if (is.null(mapping)) {
      mapping <- default_aes
   } else {
      mapping <- modifyList(mapping, default_aes)
   }

   if (parse == "emoji") {
      label_aes <-
         aes_string(label = paste0(
            "suppressMessages(emoji(",
            as.list(mapping)$label,
            "))"
         ))
      mapping <- modifyList(mapping, label_aes)
      emoji <- get_fun_from_pkg("emojifont", "emoji")
      parse <- FALSE
      family <- "EmojiOne"
   }


   layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomLabelGGtree,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
         parse = parse,
         family = family,
         label.padding = label.padding,
         label.r = label.r,
         label.size = label.size,
         na.rm = na.rm,
         ...
      ),
      check.aes = FALSE
   )
}


#' @keywords internal
stat_tree <-
  function(mapping = NULL,
           data = NULL,
           geom = "segment",
           position = "identity",
           layout = "rectangular",
           multiPhylo = FALSE,
           lineend = "round",
           MAX_COUNT = 5,
           ...,
           arrow = NULL,
           rootnode = TRUE,
           show.legend = NA,
           inherit.aes = TRUE,
           na.rm = TRUE,
           check.param = TRUE,
           continuous = "none") {
    default_aes <- aes_(
      x = ~x,
      y = ~y,
      node = ~node,
      parent = ~parent
    )
    if (multiPhylo) {
      default_aes <- modifyList(default_aes, aes_(.id = ~.id))
    }

    if (is.null(mapping)) {
      mapping <- default_aes
    } else {
      mapping <- modifyList(default_aes, mapping)
    }

    if (!is.null(arrow)) {
      rootnode <- FALSE
    }

    if (layout %in% c(
      "rectangular",
      "dendrogram",
      "fan",
      "circular",
      "inward_circular"
    )) {
      list(
        layer(
          data = data,
          mapping = mapping,
          stat = StatTreeHorizontal,
          geom = geom,
          ## GeomTreeHorizontal,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
            layout = layout,
            lineend = lineend,
            na.rm = na.rm,
            arrow = arrow,
            rootnode = rootnode,
            continuous = continuous,
            ...
          ),
          check.aes = FALSE
        ),
        layer(
          data = data,
          mapping = mapping,
          stat = StatTreeVertical,
          geom = geom,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
            layout = layout,
            lineend = lineend,
            na.rm = na.rm,
            ## arrow = arrow,
            rootnode = rootnode,
            continuous = continuous,
            ...
          ),
          check.aes = FALSE
        )
      )
    } else if (layout %in% c("slanted", "radial", "equal_angle", "daylight", "ape")) {
      line.type <-
        getOption(x = "layout.radial.linetype", default = "straight")
      geom <-
        switch(line.type,
               straight = GeomSegmentGGtree,
               curved = geom
        )
      layer(
        stat = StatTree,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          layout = layout,
          lineend = lineend,
          na.rm = na.rm,
          arrow = arrow,
          rootnode = rootnode,
          continuous = continuous,
          ...
        ),
        check.aes = FALSE
      )
    } else if (layout %in% c("ellipse", "roundrect")) {
      mapping <- modifyList(mapping, aes_(isTip = ~isTip))
      layer(
        stat = StatTreeEllipse,
        data = data,
        mapping = mapping,
        geom = GeomCurvelink,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          layout = layout,
          lineend = lineend,
          na.rm = na.rm,
          arrow = arrow,
          rootnode = rootnode,
          continuous = continuous,
          ...
        ),
        check.aes = FALSE
      )
    }
  }

#' @export
stat_qqline <-
  function(mapping = NULL,
           data = NULL,
           geom = "line",
           position = "identity",
           ...,
           distribution = stats::qnorm,
           dparams = list(),
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE) {
    layer(
      stat = StatQqline,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        distribution = distribution,
        dparams = dparams,
        na.rm = na.rm,
        ...
      )
    )
  }



# Stat & Geom

#' @keywords internal
StatTreeHorizontal <- ggplot2::ggproto(
  "StatTreeHorizontal",
  Stat,
  required_aes = c("node", "parent", "x", "y"),
  compute_group = function(data, params) {
    data
  },
  compute_panel = function(self,
                           data,
                           scales,
                           params,
                           layout,
                           lineend,
                           continuous = "none",
                           rootnode = TRUE,
                           nsplit = 100,
                           extend = 0.002) {
    .fun <- function(data) {
      df <- setup_tree_data(data)
      x <- df$x
      y <- df$y
      df$xend <- x
      df$yend <- y
      ii <-
        with(df, match(parent, node))
      df$x <- x[ii]

      if (!rootnode) {
        df <-
          dplyr::filter(
            df,
            .data$node != root_node(df)$node
          )
      }
      if (continuous != "none") {
        # using ggnewscale new_scale("color") for multiple color scales
        if (length(grep("colour_new", names(df))) ==
            1 && !"colour" %in% names(df)) {
          names(df)[grep("colour_new", names(df))] <- "colour"
        }
        if (!is.null(df$colour)) {
          if (any(is.na(df$colour))) {
            df$colour[is.na(df$colour)] <- 0
          }
          df$col2 <- df$colour
          df$col <- df$col2[ii]
        }
        # using ggnewscale new_scale("size") for multiple size scales
        if (length(grep("size_new", names(df))) ==
            1 && !"size" %in% names(df)) {
          names(df)[grep("size_new", names(df))] <- "size"
        }
        if (!is.null(df$size)) {
          if (any(is.na(df$size))) {
            df$size[is.na(df$size)] <- 0
          }
          df$size2 <- df$size
          df$size1 <- df$size2[ii]
        }
        setup_data_continuous_color_size_tree(df,
                                              nsplit = nsplit,
                                              extend = extend,
                                              continuous = continuous
        )
      } else {
        return(df)
      }
    }
    if (".id" %in% names(data)) {
      ldf <- split(data, data$.id)
      df <-
        do.call(rbind, lapply(ldf, .fun))
    } else {
      df <- .fun(data)
    }
    # using ggnewscale new_scale for multiple color or size scales
    if (length(grep("colour_new", names(data))) ==
        1 && continuous != "none") {
      names(df)[match("colour", names(df))] <-
        names(data)[grep("colour_new", names(data))]
    }
    if (length(grep("size_new", names(data))) ==
        1 && continuous != "none") {
      names(df)[match("size", names(df))] <-
        names(data)[grep("size_new", names(data))]
    }
    return(df)
  }
)

#' @keywords internal
StatTreeVertical <- ggplot2::ggproto(
  "StatTreeVertical",
  Stat,
  required_aes = c("node", "parent", "x", "y"),
  compute_group = function(data, params) {
    data
  },
  compute_panel = function(self,
                           data,
                           scales,
                           params,
                           layout,
                           lineend,
                           continuous = "none",
                           nsplit = 100,
                           extend = 0.002,
                           rootnode = TRUE) {
    .fun <- function(data) {
      df <- setup_tree_data(data)
      x <- df$x
      y <- df$y
      ii <-
        with(df, match(parent, node))
      df$x <- x[ii]
      df$y <- y[ii]
      df$xend <- x[ii]
      df$yend <- y

      if (!rootnode) {
        df <- dplyr::filter(df, .data$node != root_node(df)$node)
      }

      if (continuous != "none") {
        # using ggnewscale new_scale("color") for multiple color scales
        if (length(grep("colour_new", names(df))) ==
            1 && !"colour" %in% names(df)) {
          names(df)[grep("colour_new", names(df))] <- "colour"
        }
        if (!is.null(df$colour)) {
          if (any(is.na(df$colour))) {
            df$colour[is.na(df$colour)] <- 0
          }
          df$colour <- df$colour[ii]
        }
        # using ggnewscale new_scale("size") for multiple size scales
        if (length(grep("size_new", names(df))) ==
            1 && !"size" %in% names(df)) {
          names(df)[grep("size_new", names(df))] <- "size"
        }
        if (!is.null(df$size)) {
          if (any(is.na(df$size))) {
            df$size[is.na(df$size)] <- 0
          }
          df$size <- df$size[ii]
        }
      }
      return(df)
    }

    if (".id" %in% names(data)) {
      ldf <- split(data, data$.id)
      df <-
        do.call(rbind, lapply(ldf, .fun))
    } else {
      df <- .fun(data)
    }

    # using ggnewscale new_scale for multiple color or size scales
    if (length(grep("colour_new", names(data))) ==
        1 && continuous != "none") {
      names(df)[match("colour", names(df))] <-
        names(data)[grep("colour_new", names(data))]
    }
    if (length(grep("size_new", names(data))) ==
        1 && continuous != "none") {
      names(df)[match("size", names(df))] <-
        names(data)[grep("size_new", names(data))]
    }
    return(df)
  }
)

#' @keywords internal
StatTree <- ggplot2::ggproto(
  "StatTree",
  Stat,
  required_aes = c("node", "parent", "x", "y"),
  compute_group = function(data, params) {
    data
  },
  compute_panel = function(self,
                           data,
                           scales,
                           params,
                           layout,
                           lineend,
                           continuous = "none",
                           nsplit = 100,
                           extend = 0.002,
                           rootnode = TRUE) {
    .fun <- function(data) {
      df <- setup_tree_data(data)
      x <- df$x
      y <- df$y
      ii <- with(df, match(parent, node))
      df$x <- x[ii]
      df$y <- y[ii]
      df$xend <- x
      df$yend <- y

      if (!rootnode) {
        df <- dplyr::filter(df, .data$node != root_node(df)$node)
      }
      if (continuous != "none") {
        # using ggnewscale new_scale("color") for multiple color scales
        if (length(grep("colour_new", names(df))) ==
            1 && !"colour" %in% names(df)) {
          names(df)[grep("colour_new", names(df))] <- "colour"
        }
        if (!is.null(df$colour)) {
          if (any(is.na(df$colour))) {
            df$colour[is.na(df$colour)] <- 0
          }
          df$col2 <- df$colour
          df$col <- df$col2[ii]
        }
        # using ggnewscale new_scale("size") for multiple size scales
        if (length(grep("size_new", names(df))) == 1 &&
            !"size" %in% names(df)) {
          names(df)[grep("size_new", names(df))] <- "size"
        }
        if (!is.null(df$size)) {
          if (any(is.na(df$size))) {
            df$size[is.na(df$size)] <- 0
          }
          df$size2 <- df$size
          df$size1 <- df$size2[ii]
        }
        setup_data_continuous_color_size_tree(df,
                                              nsplit = nsplit,
                                              extend = extend,
                                              continuous = continuous
        )
      } else {
        return(df)
      }
    }
    if (".id" %in% names(data)) {
      ldf <- split(data, data$.id)
      df <- do.call(rbind, lapply(ldf, .fun))
    } else {
      df <- .fun(data)
    }

    # using ggnewscale new_scale for multiple color or size scales
    if (length(grep("colour_new", names(data))) == 1 &&
        continuous != "none") {
      names(df)[match("colour", names(df))] <-
        names(data)[grep("colour_new", names(data))]
    }
    if (length(grep("size_new", names(data))) == 1 &&
        continuous != "none") {
      names(df)[match("size", names(df))] <-
        names(data)[grep("size_new", names(data))]
    }

    return(df)
  }
)

#' @keywords internal
StatTreeEllipse <- ggplot2::ggproto(
  "StatTreeEllipse",
  Stat,
  required_aes = c("node", "parent", "x", "y", "isTip"),
  compute_group = function(data, params) {
    data
  },
  compute_panel = function(self,
                           data,
                           scales,
                           params,
                           layout,
                           lineend,
                           continuous = "none",
                           nsplit = 100,
                           extend = 0.002,
                           rootnode = TRUE) {
    if (continuous != "none") {
      cat_stop("continuous colour or size are not implemented for roundrect or ellipse layout")
    }
    df <-
      StatTree$compute_panel(
        data = data,
        scales = scales,
        params = params,
        layout = layout,
        lineend = lineend,
        continuous = continuous,
        nsplit = nsplit,
        extend = extend,
        rootnode = rootnode
      )
    df <-
      df[!(df$x == df$xend & df$y == df$yend), ]
    reverseflag <- check_reverse(df)
    if (layout == "ellipse") {
      if (reverseflag) {
        df$curvature <- ifelse(df$y > df$yend, -1, 1) * 0.5
      } else {
        df$curvature <- ifelse(df$y > df$yend, 1, -1) * 0.5
      }
      df$curveangle <-
        ifelse(df$y > df$yend, 20, 160)
    } else if (layout == "roundrect") {
      if (reverseflag) {
        df$curvature <- ifelse(df$y > df$yend, -1, 1)
      } else {
        df$curvature <- ifelse(df$y > df$yend, 1, -1)
      }
      df$curveangle <- 90
    }
    df$square <- TRUE
    return(df)
  }
)

#' @export
StatQqline <- ggplot2::ggproto(
  "StatQqline",
  Stat,
  required_aes = c("sample"),
  compute_group = function(data,
                           scales,
                           distribution = stats::qnorm,
                           dparams = list(),
                           tail = 0.25,
                           na.rm = FALSE) {
    qdist <- function(p) {
      do.call(distribution, c(list(p = p), dparams))
    }

    n <- length(data$sample)
    theoretical <- qdist(stats::ppoints(n))
    qq <-
      qq.line(data$sample,
              qdist = qdist,
              tail = tail,
              na.rm = na.rm
      )

    data.frame(
      x = theoretical,
      y = qq$intercept + theoretical * qq$slope
    )
  }
)

#' @keywords internal
qq.line <- function(sample,
                    qdist,
                    na.rm = TRUE,
                    tail = 0.25) {
  q.sample <-
    stats::quantile(sample, c(tail, 1 - tail), na.rm = na.rm)
  q.theory <- qdist(c(tail, 1 - tail))
  slope <- diff(q.sample) / diff(q.theory)
  intercept <- q.sample[1] - slope * q.theory[1]
  list(slope = slope, intercept = intercept)
}


#' @keywords internal
GeomPointGGtree <- ggplot2::ggproto(
  "GeomPointGGtree",
  ggplot2::GeomPoint,
  setup_data = function(data, params) {
    if (is.null(data$subset)) {
      return(data)
    }
    data[which(data$subset), ]
  }
)

#' @keywords internal
StatTreeData <- ggplot2::ggproto(
  "StatTreeLabel",
  Stat,
  required_aes = "node",
  compute_group = function(data, scales) {
    setup_tree_data(data)
  }
)

#' @export
label_pad <-
  function(label,
           justify = "right",
           pad = "\u00B7") {
    x <- format(label,
                width = max(nchar(label)),
                justify = justify
    )
    len <- vapply(gregexpr("^\\s+", x),
                  attr, "match.length",
                  FUN.VALUE = numeric(1)
    )
    len[len < 0] <- 0

    y <- vapply(len,
                function(i) {
                  paste0(rep(pad, each = i), collapse = "")
                },
                FUN.VALUE = character(1)
    )
    paste0(y, label)
  }

#' @keywords internal
GeomLabelGGtree <- ggplot2::ggproto(
  "GeomLabelGGtree",
  ggplot2::GeomLabel,
  setup_data = function(data, params) {
    if (is.null(data$subset)) {
      return(data)
    }
    data[which(data$subset), ]
  }
)

#' @keywords internal
GeomTextGGtree <- ggplot2::ggproto(
  "GeomTextGGtree",
  ggplot2::GeomText,
  setup_data = function(data, params) {
    if (is.null(data$subset)) {
      return(data)
    }
    data[which(data$subset), ]
  },
  draw_panel = function(data,
                        panel_scales,
                        coord,
                        parse = FALSE,
                        na.rm = TRUE,
                        check_overlap = FALSE) {
    ggplot2::GeomText$draw_panel(
      data, panel_scales, coord, parse,
      na.rm, check_overlap
    )
  },
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    colour = "black",
    size = 3.88,
    angle = 0,
    hjust = 0.5,
    vjust = 0.5,
    alpha = NA,
    family = "",
    fontface = 1,
    lineheight = 1.2
  ),
  draw_key = ggplot2::draw_key_text
)
