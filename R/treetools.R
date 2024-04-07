# xtree

#' fast tree implementation
#'
#' @name treetools
#' @rdname treetools
#' @export
#'
#' @examples
#'
#'
#' set.seed(7)
#' data <- datalearn$penguins_lite %>% tidyr::drop_na()
#' xt_obj <- xtree(data = data, formula = species ~ .)
#' predict(xt_obj, data)
xtree <- function(data, ...) {
  UseMethod("xtree")
}

#' @export
xtree.default <- function(data, ...) {
  generic_default()
}

#' @export
xtree.data.frame <- function(data, formula, maxdepth = 3) {
  out <-
    rpart::rpart(
      formula = formula,
      data = data,
      control = rpart::rpart.control(maxdepth = maxdepth)
    )


  formula <-
    as.formula(model.frame(formula = formula, data = data), env = list2env(as.list(data)))

  out <- rpart_fn(out)

  class(out) <- "xtree"
  attr(out, "outcome") <- frm_lhs(formula)
  attr(out, "type") <- set_mode_model(formula)
  out
}
#' @export
print.xtree <- function(x) {
  cat_title_head("Tree casewhen")
  cat(cli::col_silver("outcome : ", attr(x, "outcome")))
  cat("\n\n")
  if (attr(x, "type")[1] == "classification") {
    for (i in seq_along(x)) {
      cat_subtitle2("")
      cat(cli::col_blue(paste0(deparse(x[[i]]), collapse = "\n")))
      cat("\n")
    }
  } else if (attr(x, "type")[1] == "regression") {
    cat(cli::col_blue(paste0(deparse(x[[1]]), collapse = "\n")))
  }
}
#' @export
predict.xtree <- function(object, new_data = NULL) {
  if (is_empty(new_data)) {
    .data <- dplyr::cur_data_all()
    .env <- parent.frame()
    if (attr(object, "type")[1] == "classification") {
      I(split(
        purrr::map_dfr(object, ~ rlang::eval_tidy(.x, .data, .env)),
        1:nrow(.data)
      ))
    } else if (attr(object, "type")[1] == "regression") {
      purrr::simplify(lapply(seq_len(nrow(.data)), function(i) {
        rlang::eval_tidy(object[[1]],
                         data = lapply(.data[i, ], `[[`, 1),
                         env = .env
        )
      }))
    }
  } else {
    if (attr(object, "type")[1] == "classification") {
      purrr::map_dfr(object, ~ rlang::eval_tidy(.x, new_data))
    } else if (attr(object, "type")[1] == "regression") {
      rlang::eval_tidy(object[[1]], new_data)
    }
  }
}

#' fast forest implementation
#'
#' @name treetools
#' @rdname treetools
#' @export
#'
#' @examples
#'
#'
#' set.seed(7)
#' data <- datalearn$penguins_lite %>% tidyr::drop_na()
#' xf_obj <- xforest(data = data, formula = species ~ ., trees = 25)
#' predict(xf_obj, data)
xforest <- function(data, ...) {
  UseMethod("xforest")
}

#' @export
xforest.default <- function(data, ...) {
  generic_default()
}

#' @export
xforest.data.frame <-
  function(data,
           formula,
           maxdepth = 3,
           trees = 50) {
    forest_call <- call_match()
    forest_call$data <- quote(.x)
    forest_call$trees <- NULL
    forest_call[[1]] <- quote(xtree)

    data <- model.frame(formula = formula, data = data)

    formula <- DF2formula(x = object <- data)

    res_data <-
      do_call(
        "bootsplits",
        c(
          dataset = list(data),
          times = trees,
          outcome = frm_lhs(formula)
        )
      )
    res_data <- map(res_data$splits, rsample::analysis)
    nc <- ncol(data)
    data_nm <-
      map(seq_len(trees), ~ c(names(data)[1], sample(names(data)[-1], min(c(
        nc - 1, ncase(nc)
      )))))
    res_data <- map2(res_data, data_nm, ~ .x[, .y])
    out <- map(res_data, ~ eval(forest_call))
    class(out) <- "xforest"
    attr(out, "outcome") <- frm_lhs(formula)
    attr(out, "type") <- set_mode_model(formula)
    out
  }




#' @export
print.xforest <- function(x) {
  cat_title_head("Random casewhen")
  cat(cli::col_silver("outcome : ", attr(x, "outcome"), "\n"))
  cat(cli::col_silver("n trees : ", length(x)))
  cat("\n\n")

  invisible()
}
#' @export
predict.xforest <- function(object, new_data = NULL) {
  res <- map(object, ~ predict(.x, new_data))
  if (attr(object, "type")[1] == "classification") {
    bind_rows(res, .id = ".id") %>%
      group_by(.id) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      ungroup() %>%
      select(!.id)
  } else if (attr(object, "type")[1] == "regression") {
    rowMeans(do_call("cbind", res), na.rm = T)
  }
}

# rpart_fn

#' rpart to case_when
#'
#' @name treetools
#' @rdname treetools
#' @export
#'
#' @examples
#'
#' set.seed(7)
#' data <- datalearn$penguins_lite %>% tidyr::drop_na()
#'
#' rpart_fit <- rpart::rpart(formula = species ~ ., data = data)
#' rpart_fn(rpart_fit)
rpart_fn <- function(x) {
  frame <- x$frame
  ylevel <- attr(x, "ylevels")
  node <- as_num(row.names(frame))
  labs <- leaf_label(x)
  labs$depth <- tree_depth(node)
  labs <- labs[labs$depth != 0, ]
  labs$row <- seq_len(nrow(labs))
  max_d <- max(labs$depth)
  if (max_d > 1) {
    for (i in 2:max_d) {
      tmp <- labs[labs$depth <= i & labs$depth >= i - 1, ]
      tmp$parent <- NA_character_
      for (j in seq_len(nrow(tmp))) {
        if (tmp$depth[j] == i - 1) {
          tmp$parent[j] <- tmp$labels[j]
        } else {
          tmp$parent[j] <- tmp$parent[j - 1]
        }
      }
      tmp <- tmp[tmp$depth == i, ]

      tmp$labels <- paste(tmp$labels, "&", tmp$parent)

      labs$labels[tmp$row] <- tmp$labels
    }
  }
  is_leaf <- frame$var == "<leaf>"

  labs <- labs[is_leaf[-1], ]

  cond <- labs$labels

  yval <- labs[, 1:(which(names(labs) == "labels") - 1)]


  out <- if (!is.null(x$functions$print)) {
    tmpcond <-
      lapply(ylevel, function(x) {
        paste(paste(cond, "~", yval[, x]), collapse = ",\n ")
      })

    names(tmpcond) <- ylevel
    lapply(tmpcond, function(x) {
      str2lang(paste0("case_when(", x, ")"))
    })
  } else {
    tmpcond <- paste(paste(cond, "~", yval), collapse = ",\n ")
    list(str2lang(paste0("case_when(", tmpcond, ")")))
  }
  out
}

#' @keywords internal
tree_depth <- function(nodes) {
  depth <- floor(log(nodes, base = 2) + 1e-7)
  depth - min(depth)
}
#' @keywords internal
tfun_probs <- function(yval, ylevel) {
  nclass <- (ncol(yval) - 2L) / 2L
  yprob <- if (nclass < 5L) {
    format(yval[, 1L + nclass + 1L:nclass],
           digits = 3,
           nsmall = 3
    )
  } else {
    codenum_format(yval[, 1L + nclass + 1L:nclass])
  }
  colnames(yprob) <- ylevel
  yprob
}
#' @keywords internal
tfun_class <- function(yval, ylevel) {
  yprob <- rep(character(length(yval)), length(ylevel))
  yprob <- matrix(yprob, ncol = length(ylevel))
  colnames(yprob) <- ylevel
  for (i in seq_along(ylevel)) {
    yprob[, i] <- ifelse(yval == i, "1", "0")
  }
  yprob
}
#' @keywords internal
leaf_label <- function(object) {
  frame <- object$frame
  n <- nrow(frame)
  if (n == 1L) {
    return("root")
  }

  is_leaf <- (frame$var == "<leaf>")
  whichrow <- !is_leaf
  vnames <-
    frame$var[whichrow]

  index <-
    cumsum(c(1, frame$ncompete + frame$nsurrogate + !is_leaf))
  irow <-
    index[c(whichrow, FALSE)] # we only care about the primary split
  ncat <- object$splits[irow, 2L]

  lsplit <- rsplit <- character(length(irow))

  if (any(ncat < 2L)) {
    jrow <- irow[ncat < 2L]
    cutpoint <- codenum_format(object$splits[jrow, 4L])
    temp1 <- (ifelse(ncat < 0, "< ", ">="))[ncat < 2L]
    temp2 <- (ifelse(ncat < 0, ">=", "< "))[ncat < 2L]
    lsplit[ncat < 2L] <- paste(names(temp1), temp1, cutpoint)
    rsplit[ncat < 2L] <- paste(names(temp2), temp2, cutpoint)
  }

  if (any(ncat > 1L)) {
    xlevels <- attr(object, "xlevels")
    jrow <- seq_along(ncat)[ncat > 1L]
    crow <-
      object$splits[irow[ncat > 1L], 4L] # row number in csplit
    cindex <- (match(vnames, names(xlevels)))[ncat > 1L]

    for (i in seq_along(jrow)) {
      j <- jrow[i]
      splits <- object$csplit[crow[i], ]
      jnames <- names(xlevels[cindex[i]])
      lsplit[j] <-
        paste((xlevels[[cindex[i]]])[splits == 1L], collapse = "','")
      rsplit[j] <-
        paste((xlevels[[cindex[i]]])[splits == 3L], collapse = "','")
      lsplit[j] <-
        paste0(jnames, " %in% c('", lsplit[j], "')")
      rsplit[j] <-
        paste0(jnames, " %in% c('", rsplit[j], "')")
    }
  }

  node <- as_num(row.names(frame))
  parent <- match(node %/% 2L, node[whichrow])
  odd <- (as_logical(node %% 2L))

  labels <- character(n)
  labels[odd] <- rsplit[parent[odd]]
  labels[!odd] <- lsplit[parent[!odd]]
  labels[1L] <- ""
  parent[1L] <- 1

  ylevel <- attr(object, "ylevels")
  labels <- data.frame(labels = labels)
  yval <- if (!is.null(object$functions$print)) {
    if (is.null(frame$yval2)) {
      as_df(tfun_class(frame$yval, ylevel))
    } else {
      as_df(tfun_probs(frame$yval2, ylevel))
    }
  } else {
    data.frame(value = codenum_format(frame$yval))
  }

  cbind(yval, labels)
}

# misc



# tremanipulation

##' @importFrom ape as.phylo
##' @export
ape::as.phylo

##' @method as.phylo hclust
##' @export
as.phylo.hclust <- function(x, ...) {
  N <- dim(x$merge)[1]
  edge <- matrix(0L, 2 * N, 2)
  edge.length <- numeric(2 * N)
  # `node' gives the number of the node for the i#th row of x$merge
  node <- integer(N)
  node[N] <- N + 2L
  cur.nod <- N + 3L
  j <- 1L
  for (i in N:1) {
    edge[j:(j + 1), 1] <- node[i]
    for (l in 1:2) {
      k <- j + l - 1L
      y <- x$merge[i, l]
      if (y > 0) {
        edge[k, 2] <- node[y] <- cur.nod
        cur.nod <- cur.nod + 1L
        edge.length[k] <- x$height[i] - x$height[y]
      } else {
        edge[k, 2] <- -y
        edge.length[k] <- x$height[i]
      }
    }
    j <- j + 2L
  }
  if (is.null(x$labels)) {
    x$labels <- as.character(1:(N + 1))
  }
  obj <- list(
    edge = edge,
    edge.length = edge.length / 2,
    tip.label = x$labels,
    Nnode = N
  )
  class(obj) <- "phylo"
  reorder(obj)
}

##' @method as.phylo phylog
##' @export
as.phylo.phylog <- function(x, ...) {
  tr <- ape::read.tree(text = x$tre)
  n <- length(tr$tip.label)
  edge.length <- numeric(dim(tr$edge)[1])
  term <- which(tr$edge[, 2] <= n)
  inte <- which(tr$edge[, 2] > n)
  edge.length[term] <- x$leaves[tr$tip.label]
  edge.length[inte] <- x$nodes[tr$node.label][-1]
  tr$edge.length <- edge.length
  if (x$nodes["Root"] != 0) {
    tr$edge.root <- x$nodes["Root"]
    names(tr$edge.root) <- NULL
  }
  tr
}

##' @method as.phylo tbl_df
##' @export
as.phylo.tbl_df <- function(x, length, ...) {
  x <- as_tibble(x) %>% mutate_if(is.factor, as.character)

  edge.length <- NULL
  length_var <- quo_name(enexpr(length))

  if (length_var != "") {
    edge.length <- as.numeric(x[[length_var]])
  } else {
    length_var <- NULL
  }

  edge <- check_edgelist(x)
  indx <- attr(edge, "indx")
  if (!is.null(indx) && !is.null(edge.length)) {
    edge.length <- edge.length[indx]
    attr(edge, "indx") <- NULL
  }
  phylo <- ape::read.tree(
    text = .write.tree4(edge,
                        id_as_label = TRUE,
                        edge.length = edge.length
    ),
    ...
  )

  attr(phylo, "length_var") <- length_var
  return(phylo)
}

##' @method as.phylo data.frame
##' @export
as.phylo.data.frame <- as.phylo.tbl_df

##' @method as.phylo matrix
##' @export
as.phylo.matrix <- as.phylo.tbl_df

##' @method as.phylo phylo4
##' @export
as.phylo.phylo4 <- function(x, ...) {
  edge <- x@edge
  edge.filter <- edge[, 1] != 0
  edge <- edge[edge.filter, ]
  edge.length <- x@edge.length
  edge.length <- edge.length[edge.filter]
  tip.id <- sort(setdiff(edge[, 2], edge[, 1]))
  tip.label <- x@label[tip.id]
  phylo <- list(
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

##' @method as.phylo pvclust
##' @export
as.phylo.pvclust <- function(x, ...) {
  as.phylo(x$hclust, ...)
}

##' @method as.phylo ggtree
##' @export
as.phylo.ggtree <- function(x, ...) {
  d <- as_tibble(x$data)
  class(d) <- c("tbl_tree", "tbl_df", "tbl", "data.frame")
  as.phylo(d)
}

##' @method as.phylo igraph
##' @export
as.phylo.igraph <- function(x, ...) {
  edge <- igraph::get.edgelist(x)
  as.phylo(edge)
}

##' @method as.phylo formula
##' @export
as.phylo.formula <- function(x,
                             data = parent.frame(),
                             collapse = TRUE,
                             ...) {
  # Testing formula syntax:
  err <- "Formula must be of the kind ~A1/A2/.../An."
  if (length(x) != 2) {
    cat_stop(err)
  }
  if (x[[1]] != "~") {
    cat_stop(err)
  }
  f <- x[[2]]
  taxo <- list()
  while (length(f) == 3) {
    if (f[[1]] != "/") {
      cat_stop(err)
    }
    f3.txt <- deparse(f[[3]])
    if (!is.factor(data[[f3.txt]])) {
      cat_stop(paste("Variable", f3.txt, "must be a factor"))
    }
    taxo[[f3.txt]] <- data[[f3.txt]]
    if (length(f) > 1) {
      f <- f[[2]]
    }
  }
  f.txt <- deparse(f)
  if (!is.factor(data[[f.txt]])) {
    cat_stop(paste("Variable", f.txt, "must be a factor."))
  }
  taxo[[f.txt]] <- data[[f.txt]]
  taxo.data <- as.data.frame(taxo)
  leaves.names <- as.character(taxo.data[, 1])
  taxo.data[, 1] <- seq_row(taxo.data)
  # Now builds the phylogeny:
  f.rec <- function(subtaxo) {
    # Recurrent utility function
    u <- ncol(subtaxo)
    levels <- unique(subtaxo[, u])
    if (u == 1) {
      if (length(levels) != nrow(subtaxo)) {
       cat_warn("leaves names are not unique.")
      }
      return(as.character(subtaxo[, 1]))
    }
    t <- character(length(levels))
    for (l in seq_along(levels)) {
      x <- f.rec(subtaxo[subtaxo[, u] == levels[l], ][1:(u - 1)])
      t[l] <- paste0(
        "(", paste(x, collapse = ","), ")",
        as.character(levels[l])
      )
    }
    t
  }
  string <-
    paste0("(", paste(f.rec(taxo.data), collapse = ","), ");")
  phy <- ape::read.tree(text = string)
  if (collapse) {
    phy <- collapse.singles(phy)
  }
  phy$tip.label <- leaves.names[as.numeric(phy$tip.label)]
  phy
}

##' @method as.phylo Node
##' @export
as.phylo.Node <-
 utils::getFromNamespace(x = "as.phylo.Node", ns = "data.tree")

##' @method as.phylo igraph
##' @export
as.phylo.party <- function(x, ...) {
  as.Node.party <-
   utils::getFromNamespace(x = "as.Node.party", ns = "data.tree")
  as.phylo.Node(as.Node.party(x))
}

##' @method as.phylo igraph
##' @export
as.phylo.dendrogram <- function(x, ...) {
  as.Node.dendrogram <-
   utils::getFromNamespace(x = "as.Node.dendrogram", ns = "data.tree")
  as.phylo.Node(as.Node.dendrogram(x))
}

##' @method as.phylo igraph
##' @export
as.phylo.rpart <- function(x, ...) {
  as.Node.rpart <-
   utils::getFromNamespace(x = "as.Node.rpart", ns = "data.tree")
  as.phylo.Node(as.Node.rpart(x))
}







#' @keywords internal
old2new.phylo <- function(phy) {
  mode(phy$edge) <- "numeric"
  phy$Nnode <- -min(phy$edge)
  n <- length(phy$tip.label)
  NODES <- phy$edge < 0
  phy$edge[NODES] <- n - phy$edge[NODES]
  phy
}
#' @keywords internal
new2old.phylo <- function(phy) {
  NTIP <- length(phy$tip.label)
  NODES <- phy$edge > NTIP
  phy$edge[NODES] <- NTIP - phy$edge[NODES]
  mode(phy$edge) <- "character"
  phy$Nnode <- NULL
  phy
}


##' @export
get.tree <- function(x, ...) {
  as.phylo(x, ...)
}



##' @export
root_node <- function(.data, ...) {
  UseMethod("root_node")
}

##' @export
root_node.tbl_tree <- function(.data, ...) {
  valid_tbl_tree(.data)
  .data[.data$parent == .data$node, ]
}

##' @export
root_node.phylo <- function(.data, ...) {
  edge <- .data[["edge"]]
  if (!is.null(attr(.data, "order")) &&
      attr(.data, "order") == "postorder") {
    return(edge[nrow(edge), 1])
  }

  parent <- unique(edge[, 1])
  child <- unique(edge[, 2])
  root <- parent[!parent %in% child]
  if (length(root) > 1) {
    cat_stop("multiple roots found...")
  }
  return(root)
}

##' @export
root_node.treedata <- function(.data, ...) {
  root_node.phylo(as.phylo(.data), ...)
}

##' @export
root_node.data.frame <- function(.data, node) {
  root <- which(is.na(.data$parent))
  if (length(root) == 0) {
    root <- .data$node[.data$parent == .data$node]
  }
  return(root)
}




##' @export
nodelab <- function(tree, id) {
  UseMethod("nodelab")
}
##' @export
nodelab.tbl_tree <- function(tree, id) {
  tree$label[match(id, tree$node)]
}

##' @export
nodelab.ggtree <- function(tree, id) {
  nodelab(tree$data, id)
}

##' @export
nodelab.phylo <- function(tree, id) {
  nodelab(as_tibble(tree), id)
}

##' @export
nodelab.treedata <- function(tree, id) {
  nodelab(as.phylo(tree), id)
}




##' @export
nodeid <- function(tree, label) {
  UseMethod("nodeid")
}

##' @export
nodeid.tbl_tree <- function(tree, label) {
  tree$node[match(label, tree$label)]
}

##' @export
nodeid.ggtree <- function(tree, label) {
  nodeid(tree$data, label)
}

##' @export
nodeid.phylo <- function(tree, label) {
  ## nodeid(as_tibble(tree), label)
  lab <- c(tree$tip.label, tree$node.label)
  match(label, lab)
}

##' @export
nodeid.treedata <- function(tree, label) {
  nodeid(as.phylo(tree), label)
}

##' @method left_join treedata
##' @export
left_join.treedata <- function(x, y, by = NULL, copy = FALSE, ...) {
  dots <- rlang::quos(...)
  suffix <- c("", ".y")
  if ("suffix" %in% names(dots)) {
    dots <- dots[names(dots) != "suffix"]
  }

  dat <- .extract_annotda.treedata(x)
  ornm <- colnames(dat)
  da <- dat %>%
    dplyr::left_join(y,
                     by = by,
                     copy = copy,
                     suffix = suffix,
                     !!!dots
    )

  if (any(duplicated(da$node))) {
    da %<>% .internal_nest(keepnm = ornm)
  }

  tr <- .update.td.join(td = x, da = da)
  return(tr)
}

##' @method left_join phylo
##' @export
left_join.phylo <- function(x, y, by = NULL, copy = FALSE, ...) {
  x <- treedata(phylo = x)
  tr <- x %>% left_join(y, by = by, copy = copy, ...)
  return(tr)
}

.update.td.join <- function(td, da) {
  aa <- names(attributes(td@phylo))
  aa <-
    aa[!aa %in% c("names", "class", "order", "reroot", "node_map")]
  data.nm <- get_fields_treedata(td)
  if (length(data.nm) == 1 && data.nm == "") {
    td@data <- tibble()
  } else {
    td@data <- da %>% select(c("node", data.nm))
  }
  extra.nm <-
    colnames(da)[!colnames(da) %in% c("node", "label", "isTip", data.nm, aa)]
  if (length(extra.nm) > 0) {
    td@extraInfo <- da %>% select(c("node", extra.nm))
  }
  return(td)
}


get_fields_treedata <- function(object) {
  get_fields_data <- function(object) {
    if (nrow(object@data) > 0) {
      fields <- colnames(object@data)
      fields <- fields[fields != "node"]
    } else {
      fields <- ""
    }
    fields
  }
  get_fields_extraInfo <- function(object) {
    extraInfo <- object@extraInfo
    if (nrow(extraInfo) > 0) {
      cn <- colnames(extraInfo)
      i <-
        match(
          c(
            "x",
            "y",
            "isTip",
            "node",
            "parent",
            "label",
            "branch",
            "branch.length"
          ),
          cn
        )
      i <- i[!is.na(i)]
      fields <- cn[-i]
      return(fields)
    } else {
      return(character(0))
    }
  }
  fields1 <- get_fields_data(object)
  fields2 <- get_fields_extraInfo(object)
  return(c(fields1, fields2))
}





##' @method merge tbl_tree
##' @export
merge.tbl_tree <- function(x, y, ...) {
  res <- NextMethod()
  class(res) <- class(x)
  return(res)
}

##' @method unnest treedata
##' @export
unnest.treedata <- function(data,
                            cols,
                            ...,
                            keep_empty = FALSE,
                            ptype = NULL,
                            names_sep = NULL,
                            names_repair = "check_unique") {
  tbl_df_returned_message %>%
    pillar::style_subtle() %>%
    writeLines()
  cols <- rlang::enquo(cols)
  data <- .extract_annotda.treedata(data)
  data <- unnest(
    data,
    !!cols,
    ...,
    keep_empty = keep_empty,
    ptype = ptype,
    names_sep = names_sep,
    names_repair = names_repair
  )
  return(data)
}


##' @method mutate tbl_tree
##' @export
mutate.tbl_tree <- function(.data, ...) {
  res <- NextMethod()
  class(res) <- class(.data)
  res
}

##' @method mutate treedata
##' @export
mutate.treedata <- function(.data, ..., keep.td = TRUE) {
  dots <- rlang::quos(...)
  dat <- .extract_annotda.treedata(.data)
  da <- dplyr::mutate(dat, !!!dots)
  if (keep.td) {
    .data <- .update.treedata(
      td = .data,
      da = da,
      dat = dat,
      type = "extra"
    )
    return(.data)
  }
  return(da)
}

##' @export
child <- function(.data, .node, ...) {
  UseMethod("child")
}
##' @method child tbl_tree
##' @export
child.tbl_tree <- function(.data, .node, ...) {
  valid_tbl_tree(.data)

  if (is.character(.node)) {
    .node <- .data$node[.data$label == .node]
  }

  .data[.data$parent == .node & .data$parent != .data$node, ]
}


##' @export
offspring <- function(.data, .node, tiponly, self_include, ...) {
  UseMethod("offspring")
}
##' @method offspring tbl_tree
##' @export
offspring.tbl_tree <-
  function(.data,
           .node,
           tiponly = FALSE,
           self_include = FALSE,
           ...) {
    if (missing(.node) || is.null(.node)) {
      cat_stop(".node is required")
    }
    if (length(.node) == 1) {
      res <- offspring.tbl_tree_item(
        .data = .data,
        .node = .node,
        tiponly = tiponly,
        self_include = self_include,
        ...
      )
    } else {
      res <- lapply(.node, function(node) {
        offspring.tbl_tree_item(
          .data = .data,
          .node = node,
          tiponly = tiponly,
          self_include = self_include,
          ...
        )
      })
      names(res) <- .node
    }
    return(res)
  }
offspring.tbl_tree_item <-
  function(.data,
           .node,
           tiponly = FALSE,
           self_include = FALSE,
           ...) {
    x <- child.tbl_tree(.data, .node)

    ## https://github.com/GuangchuangYu/ggtree/issues/239
    rn <- root_node.tbl_tree(.data)$node
    x <- x[x$node != rn, ]

    if (nrow(x) == 0) {
      if (self_include) {
        x <- .data[.data$node == .node, ]
      }

      return(x)
    }

    ## id <- x$node
    ## i <- 1
    ## while(i <= length(id)) {
    ##     id <- c(id, child(.data, id[i])$node)
    ##     i <- i + 1
    ## }
    ## filter_(.data, ~ node %in% id)

    parent <- .data$parent
    children <- .data$node
    ## n <- length(parent)
    n <- max(parent)

    kids <- vector("list", n)
    for (i in seq_along(parent)) {
      kids[[parent[i]]] <- c(kids[[parent[i]]], children[i])
    }

    id <- x$node
    i <- 1
    while (i <= length(id)) {
      id <- c(id, kids[[id[i]]])
      i <- i + 1
    }

    if (self_include) {
      id <- c(.node, id)
    }

    sp <- .data[children %in% id, ]
    if (tiponly) {
      return(sp[sp$node < rn, ])
    }
    return(sp)
  }
##' @method offspring ggtree
##' @export
offspring.ggtree <-
  function(.data,
           .node,
           tiponly = FALSE,
           self_include = FALSE,
           ...) {
    offspring.tbl_tree(
      .data$data,
      .node = .node,
      tiponly = tiponly,
      self_include = self_include,
      ...
    )
  }


##' @export
MRCA <- function(.data, ...) {
  UseMethod("MRCA")
}
##' @method MRCA tbl_tree
##' @export
MRCA.tbl_tree <- function(.data, .node1, .node2 = NULL, ...) {
  if (length(.node1) == 1 && length(.node2) == 1) {
    return(MRCA.tbl_tree_internal(.data, .node1, .node2, ...))
  } else if (is.null(.node2) && length(.node1) >= 1) {
    if (length(.node1) == 1) {
      return(itself(.data, .node1))
    }
    ## else length(.node1) > 1
    node <- MRCA.tbl_tree_internal(.data, .node1[1], .node1[2])
    if (length(.node1) > 2) {
      for (i in 3:length(.node1)) {
        node <- MRCA.tbl_tree_internal(.data, .node1[i], node$node)
      }
    }
    return(node)
  } else {
    cat_stop("invalid input of '.node1' and '.node2'...")
  }
}
MRCA.tbl_tree_internal <- function(.data, .node1, .node2, ...) {
  anc1 <- ancestor(.data, .node1)
  if (nrow(anc1) == 0) {
    ## .node1 is root
    return(anc1)
  }
  if (.node2 %in% anc1$node) {
    ## .node2 is the ancestor of .node1
    return(filter(anc1, .data$node == .node2))
  }
  p <- parent(.data, .node2)
  if (nrow(p) == 0) {
    ## .node2 is root
    return(p)
  }
  while (!p$node %in% anc1$node) {
    p <- parent(.data, p$node)
  }
  return(p)
}
##' @method MRCA ggtree
##' @export
MRCA.ggtree <- function(.data, .node1, .node2 = NULL, ...) {
  MRCA(.data$data, .node1, .node2 = .node2, ...)[["node"]]
}


##' @export
parent <- function(.data, .node, ...) {
  UseMethod("parent")
}
##' @method parent tbl_tree
##' @export
parent.tbl_tree <- function(.data, .node, ...) {
  valid_tbl_tree(.data)
  ## x <- filter_(.data, ~ (node == .node | label == .node) & node != parent)
  ## if (nrow(x) == 0) ## root node
  ##     return(x)
  ## ## https://stackoverflow.com/questions/34219912/how-to-use-a-variable-in-dplyrfilter
  ## filter_(.data, interp(~node == p, p = x$parent))

  ndata <- itself(.data, .node)
  .node <- ndata$node
  pnode <- ndata$parent

  if (pnode == .node) {
    return(.data[0, ])
  } ## empty tibble
  .data[.data$node == pnode, ]
}
itself <- function(.data, .node) {
  if (is.numeric(.node)) {
    i <- which(.data$node == .node)
  } else {
    i <- which(.data$label == .node)
  }

  ## .data[which(.data$node == .node | .data$label == .node), ]
  return(.data[i, ])
}


##' @export
ancestor <- function(.data, .node, ...) {
  UseMethod("ancestor")
}
##' @method ancestor tbl_tree
##' @export
ancestor.tbl_tree <- function(.data, .node, ...) {
  ## prevent using filter

  ndata <- itself(.data, .node)
  ## ndata <- filter_(.data, ~ (node == .node | label == .node))
  .node <- ndata$node
  pnode <- ndata$parent

  if (.node == pnode) {
    ## root node
    return(parent(.data, .node)) ## empty tibble
  }

  parent <- .data$parent
  children <- .data$node
  n <- length(children)

  pp <- vector("integer", n)
  for (i in seq_along(children)) {
    pp[[children[i]]] <- parent[i]
  }

  id <- pnode
  i <- 1
  while (i <= length(id)) {
    pnode <- pp[id[i]]
    if (pnode == id[i]) {
      break
    }
    id <- c(id, pnode)
    i <- i + 1
  }
  ## filter_(.data, ~ node %in% id)
  .data[children %in% id, ]
}


##' @export
sibling <- function(.data, ...) {
  UseMethod("sibling")
}
##' @method sibling tbl_tree
##' @export
sibling.tbl_tree <- function(.data, .node, ...) {
  valid_tbl_tree(.data)

  p <- parent(.data, .node)
  if (nrow(p) == 0) {
    # if root node, return empty tibble
    return(p)
  }
  child(.data, p$node) %>% filter(.data$node != .node)
}



##' @method pull treedata
##' @export
pull.treedata <- function(.data,
                          var = -1,
                          name = NULL,
                          ...) {
  var <- rlang::enquo(var)
  name <- rlang::enquo(name)
  dat <- .extract_annotda.treedata(.data)
  dplyr::pull(dat, var = !!var, name = !!name, ...)
}

##' @method pull phylo
##' @export
pull.phylo <- pull.treedata

##' @method full_join treedata
##' @export
full_join.treedata <- function(x,
                               y,
                               by = NULL,
                               copy = FALSE,
                               suffix = c(".x", ".y"),
                               ...) {
  by <- match.arg(by, c("node", "label"))
  y <- as_tibble(y)
  if (by == "label") {
    ntip <- Ntip(x)
    N <- Nnode2(x)
    label <- rep(NA, N)
    label[1:ntip] <- x@phylo[["tip.label"]]
    if (!is.null(x@phylo$node.label)) {
      label[(ntip + 1):N] <- x@phylo$node.label
    }
    lab <- tibble(node = 1:N, label = label)
    y <- full_join(lab, y, by = "label") %>% select(-.data$label)
  }

  if (nrow(x@extraInfo) == 0) {
    x@extraInfo <- y
  } else {
    x@extraInfo <-
      full_join(x@extraInfo,
                y,
                by = "node",
                copy = copy,
                suffix = suffix
      )
  }
  return(x)
}

##' @method full_join phylo
##' @export
full_join.phylo <- function(x,
                            y,
                            by = NULL,
                            copy = FALSE,
                            suffix = c(".x", ".y"),
                            ...) {
  full_join(
    as_tibble(x),
    y = y,
    by = by,
    copy = copy,
    suffix = suffix,
    ...
  ) %>%
    as.treedata()
}




##' @export
Ntip <- function(phy) {
  UseMethod("Ntip")
}
##' @export
Ntip.multiPhylo <- function(phy) {
  labs <- attr(phy, "TipLabel")
  if (is.null(labs)) {
    sapply(unclass(phy), Ntip.phylo)
  } else {
    set_names(rep(length(labs), length(phy)), names(phy))
  }
}
##' @export
Ntip.phylo <- function(phy) {
  length(phy$tip.label)
}
##' @export
Ntip.treedata <- function(phy) {
  Ntip(as.phylo(phy))
}



##' @export
Nnode <- function(phy, ...) {
  UseMethod("Nnode")
}
##' @export
Nnode.multiPhylo <- function(phy, internal.only = TRUE, ...) {
  res <- sapply(unclass(phy), "[[", "Nnode")
  if (internal.only) {
    return(res)
  }
  res + Ntip.multiPhylo(phy)
}
##' @export
Nnode.phylo <- function(phy, internal.only = TRUE, ...) {
  if (internal.only) {
    return(phy$Nnode)
  }
  phy$Nnode + length(phy$tip.label)
}
##' @export
Nnode.treedata <- function(phy, internal.only = TRUE, ...) {
  Nnode(as.phylo(phy), internal.only = internal.only, ...)
}

Nnode2 <- function(tree) {
  Nnode(tree, internal.only=FALSE)
}


##' @export
as.treedata <- function(tree, ...) {
  UseMethod("as.treedata")
}

##' @export
as.treedata.phylo <- function(tree, boot = NULL, ...) {
  ## boot is output from boot.phylo
  res <- new("treedata",
             phylo = tree
  )

  if (!is.null(boot)) {
    res@data <- tibble(node = tree_node_ids(tree), bootstrap = boot)
  }
  return(res)
}

##' @export
as.treedata.phylo4 <- function(tree, ...) {
  new("treedata",
      phylo = as.phylo(tree)
  )
}

##' @export
as.treedata.phylo4d <- function(tree, ...) {
  d <- as_tibble(tree@data)
  d$node <- as.numeric(rownames(tree@data))

  new("treedata",
      phylo = as.phylo.phylo4(tree),
      data = d
  )
}

##' @export
as.treedata.ggtree <- function(tree, ...) {
  d <- as_tibble(tree$data)
  class(d) <- c("tbl_tree", "tbl_df", "tbl", "data.frame")
  as.treedata(d, ...)
}

##' @export
as.treedata.tbl_df <- function(tree, ...) {
  edgelist <- as_tibble(tree)
  edge <- check_edgelist(edgelist)
  indx <- attr(edge, "indx")
  if (!is.null(indx)) {
    edgelist <- edgelist[indx, ]
    attr(edge, "indx") <- NULL
  }
  phylo <- as.phylo(edgelist, ...)

  res <- new("treedata",
             phylo = phylo
  )

  if (ncol(edgelist) >= 3) {
    d <- edgelist[, -c(1, 2)]
    length_var <- attr(phylo, "length_var")

    if (!is.null(length_var)) {
      d <- d[, names(d) != length_var]
      if (ncol(d) == 0) {
        return(res)
      }
    }

    lab <- c(phylo$tip.label, phylo$node.label)

    # edge <- check_edgelist(edgelist)
    children <- edge[, 2]

    d$node <- match(children, lab)
    res@data <- d
  }

  return(res)
}

##' @export
as.treedata.data.frame <- as.treedata.tbl_df

##' @export
as.treedata.matrix <- as.treedata.tbl_df

##' @export
as.treedata.pvclust <- function(tree, ...) {
  phylo <- as.phylo.hclust_node(tree$hclust)

  ## tranforming the pvclust bootstraps values to tibble with key column:"label"
  tree_boots <-
    (round(tree$edges[, c("si", "au", "bp")], 2) * 100) %>%
    as_tibble() %>%
    mutate(label = paste0(seq_len(Nnode(phylo)), "_edge"))

  full_join(phylo, tree_boots)
}

as.phylo.hclust_node <- function(x) {
  N <- dim(x$merge)[1]
  edge <- matrix(0L, 2 * N, 2)
  edge.length <- numeric(2 * N)
  node <- integer(N)
  node[N] <- N + 2L
  cur.nod <- N + 3L
  j <- 1L
  for (i in N:1) {
    edge[j:(j + 1), 1] <- node[i]
    for (l in 1:2) {
      k <- j + l - 1L
      y <- x$merge[i, l]
      if (y > 0) {
        edge[k, 2] <- node[y] <- cur.nod
        cur.nod <- cur.nod + 1L
        edge.length[k] <- x$height[i] - x$height[y]
      } else {
        edge[k, 2] <- -y
        edge.length[k] <- x$height[i]
      }
    }
    j <- j + 2L
  }
  if (is.null(x$labels)) {
    x$labels <- as.character(1:(N + 1))
  }
  node.lab <- order(node) # here we keep the order for the edges
  obj <- list(
    edge = edge,
    edge.length = edge.length / 2,
    tip.label = x$labels,
    Nnode = N,
    node.label = paste(node.lab, "_edge", sep = "")
  ) # export it to the final object
  class(obj) <- "phylo"
  stats::reorder(obj)
}

##' @importFrom tibble as_tibble
##' @method as_tibble phylo
##' @export
as_tibble.phylo <- function(x, ...) {
  phylo <- x
  ntip <- Ntip(phylo)
  N <- Nnode(phylo, internal.only = FALSE)

  tip.label <- phylo[["tip.label"]]
  edge <- phylo[["edge"]]
  colnames(edge) <- c("parent", "node")
  res <- as_tibble(edge)
  if (!is.null(phylo$edge.length)) {
    res$branch.length <- phylo$edge.length
  }

  label <- rep(NA, N)
  label[1:ntip] <- tip.label
  if (!is.null(phylo$node.label)) {
    label[(ntip + 1):N] <- phylo$node.label
  }
  ## isTip <- rep(FALSE, N)
  ## isTip[1:ntip] <- TRUE

  label.df <- tibble(node = 1:N, label = label) # , isTip = isTip)
  res <- full_join(res, label.df, by = "node")

  idx <- is.na(res$parent)
  res$parent[idx] <- res$node[idx]

  if (!is.null(phylo$edge.length) && !is.null(phylo$root.edge)) {
    res$branch.length[res$parent == res$node] <- phylo$root.edge
  }

  res <- res[order(res$node), ]
  aa <- names(attributes(phylo))
  group <-
    aa[!aa %in% c("names", "class", "order", "reroot", "node_map")]
  if (length(group) > 0) {
    for (group_ in group) {
      ## groupOTU & groupClade
      group_info <- attr(phylo, group_)
      if (length(group_info) == nrow(res)) {
        res[[group_]] <- group_info
      }
    }
  }
  class(res) <- c("tbl_tree", class(res))
  res
}

##' @method as_tibble treedata
##' @importFrom tibble as_tibble
##' @export
as_tibble.treedata <- function(x, ...) {
  res <- as_tibble(x@phylo)
  tree_anno <- as_tibble(get_tree_data(x))
  if (nrow(tree_anno) > 0) {
    by <- "node"
    tree_anno$node <- as.integer(tree_anno$node)
    if ("parent" %in% colnames(tree_anno)) {
      by <- c(by, "parent")
      tree_anno$parent <- as.integer(tree_anno$parent)
    }
    res <- full_join(res, tree_anno, by = by)
  }
  return(res)
}

##' @importFrom methods new
treedata <- function(...) {
  new("treedata", ...)
}

get_tree_data <- function(tree_object) {
  tree_anno <- tree_object@data
  extraInfo <- tree_object@extraInfo

  if (nrow(tree_anno) == 0 && nrow(extraInfo) == 0) {
    return(NULL)
  }

  if (nrow(tree_anno) == 0) {
    extraInfo$node <- as.integer(extraInfo$node)
    return(extraInfo)
  }
  if (nrow(extraInfo) == 0) {
    tree_anno$node <- as.integer(tree_anno$node)
    return(tree_anno)
  }

  tree_anno$node <- as.integer(tree_anno$node)
  extraInfo$node <- as.integer(extraInfo$node)

  full_join(tree_anno, extraInfo, by = "node")
}
#' @keywords internal
.internal_nest <- function(x, keepnm, ..., .names_sep = NULL) {
  nest <- utils::getFromNamespace("nest", "tidyr")
  if (missing(...)) {
    idx <- x %>% vapply(is.list, logical(1))
    clnm <- colnames(x)
    clnm <- clnm[!idx]
    clnm <- clnm[!clnm %in% keepnm]
    params <- c(list(x), lapply(clnm, function(x) {
      x
    }))
    names(params) <- c(".data", clnm)
  } else {
    res <- nest(.data = x, ..., .names_sep = .names_sep)
    return(res)
  }
  if (!is.null(.names_sep)) {
    params <- c(params, .names_sep = .names_sep)
  }
  res <- do.call(nest, params)
  return(res)
}
#' @keywords internal
.assign_parent_status <- function(tr, df, variable) {
  yy <- df[[variable]]
  na.idx <- which(is.na(yy))
  if (length(na.idx) > 0) {
    tree <- get.tree(tr)
    nodes <- get_nodes_by_postorder(tree)
    for (curNode in nodes) {
      children <- child(tree, curNode)
      if (length(children) == 0) {
        next
      }
      idx <- which(is.na(yy[children]))
      if (length(idx) > 0) {
        yy[children[idx]] <- yy[curNode]
      }
    }
  }
  df[, variable] <- yy
  return(df)
}
#' @keywords internal
.assign_child_status <-
  function(tr, df, variable, yscale_mapping = NULL) {
    yy <- df[[variable]]
    if (!is.null(yscale_mapping)) {
      yy <- yscale_mapping[yy]
    }

    na.idx <- which(is.na(yy))
    if (length(na.idx) > 0) {
      tree <- get.tree(tr)
      nodes <- rev(get_nodes_by_postorder(tree))
      for (curNode in nodes) {
        parent <- parent(tree, curNode)
        if (parent == 0) {
          ## already reach root
          next
        }
        idx <- which(is.na(yy[parent]))
        if (length(idx) > 0) {
          child <- child(tree, parent)
          yy[parent[idx]] <- mean(yy[child], na.rm = TRUE)
        }
      }
    }
    df[, variable] <- yy
    return(df)
  }

#' @keywords internal
tree_tip_ids <- function(tree) {
  1:Ntip(tree)
}
#' @keywords internal
tree_node_ids <- function(tree, internal.only = TRUE) {
  if (internal.only) {
    return(Ntip(tree) + 1:Nnode(tree, internal.only))
  }
  1:Nnode(tree, internal.only)
}
#' @keywords internal
valid_tbl_tree <-
  function(object, cols = c("parent", "node", "label")) {
    cc <- cols[!cols %in% colnames(object)]
    if (length(cc) > 0) {
      msg <-
        paste0(
          "invalid tbl_tree object.\n  missing column:\n    ",
          paste(cc, collapse = ","),
          "."
        )
    }
  }
#' @keywords internal
.extract_annotda.treedata <- function(x) {
  if (inherits(x, "treedata")) {
    annotda <- get_data_tree(x)
    x <- x@phylo
  } else {
    annotda <- NULL
  }
  trdf <- x %>%
    as_tibble() %>%
    .internal_add_isTip() %>%
    drop_class(name = "tbl_tree")

  if (!any(is.null(annotda) || nrow(annotda) == 0)) {
    annotda <- trdf %>%
      dplyr::left_join(annotda, by = "node")
  } else {
    annotda <- trdf
  }
  annotda <-
    annotda[, !colnames(annotda) %in% c("parent", "branch.length")]
  return(annotda)
}

get_data_tree <- function(tree_object) {
  tree_anno <- tree_object@data
  extraInfo <- tree_object@extraInfo

  if (nrow(tree_anno)==0 && nrow(extraInfo)==0){
    return(NULL)
  }

  if (nrow(tree_anno) == 0) {
    extraInfo$node <- as.integer(extraInfo$node)
    return(extraInfo)
  }
  if (nrow(extraInfo) == 0) {
    tree_anno$node <- as.integer(tree_anno$node)
    return(tree_anno)
  }

  tree_anno$node <- as.integer(tree_anno$node)
  extraInfo$node <- as.integer(extraInfo$node)

  full_join(tree_anno, extraInfo, by = "node")
}



#' @keywords internal
scale_y <- function(phylo, df, yscale, layout, ...) {
  if (yscale == "none") {
    return(df)
  }
  if (!yscale %in% colnames(df)) {
    cat_warn("yscale is not available...\n")
    return(df)
  }
  if (is.numeric(df[[yscale]])) {
    y <- get_y_coord_scale_numeric(phylo, df, yscale, ...)
  } else {
    y <- get_y_coord_scale_category(phylo, df, yscale, ...)
  }

  df[, "y"] <- y

  return(df)
}
#' @keywords internal
scale_x_by_time <- function(df, as.Date = FALSE) {
  time <-
    with(df, gsub(".*[_/]{1}(\\d+\\.*\\d+)$", "\\1", label[isTip])) %>% as.numeric()
  latest <- which.max(time)

  scale_x_by_time_from_mrsd(df, decimal2date(time[latest]), as.Date)
}
#' @keywords internal
scale_x_by_time_from_mrsd <- function(df, mrsd, as.Date) {
  mrsd %<>% as.Date
  date <- date2decimal(mrsd)

  df$x <- df$x + date - max(df$x)
  df$branch <- with(df, (x[match(parent, node)] + x) / 2)
  ## df$branch <- (df[df$parent, "x"] + df[, "x"])/2

  if (as.Date) {
    df$x <- decimal2date(df$x)
    df$branch <- decimal2date(df$branch)
  }

  return(df)
}
#' @keywords internal
date2decimal <- function(x) {
  if (is(x, "numeric")) {
    return(x)
  }

  if (is(x, "character")) {
    x <- as.Date(x)
  }
  year <- format(x, "%Y")
  y <- x - as.Date(paste0(year, "-01-01"))
  year <- as.numeric(year)
  ndate <- ifelse(year %% 4 == 0, 366, 365)
  year + as.numeric(y) / ndate
}
#' @keywords internal
decimal2date <- function(x) {
  date <- as.Date(paste0(floor(x), "-01-01"))
  date + as.numeric(sub("^\\d+", "0", x)) * 365
}

#' @keywords internal
drop_class <- function(x, name) {
  class(x) <- class(x)[!class(x) %in% name]
  x
}
#' @keywords internal
get_node_num_tree <- function(tree) {
  Nnode(tree, internal.only = FALSE)
}

#' @keywords internal
get_x_coord2 <-
  function(x,
           root,
           parent,
           child,
           len,
           start = 0,
           rev = FALSE) {
    x[root] <- start
    x[-root] <- NA ## only root is set to start, by default 0

    currentNode <- root
    direction <- 1
    if (rev == TRUE) {
      direction <- -1
    }

    ignore_negative_edge <-
      getOption("ignore.negative.edge", default = FALSE)

    if (any(len < 0) && !ignore_negative_edge) {
      warning_wrap(
        "The tree contained negative ",
        ifelse(sum(len < 0) > 1, "edge lengths", "edge length"),
        ". If you want to ignore the ",
        ifelse(sum(len < 0) > 1, "edges", "edge"),
        ", you can set 'options(ignore.negative.edge=TRUE)', then re-run ggtree."
      )
    }
    while (anyNA(x)) {
      idx <- which(parent %in% currentNode)
      newNode <- child[idx]
      if (ignore_negative_edge) {
        x[newNode] <- x[parent[idx]] + len[idx] * direction * sign(len[idx])
      } else {
        x[newNode] <- x[parent[idx]] + len[idx] * direction
      }
      currentNode <- newNode
    }

    x
  }
#' @keywords internal
get_x_coord <- function(tr) {
  edge <- tr$edge
  parent <- edge[, 1]
  child <- edge[, 2]
  root <- root_node(tr)

  len <- tr$edge.length

  n <- get_node_num_tree(tr)
  x <- numeric(n)
  get_x_coord2(x, root, parent, child, len)
}
#' @keywords internal
get_x_coord_no_length <- function(tr) {
  edge <- tr$edge
  parent <- edge[, 1]
  child <- edge[, 2]
  root <- root_node(tr)

  len <- tr$edge.length

  N <- get_node_num_tree(tr)
  x <- numeric(N)
  ntip <- Ntip(tr)
  currentNode <- 1:ntip
  x[-currentNode] <- NA

  cl <- split(child, parent)
  child_list <- list()
  child_list[as.numeric(names(cl))] <- cl

  while (anyNA(x)) {
    idx <- match(currentNode, child)
    pNode <- parent[idx]
    ## child number table
    p1 <- table(parent[parent %in% pNode])
    p2 <- table(pNode)
    np <- names(p2)
    i <- p1[np] == p2
    newNode <- as.numeric(np[i])

    exclude <- rep(NA, max(child))
    for (j in newNode) {
      x[j] <- min(x[child_list[[j]]]) - 1
      exclude[child_list[[j]]] <- child_list[[j]]
    }
    exclude <- exclude[!is.na(exclude)]

    ## currentNode %<>% `[`(!(. %in% exclude))
    ## currentNode %<>% c(., newNode) %>% unique
    currentNode <- currentNode[!currentNode %in% exclude]
    currentNode <- unique(c(currentNode, newNode))
  }
  x <- x - min(x)
  return(x)
}


#' @keywords internal
get_y_coord <- function(tr,
                        step = 1,
                        tip.order = NULL) {
  Ntip <- length(tr[["tip.label"]])
  N <- get_node_num_tree(tr)

  edge <- tr[["edge"]]
  parent <- edge[, 1]
  child <- edge[, 2]

  cl <- split(child, parent)
  child_list <- list()
  child_list[as.numeric(names(cl))] <- cl

  y <- numeric(N)
  if (is.null(tip.order)) {
    tip.idx <- child[child <= Ntip]
    y[tip.idx] <- 1:Ntip * step
  } else {
    tip.idx <- 1:Ntip
    y[tip.idx] <- match(tr$tip.label, tip.order) * step
  }
  y[-tip.idx] <- NA

  pvec <- integer(max(tr$edge))
  pvec[child] <- parent

  currentNode <- 1:Ntip
  while (anyNA(y)) {
    pNode <- unique(pvec[currentNode])

    idx <-
      sapply(pNode, function(i) {
        all(child_list[[i]] %in% currentNode)
      })
    newNode <- pNode[idx]

    y[newNode] <- sapply(newNode, function(i) {
      mean(y[child_list[[i]]], na.rm = TRUE)
    })

    currentNode <-
      c(currentNode[!currentNode %in% unlist(child_list[newNode])], newNode)
  }

  return(y)
}
#' @keywords internal
get_y_coord_scale_numeric <- function(tr, df, yscale, ...) {
  df <- .assign_parent_status(tr, df, yscale)
  df <- .assign_child_status(tr, df, yscale)

  y <- df[, yscale]

  if (anyNA(y)) {
    cat_warn("NA found in y scale mapping, all were setting to 0")
    y[is.na(y)] <- 0
  }

  return(y)
}
#' @keywords internal
get_y_coord_scale_category <-
  function(tr, df, yscale, yscale_mapping = NULL, ...) {
    if (is.null(yscale_mapping)) {
      cat_stop(
        "yscale is category variable, user should provide yscale_mapping,
             which is a named vector, to convert yscale to numberical values..."
      )
    }
    if (!is(yscale_mapping, "numeric") ||
        is.null(names(yscale_mapping))) {
      cat_stop("yscale_mapping should be a named numeric vector...")
    }

    if (yscale == "label") {
      yy <- df[[yscale]]
      ii <- which(is.na(yy))
      if (length(ii)) {
        ## df[ii, yscale] <- df[ii, "node"]
        df[[yscale]][ii] <- as.character(df[["node"]][ii])
      }
    }

    ## assign to parent status is more prefer...
    df <- .assign_parent_status(tr, df, yscale)
    df <- .assign_child_status(tr, df, yscale, yscale_mapping)

    y <- df[[yscale]]

    if (anyNA(y)) {
      cat_warn("NA found in y scale mapping, all were setting to 0")
      y[is.na(y)] <- 0
    }
    return(y)
  }


#' @keywords internal
set_branch_length <- function(tree_object, branch.length) {
  if (branch.length == "branch.length") {
    return(tree_object)
  } else if (branch.length == "none") {
    tree_object@phylo$edge.length <- NULL
    return(tree_object)
  }

  if (is(tree_object, "phylo")) {
    return(tree_object)
  }

  tree_anno <- get_data_tree(tree_object)
  tree_anno$node <- as.integer(tree_anno$node)

  phylo <- as.phylo(tree_object)

  cn <- colnames(tree_anno)
  cn <- cn[!cn %in% c("node", "parent")]

  length <- match.arg(branch.length, cn)

  if (all(is.na(as.numeric(tree_anno[[length]])))) {
    cat_stop("branch.length should be numerical attributes...")
  }

  edge <- phylo$edge
  colnames(edge) <- c("parent", "node")
  edge <- as_tibble(edge)

  dd <- full_join(edge, tree_anno, by = "node")

  dd <- dd[match(edge[["node"]], dd[["node"]]), ]
  len <- unlist(dd[[length]])
  len <- as.numeric(len)
  len[is.na(len)] <- 0

  phylo$edge.length <- len

  tree_object@phylo <- phylo
  return(tree_object)
}
#' @keywords internal
check_edgelist <- function(edgelist) {
  if (dim(edgelist)[2] < 2) {
    cat_stop(
      "input should be a matrix of edge list that holds the relationships in the first two columns"
    )
  }
  if (length(unique(edgelist[[1]])) > length(unique(edgelist[[2]]))) {
    children <- edgelist[[1]]
    parents <- edgelist[[2]]
  } else {
    children <- edgelist[[2]]
    parents <- edgelist[[1]]
  }
  root1 <- unique(parents[!(parents %in% children)])
  root2 <- unique(parents[parents == children])
  if (length(root1) != 1 && length(root2) != 1) {
    cat_stop("Cannot find root. network is not a tree!")
  }
  if (length(root1) != 1 && length(root2) == 1) {
    indx <- parents != children
    parents <- parents[indx]
    children <- children[indx]
    edge <- matrix(c(parents, children), ncol = 2)
    attr(edge, "indx") <- indx
  } else {
    edge <- matrix(c(parents, children), ncol = 2)
  }
  return(edge)
}
#' @keywords internal
.write.tree4 <-
  function(edge,
           digits = 10,
           tree.prefix = "",
           root.edge = NULL,
           edge.length = NULL,
           tip.label = NULL,
           node.label = NULL,
           id_as_label = FALSE,
           node_anno = NULL) {
    edge.label <- edge
    edge <- matrix(as.numeric(as.factor(edge.label)), ncol = 2)

    f.d <- paste("%.", digits, "g", sep = "")

    to_tiplab <- function(edge, i) {
      edge.label[edge[, 2] == i, 2][1]
    }

    to_nodelab <- function(edge, i) {
      edge.label[edge[, 1] == i, 1][1]
    }

    brl <- !is.null(edge.length)
    nodelab <- !is.null(node.label)

    if (id_as_label) {
      nodelab <- TRUE
    }

    cp <- function(x) {
      STRING[k] <<- x
      k <<- k + 1
    }

    add.internal <- function(i) {
      cp("(")
      desc <- kids[[i]]
      for (j in desc) {
        if (j %in% edge[, 1]) {
          add.internal(j)
        } ## if (j > n) add.internal(j)
        else {
          add.terminal(ind[j])
        }
        if (j != desc[length(desc)]) {
          cp(",")
        }
      }
      cp(")")
      ## if (nodelab && i > n) {
      if (nodelab && i %in% edge[, 1]) {
        ## cp(phy$node.label[i - n]) # fixed by Naim Matasci (2010-12-07)
        if (is.null(node_anno)) {
          if (id_as_label) {
            nl <- to_nodelab(edge, i)
          } else {
            nl <- node.label[i - n]
          }
        } else if (!is.na(node_anno[i])) {
          if (id_as_label) {
            nl <- paste0(to_nodelab(edge, i), node_anno[i])
          } else {
            nl <- paste0(node.label[i - n], node_anno[i])
          }
        }
        cp(nl)
      } else if (i %in% edge[, 1] && !is.null(node_anno)) {
        cp(node_anno[i])
      }
      if (brl) {
        cp(":")
        cp(sprintf(f.d, edge.length[ind[i]]))
      }
    }

    add.terminal <- function(i) {
      ii <- edge[i, 2]
      if (is.null(node_anno) || is.na(node_anno[ii])) {
        if (id_as_label) {
          tl <- to_tiplab(edge, ii)
        } else {
          tl <- tip.label[ii]
        }
      } else {
        if (id_as_label) {
          tl <- paste0(to_tiplab(edge, ii), node_anno[ii])
        } else {
          tl <- paste0(tip.label[ii], node_anno[ii])
        }
      }
      cp(tl)
      if (brl) {
        cp(":")
        cp(sprintf(f.d, edge.length[i]))
      }
    }

    Ntip <- function(edge) {
      tip <- edge[, 2][!edge[, 2] %in% edge[, 1]]
      length(tip)
    }

    Nnode <- function(edge) {
      tip <- edge[, 2][!edge[, 2] %in% edge[, 1]]
      node <- edge[, 1][!edge[, 1] %in% tip]
      length(node)
    }

    ## borrowed from phangorn:
    parent <- edge[, 1]
    children <- edge[, 2]
    n <- Ntip(edge)
    kids <- vector("list", n + Nnode(edge))

    for (i in seq_along(parent)) {
      kids[[parent[i]]] <- c(kids[[parent[i]]], children[i])
    }

    ind <- match(1:max(edge), edge[, 2])

    LS <- 4 * n + 5
    if (brl) {
      LS <- LS + 4 * n
    }
    if (nodelab) {
      LS <- LS + n
    }
    STRING <- character(LS)
    k <- 1
    cp(tree.prefix)
    cp("(")
    getRoot <- function(edge) {
      edge[, 1][!match(edge[, 1], edge[, 2], 0)][1]
    }
    root <-
      getRoot(edge) # replaced n+1 with root - root has not be n+1
    desc <- kids[[root]]
    for (j in desc) {
      if (j %in% edge[, 1]) {
        add.internal(j)
      } ## if (j > n) add.internal(j)
      else {
        add.terminal(ind[j])
      }
      if (j != desc[length(desc)]) {
        cp(",")
      }
    }

    if (is.null(root.edge)) {
      cp(")")
      if (nodelab) {
        if (!is.null(node_anno) && !is.na(node_anno[root])) {
          if (id_as_label) {
            cp(paste0(to_nodelab(edge, root), node_anno[root]))
          } else {
            cp(paste0(node.label[1], node_anno[root]))
          }
        } else {
          if (id_as_label) {
            cp(to_nodelab(edge, root))
          } else {
            cp(node.label[1])
          }
        }
      } else if (!is.null(node_anno) &&
                 !is.na(node_anno[root])) {
        cp(node_anno[root])
      }
      cp(";")
    } else {
      cp(")")
      if (nodelab) {
        cp(node.label[1])
      }
      cp(":")
      cp(sprintf(f.d, root.edge))
      cp(";")
    }
    paste(STRING, collapse = "")
  }
#' @keywords internal
calculate_branch_mid <- function(res, layout) {
  if (layout %in% c("equal_angle", "daylight", "ape")) {
    res$branch.y <- with(res, (y[match(parent, node)] + y) / 2)
    res$branch.y[is.na(res$branch.y)] <- 0
  }
  res$branch <- with(res, (x[match(parent, node)] + x) / 2)
  if (!is.null(res[["branch.length"]])) {
    res$branch.length[is.na(res$branch.length)] <- 0
  }
  res$branch[is.na(res$branch)] <- 0
  if (layout %in% c("equal_angle", "daylight", "ape")) {
    res$branch.x <- res$branch
  }
  return(res)
}
#' @keywords internal
calculate_angle <- function(data) {
  data$angle <- 360 / (diff(range(data$y)) + 1) * data$y
  return(data)
}
#' @keywords internal
get_nodes_by_postorder <- function(tree) {
  tree <- ape::reorder.phylo(tree, "postorder")
  unique(rev(as.vector(t(tree$edge[, c(2, 1)]))))
}
#' @keywords internal
collapse.singles <- function(tree, root.edge = FALSE) {
  n <- length(tree$tip.label)
  tree <- reorder(tree) # this works now
  e1 <- tree$edge[, 1]
  e2 <- tree$edge[, 2]

  tab <- tabulate(e1)
  if (all(tab[-c(1:n)] > 1)) {
    return(tree)
  } # tips are zero

  if (is.null(tree$edge.length)) {
    root.edge <- FALSE
    wbl <- FALSE
  } else {
    wbl <- TRUE
    el <- tree$edge.length
  }

  if (root.edge) {
    ROOTEDGE <- 0
  }

  ## start with the root node:
  ROOT <- n + 1L
  while (tab[ROOT] == 1) {
    i <- which(e1 == ROOT)
    ROOT <- e2[i]
    if (wbl) {
      if (root.edge) {
        ROOTEDGE <- ROOTEDGE + el[i]
      }
      el <- el[-i]
    }
    e1 <- e1[-i]
    e2 <- e2[-i]
  }

  singles <- which(tabulate(e1) == 1)
  if (length(singles) > 0) {
    ii <- sort(match(singles, e1), decreasing = TRUE)
    jj <- match(e1[ii], e2)
    for (i in seq_along(singles)) {
      e2[jj[i]] <- e2[ii[i]]
      if (wbl) {
        el[jj[i]] <- el[jj[i]] + el[ii[i]]
      }
    }
    e1 <- e1[-ii]
    e2 <- e2[-ii]
    if (wbl) {
      el <- el[-ii]
    }
  }
  Nnode <- length(e1) - n + 1L

  oldnodes <- unique(e1)
  if (!is.null(tree$node.label)) {
    tree$node.label <- tree$node.label[oldnodes - n]
  }
  newNb <- integer(max(oldnodes))
  newNb[ROOT] <- n + 1L
  sndcol <- e2 > n
  e2[sndcol] <- newNb[e2[sndcol]] <- n + 2:Nnode
  e1 <- newNb[e1]
  tree$edge <- cbind(e1, e2, deparse.level = 0)
  tree$Nnode <- Nnode
  if (wbl) {
    if (root.edge) {
      tree$root.edge <- ROOTEDGE
    }
    tree$edge.length <- el
  }
  tree
}
