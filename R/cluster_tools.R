# cluster_descriptions ----------------------------------------------------



#' cluster_descriptions
#'
#' @name cluster_descriptions
#' @description generar un perfil de cada cluster
#'
#'
#' @param x dataset
#' @param krange vector con la cantidad de cluster a evaluar
#' @param criterion criterio de desicion de cluster. "ch" or "asw".
#' Calinski-Harabasz criterion o average silhouette width.
#' @param runs n centros aleatorios iniciales
#' @param limit limite de desicion de seleccion para buscar
#' el modelo mas siemple entre con 'limit' corete
#'
#' @rdname cluster_tools
#'
#' @examples
#' set.seed(7)
#'
#' object<-cluster_descriptions(data = mgalda::datalearn$iris[,1:2], krange = 3:10)
#' autoplot(object)
#' rm(list = ls())
#' @export
cluster_descriptions <- function(data, ...) {
  UseMethod("cluster_descriptions")
}
#' @export
cluster_descriptions.default <- function(data, ...) {
  generic_default()
}
#' @export
cluster_descriptions.data.frame <-
  function(data,
           krange = 5:15,
           criterion = c("ch", "asw"),
           runs = 25,
           limit = 10) {
    criterion <- match.arg(criterion, c("ch", "asw"))

    object <- xpca(data, everything())

    var_coord <- object$inf$var_results$coord

    colnames(var_coord) <-
      gsub(
        pattern = "_",
        replacement = "",
        x = toupper(colnames(var_coord))
      )

    var_coord <- as_df(var_coord)

    num_comp <- object$object$num_comp

    cols_names <- paste0("PC", seq_len(num_comp))
    data_coord <-
      as_df(predict(object, data)[, cols_names])

    cum_perc_var <-
      object$inf$variances$cumulative_percent_variance[seq_len(num_comp)]

    cum_perc_var <-
      as_df(t(cum_perc_var))

    colnames(cum_perc_var) <- cols_names

    var_coord <- var_coord[, names(data_coord)]

    cl_coord <-
      interp(quote(runskmeans(
        x = x,
        krange = krange,
        criterion = criterion,
        runs = runs,
        limit = limit
      )),
      x = data_coord,
      krange = krange,
      criterion = criterion,
      runs = runs,
      limit = limit
      )

    cl_coord <- eval(cl_coord)

    centers_coord <-
      cl_coord$result$centers[, seq_len(num_comp)]

    dist_coord <- RANN::nn2(data = var_coord, query = centers_coord)

    dist_d <- dist_coord$nn.dists
    id_d <- dist_coord$nn.idx

    id_d <- apply(id_d, 2, function(x) {
      rownames(var_coord)[x]
    })

    thr_dist <- max(dist_d[, 1])

    dist_d <- dist_d <= thr_dist

    dist_d[, 1] <- TRUE

    cols_descr <-
      lapply(seq_len(nrow(dist_d)), function(x) {
        id_d[x, dist_d[x, ]]
      })

    out <-
      list(
        data = data,
        cols_cl = cols_descr,
        pca = object,
        cluster = cl_coord
      )

    class(out) <- "cluster_descriptions"
    out
  }
#' @export
print.cluster_descriptions <- function(x) {
  print(data_summary(x$data))
  print(x$pca)
  print(x$cluster)
}
#' @export
predict.cluster_descriptions <- function(object, new_data) {
  if (!all(names(object$data) %in% names(new_data))) {
    cat_stop("columnas perdidas en new_data")
  }

  centers <- object$cluster$result$centers
  pca_new_data <- as_df(predict(object$pca, new_data))
  partition <-
    RANN::nn2(
      data = centers,
      query = pca_new_data[, colnames(centers)],
      k = 1
    )$nn.idx
  new_data$.cluster <- as_int(partition)

  new_data
}

#' @method autoplot cluster_descriptions
#' @export
autoplot.cluster_descriptions <- function(x){
  data<-x$cluster$data_org
  data$cluster <-  factor(x$cluster$result$cluster)
  grf_point(PC1~PC2,color = ~cluster,data = data)
}

# runskmeans --------------------------------------------------------------
#' @name cluster_tools
#' @param x dataset
#' @param krange vector con la cantidad de cluster a evaluar
#' @param criterion criterio de desicion de cluster. "ch" or "asw".
#' Calinski-Harabasz criterion o average silhouette width.
#' @param runs n centros aleatorios iniciales
#' @param limit limite de desicion de seleccion para buscar
#' el modelo mas siemple entre con 'limit' corete
#'
#' @rdname cluster_tools
#' @examples
#' set.seed(7)
#' #'
#' object<-runskmeans(x = mgalda::datalearn$iris[,1:2], krange = 5:20)
#' autoplot(object,sepal_length,sepal_width)
#'
#' rm(list = ls())
#' @export
runskmeans <- function(x, ...) {
  UseMethod("runskmeans")
}

#' @rdname cluster_tools
#' @export
runskmeans.default <- function(x, ...) {
  generic_default()
}

runskmeans_impl <- function(x,k,criterion = "ch", runs = 25) {
  if (k > 1) {
    min_ss <- Inf
    kmopt <- NULL

    for (i in 1:runs) {
      kmm <-
        kmeans(
          x = x,
          centers = k,
          nstart = nsplits(nrow(x))
        )
      kmm <- compact(kmm)
      swss <- sum(kmm$withinss)
      if (swss < min_ss) {
        kmopt <- kmm
        min_ss <- swss
      }
    }
    crit <-
      switch(criterion,
             asw = suppressall(cluster_stats(
               as_tbl(x),
               kmopt$cluster
             )$clus_silwidth[k]),
             ch = calinhara(x, kmopt$cluster)
      )

    swss <- compact(swss)
    swss <- sum(kmm$withinss)
    if (swss < min_ss) {
      kmopt <- kmm
      min_ss <- swss
    }
  }

  c(kmm,crit = crit,swss = min_ss)
}
runskmeans_best <- function(km, krange, limit, criterion) {
  if (criterion == "asw") {
    data <-
      tibble(asw = map_dbl(km, ~ do_try(.x$crit) %||% -1), k = seq_along(km))
    res <-
      select_pctloss(
        x = data,
        k,
        metric = asw,
        maximize = T,
        limit = limit
      )
  } else {
    data <-
      tibble(ch = map_dbl(km, ~ do_try(.x$crit) %||% -1), k = seq_along(km))
    res <-
      select_pctloss(
        x = data,
        k,
        metric = ch,
        maximize = T,
        limit = limit
      )
  }
  res
}

#' @rdname cluster_tools
#' @export
runskmeans.data.frame <-
  function(x,
           krange = 5:10,
           criterion = c("ch", "asw"),
           runs = 25,
           limit = 10){
    x <- force(x)
    cli::cli_progress_step("Validacion y setup")

    kcall <- match.call()
    kcall[[1L]] <- NULL
    kcall <- lapply(kcall, eval,envir= env_call())
    criterion <- match.arg(criterion, c("ch", "asw"))
    args_k <- list(
      krange = 5:10,
      criterion = "ch",
      runs = 25,
      limit = 10
    )

    args_k <-
      args_k[!names(args_k) %in% names(kcall)]

    kcall <- c(kcall, args_k)

    if (!is.data.frame(x)) {
      cat_return_null(msg ="no es data frame")

    }

    kcall$criterion <- criterion
    krange <- as_int(krange)
    runs <- as_int(runs)

    if (!any(are_dblint(x))) {
      cat_return_null(msg ="no existen datatos numericos")

    }

    if (nrow(unique(x)) < min(krange)) {
      cat_return_null(msg ="mas centros que observaciones")

    }
    cli::cli_progress_done()

    prepro <- dclean(x)

    k <- 1

    km <- vector(mode = "list", length = max(krange))
    cli::cli_progress_step("Run kmeans { perc_format(which(k == krange)/length(krange) %||% 0) }", )
    for (k in krange) {
      cli::cli_progress_update()
      km[[k]] <-
        runskmeans_impl(
          x = prepro$dat,
          k = k,
          criterion = criterion,
          runs = runs
        )
    }

    res <-
      runskmeans_best(km = km,
                      krange = krange,
                      limit = limit,
                      criterion = criterion
      )

    cli::cli_progress_done()
    c1 <- km[[res$k]] %class% "kmeans"
    c1$bestk <- res$k


    if (inherits(c1, "try-error")) {
      out <- NULL
      cli::cli_alert_danger("No se encuentra un resultado.")
    } else {
      partition <- c1$cluster
      cl <- list()
      nc <- c1$bestk
      for (i in 1:nc) {
        cl[[i]] <- partition == i
      }
      out <-
        list(
          data_org = x,
          result = c1,
          nc = nc,
          clusterlist = cl,
          preprocess = prepro,
          partition = partition,
          criterion = criterion,
          clustermethod = "kmeans"
        )

      cli::cli_alert_success("fit. Mejor resultado { out$nc } cluster(s).")
    }

    class(out) <- c("runskmeans", "cluster_tools")

    out[["cluster_stats"]] <- cluster_stats(out)
    out

  }


#' @keywords internal
dclean <- function(x) {
  cli::cli_progress_step("Clean data")

  prepro <- list()
  if (any(are_dblint(x))) {
    cnt_sc <- xstandarice(x, where(is_dbl))
    prepro[["estandarizar"]] <- cnt_sc
    x <- predict(object = cnt_sc, new_data = x)
  }

  if (any(are_fctchr(x))) {
    dmy <- getdummy(.data = x, where(is_fctchr))
    prepro[["dummy"]] <- dmy
    x <- predict(object = dmy, new_data = x)
  }

  cli::cli_progress_done("Clean data")
  x <- as_df(as_mtx(x))
  list(dat = x, prepro = prepro)
}


#' @export
print.runskmeans <-
  function(x) {
    cat_title_head("Runs kmeans")
    cat_subtitle1("{ x$nc } cluster. con { x$criterion} criterio ")
    cat("\n")
    cat_subtitle2("distribuidos ")
    print(table(x$partition))
    cat("\n")
    print(cluster_stats(x = x))
  }


#' @method autoplot runskmeans
#' @export
autoplot.runskmeans <- function(object, x = NULL, y = NULL) {
  x <- enexpr(x)
  y <- enexpr(y)
  if (is.name(x)) {
    x <- lang2str(x)
  }
  if (is.name(y)) {
    y <- lang2str(y)
  }
  data <- object$data_org
  data$cluster <- factor(object$partition)
  grf_point(make_formula(lhs = y, rhs = x),
            color = ~cluster,
            data = data
  )
}

# cluster_boot ------------------------------------------------------------

#' cluster boots
#'
#' @name cluster_boot
#' @param x dataset
#' @param krange vector con la cantidad de cluster a evaluar
#' @param criterion criterio de desicion de cluster. "ch" or "asw".
#' Calinski-Harabasz criterion o average silhouette width.
#' @param runs n centros aleatorios iniciales
#' @param limit limite de desicion de seleccion para buscar
#' el modelo mas siemple entre con 'limit' corete
#' @param rep numero de repeticiones de la particion
#' @param times numero de particiones
#' @rdname cluster_tools
#' @examples
#' set.seed(7)
#'
#' object <-
#'   cluster_boot(
#'     x = mgalda::datalearn$iris[, 1:2],
#'     krange = 4:15,
#'     rep = 2,
#'     times = 4
#'   )
#' autoplot(object, sepal_length, sepal_width)
#'
#' rm(list = ls())
#' @export

cluster_boot <-
  function(x, ...) {
    UseMethod("cluster_boot")
  }

#' @rdname cluster_tools
#' @export
cluster_boot.default <-
  function(x, ...) {
    generic_default()
  }

#' @rdname cluster_tools
#' @export

cluster_boot.data.frame <-
  function(x,
           krange = 10:15,
           criterion = c("ch", "asw"),
           runs = 100,
           rep = 3,
           times = 10,
           limit = 10) {

    criterion <- match.arg(criterion, c("ch", "asw"))
    x <- force(x)

    if (!is.data.frame(x)) {
      cat_return_null(msg ="no es data frame")
      return(NULL)
    }

    if (!any(are_dblint(x))) {
      cat_return_null(msg ="no existen datatos numericos")
      return(NULL)
    }

    if (nrow(unique(x)) < min(krange)) {
      cat_return_null(msg ="mas centros que observaciones")
      return(NULL)
    }
    c1 <-
      do_call(
        f = "runskmeans",
        .args = c(
          x = list(x),
          criterion = criterion,
          krange = list(krange),
          limit = limit,
          runs = runs
        )
      )
    cli::cli_progress_step("Resamples")
    resamples <-
      do_call(f = "random_rset", .args = c(list(dataset = as_tbl(c1$preprocess$dat)),
                                           times = times,
                                           rep = rep
      ))$splits

    resamples <-
      map(resamples, ~ as_tbl(rsample::analysis(.x)))

    ecd <- ecd_cboot(data =c1$preprocess$dat)
    jsd <- jitter_cboot(
      data = c1$preprocess$dat,
      cod = cov(as.matrix(c1$preprocess$dat)),
      tuning = .05,
      ecd = ecd
    )

    resamples <-
      map(resamples, ~ as_tbl(res_cluster(
        data = .x,
        ecd = ecd,
        jsd = jsd,
        tuning = .05
      )))

    z <- 1
    cli::cli_progress_step("Run Boots { z }")

    centers <- map(
      .x = resamples,
      .f = ~ suppressall(kmeans(
        x = as_tbl(.x),
        centers = c1$nc
      )$centers
      ))

    z <- z + 1
    cli::cli_progress_update()
    btotss<-map_dbl(centers,.f = ~betw_totss(centers = .x,resamples = resamples))

    z <- z + 1
    cli::cli_progress_update()
    centers <- centers[is.finite(btotss)]

    btotss <- btotss[is.finite(btotss)]

    centers <- centers[[which.max(btotss)]]

    z <- z + 1
    cli::cli_progress_update()

    c1 <-
      list(
        scale_dataset =
          list(
            data = c1$preprocess$dat,
            data_original = x,
            preparacion = c1$preprocess$prepro
          ),
        nc = c1$nc,
        cluster_stats = c1$cluster_stats,
        clustermethod = c1$clustermethod,
        criterion = c1$criterion,
        result = suppressall(kmeans(x = c1$preprocess$dat,centers = centers))
      )

    z <- z + 1
    cli::cli_progress_update()
    c1$partition <- c1$result$cluster
    c1$centers <- c1$result$centers
    class(c1) <- c("cluster_boot", "cluster_tools")
    cli::cli_alert_success("fit boots.")
    c1

  }

#' @export
print.cluster_boot <- function(x) {
  cat_title_head("Cluster kmeans")
  cat_subtitle1("{ x$nc } cluster. con { x$criterion} criterio ")
  cat("\n")
  cat_subtitle2("distribuidos ")
  print(table(x$partition))
  cat("\n")
  print(cluster_stats(x = x))
  cat_endline()
}


#' @method autoplot cluster_boot
#' @export
autoplot.cluster_boot <- function(object, x = NULL, y = NULL) {
  x <- enexpr(x)
  y <- enexpr(y)
  if (is.name(x)) {
    x <- lang2str(x)
  }
  if (is.name(y)) {
    y <- lang2str(y)
  }
  data <- object$scale_dataset$data_original
  data$cluster <- factor(object$partition)
  grf_point(make_formula(lhs = y, rhs = x),
            color = ~cluster,
            data = data
  )
}


ecd_cboot <- function(data) {
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  ecd <- eigen(cov(data), symmetric = TRUE)
  ecd$values[ecd$values < 0] <- 0
  ecd$values[is.na(ecd$values)] <- 0
  ecd
}
jitter_cboot <- function(data, cod, tuning, ecd) {
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  jsd <- numeric(0)
  n <- nrow(data)
  rotdata <- data %*% solve(t(ecd$vectors))
  for (i in seq_col(data)) {
    sx <- sort(rotdata[, i])
    dx <- sx[2:n] - sx[1:(n - 1)]
    dx <- dx[dx > 0]
    jsd[i] <- quantile(dx, tuning)
  }
  jsd
}
res_cluster <- function(data, ecd, jsd, tuning = .05) {
  .res_boots <- function(data, ...) {
    bsamp <- sample(nrow(data), nrow(data), replace = TRUE)
    data[bsamp, ]
  }
  .res_subset <- function(data, ...) {
    bsamp <- sample(nrow(data), floor(nrow(data) / 2), replace = FALSE)
    data[bsamp, ]
  }
  .res_jitter <- function(data, ecd, jsd, ...) {
    jnoise_jitter <- matrix(0, ncol = ncol(data), nrow = nrow(data))
    for (j in seq_col(data)) {
      jnoise_jitter[, j] <- rnorm(nrow(data), sd = jsd[j])
    }
    jnoise_jitter <- jnoise_jitter %*% t(ecd$vectors)
    data + jnoise_jitter
  }
  .res_bojit <- function(data, ecd, jsd, ...) {
    bsamp <- sample(nrow(data), nrow(data), replace = TRUE)
    jnoise_bojit <-
      matrix(0, ncol = ncol(data), nrow = nrow(data))
    for (j in seq_col(data)) {
      jnoise_bojit[, j] <- rnorm(nrow(data), sd = jsd[j])
    }
    jnoise_bojit <- jnoise_bojit %*% t(ecd$vectors)
    data[bsamp, ] + jnoise_bojit
    data
  }
  .res_noise <- function(data, ecd, tuning, ...) {
    noiseind <- as_logical(rbinom(nrow(data), 1, tuning))
    nn <- sum(noiseind)
    jnoise <- matrix(0, ncol = ncol(data), nrow = nn)
    for (j in seq_col(data)) {
      jnoise[, j] <- runif(
        nn,
        min = -tuning * sqrt(ecd$values[j]),
        max = tuning * sqrt(ecd$values[j])
      )
    }
    jnoise <- t(t(jnoise %*% t(ecd$vectors)) + colMeans(data))
    mdata <- data
    #bsamp <- (1:nrow(data))[!noiseind]
    mdata[noiseind, ] <- jnoise
    mdata
  }
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  cl_method <-
    sample(c("boots", "bojit", "jitter", "subset", "noise"), 1)

  fn <- switch(cl_method,
               boots = .res_boots,
               bojit = .res_bojit,
               jitter = .res_jitter,
               subset = .res_subset,
               noise = .res_noise
  )

  fn(
    data = data,
    ecd = ecd,
    jsd = jsd,
    tuning = tuning
  )
}
betw_totss<-function(centers,resamples){
  km <- suppressall(map(resamples,~do_try(kmeans(x = .x,centers = centers))))
  mean(as.numeric(map(km,~.x$betweenss/.x$totss)))
}



# cluster_stats -----------------------------------------------------------

#' cluster_stats
#'
#' @rdname cluster_tools
#' @name cluster_stats
#' @examples
#' set.seed(7)
#'
#' cluster_stats(x = mgalda::datalearn$iris[, 1:4], clustering = kmeans(x = mgalda::datalearn$iris[, 1:4], centers = 5)$cluster)
#' @export
cluster_stats <-
  function(x, clustering, w = NULL, ...) {
    UseMethod("cluster_stats")
  }

#' @export
cluster_stats.default <-
  function(x, ...) {
    generic_default()
  }
#' @export
cluster_stats.data.frame <-
  function(x,
           clustering,
           w = NULL,
           clean_data = TRUE) {
    if (clean_data) {
      x <- dclean(x = x)$dat
    }

    nc <- vctrs::vec_unique_count(clustering)

    distance <- dist(x, method = "euclidean")

    dunn <- dunn(clustering, dmatrix = distance)

    sii <- cluster::silhouette(clustering, distance)
    sc <- summary(sii)
    wss <- weightedss(x, clustering)

    conn <- connectivity(clustering, dmatrix = distance)

    metrics <-
      list(
        n = length(clustering),
        cluster_number = nc,
        cluster_size = table(clustering),
        min.cluster.size = min(table(clustering)),
        intra_clust = dunn$intra_clust,
        inter_clust = dunn$inter_clust,
        max_intraclust = max(dunn$intra_clust, na.rm = TRUE),
        min_interclust = min(dunn$inter_clust, na.rm = TRUE),
        clus_silwidths = sc$clus.avg.widths,
        silwidth = sc$avg.width,
        dunn = dunn$dunn,
        connectivity = conn
      )

    out <- c(metrics, wss)
    class(out) <- c("cluster_stats", class(out))
    out
  }

#' @export
cluster_stats.cluster_tools <-
  function(x, ...) {
    if (!is_emptyna(x$cluster_stats)) {
      return(x$cluster_stats)
    }

    clustering <- x$partition
    x <- as_tbl(x$preprocess$dat)

    xplit <- split(x, clustering)

    xnrow <- vapply(xplit, nrow, numeric(1))

    if (any(xnrow > 100)) {
      for (i in seq_along(xnrow)) {
        if (xnrow[i] > 100) {
          xplit[[i]] <- slice_sample(.data = xplit[[i]], n = 100)
        }
      }

      x <- bind_rows(xplit, .id = "clustering")

      clustering <- as_int(x$clustering)
      x$clustering <- NULL
    }


    nc <- vctrs::vec_unique_count(clustering)

    distance <- dist(x, method = "euclidean")

    dunn <- dunn(clustering, dmatrix = distance)

    sii <- cluster::silhouette(clustering, distance)
    sc <- summary(sii)
    wss <- weightedss(x, clustering)

    conn <- connectivity(clustering, dmatrix = distance)

    metrics <-
      list(
        n = length(clustering),
        cluster_number = nc,
        cluster_size = table(clustering),
        min.cluster.size = min(table(clustering)),
        intra_clust = dunn$intra_clust,
        inter_clust = dunn$inter_clust,
        max_intraclust = max(dunn$intra_clust, na.rm = TRUE),
        min_interclust = min(dunn$inter_clust, na.rm = TRUE),
        clus_silwidths = sc$clus.avg.widths,
        silwidth = sc$avg.width,
        dunn = dunn$dunn,
        connectivity = conn
      )

    out <- c(metrics, wss)
    class(out) <- c("cluster_stats", class(out))
    out
  }


#' @export
print.cluster_stats <-
  function(x) {
    cat_subtitle1("Estadisticos")
    cat("\n")
    cat("\n")
    cat("    n               :", x$n, "\n")
    cat("    average between :", mean(x$bss_per_cluster), "\n")
    cat("    average within  :", mean(x$wss_per_cluster), "\n")
    cat("    dunn            :", x$dunn, "\n")
    cat("    avg silwidth    :", x$silwidth, "\n")
    cat(
      "    %bss            :",
      scales::label_percent(
        big.mark = ".",
        decimal.mark = ",",
        accuracy = .1
      )(x$bss / x$totss),
      "\n"
    )
    cat(
      "    %wss            :",
      scales::label_percent(
        big.mark = ".",
        decimal.mark = ",",
        accuracy = .1
      )(x$wss / x$totss),
      "\n"
    )
  }



# neig_centers ------------------------------------------------------------


#' neig_centers
#'
#'
#' @name neig_centers
#' @description encontrar el k neighbors mas cercano al centro
#'
#' @param x  cluster_tools o data.frame object
#' @param ... variables por las cuales se agrupara
#' @param neighbors cantidad de observaciones a buscar
#'
#' @rdname cluster_tools
#' @examples
#' set.seed(7)
#'
#' neig_centers(x = mgalda::datalearn$iris, species, neighbors = 1)
#' @export

neig_centers <-
  function(x, ...) {
    UseMethod("neig_centers")
  }

#' @export
neig_centers.default <-
  function(x, ...) {
    generic_default()
  }

#' @export
neig_centers.cluster_tools <-
  function(x, ..., neighbors = 1) {
    check_inherits(x, c("cluster_boot", "runskmeans"))

    #groups <- rlang::enexprs(...)
    data <- as_tbl(x$preprocess$dat)
    d_ori <- x$data_org
    d_ori$cluster <- x$partition
    data_groups <- dplyr::select(d_ori, ..., .data$cluster)
    #data_groups <- dplyr::select(d_ori, !!!groups, .data$cluster)
    g <- pull(tidyr::unite(data = data_groups, "g"), .data$g)
    data$groups <- g
    data$.row <- seq_along(g)

    centers <-
      data %>%
      dplyr::select(!.data$.row) %>%
      group_by(.data$groups) %>%
      summarise_all(mean) %>%
      nest(centers = !c(.data$groups))

    data <- data %>%
      nest(points = !c(.data$groups)) %>%
      inner_join(centers, by = "groups")
    data$points <-
      map2(
        .x = data$centers,
        .y = data$points,
        .f = ~ mutate(.y,
                      dist = RANN::nn2(
                        data = .x,
                        query = dplyr::select(.y, !.data$.row)
                      )$nn.dists[, 1]
        )
      )
    data <- data %>%
      unnest(.data$points) %>%
      dplyr::select(.data$.row,.data$groups, .data$dist) %>%
      arrange(.data$.row)

    out <- bind_cols(data, d_ori)%>%
      group_by(.data$groups, .data$cluster) %>%
      arrange(.data$dist) %>%
      mutate(dist = row_number()) %>%
      filter(dist <= neighbors) %>%
      ungroup() %>%
      dplyr::select(!c(.data$.row, .data$dist,.data$groups))


    class(out) <- c(class(out), "neig_centers")
    out
  }

#' @export
neig_centers.data.frame <-
  function(x, ..., neighbors = 1) {
    # groups <- rlang::enexprs(...)

    data <- dplyr::select(x, -c(...))

    data <-
      predict(xprepro(.data = data, everything(), pca = T), data)

    data_groups <- dplyr::select(x, ...)
    g <- pull(tidyr::unite(data = data_groups, "g"), g)
    data$groups <- g
    data$.row <- seq_along(g)

    centers <- data %>%
      dplyr::select(!.data$.row) %>%
      group_by(.data$groups) %>%
      summarise_all(mean) %>%
      nest(centers = !c(.data$groups))

    data <- data %>%
      nest(points = !c(.data$groups)) %>%
      inner_join(centers, by = "groups")

    data$points <-
      map2(
        .x = data$centers,
        .y = data$points,
        .f = ~ mutate(.y,
                      dist = RANN::nn2(
                        data = .x,
                        query = dplyr::select(.y, !.data$.row)
                      )$nn.dists[, 1]
        )
      )

    data <- data %>%
      unnest(.data$points) %>%
      dplyr::select(.data$.row, .data$dist) %>%
      arrange(.data$.row)

    out <- bind_cols(data, x, cluster = g) %>%
      group_by(.data$cluster) %>%
      arrange(.data$dist) %>%
      mutate(dist = row_number()) %>%
      filter(.data$dist <= neighbors) %>%
      ungroup() %>%
      dplyr::select(!c(.data$.row, .data$dist))
    class(out) <- c(class(out), "neig_centers")
    out
  }



# equal_size_cluster ------------------------------------------------------

#' equal_size_cluster
#'
#' @name equal_size_cluster
#'
#' @param x dataset
#' @param k numero de observaciones por cluster
#' @param max_rep max numero de repeticiones
#' @param no_improve numero de repeticiones continuas sin mejora
#' para detener el algotitmo
#' @param verbose print resultados del loop
#' @param tolerance tolerancia
#' @param statfn funcion a optimizar
#'
#' @rdname cluster_tools
#' @examples
#' set.seed(7)
#'
#' object <-
#'   equal_size_cluster(
#'     x = mgalda::datalearn$iris[, 1:2],
#'     k = 10,
#'     max_rep = 100
#'   )
#' autoplot(object, sepal_length, sepal_width)
#'
#' rm(list = ls())
#' @export

equal_size_cluster <- function(x, ...) {
  UseMethod("equal_size_cluster")
}

#' @export
equal_size_cluster.default <-
  function(x, ...) {
    generic_default()
  }
#' @export
equal_size_cluster.data.frame <-
  function(x,
           k = 5,
           max_rep = 20,
           no_improve = 5,
           verbose = TRUE,
           tolerance = .001,
           statfn = default_statfn_cluster) {
    cli::cli_progress_step("Setup")

    curr_stat <- 1e10
    no_imp <- 0
    iter <- 1

    k_init <-
      suppressMessages(cluster_boot(x = x, krange = k, rep = 5))
    data <- as_tbl(k_init$scale_dataset$data)

    newcenters <- centers <- as_tbl(k_init$centers)

    if (verbose) {
      cli::cli_progress_message("Iteraciones")
      cat("\n")
    } else {
      cli::cli_progress_step("Iter { iter }", msg_done = "Iters OK")
    }


    while (iter <= max_rep) {

      data_centr <-
        bind_cols(
          data,
          suppressWarnings(as_tbl(
            RANN::nn2(
              data = newcenters,
              query = get_data_noised(data)
            )$nn.dists
          ))
        )

      data_centr$assigned <- 0
      data_centr$index <- 1:nrow(data_centr)
      working <- data_centr
      n_round <- nrow(data) - (nrow(data) %% k)

      for (i in 1:n_round) {
        j <- if (i %% k == 0) {
          k
        } else {
          (i %% k)
        }
        itemloc <-
          working$index[which(working[, (paste0("V", j))] ==
                                min(working[, (paste0("V", j))]))[1]]

        data_centr$assigned[data_centr$index == itemloc] <- j

        working <- working %>% filter(.data$index != itemloc)
      }

      if (nrow(working) > 0) {
        for (i in 1:nrow(working)) {
          data_centr$assigned[data_centr$index == working$index[i]] <-
            which(working[i, (paste0("V", 1:k))] == min(working[i, (paste0("V", 1:k))]))
        }
      }

      new_stat <- statfn(data, data_centr[["assigned"]])


      if (new_stat < curr_stat * (1 - tolerance)) {

        curr_stat <- new_stat
        centers <-
          data_centr %>%
          dplyr::select(.data$assigned, all_of(colnames(centers))) %>%
          group_by(.data$assigned) %>%
          summarise_all(mean) %>%
          ungroup() %>%
          dplyr::select(all_of(colnames(centers)))

        newcenters <- centers
        no_imp <- 0
      } else {

        no_imp <- no_imp + 1
        if (no_imp %% 2 == 0) {
          newcenters <-
            kmeans(
              x = get_data_noised(get_data_random(data)),
              centers = k,
              nstart = ncase(nrow(data))
            )$centers
          newcenters <- as_tbl(newcenters)
        } else {
          newcenters <- get_data_noised(centers)
        }
      }

      if (verbose) {
        cat_message("iter { iter }; wss : { round_sign(curr_stat) }; no improve = { no_imp }",
                    .envir = env_curr()
        )
      } else {
        cli::cli_progress_update()
      }


      if (no_imp >= no_improve) {
        iter <- max_rep + 1
      }
      iter <- iter + 1
    }

    if (verbose) {
      cli::cli_progress_message("Fin de iteraciones")
    } else {
      cli::cli_progress_done()
    }


    out <-
      list(
        data_org = x,
        result = cluster_stats(
          x = k_init$scale_dataset$data,
          clustering = data_centr[["assigned"]],
          clean_data = F
        ),
        nc = k,
        preprocess = k_init$scale_dataset$preparacion,
        partition = data_centr[["assigned"]],
        clustermethod = "equal_size"
      )
    cli::cli_alert_success("fit equal size cluster.")
    class(out) <- c("equal_size_cluster", "cluster_tools")
    out

  }


#' @export
print.equal_size_cluster <- function(x) {
  rng <-
    function(x) {
      x <- range(table(x))
      paste0(
        "[",
        num_format(x[1]),
        "-",
        num_format(x[2]),
        "]"
      )
    }
  cat_title_head("Equal size Cluster (kmean based)")
  cat_subtitle2("Se consideran { x$nc } cluster con { rng(x$partition) } tamanos ")
  cat("\n")
  print(x$result)
}

#' @method autoplot equal_size_cluster
#' @export
autoplot.equal_size_cluster <- function(object, x = NULL, y = NULL) {
  x <- enexpr(x)
  y <- enexpr(y)
  if (is.name(x)) {
    x <- lang2str(x)
  }
  if (is.name(y)) {
    y <- lang2str(y)
  }
  data <- object$data_org
  data$cluster <- factor(object$partition)
  grf_point(make_formula(lhs = y, rhs = x),
            color = ~cluster,
            data = data
  )
}


#' @export
default_statfn_cluster <- function(data, clustering) {
  x <-
    map(split(data, clustering),
        ~ as_tbl(RANN::nn2(.x)$nn.dists),
        diag = F
    )

  sum(map_dbl(x, ~ sum(rowMeans(
    dplyr::select(.x, !V1)
  ))))
}
get_data_noised <- function(data) {
  .res_cluster <- function(data, ecd, jsd, tuning = .05) {
    .res_jitter <- function(data, ecd, jsd, ...) {
      jnoise_jitter <- matrix(0, ncol = ncol(data), nrow = nrow(data))
      for (j in seq_col(data)) {
        jnoise_jitter[, j] <- rnorm(nrow(data), sd = jsd[j])
      }
      jnoise_jitter <- jnoise_jitter %*% t(ecd$vectors)
      data + jnoise_jitter
    }
    .res_bojit <- function(data, ecd, jsd, ...) {
      bsamp <- sample(nrow(data), nrow(data), replace = TRUE)
      jnoise_bojit <-
        matrix(0, ncol = ncol(data), nrow = nrow(data))
      for (j in seq_col(data)) {
        jnoise_bojit[, j] <- rnorm(nrow(data), sd = jsd[j])
      }
      jnoise_bojit <- jnoise_bojit %*% t(ecd$vectors)
      data[bsamp, ] + jnoise_bojit
      data
    }
    .res_noise <- function(data, ecd, tuning, ...) {
      noiseind <- as_logical(rbinom(nrow(data), 1, tuning))
      nn <- sum(noiseind)
      jnoise <- matrix(0, ncol = ncol(data), nrow = nn)
      for (j in seq_col(data)) {
        jnoise[, j] <- runif(
          nn,
          min = -tuning * sqrt(ecd$values[j]),
          max = tuning * sqrt(ecd$values[j])
        )
      }
      jnoise <- t(t(jnoise %*% t(ecd$vectors)) + colMeans(data))
      data[noiseind, ] <- jnoise
      data
    }
    if (!is.matrix(data)) {
      data <- as.matrix(data)
    }
    cl_method <-
      sample(c("bojit", "jitter", "noise"), 1)

    fn <- switch(cl_method,
                 bojit = .res_bojit,
                 jitter = .res_jitter,
                 noise = .res_noise
    )

    fn(
      data = data,
      ecd = ecd,
      jsd = jsd,
      tuning = tuning
    )
  }
  ecd <- ecd_cboot(data = data)
  jsd <- jitter_cboot(
    data = data,
    cod = cov(as.matrix(data)),
    tuning = .05,
    ecd = ecd
  )

  as_tbl(.res_cluster(
    data = data,
    ecd = ecd,
    jsd = jsd,
    tuning = .05
  ))
}
get_data_random <- function(x) {
  x <- eval(bquote_w(x),envir = env_call())
  x<-random_rset(dataset = x,times = 5,rep = 5)
  x <- slice_sample(group_by(x, id2), n = 1)$splits
  slice_sample(purrr::map_dfr(x, rsample::analysis), prop = .5)
}




# optimize_cluster ---------------------------------------------------------

#' optimize_cluster
#'
#' @name cluster_tools
#'
#' @param .data dataset
#' @param threshold  tolerancia
#' @param times particiones
#' @param rep repeticiones
#' @param interval intervalo de busqueda
#'
#' @rdname cluster_tools
#'
#' @examples
#' set.seed(7)
#'
#' optimize_cluster(.data = mgalda::datalearn$iris, interval = c(4, 10))
#'
#' rm(list = ls())
#' @export
optimize_cluster <-
  function(.data,
           threshold = .1,
           times = 5,
           rep = 3,
           interval = c(2, 25)) {
    dat <- as_tbl(dclean(.data)$dat)

    get_withinss <- function(x) {
      k <-
        map(dataresamples, ~ kmeans(x = .x, centers = round(x)))
      k <- mean(map_dbl(k, ~ .x$tot.withinss / .x$totss))
      round(x = abs(k - threshold), digits = 2)
    }

    op <- c()
    i <- 1
    cli::cli_progress_step("iter optimize { perc_format(i/rep) }")
    for (i in 1:rep) {
      dataresamples <-
        eval_expr(random_rset(
          dataset = !!dat,
          times = !!times,
          rep = !!rep
        )$splits)

      dataresamples <- map(dataresamples, rsample::analysis)
      cli::cli_progress_update()
      op[i] <- optimize(
        f = get_withinss,
        interval = interval,
        maximum = F
      )$minimum
    }
    cli::cli_progress_done()
    cli::cli_alert_success("fit.
                           { trunc(mean(op, na.rm = TRUE)) } cluster(s).")
    trunc(mean(op, na.rm = TRUE))
  }


# Hierachical clustering --------------------------------------------------

#' Hierachical clustering
#' Convenient methods for hierachical clustering
#'
#' @param x data frame
#' @param method method to use, see \code{\link{hclust}}
#' @param metric distance metric to use, see \code{\link{dist}}
#' @param n number of clusters to retrieve, see \code{\link{cut}}
#' @export
hierarchical <- function(x, method = "complete", metric = "euclidean", n = 5) {
  if (is.null(dim(x)) | is_dblint(x)) {
    x <- data.frame(x = x)
  }

  if (any(are_fctchr(x))) {
    x <- do_getdummy(.data = x, where(is_fctchr))
  }

  if (ncol(x) > 5) {
    x <-
      do_select_pca(
        .data = x,
        everything(),
        threshold = .85,
        num_comp = 5
      )
  }

  if (metric == "correlation") {
    x <- scale(x)
    metric <- "euclidean"
  }
  as_vct(cutree(hclust(dist(x, metric), method = method), k = n))
}


# Hierarchical k-means clustering -----------------------------------------


#' Hierarchical k-means clustering
#'
#' @examples
#' # Scale the data
#' df <- scale(iris[,1:4])
#'
#' # Compute hierarchical k-means clustering
#' res.hk <- hkmeans(df, 4)
#'
#' # Elements returned by hkmeans()
#' names(res.hk)
#'
#' # Print the results
#' res.hk
#' @export
hkmeans <-
  function(x,
           k,
           hc.metric = "euclidean",
           hc.method = "ward.D2",
           iter.max = 10,
           km.algorithm = "Hartigan-Wong") {
    res.hc <-
      stats::hclust(stats::dist(x, method = hc.metric), method = hc.method)
    grp <- stats::cutree(res.hc, k = k)
    clus.centers <- stats::aggregate(x, list(grp), mean)[, -1]
    res.km <- kmeans(x,
                     centers = clus.centers,
                     iter.max = iter.max,
                     algorithm = km.algorithm
    )
    class(res.km) <- "hkmeans"
    res.km$data <- x
    res.km$hclust <- res.hc
    res.km$clus.centers <- clus.centers
    res.km
  }



#' @export
print.hkmeans <- function(x, ...) {
  cat(
    "Hierarchical K-means clustering with ",
    length(x$size),
    " clusters of sizes ",
    paste(x$size, collapse = ", "),
    "\n",
    sep = ""
  )
  cat("\nCluster means:\n")
  print(x$centers, ...)
  cat("\nClustering vector:\n")
  print(x$cluster, ...)
  cat("\nWithin cluster sum of squares by cluster:\n")
  print(x$withinss, ...)
  ratio <- sprintf(
    " (between_SS / total_SS = %5.1f %%)\n",
    100 * x$betweenss / x$totss
  )
  cat(sub(".", getOption("OutDec"), ratio, fixed = TRUE),
      "Available components:\n",
      sep = "\n"
  )
  print(names(x))
  if (!is.null(x$ifault) && x$ifault == 2L) {
    cat("Warning: did *not* converge in specified number of iterations\n")
  }
  invisible(x)
}
#' @export
hkmeans_tree <- function(hkmeans, rect.col = NULL, ...) {
  res.hk <- hkmeans
  if (is.null(rect.col)) {
    rect.col <- unique(res.hk$cluster)
  }
  plot(res.hk$hclust,
       hang = -1,
       sub = "",
       xlab = "",
       ...
  )
  k <- length(unique(res.hk$cluster))
  stats::rect.hclust(res.hk$hclust, k = k, border = rect.col)
}


# knn_classify ------------------------------------------------------------

#' with classifly.
#'
#' @param formula classification formula
#' @param data training data set
#' @param k number of neighbours to use
#' @keywords classif
#' @export
knn_classify <- function(data, ...) {
  UseMethod("knn_classify")
}

#' @export
knn_classify.default <- function(data, ...) {
  generic_default()
}

#' @export
knn_classify.data.frame <- function(data, formula, k = 2) {
  assert_data.frame(x = data, severity = "stop")
  structure(list(
    terms = terms(formula, data = data),
    data = data,
    k = k
  ), class = "knn_classify")
}

#' @export
predict.knn_classify <- function(object, new_data, ...) {
  variables <- function(model) {
    list(
      response = all.vars(model$terms[[2]]),
      predictors = all.vars(model$terms[[3]])
    )
  }
  .lv <- function(x) {
    lvs <- levels(x)
    x <- as_chr(x)
    x <- nullattr(x)
    factor(x, lvs)
  }


  v <- variables(object)


  preds <- class::knn(
    as_mtx(object$data[, v$predictors]),
    as_mtx(new_data[, v$predictors]),
    object$data[[v$response]],
    k = object$k,
    prob = TRUE
  )

  tibble(
    .pred_class = .lv(preds),
    .pred_prob = attr(preds, "prob")
  )
}


# otros -----------------------------------------------------------------------------

#' @keywords internal
dunn <-
  function(clustering,
           dmatrix = NULL,
           method = "euclidean") {
    if ("dist" %in% class(dmatrix)) {
      distance <- as_mtx(dmatrix)
    } else {
      distance <- as_mtx(dist(dmatrix))
    }
    nc <- max(clustering)
    inter_clust <- matrix(NA, nc, nc)
    intra_clust <- rep(NA, nc)
    for (i in 1:nc) {
      c1 <- which(clustering == i)
      for (j in i:nc) {
        if (j == i) {
          intra_clust[i] <- max(distance[c1, c1])
        }
        if (j > i) {
          c2 <- which(clustering == j)
          inter_clust[i, j] <- min(distance[c1, c2])
        }
      }
    }
    list(
      dunn = min(inter_clust, na.rm = TRUE) / max(intra_clust, na.rm = TRUE),
      inter_clust = inter_clust,
      intra_clust = intra_clust
    )
  }


#' @keywords internal
weightedss <- function(x, clustering, w = NULL) {
  scol <- ncol(x)
  x <- as_tbl(x)

  if (is.null(w)) {
    w <- rep(1, scol)
  }
  k <- length(unique(clustering))
  x_scaled <- t(t(x) * sqrt(w))
  cluster_centers <-
    stats::aggregate(x_scaled,
                     by = list(clustering),
                     FUN = mean
    )[, -1]
  cluster_densities <- table(clustering)
  matrix_wss <- t(sapply(
    1:k,
    FUN = wss_components,
    x = x_scaled,
    cl = clustering,
    simplify = T
  ))

  if (scol == 1) {
    cluster_centers <-
      matrix(cluster_centers,
             nrow = k,
             ncol = scol,
             byrow = TRUE
      )

    cluster_densities <-
      matrix(
        cluster_densities,
        nrow = k,
        ncol = scol,
        byrow = TRUE
      )
  }
  matrix_bss <-
    matrix(
      apply(x_scaled, 2, mean),
      nrow = k,
      ncol = scol,
      byrow = T
    )

  matrix_bss <- (cluster_centers - matrix_bss)^2 * cluster_densities

  wss_per_feature <- apply(matrix_wss, 2, sum)
  wss_per_cluster <- apply(matrix_wss, 1, sum)
  bss_per_feature <- apply(matrix_bss, 2, sum)
  bss_per_cluster <- apply(matrix_bss, 1, sum)
  bss <- sum(bss_per_cluster)
  wss <- sum(wss_per_cluster)
  names(bss_per_feature) <- colnames(x)
  names(wss_per_feature) <- colnames(x)
  names(bss_per_cluster) <- 1:k
  if (is_num(x = wss_per_cluster)) {
    wss_per_cluster <- rep(0, k)
  }

  names(wss_per_cluster) <- 1:k
  list(
    bss_per_feature = bss_per_feature,
    wss_per_feature = wss_per_feature,
    bss_per_cluster = bss_per_cluster,
    wss_per_cluster = wss_per_cluster,
    bss = bss,
    wss = wss,
    totss = bss + wss
  )
}

#' @keywords internal
wss_components <- function(k, x, cl) {
  res <- x[cl == k, ]
  if (is.null(dim(res)) == F) {
    res <- apply(res, 2, scale, center = T, scale = F)
    res <- apply(res, 2, function(x) {
      sum(x^2)
    })
  } else {
    res <- rep(0, dim(x)[2])
  }
  return(res)
}

#' @keywords internal
connectivity <-
  function(clustering,
           dmatrix = NULL,
           method = "euclidean",
           neighb_size = trunc(length(clustering) * .025)) {
    if ("dist" %in% class(dmatrix)) {
      distance <- as_mtx(dmatrix)
    } else {
      distance <- as_mtx(dist(dmatrix))
    }

    nearest <-
      apply(distance, 2, function(x) {
        sort(x, ind = TRUE)$ix[2:(neighb_size + 1)]
      })

    if (!is_df(nearest)) {
      nearest <- as_mtx(nearest)
    }

    nr <- nrow(nearest)
    nc <- ncol(nearest)
    same <- matrix(clustering,
                   nrow = nr,
                   ncol = nc,
                   byrow = TRUE
    ) !=
      matrix(clustering[nearest], nrow = nr, ncol = nc)
    sum(same * matrix(1 / 1:neighb_size, nrow = nr, ncol = nc))
  }


#' @keywords internal
calinhara <-
  function(x, clustering) {
    cn <- max(clustering)
    x <- as_mtx(x)
    p <- ncol(x)
    n <- nrow(x)
    cln <- rep(0, cn)
    .w <- matrix(0, p, p)
    for (i in 1:cn) {
      cln[i] <- sum(clustering == i)
    }
    for (i in 1:cn) {
      clx <- x[clustering == i, ]
      cclx <- cov(as_mtx(clx))
      if (cln[i] < 2) {
        cclx <- 0
      }
      .w <- .w + ((cln[i] - 1) * cclx)
    }
    .s <- (n - 1) * cov(x)
    .b <- .s - .w
    (n - cn) * sum(diag(.b)) / ((cn - 1) * sum(diag(.w)))

  }

#' @export
ellipse <-
  function(data,
           npoints = 1000,
           cl = 0.95,
           mean = colMeans(data),
           cov = var(data),
           df = nrow(data)) {
    norm.vec <- function(x) {
      x / sqrt(sum(x^2))
    }

    p <- length(mean)
    ev <- eigen(cov)

    sphere <- matrix(rnorm(npoints * p), ncol = p)
    cntr <- t(apply(sphere, 1, norm.vec))

    cntr <- cntr %*% diag(sqrt(ev$values)) %*% t(ev$vectors)
    cntr <- cntr * sqrt(p * (df - 1) * qf(cl, p, df - p) / (df * (df - p)))
    if (!missing(data)) {
      colnames(cntr) <- colnames(data)
    }

    cntr + rep(mean, each = npoints)
  }


