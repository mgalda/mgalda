#' @include util_env.R
NULL


# group-generics -

#' random_args
#'
#' @name distr_generic_n_ops
#' @rdname distr_generic_n_ops
#'
#' @examples
#'
#'
#' vec_cont <- xrandom(n = 1000, mean = 2, sd = 5)
#' vec_cont <- safe_vec(vec_cont)
#' vec_dist <- rbinom(1000, 20:35, .5)
#'
#' dist_cont <- density_integrate(x = vec_cont)
#' dist_dist <- density_integrate(x = vec_dist)
#'
#' dist_cont+dist_dist
#' dist_cont>dist_dist
#' dist_cont<=dist_dist
#' dist_dist*2
#' dist_dist-2
#' abs(dist_cont)
#' sign(dist_cont)
#'
#'
#' vec_cont<-xrandom(n = 1000,mean = 20,sd = 3)
#' vec_cont <- safe_vec(vec_cont)
#' vec_dist <- rbinom(1000,20:35,.5)
#'
#' dist_cont <- density_integrate(x = vec_cont)
#' dist_dist <- density_integrate(x = vec_dist)
#'
#' resupport_distribution(f = dist_cont,support = c(18,22),method = "reflect")
#' retype_distribution(f = dist_cont,type = "discrete",method = "piecelin")
#' regrid_distribution(f = dist_cont,n_grid = 1000)
#' mix_distribution(f_list = list(dist_dist,dist_cont))
#' smooth_distribution(f = dist_cont,n_sample = 100)
#' estimate_distribution(f = dist_dist,stat = mean,sample_size = 100)
#' recenter_distribution(f = dist_cont,to = 10)
#' respread_distribution(f = dist_cont,to = 10)
#' #tail_distribution(f = dist_cont,level = .5,method = "trim")
#'
#'
#' distrstats_interval(f = dist_cont)
#' distrstats_center(f = dist_cont,method = "mean")
#' distrstats_center(f = dist_cont,method = "mode")
#' distrstats_moment(f = dist_cont,order = 1)
#' distrstats_spread(f = dist_cont,method = "var")
#' distrstats_spread(f = dist_cont,method = "mad")
#'
#' distrstats_interval(f = dist_dist)
#' distrstats_center(f = dist_dist,method = "mean")
#' distrstats_center(f = dist_dist,method = "mode")
#' distrstats_moment(f = dist_dist,order = 1)
#' distrstats_spread(f = dist_dist,method = "var")
#' distrstats_spread(f = dist_dist,method = "mad")
#'
#' distrstats_separation(f = dist_dist,g = dist_cont)
#'
#'
#'

NULL


#' @export
Summary.distrobj <- function(..., na.rm = FALSE) {
   summary_allany <- function(gen, f_list) {
      if (lgl_all(f_list, inherits, what = "density_integrate")) {
         pp <- f_list[[1]]$prob[which.max(f_list[[1]]$cumprob)]
         switch(gen,
                all = ifelse(pp == 1, TRUE, FALSE),
                any = ifelse(pp > 0, TRUE, FALSE)
         )
      } else {
         d_zero <- density_integrate(rep(0, 1000))

         prob_false <- vapply(f_list, function(f) {
            prob_equal(f, d_zero)
         }, numeric(1))

         switch(gen,
                all = boolean_dist(prod(1 - prob_false)),
                any = boolean_dist(1 - prod(prob_false))
         )
      }
   }
   summary_sumprod <- function(gen, f_list) {
      lq_f <- lapply(f_list, as_quan)
      lp_f <- lapply(f_list, as_dens)
      nf <- length(f_list)

      xnum <- lapply(lq_f, function(f) {
         round(f(ppoints(1e5 / nf)), 5)
      })

      xnum <- unique(unlist(xnum))

      mnum <- sapply(lp_f, function(f) {
         f(xnum)
      })

      mnum <- apply(mnum, 1, get(gen))

      as_density_integrate(data.frame(
         x = xnum,
         y = mnum,
         prob = mnum
      ))
   }

   summary_impl <- function(gen, f_list) {
      lq_f <- lapply(f_list, as_quan)
      lp_f <- lapply(f_list, as_dens)
      nf <- length(f_list)

      xnum <- lapply(lq_f, function(f) {
         round(f(ppoints(1e5 / nf)), 5)
      })

      get(gen)(unlist(xnum))

   }

   e_list <- list(...)

   lgl_e <- vapply(e_list, is_single_number, logical(1))
   e_list[lgl_e] <-
      lapply(e_list[lgl_e], function(x) {
         density_integrate(rep(x, 1000))
      })
   lgl_e <-
      vapply(e_list, function(x) {
         !inherits(x, "density_integrate")
      }, logical(1))

   e_list[lgl_e] <-
      lapply(e_list[lgl_e], coerce2density_integrate)


   if (.Generic %in% c("all", "any")) {
      out <- summary_allany(gen = .Generic, f_list = e_list)
   } else if (.Generic %in% c("sum", "prod")) {
      out <- summary_sumprod(gen = .Generic, f_list = e_list)
   } else {
      out <- summary_impl(gen = .Generic, f_list = e_list)
   }
   out
}

#' @rdname distr_generic_n_ops
#' @export
Math.distrobj <- function(x, ...) {
   math_abs <- function(f) {
      x <- abs(random_dist(f, 100000))
      density_integrate(x, from = 0)
   }
   math_sign <- function(f) {
      p_f <- as_prob(f)
      d_f <- as_dens(f)

      prob_zero <- d_f(0)

      prob_leq_zero <- p_f(0)

      x <- c(-1, 0, 1)
      prob <-
         c(prob_leq_zero - prob_zero, prob_zero, 1 - prob_leq_zero)

      density_integrate(sample(x, 10000, T, prob))
   }

   math_pdqr_impl <- function(gen, f, ...) {
      p <- ppoints(1e5)
      f_d <- as_dens(f)
      f_q <- as_quan(f)

      x <- f_q(p)
      x <- get(gen)(x, ...)
      density_integrate(x[!is_nonnum(x)])
   }


   if (!inherits(x, c("density_integrate"))) {
      x <- coerce2density_integrate(x)
   }
   # Speed optimization (skips possibly expensive assertions)
   switch(.Generic,
          abs = math_abs(x),
          sign = math_sign(x),
          math_pdqr_impl(.Generic, x, ...)
   )
}

#' @rdname distr_generic_n_ops
#' @export
Ops.distrobj <- function(e1, e2) {
   ocall <- rlang::call_args(match.call())
   if (missing(e1)) {
      e1 <- e2
      e2 <- NULL
      ocall$e1 <- ocall$e2
      ocall$e2 <- NULL
   }

   if (is_dblint(e1)) {
      tmp <- e1
      e1 <- e2
      ocall$e1 <- ocall$e2
      e2 <- tmp
      ocall$e2 <- tmp
   }

   if (!inherits(e1, "density_integrate")) {
      e1 <- coerce2density_integrate(e1)
   }

   if (is_empty(e2)) {
      out <- switch(
         .Generic,
         `+` = e1,
         `-` = suppressall(reflect_around_zero(e1)),
         `!` = suppressall(negate_distr(e1))
      )
      ocall$e1 <- e1
      ocall$gen <- .Generic
   } else {
      if (!inherits(e2, c("density_integrate", "numeric", "integer"))) {
         e2 <- coerce2density_integrate(e2)
      }


      e_list <- list(e1, e2)

      lgl_e <- vapply(e_list, is_single_number, logical(1))

      if (any(!lgl_e)) {
         ocall[!lgl_e] <- e_list[!lgl_e]
      }
      ocall$gen <- .Generic

      if (is_ops_linear(.Generic, e1, e2)) {
         return(suppressall(ops_linear(.Generic, e1, e2)))

      } else if (.Generic %in% c("&", "|")) {
         return(suppressall(ops_logic(.Generic, e1, e2)))

      }


      if (.Generic %in% c(">=", ">", "<=", "<", "==", "!=")) {
         out <- ops_compare(.Generic, e1, e2)
      } else {
         e_list[lgl_e] <-
            lapply(e_list[lgl_e], function(x) {
               density_integrate(rep(x, 1000))
            })
         e1 <- e_list[[1]]
         e2 <- e_list[[2]]

         n_sample <- 10000
         args_new <- list()



         res <- trasnfor_random_distribution(
            f_list = e_list,
            trans = get(.Generic),
            n_sample = n_sample,
            args_new = args_new
         )


         # Ensure that `res` doesn't have values outside of reasonable support

         return(repair_group_gen_support(res, .Generic, supp_list = lapply(e_list, distfn_domain_num)))
      }

   }
   attr(out, "call") <- ocall
   class(out) <- c("distrobj", "ops_distr", "density_integrate")
   out
}

#' @export
print.ops_distr <- function(x) {
   xc <- attr(x, "call")
   if (length(xc) == 2) {
      d_e1 <- mgalda::num_format(x = range(xc$e1$x))
      d_e1 <- paste0("[", d_e1[1], " , ", d_e1[2], "]")
      d_e1 <- paste("e1 ", d_e1)


      d_gen <- paste(xc$gen, paste0("'", d_e1, "'"))
   } else {
      d_e1 <- mgalda::num_format(x = range(xc$e1$x))
      d_e1 <- paste0("[", d_e1[1], " , ", d_e1[2], "]")
      d_e1 <- paste("e1 ", d_e1)

      if (is.numeric(xc$e2)) {
         d_e2 <- xc$e2
      } else {
         d_e2 <- mgalda::num_format(x = range(xc$e2$x))
         d_e2 <- paste0("[", d_e2[1], " , ", d_e2[2], "]")
         d_e2 <- paste("e2 ", d_e2)
      }
      d_gen <-
         paste(paste0("'", d_e1, "'"), xc$gen, paste0("'", d_e2, "'"))
      pp <- x$prob[x$x == 1]
      pp <- mgalda::perc_format(ifelse(is_emptyna(pp),0,pp))
      d_gen <-
         paste(d_gen, ": prob ~", pp)
      d_gen
   }
   print(d_gen)
   invisible()
}

#' @keywords internal
repair_group_gen_support <-
   function(f, gen, supp_list, repair_supp_method = "reflect") {
      new_f_supp <- switch(gen,
                           sqrt = sqrt(pmax(0, supp_list[[1]])),
                           floor = floor(supp_list[[1]]),
                           ceiling = ceiling(supp_list[[1]]),
                           trunc = trunc(supp_list[[1]]),
                           round = round(supp_list[[1]]),
                           signif = signif(supp_list[[1]]),
                           exp = exp(supp_list[[1]]),
                           log = inf_to_na(log(supp_list[[1]])),
                           expm1 = expm1(supp_list[[1]]),
                           log1p = inf_to_na(log1p(supp_list[[1]])),
                           cos = repair_supp_periodic(
                              op = `cos`,
                              supp = supp_list[[1]],
                              ref_points = c(1, 2) * pi,
                              period = 2 * pi
                           ),
                           sin = repair_supp_periodic(
                              op = `sin`,
                              supp = supp_list[[1]],
                              ref_points = c(-1, 1) * 0.5 * pi,
                              period = 2 * pi
                           ),
                           tan = repair_supp_periodic(
                              op = `tan`,
                              supp = supp_list[[1]],
                              # Inside nudges are needed to explicitly state side of special points
                              ref_points = c(-1, 1) * 0.5 * pi + 1e-15 * c(1, -1),
                              period = pi
                           ),
                           cospi = repair_supp_periodic(
                              op = `cospi`,
                              supp = supp_list[[1]],
                              ref_points = c(1, 2),
                              period = 2
                           ),
                           sinpi = repair_supp_periodic(
                              op = `sinpi`,
                              supp = supp_list[[1]],
                              ref_points = c(-1, 1) * 0.5,
                              period = 2
                           ),
                           tanpi = repair_supp_periodic(
                              op = `tanpi`,
                              supp = supp_list[[1]],
                              # Inside nudges are needed to explicitly state side of special points
                              ref_points = c(-1, 1) * 0.5 + 1e-15 * c(1, -1),
                              period = 1
                           ),
                           acos = repair_supp_monotone(
                              op = `acos`,
                              supp = supp_list[[1]],
                              input_bounds = c(-1, 1),
                              increasing_op = FALSE
                           ),
                           asin = repair_supp_monotone(
                              op = `asin`,
                              supp = supp_list[[1]],
                              input_bounds = c(-1, 1)
                           ),
                           atan = atan(supp_list[[1]]),
                           cosh = repair_supp_cosh(supp_list[[1]]),
                           sinh = sinh(supp_list[[1]]),
                           tanh = tanh(supp_list[[1]]),
                           acosh = repair_supp_monotone(
                              op = `acosh`,
                              supp = supp_list[[1]],
                              input_bounds = c(1, Inf)
                           ),
                           asinh = asinh(supp_list[[1]]),
                           atanh = repair_supp_monotone(
                              op = `atanh`,
                              supp = supp_list[[1]],
                              input_bounds = c(-1, 1)
                           ),
                           lgamma = simulate_repair_supp(`lgamma`, supp_list),
                           gamma = simulate_repair_supp(`gamma`, supp_list),
                           digamma = simulate_repair_supp(`digamma`, supp_list),
                           trigamma = simulate_repair_supp(`trigamma`, supp_list),
                           `+` = supp_list[[1]] + supp_list[[2]],
                           `-` = repair_supp_subtraction(supp_list[[1]], supp_list[[2]]),
                           `*` = repair_supp_multiplication(supp_list[[1]], supp_list[[2]]),
                           `/` = repair_supp_division(supp_list[[1]], supp_list[[2]]),
                           `^` = simulate_repair_supp(`^`, supp_list),
                           `%%` = simulate_repair_supp(`%%`, supp_list),
                           `%/%` = floor(repair_supp_division(supp_list[[1]], supp_list[[2]])),
                           sum = repair_supp_sum(supp_list),
                           prod = repair_supp_prod(supp_list),
                           min = repair_supp_min(supp_list),
                           max = repair_supp_max(supp_list),
                           meta_support(f)
      )

      resupport_distribution(f, support = new_f_supp, method = repair_supp_method)
   }

# compare -

#' @keywords internal
ops_compare <- function(gen, e1, e2) {
   if (is_single_number(e2)) {
      r_e1 <- random_dist(e1, n = 10000)

      pp <- switch(gen,
                   `>=` = sum(r_e1 >= e2),
                   `>`  = sum(r_e1 > e2),
                   `<=` = sum(r_e1 <= e2),
                   `<`  = sum(r_e1 < e2),
                   `==` = sum(r_e1 == e2),
                   `!=` = sum(r_e1 != e2)
      )
      boolean_dist(prob_true = pp / 10000)
   } else if (is_single_number(e1)) {
      r_e2 <- random_dist(e2, n = 10000)

      pp <- switch(gen,
                   `>=` = sum(e1 >= r_e2),
                   `>`  = sum(e1 > r_e2),
                   `<=` = sum(e1 <= r_e2),
                   `<`  = sum(e1 < r_e2),
                   `==` = sum(e1 == r_e2),
                   `!=` = sum(e1 != r_e2)
      )
      boolean_dist(prob_true = pp / 10000)
   } else {
      switch(gen,
             `>=` = form_geq(e1, e2),
             `>`  = form_greater(e1, e2),
             `<=` = form_leq(e1, e2),
             `<`  = form_less(e1, e2),
             `==` = form_equal(e1, e2),
             `!=` = form_not_equal(e1, e2)
      )
   }
}

#' @keywords internal
boolean_dist <- function(prob_true) {
   as_density_integrate(data.frame(
      x = c(0, 1),
      y = c(1 - prob_true, prob_true),
      prob = c(1 - prob_true, prob_true)
   ))
}

#' @keywords internal
form_geq <- function(f, g) {
   boolean_dist(prob_true = prob_geq(f, g))
}

#' @keywords internal
form_greater <- function(f, g) {
   boolean_dist(prob_true = prob_greater(f, g))
}

#' @keywords internal
form_leq <- function(f, g) {
   boolean_dist(prob_true = 1 - prob_greater(f, g))
}

#' @keywords internal
form_less <- function(f, g) {
   boolean_dist(prob_true = 1 - prob_geq(f, g))
}

#' @keywords internal
form_equal <- function(f, g) {
   boolean_dist(prob_true = prob_equal(f, g))
}

#' @keywords internal
form_not_equal <- function(f, g) {
   boolean_dist(prob_true = 1 - prob_equal(f, g))
}

#' @keywords internal
prob_geq <- function(f, g) {
   # Early returns for edge cases
   f_supp <- distfn_domain_num(f)
   g_supp <- distfn_domain_num(g)
   if (f_supp[1] > g_supp[2]) {
      return(1)
   }
   if (g_supp[1] > f_supp[2]) {
      return(0)
   }

   # Actual computations
   if (is_discrete_distribution(f)) {
      prob_geq_dis_any(f, g)
   } else {
      if (is_discrete_distribution(g)) {
         1 - prob_geq_dis_any(g, f)
      } else {
         prob_geq_con_con(f, g)
      }
   }
}

#' @keywords internal
prob_equal <- function(f, g) {
   if ((attr(f, "type") == "discrete") &&
       (attr(f, "type") == "discrete")) {
      f_x_tbl <- as_tbl(f)
      g_x_tbl <- as_tbl(g)
      x_f <- f_x_tbl[["x"]]

      d_g_at_x_f <- numeric(length(x_f))
      inds <- match(x_f, g_x_tbl[["x"]], nomatch = NA)
      good_inds <- !is.na(inds)
      d_g_at_x_f[good_inds] <- g_x_tbl[["prob"]][inds[good_inds]]

      sum(f_x_tbl[["prob"]] * d_g_at_x_f)
   } else {
      0
   }
}

#' @keywords internal
prob_greater <- function(f, g) {
   prob_geq(f, g) - prob_equal(f, g)
}

#' @keywords internal
prob_geq_dis_any <- function(f, g) {
   f_x_tbl <- as_tbl(f)
   x_f <- f_x_tbl[["x"]]

   if (attr(g, "type") == "discrete") {
      g_x_tbl <- as_tbl(g)
      cumprob_g <- numeric(length(x_f))

      inds <- findInterval(x_f, g_x_tbl[["x"]])
      inds_isnt_zero <- inds != 0

      cumprob_g[inds_isnt_zero] <-
         g_x_tbl[["cumprob"]][inds[inds_isnt_zero]]
   } else {
      cumprob_g <- probability_dist(g, x_f)
   }

   sum(f_x_tbl[["prob"]] * cumprob_g)
}

#' @keywords internal
intersection_x <- function(f, g) {
   f_x <- as_tbl(f)[["x"]]
   g_x <- as_tbl(g)[["x"]]
   f_supp <- distfn_domain_num(f)
   g_supp <- distfn_domain_num(g)
   g_x <- g_x[(g_x >= f_supp[1]) & (g_x <= f_supp[2])]
   f_x <- f_x[(f_x >= g_supp[1]) & (f_x <= g_supp[2])]
   sort(union(f_x, g_x))
}

#' @keywords internal
prob_geq_con_con <- function(f, g) {
   con_geq_piece_integral <- function(diff_x,
                                      y_f_left,
                                      y_g_left,
                                      slope_f,
                                      slope_g,
                                      cumprob_g_left) {
      h <- diff_x

      piece_integrals <- slope_f * slope_g * h^4 / 8 +
         (2 * slope_f * y_g_left * h^3 + slope_g * y_f_left * h^3) / 6 +
         (slope_f * cumprob_g_left * h^2 + y_f_left * y_g_left * h^
             2) / 2 +
         y_f_left * cumprob_g_left * h

      sum(piece_integrals)
   }
   inters_x <- intersection_x(f, g)

   n <- length(inters_x)
   inters_left <- inters_x[-n]
   inters_right <- inters_x[-1]
   inters_mid <- (inters_left + inters_right) / 2

   f_x_tbl <- as_tbl(f)
   f_x_tbl <- f_x_tbl[, c("x", "y", "cumprob")]
   coeffs_f <- compute_piecelin_density_coeffs(
      dist = f_x_tbl,
      ind_vec = findInterval(inters_mid, f_x_tbl[["x"]])
   )
   g_x_tbl <- as_tbl(g)
   g_x_tbl <- g_x_tbl[, c("x", "y", "cumprob")]
   coeffs_g <- compute_piecelin_density_coeffs(
      dist = g_x_tbl,
      ind_vec = findInterval(inters_mid, g_x_tbl[["x"]])
   )

   cumprob_g_left <- probability_dist(g, inters_left)

   inters_res <- con_geq_piece_integral(
      diff_x = diff(inters_x),
      y_f_left = density_dist(f, inters_left),
      y_g_left = density_dist(g, inters_left),
      slope_f = coeffs_f[["slope"]],
      slope_g = coeffs_g[["slope"]],
      cumprob_g_left = cumprob_g_left
   )

   f_geq_outside <- 1 - probability_dist(f, distfn_domain_num(g)[2])

   inters_res + f_geq_outside
}

#' @keywords internal
reflect_around_zero <- function(f) {
   refl_x_tbl <- reflect_x_dist(f, 0)

   refl_x_tbl <- as_l(mutate(refl_x_tbl, prob = .data$y))
   class(refl_x_tbl) <- class(f)
   refl_x_tbl
}

#' @keywords internal
reflect_x_dist <- function(f, around) {
   x_tbl <- dplyr::select(as_tbl(f), !quote(prob))
   res <- x_tbl[rev(seq_len(nrow(x_tbl))), ]
   res[["x"]] <- around + (around - res[["x"]])

   x_tbl_probs <- switch(attr(f, "type"),
                         discrete = diff(c(0, x_tbl[["cumprob"]])),
                         continuous = diff(c(x_tbl[["cumprob"]], 1))
   )
   x_tbl_probs <- ifelse(x_tbl_probs < 0, 0, x_tbl_probs)
   res[["cumprob"]] <- cumsum(rev(x_tbl_probs))

   row.names(res) <- NULL

   res
}

#' @keywords internal
negate_distr <- function(f) {
   prob_zero <- if (attr(f, "type") == "discrete") {
      density_dist(f, 0)
   } else {
      0
   }
   density_integrate(sample(
      c(0, 1),
      size = 10000,
      replace = T,
      prob = c(1 - prob_zero, prob_zero)
   ))
}


# linear -

#' @keywords internal
is_ops_linear <- function(gen, e1, e2) {
   e1_is_number <- is_single_number(e1)
   e2_is_number <- is_single_number(e2)

   ((gen %in% c("+", "-", "*")) &&
         (e1_is_number || e2_is_number)) ||
      ((gen == "/") && (e2_is_number))
}

#' @keywords internal
is_single_number <- function(x,
                             min_val = NULL,
                             max_val = NULL) {
   res <- is.numeric(x) && (length(x) == 1) && is.finite(x)
   is_geq <- if (is.null(min_val)) {
      TRUE
   } else {
      x >= min_val
   }
   is_leq <- if (is.null(max_val)) {
      TRUE
   } else {
      x <= max_val
   }

   res && is_geq && is_leq
}
#' @keywords internal
ops_linear <- function(gen, e1, e2) {
   # It is assumed that this function is run only if `is_ops_linear` gave `TRUE`
   e1_is_number <- is_single_number(e1)

   # `e1` and `e2` should be exactly one single number and one distrobj-function
   if (e1_is_number) {
      if (!is_distrobj(e2)) {
         cat_stop("Second argument of `{ gen } ` should be distrobj-object")
         return(NULL)
      }
      ops_meta <-
         list(
            e1_num = e1,
            e2_num = distfn_domain_num(e2),
            distrobj = e2
         )
   } else {
     if (!is_distrobj(e1)) {
       cat_stop("Second argument of `{ gen } ` should be distrobj-object")
       return(NULL)
     }

     ops_meta <-
       list(
         e1_num = distfn_domain_num(e1),
         e2_num = e2,
         distrobj = e1
       )
   }

   # Speed optimization (skips possibly expensive assertions)

   if ((gen == "-") && e1_is_number) {
     # This relies on custom implementations of `-` which takes one argument and
     # `+` which takes two arguments and goes into the next clause
     (-e2) + e1
   } else {
     # Output is done by transforming support linearly based on the input
     # operation `gen` and number
     res_supp <- get(gen)(ops_meta[["e1_num"]], ops_meta[["e2_num"]])

     resupport_distribution(ops_meta[["distrobj"]], res_supp, method = "linear")
   }
}

# logics -

#' @keywords internal
ops_logic <- function(gen, e1, e2) {
  d_zero <- density_integrate(rep(0, 1000))

  prob_false_1 <- prob_equal(e1, d_zero)
  prob_false_2 <- prob_equal(e2, d_zero)

  switch(gen,
         `&` = boolean_dist((1 - prob_false_1) * (1 - prob_false_2)),
         `|` = boolean_dist(1 - prob_false_1 * prob_false_2)
  )
}

# resupport -

#' @rdname distr_generic_n_ops
#' @export
resupport_distribution <-
  function(f,
           support,
           method = c("reflect", "trim", "winsor", "linear")) {
    if (all(is.na(support))) {
      return(f)
    }
    coalesce_pair <- function(x, y) {
      res <- x
      x_is_na <- is.na(x)
      res[x_is_na] <- y[x_is_na]

      res
    }
    supp <- coalesce_pair(support, distfn_domain_num(f))
    if (supp[1] > supp[2]) {
      cat_stop("After imputing `NA`s `support` equals ({supp[1]}, {supp[2]}) which is not proper.")
      return(NULL)
    }

    switch(method,
           reflect = resupport_reflect(f, supp),
           trim = resupport_trim(f, supp),
           winsor = resupport_winsor(f, supp),
           linear = resupport_linear(f, supp)
    )
  }

#' @keywords internal
resupport_reflect <- function(f, support) {
  resupport_reflect_impl <- function(x, around, type) {
    res <- x[rev(seq_len(nrow(x))), ]
    res[["x"]] <- around + (around - res[["x"]])

    x_probs <- switch(type,
                      discrete = diff(c(0, x[["cumprob"]])),
                      continuous = diff(c(x[["cumprob"]], 1))
    )
    res[["cumprob"]] <- cumsum(rev(x_probs))

    row.names(res) <- NULL

    res
  }

  stack_distrobj <- function(xtbl_distrobj, type) {
    # It is assumed that all "x_tbl"s have the same type
    res <- switch(type,
                  discrete = stack_distrobj_dis(xtbl_distrobj),
                  continuous = stack_distrobj_con(xtbl_distrobj, type)
    )
    row.names(res) <- NULL

    res
  }

  stack_distrobj_dis <- function(xtbl_distrobj) {
    x <- unlist(lapply(xtbl_distrobj, `[[`, i = "x"))
    prob <- unlist(lapply(xtbl_distrobj, `[[`, i = "prob"))

    if (anyDuplicated(x) != 0) {
      vals <- sort(unique(x))
      x_val_id <- match(x, vals)
      prob <- as_vct(tapply(prob, x_val_id, sum))
      x <- vals
    }

    data.frame(x = x, prob = prob)
  }

  stack_distrobj_con <- function(xtbl_distrobj, type) {
    # Determine grounding direction for every 'x_tbl' so that resulting edges of
    # output don't get unnecessary grounding
    xtbl_distrobj_range <- lapply(xtbl_distrobj, function(x_tbl) {
      range(x_tbl[["x"]])
    })
    res_range <- range(unlist(xtbl_distrobj_range))
    ground_dir <- vapply(seq_along(xtbl_distrobj), function(i) {
      cur_range <- xtbl_distrobj_range[[i]]

      ground_left <- !is_near(cur_range[1], res_range[1])
      ground_right <- !is_near(cur_range[2], res_range[2])

      if (ground_left) {
        res <- if (ground_right) {
          "both"
        } else {
          "left"
        }
      } else {
        res <- if (ground_right) {
          "right"
        } else {
          "none"
        }
      }

      res
    }, character(1))

    # Grounding is needed to ensure that `x_tbl` doesn't affect its outside
    xtbl_distrobj_grounded <-
      lapply(seq_along(xtbl_distrobj), function(i) {
        ground_distr_dir(xtbl_distrobj[[i]], direction = ground_dir[i], type = type)
      })

    # Compute grid 'x_tbl' stack
    x <- unlist(lapply(xtbl_distrobj_grounded, `[[`, i = "x"))
    x <- sort(unique(x))

    # Stack 'x_tbl' by evaluating grounded versions of 'x_tbl's at output x-grid
    x_tbl_funs <- lapply(xtbl_distrobj_grounded, enfun_x)
    y_at_x <- lapply(x_tbl_funs, do.call, list(x))
    y_mat <- matrix(unlist(y_at_x), nrow = length(x))
    y <- rowSums(y_mat)

    data.frame(x = x, y = y)
  }


  ground_distr_dir <- function(x_tbl,
                               direction = "both",
                               h = 1e-8,
                               type) {
    if ((type == "discrete") ||
        !(direction %in% c("left", "right", "both"))) {
      return(x_tbl)
    }

    x <- x_tbl[["x"]]
    y <- x_tbl[["y"]]
    n <- nrow(x_tbl)
    x_tbl_fun <- enfun_x(x_tbl)

    ground_left <-
      (direction %in% c("left", "both")) && !is_zero(y[1])
    ground_right <-
      (direction %in% c("right", "both")) && !is_zero(y[n])

    res <- x_tbl

    # Grounding on certain edge is done by adding one point (close to edge)
    # outside of support and, in case there isn't a "close" one present, one on
    # the inside. Y-values are: zero for outside, respective density value for
    # inside. Then y-value of edge knot is modified so as to preserve total
    # probability of one.

    if (ground_left) {
      x_diff <- (x[2] - x[1])
      # Using `2*h` instead of `h` to avoid numerical representation issues
      if (x_diff > 2 * h) {
        # Case when inner point should be added because there is no "close" knot
        # in input data
        res <- res[c(1, 1, 1:n), ]
        res[["x"]][c(1, 3)] <- x[1] + h * c(-1, 1)
        res[["y"]][c(1, 2, 3)] <-
          c(0, 0.5 * res[["y"]][2], x_tbl_fun(x[1] + h))
      } else {
        # Case when inner point shouldn't be added
        res <- res[c(1, 1:n), ]
        res[["x"]][c(1)] <- x[1] - h
        res[["y"]][c(1, 2)] <-
          c(0, res[["y"]][2] * x_diff / (x_diff + h))
      }
    }

    if (ground_right) {
      n_n <- nrow(res)
      x_n <- res[["x"]]
      x_diff <- (x_n[n_n] - x_n[n_n - 1])
      # Using `2*h` instead of `h` to avoid numerical representation issues
      if (x_diff > 2 * h) {
        res <- res[c(1:n_n, n_n, n_n), ]
        res[["x"]][n_n + c(0, 2)] <- x_n[n_n] + h * c(-1, 1)
        res[["y"]][n_n + c(0, 1, 2)] <-
          c(x_tbl_fun(x_n[n_n]), 0.5 * res[["y"]][n_n + 1], 0)
      } else {
        res <- res[c(1:n_n, n_n), ]
        res[["x"]][n_n + c(1)] <- x_n[n_n] + h
        res[["y"]][n_n + c(0, 1)] <-
          c(res[["y"]][n_n] * x_diff / (x_diff + h), 0)
      }
    }

    rownames(res) <- NULL
    res
  }

  f_supp <- distfn_domain_num(f)
  f_x_tbl <- as_tbl(f)

  # Sum up densities for possible reflections
  xtbl_distrobj <- list(f_x_tbl)



  if (support[1] > f_supp[1]) {
    xtbl_distrobj <-
      c(xtbl_distrobj, list(resupport_reflect_impl(f_x_tbl, support[1], type = attr(f, "type"))))
  }
  if (support[2] < f_supp[2]) {
    xtbl_distrobj <-
      c(xtbl_distrobj, list(resupport_reflect_impl(f_x_tbl, support[2], type = attr(f, "type"))))
  }

  x_tbl <- stack_distrobj(xtbl_distrobj, type = attr(f, "type"))
  res <- as_density_integrate(x_tbl)

  # Trim total sum to supplied support
  resupport_distribution(res, support, "trim")
}

#' @keywords internal
resupport_trim <- function(f, support) {
  resupport_trim_dis <- function(f, support) {
    res_x <- filter_support(as_tbl(f), support)

    if (sum(res_x[["prob"]]) <= 0) {
      return(NULL)
    }

    as_density_integrate(res_x)
  }

  resupport_trim_con <- function(f, support) {
    d_f <- as_dens(f)
    edge_y <- d_f(support)

    # Add new rows to "x_tbl" metadata of `f` to capture "trimming" behavior.
    # However, if edge of `support` is outside of `f`'s support, then it is not
    # added. This is needed to not change shape of distribution.
    x_tbl_plus <-
      union_inside_distrobj(x = as_tbl(f), y = data.frame(x = support, y = edge_y))
    res_x <- filter_support(x_tbl_plus, support)

    if (trapez_integral(res_x[["x"]], res_x[["y"]]) <= 0) {
      return(NULL)
    }

    as_density_integrate(res_x)
  }

  switch(attr(f, "type"),
         discrete = resupport_trim_dis(f, support),
         continuous = resupport_trim_con(f, support)
  )
}

#' @keywords internal
resupport_winsor <- function(f, support) {
  resupport_winsor_dis <- function(f, support) {
    f_x_tbl <- as_tbl(f)
    x <- f_x_tbl[["x"]]
    x[x <= support[1]] <- support[1]
    x[x >= support[2]] <- support[2]
    f_x_tbl[["x"]] <- x

    as_density_integrate(f_x_tbl)
  }

  resupport_winsor_con <- function(f, support, h = 1e-8) {
    p_f <- as_prob(f)
    f_x_tbl <- as_tbl(f)
    f_supp <- distfn_domain_num(f)

    # Early return extreme cases
    if (support[1] >= f_supp[2]) {
      return(as_density_integrate(data.frame(x = support[1], y = 1)))
    }
    if (support[2] <= f_supp[1]) {
      return(as_density_integrate(data.frame(x = support[2], y = 1)))
    }

    x_tbl <- f_x_tbl
    # Winsor left
    if (support[1] > f_supp[1]) {
      x_tbl <- add_knots(x_tbl, support[1] + c(0, h))
      x_tbl <- filter_support(x_tbl, c(support[1], f_supp[2]))
      tail_prob <- p_f(support[1])
      x_tbl <- increase_tail_weight(x_tbl, tail_prob, "left")
    }

    # Winsor right
    if (support[2] < f_supp[2]) {
      x_tbl <- add_knots(x_tbl, support[2] - c(h, 0))
      x_tbl <- filter_support(x_tbl, c(f_supp[1], support[2]))
      tail_prob <- 1 - p_f(support[2])
      x_tbl <- increase_tail_weight(x_tbl, tail_prob, "right")
    }

    as_density_integrate(x_tbl)
  }

  increase_tail_weight <- function(x_tbl, by_prob, edge) {
    n <- nrow(x_tbl)
    x <- x_tbl[["x"]]
    y <- x_tbl[["y"]]

    if (edge == "left") {
      present_prob <- (y[1] + y[2]) * (x[2] - x[1]) / 2
      to_prob <- present_prob + by_prob
      y[1] <- 2 * to_prob / (x[2] - x[1]) - y[2]
    } else if (edge == "right") {
      present_prob <- (y[n - 1] + y[n]) * (x[n] - x[n - 1]) / 2
      to_prob <- present_prob + by_prob
      y[n] <- 2 * to_prob / (x[n] - x[n - 1]) - y[n - 1]
    }

    data.frame(x = x, y = y)
  }

  if (support[1] == support[2]) {
    return(as_density_integrate(data.frame(x = support[1], y = 1)))
  }

  switch(attr(f, "type"),
         discrete = resupport_winsor_dis(f, support),
         continuous = resupport_winsor_con(f, support)
  )
}

#' @keywords internal
resupport_linear <- function(f, support) {
  if (support[1] == support[2]) {
    # Return dirac-like function
    return(as_density_integrate(data.frame(x = support[1], y = 1)))
  }

  f_supp <- distfn_domain_num(f)

  if (f_supp[1] == f_supp[2]) {
    return(NULL)
  }

  res_x <- as_tbl(f)
  res_x[["x"]] <- extrap_lin(
    x_1 = f_supp[1],
    x_2 = f_supp[2],
    y_1 = support[1],
    y_2 = support[2],
    x_target = res_x[["x"]]
  )

  as_density_integrate(res_x)
}

#' @keywords internal
is_near <- function(x, y, tol = 10^(-8)) {
  abs(x - y) < tol
}

#' @keywords internal
enfun_x <- function(.data,
                    x = "x",
                    y = "y",
                    method = "linear") {
  stats::approxfun(.data[[x]],
                   .data[[y]],
                   method = method,
                   yleft = 0,
                   yright = 0
  )
}

#' @keywords internal
is_zero <- function(x) {
  is_near(x, 0, tol = 1e-12)
}

#' @keywords internal
filter_support <- function(.data, support, x = "x") {
  in_support <-
    (.data[[x]] >= support[1]) & (.data[[x]] <= support[2])
  .data[in_support, ]
}

#' @keywords internal
union_inside_distrobj <- function(x, y) {
  get_x_tbl_sec_col <- function(x_tbl) {
    if ("prob" %in% names(x_tbl)) {
      "prob"
    } else {
      "y"
    }
  }
  # Remove rows from `x_tbl_new` which are outside from `x_tbl_orig`'s support
  orig_supp <- range(x[["x"]])
  y <- filter_support(y, orig_supp)

  second_col <-
    get_x_tbl_sec_col(x[, names(x) %in% names(y)])

  res <- data.frame(x = c(x[["x"]], y[["x"]]))
  res[[second_col]] <-
    c(x[[second_col]], y[[second_col]])

  # Remove rows with duplicate "x" (leaving in output rows from `x_tbl_orig`)
  res <- res[!duplicated(res[["x"]]), ]

  res <- res[order(res[["x"]]), ]
  row.names(res) <- NULL

  res
}

#' @keywords internal
add_knots <- function(x_tbl, at_x, only_inside = TRUE) {
  # Ensure that present knots aren't get duplicated
  at_x <- setdiff(at_x, x_tbl[["x"]])

  # Add only knots inside current range if `only_inside` is `TRUE`
  if (only_inside) {
    supp <- range(x_tbl[["x"]])
    at_x <- at_x[(at_x > supp[1]) & (at_x < supp[2])]
  }

  if (length(at_x) == 0) {
    return(x_tbl)
  }

  new_x <- c(x_tbl[["x"]], at_x)
  new_y <- c(x_tbl[["y"]], enfun_x(x_tbl)(at_x))
  x_order <- order(new_x)

  data.frame(x = new_x[x_order], y = new_y[x_order])
}

#' @keywords internal
increase_tail_weight <- function(data, by_prob, edge) {
  n <- nrow(data)
  x <- data[["x"]]
  y <- data[["y"]]

  if (edge == "left") {
    present_prob <- (y[1] + y[2]) * (x[2] - x[1]) / 2
    to_prob <- present_prob + by_prob
    y[1] <- 2 * to_prob / (x[2] - x[1]) - y[2]
  } else if (edge == "right") {
    present_prob <- (y[n - 1] + y[n]) * (x[n] - x[n - 1]) / 2
    to_prob <- present_prob + by_prob
    y[n] <- 2 * to_prob / (x[n] - x[n - 1]) - y[n - 1]
  }

  data.frame(x = x, y = y)
}

#' @keywords internal
extrap_lin <- function(x_1, x_2, y_1, y_2, x_target) {
  slope <- (y_2 - y_1) / (x_2 - x_1)
  inter <- y_1 - slope * x_1

  slope * x_target + inter
}

#' @keywords internal
is_periodically_inside <- function(x, interval, period) {
  k_left <- ceiling((interval[1] - x) / period)
  k_right <- floor((interval[2] - x) / period)

  k_left <= k_right
}

#' @keywords internal
inf_to_na <- function(x) {
  ifelse(is.infinite(x), NA_real_, x)
}

#' @keywords internal
repair_supp_monotone <-
  function(op,
           supp,
           input_bounds = c(-Inf, Inf),
           increasing_op = TRUE) {
    supp <-
      c(max(supp[1], input_bounds[1]), min(supp[2], input_bounds[2]))
    if (!increasing_op) {
      supp <- supp[2:1]
    }

    inf_to_na(op(supp))
  }

#' @keywords internal
repair_supp_cosh <- function(supp) {
  if ((supp[1] < 0) && (0 < supp[2])) {
    c(1, max(cosh(supp)))
  } else {
    cosh(supp)
  }
}

#' @keywords internal
repair_supp_subtraction <- function(e1_supp, e2_supp) {
  c(e1_supp[1] - e2_supp[2], e1_supp[2] - e2_supp[1])
}

#' @keywords internal
repair_supp_multiplication <- function(e1_supp, e2_supp) {
  # Here `na.rm = TRUE` is needed to avoid `NaN`s in case `0*Inf` when called
  # inside `repair_supp_division()`
  range(e1_supp[1] * e2_supp, e1_supp[2] * e2_supp, na.rm = TRUE)
}

#' @keywords internal
repair_supp_division <- function(e1_supp, e2_supp) {
  inf_to_na(repair_supp_multiplication(e1_supp, repair_supp_inverse(e2_supp)))
}

#' @keywords internal
repair_supp_inverse <- function(supp) {
  if ((supp[1] > 0) || (supp[2] < 0)) {
    1 / supp[2:1]
  } else if (supp[1] == 0) {
    # In this case `supp[2]` can't be 0
    c(1 / supp[2], Inf)
  } else if (supp[2] == 0) {
    # In this case `supp[1]` can't be 0
    c(-Inf, 1 / supp[1])
  } else {
    # Case when 0 is strictly inside support
    c(-Inf, Inf)
  }
}

#' @keywords internal
repair_supp_sum <- function(supp_list) {
  supp_left <-
    sum(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 1))
  supp_right <-
    sum(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 2))

  c(supp_left, supp_right)
}

#' @keywords internal
repair_supp_prod <- function(supp_list) {
  if (length(supp_list) == 1) {
    supp_list[[1]]
  } else {
    Reduce(repair_supp_multiplication, supp_list[-1], init = supp_list[[1]])
  }
}

#' @keywords internal
repair_supp_min <- function(supp_list) {
  supp_left <-
    min(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 1))
  supp_right <-
    min(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 2))

  c(supp_left, supp_right)
}

#' @keywords internal
repair_supp_max <- function(supp_list) {
  supp_left <-
    max(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 1))
  supp_right <-
    max(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 2))

  c(supp_left, supp_right)
}

#' @keywords internal
simulate_repair_supp <- function(op, supp_list, n = 1e4) {
  smpl_list <- lapply(supp_list, function(supp) {
    stats::runif(n, min = supp[1], max = supp[2])
  })

  smpl <- do.call(op, smpl_list)

  inf_to_na(range(smpl, na.rm = TRUE))
}

# transformaciones -

#' @rdname distr_generic_n_ops
#' @export
trasnfor_distribution <-
  function(f_list,
           trans,
           ...,
           method = "random",
           n_sample = 10000,
           args_new = list()) {
    if (is_distrobj(f_list)) {
      out <- trasnfor_self_distribution(
        f = f_list,
        trans = trans,
        ...,
        method = method,
        args_new = list()
      )
    } else if (is.list(f_list) & lgl_all(f_list, is_distrobj)) {
      if (length(f_list) == 1) {
        out <- trasnfor_self_distribution(
          f = f_list[[1]],
          trans = trans,
          ...,
          method = method,
          args_new = list()
        )
      } else {
        out <- switch(method,
                      random = trasnfor_random_distribution(
                        f_list = f_list,
                        trans = trans,
                        n_sample = n_sample,
                        args_new = args_new
                      ),
                      bruteforce = trasnfor_bruteforce_distribution(
                        f_list = f_list,
                        trans = trans,
                        args_new = args_new
                      )
        )
      }
    }
    out
  }

#' @keywords internal
trasnfor_self_distribution <-
  function(f,
           trans,
           ...,
           method = "random",
           args_new = list()) {
    switch(method,
           random = trasnfor_random_distribution(
             f_list = list(f),
             trans = trans,
             n_sample = n_sample,
             args_new = args_new
           ),
           bruteforce = trasnfor_bruteforce_distribution(
             f_list = list(f),
             trans = trans,
             args_new = args_new
           )
    )
  }

#' @keywords internal
trasnfor_bruteforce_distribution <-
  function(f_list, trans, ..., args_new = list()) {
    type_e1 <- function(l) {
      lgl <- vapply(l, is_distrobj, logical(1))
      attr(l[lgl][[1]], "type")
    }
    list_grid <- function(vec_list) {
      res <- as_l(do.call(expand.grid, vec_list))
      names(res) <- NULL
      attributes(res) <- NULL
      res
    }
    type <- type_e1(f_list)
    f_list <- lapply(f_list, function(f) {
      if (is_single_number(f)) {
        as_density_integrate(data.frame(x = f, y = 1))
      } else {
        retype_distribution(f, type = "discrete", method = "piecelin")
      }
    })
    x_tbl_list <- lapply(f_list, as_tbl)
    x_list <- lapply(x_tbl_list, function(x_tbl) {
      x_tbl[["x"]]
    })
    x_grid <- list_grid(x_list)
    prob_list <- lapply(x_tbl_list, function(x_tbl) {
      x_tbl[["prob"]]
    })
    prob_grid <- list_grid(prob_list)
    x_new <- do.call(trans, args = c(x_grid, list(...)))
    prob_new <-
      Reduce(f = `*`, x = prob_grid[-1], init = prob_grid[[1]])
    if (is.logical(x_new)) {
      prob_true <- sum(prob_new[x_new], na.rm = TRUE)
      return(boolean_dist(prob_true))
    }
    x_tbl <- data.frame(x = x_new, prob = prob_new)
    dis_res <- as_density_integrate(x_tbl)
    retype_distribution(dis_res, type, method = "piecelin")
  }

#' @keywords internal
trasnfor_random_distribution <- function(f_list, ..., n_sample, trans) {
  r_dots <- lapply(f_list, function(f) {
    if (is_distrobj(f)) {
      as_rand(f)
    } else {
      function(n) {
        rep(f, length.out = n)
      }
    }
  })
  smpl_list <- lapply(r_dots, do.call, args = list(n_sample))
  args_list <- c(smpl_list, list(...))
  args_list <- compact(args_list)
  smpl <- do.call(trans, args = args_list)
  smpl <- smpl[!is_nonnum(smpl)]
  if (is.logical(smpl)) {
    prob_true <- mean(smpl, na.rm = TRUE)
    return(boolean_dist(prob_true))
  }
  density_integrate(smpl)
}

# retype -

#' @rdname distr_generic_n_ops
#' @export
retype_distribution <-
  function(f,
           type = NULL,
           method = c("piecelin", "dirac", "value")) {
    switch(type,
           discrete = retype_dis(f, method),
           continuous = retype_con(f, method)
    )
  }

#' @keywords internal
retype_dis <- function(f, method) {
  retype_dis_value <- function(f) {
    x_tbl <- as_tbl(f)
    # Renormalization of "prob" column will be done inside `new_*()` function
    new_x_tbl <- data.frame(x = x_tbl[["x"]], prob = x_tbl[["y"]])

    as_density_integrate(new_x_tbl)
  }
  retype_dis_piecelin <- function(f) {
    x_tbl <- as_tbl(f)
    n <- nrow(x_tbl)

    x_lag <- x_tbl[["x"]][-n]
    x_lead <- x_tbl[["x"]][-1]
    y_lag <- x_tbl[["y"]][-n]
    y_lead <- x_tbl[["y"]][-1]
    y_sum <- y_lag + y_lead

    # Output `x` values are computed as intervals' centers of mass
    x_mass <-
      (x_lag * (y_lag + y_sum) + x_lead * (y_lead + y_sum)) / (3 * y_sum)
    ## If interval has zero probability then its centre is set to the middle
    x_mass_bad <- !is.finite(x_mass)
    x_mass[x_mass_bad] <-
      (x_lag[x_mass_bad] + x_lead[x_mass_bad]) / 2

    # Output probabilities are computed as probabilites (mass) of intervals
    prob <- diff(x_tbl[["cumprob"]])

    # Creating pdqr-function
    as_density_integrate(data.frame(x = x_mass, prob = prob))
  }
  retype_dis_dirac <- function(f) {
    type <- ifelse(is_discrete_distribution(f$x), "discrete", "continuous")
    x_tbl <- as_tbl(f)
    # Ensure presense of zero densities
    x_tbl <- ground_distr_dir(x_tbl, type)

    # One output "discrete" value is computed as mean of two consequtive "x"s with
    # zero density. Each "x" corresponds to only one "discrete" value counting
    # from left: x_1 and x_2 are used to compute first "discrete" value; x_3 and
    # x_4 - second, and so on. Probability of "discrete" value is computed as
    # difference in cumulative probabilities between corresponding right and left
    # "x" values.

    y_is_zero <- is_zero(x_tbl[["y"]])
    n_y_zero <- sum(y_is_zero)
    dis_groups <-
      rep(seq_len(n_y_zero),
          each = 2,
          length.out = n_y_zero
      )

    new_x <- tapply(x_tbl[["x"]][y_is_zero], dis_groups, mean)
    # Remove dimnames
    new_x <- as_vct(new_x)

    new_prob <- tapply(
      x_tbl[["cumprob"]][y_is_zero],
      dis_groups,
      function(cumprob) {
        # This custom function is used instead of `diff()` if, for some reason,
        # there are odd number of zero density rows in `x_tbl`. In that case
        # output probability is zero.
        cumprob[length(cumprob)] - cumprob[1]
      }
    )
    # Remove dimnames

    new_prob <- as_vct(new_prob)

    as_density_integrate(data.frame(x = new_x, prob = new_prob))
  }

  type <- ifelse(is_discrete_distribution(f$x), "discrete", "continuous")
  if (type == "discrete") {
    return(f)
  }

  switch(method,
         value = retype_dis_value(f),
         piecelin = retype_dis_piecelin(f),
         dirac = retype_dis_dirac(f)
  )
}

#' @keywords internal
retype_con <- function(f, method) {
  retype_con_value <- function(f) {
    x_tbl <- as_tbl(f)
    # Renormalization of "y" column will be done inside `new_*()` function
    new_x_tbl <- data.frame(x = x_tbl[["x"]], y = x_tbl[["prob"]])

    as_density_integrate(new_x_tbl)
  }
  retype_con_piecelin <- function(f) {
    y_from_p_grid <- function(x, p) {
      # It is assumed that `x` has no duplicates and sorted increasingly
      # Values of density are computed as numerical derivatives at `x` points
      p_lin_derivs <- diff(p) / diff(x)
      n_int <- length(p_lin_derivs)
      y <- 0.5 * c(
        # Boundary derivatives need special treatment for accuracy reasons
        3 * p_lin_derivs[1] - p_lin_derivs[2],
        p_lin_derivs[-n_int] + p_lin_derivs[-1],
        -p_lin_derivs[n_int - 1] + 3 * p_lin_derivs[n_int]
      )

      # Extra cutoffs to ensure positive `y`. Sometimes it is needed when `y` is
      # small near edges.
      y <- pmax(y, 0)

      # As `y` is not exact solution, total integral will not equal 1 so
      # renormalization should be done.
      y / trapez_integral(x, y)
    }
    x_tbl <- as_tbl(f)

    n <- nrow(x_tbl)
    if (n < 4) {
      cat_stop(
        'For conversion to "continuous" type `retype_distribution()` needs at least 4 ',
        'unique "x" values.'
      )
    }

    x <- x_tbl[["x"]]
    prob <- x_tbl[["prob"]]

    # Values of `x` grid (except first and last elements) of "continuous" output
    # is approximated as convex combination of nearest "centers of mass":
    # `x_mid = (1-alpha)*x_mass_left + alpha*x_mass_right`. Here `alpha` is
    # approximated based on two separate assumptions:
    # *Assumption 1*: if locally `y_left` (`y` value in "true" `x` to the left of
    # `x_mid`) = `y_mid` = `y_right` (by analogy with `y_left`), then
    # `alpha = prob_left / (prob_left + prob_right)`.
    # *Assumption 2*: if locally "true" values of `x` are equidistant then `alpha`
    # lie inside [1/3; 2/3] interval.
    # Final approximation is formed by combining these conclusions
    prob_sum <- prob[1:(n - 1)] + prob[2:n]
    alpha <- pmin(pmax(prob[1:(n - 1)] / prob_sum, 1 / 3), 2 / 3)
    alpha[!is.finite(alpha)] <- 0.5

    x_grid <- numeric(n + 1)
    x_grid[2:n] <- (1 - alpha) * x[1:(n - 1)] + alpha * x[2:n]
    # First and last `x` are approximated so that first and last `x` triplets are
    # equidistant
    x_grid[1] <- x_grid[2] - (x_grid[3] - x_grid[2])
    x_grid[n + 1] <- x_grid[n] + (x_grid[n] - x_grid[n - 1])

    # Output `y` grid is approximated in 'pdqr' fashion
    p_grid <- c(0, cumsum(prob))
    y <- y_from_p_grid(x_grid, p_grid)

    as_density_integrate(data.frame(x = x_grid, y = y))
  }
  retype_con_dirac <- function(f, h = 1e-8) {
    x_tbl <- as_tbl(f)
    x <- x_tbl[["x"]]
    half_diff_x <- diff(x) / 2

    # Vector of "dirac" radius values
    left_h_vec <- pmin(h, c(h, half_diff_x))
    right_h_vec <- pmin(h, c(half_diff_x, h))
    h_vec <- pmin(left_h_vec, right_h_vec)

    y_zero <- rep(0, length(x))
    new_x <- c(x - h_vec, x, x + h_vec)
    new_y <- c(y_zero, x_tbl[["prob"]] / h_vec, y_zero)

    as_density_integrate(data.frame(x = new_x, y = new_y))
  }

  type <- ifelse(is_discrete_distribution(f$x), "discrete", "continuous")
  if (type == "continuous") {
    return(f)
  }

  switch(method,
         value = retype_con_value(f),
         piecelin = retype_con_piecelin(f),
         dirac = retype_con_dirac(f)
  )
}


ground_distr_dir <- function(x_tbl,
                             type,
                             direction = "both",
                             h = 1e-8) {
  if ((type == "discrete") ||
      !(direction %in% c("left", "right", "both"))) {
    return(x_tbl)
  }

  x <- x_tbl[["x"]]
  y <- x_tbl[["y"]]
  n <- nrow(x_tbl)
  x_tbl_fun <- enfun_x(x_tbl)

  ground_left <-
    (direction %in% c("left", "both")) && !is_zero(y[1])
  ground_right <-
    (direction %in% c("right", "both")) && !is_zero(y[n])

  res <- x_tbl

  # Grounding on certain edge is done by adding one point (close to edge)
  # outside of support and, in case there isn't a "close" one present, one on
  # the inside. Y-values are: zero for outside, respective density value for
  # inside. Then y-value of edge knot is modified so as to preserve total
  # probability of one.

  if (ground_left) {
    x_diff <- (x[2] - x[1])
    # Using `2*h` instead of `h` to avoid numerical representation issues
    if (x_diff > 2 * h) {
      # Case when inner point should be added because there is no "close" knot
      # in input data
      res <- res[c(1, 1, 1:n), ]
      res[["x"]][c(1, 3)] <- x[1] + h * c(-1, 1)
      res[["y"]][c(1, 2, 3)] <-
        c(0, 0.5 * res[["y"]][2], x_tbl_fun(x[1] + h))
    } else {
      # Case when inner point shouldn't be added
      res <- res[c(1, 1:n), ]
      res[["x"]][c(1)] <- x[1] - h
      res[["y"]][c(1, 2)] <-
        c(0, res[["y"]][2] * x_diff / (x_diff + h))
    }
  }

  if (ground_right) {
    n_n <- nrow(res)
    x_n <- res[["x"]]
    x_diff <- (x_n[n_n] - x_n[n_n - 1])
    # Using `2*h` instead of `h` to avoid numerical representation issues
    if (x_diff > 2 * h) {
      res <- res[c(1:n_n, n_n, n_n), ]
      res[["x"]][n_n + c(0, 2)] <- x_n[n_n] + h * c(-1, 1)
      res[["y"]][n_n + c(0, 1, 2)] <-
        c(x_tbl_fun(x_n[n_n]), 0.5 * res[["y"]][n_n + 1], 0)
    } else {
      res <- res[c(1:n_n, n_n), ]
      res[["x"]][n_n + c(1)] <- x_n[n_n] + h
      res[["y"]][n_n + c(0, 1)] <-
        c(res[["y"]][n_n] * x_diff / (x_diff + h), 0)
    }
  }

  rownames(res) <- NULL
  res
}

# otras -
#' @rdname distr_generic_n_ops
#' @export
regrid_distribution <- function(f, n_grid, method = c("x", "q")) {
  early_regrid <- function(f, n_grid) {
    # Return dirac-like function at median if `n_grid == 1`
    if (n_grid == 1) {
      med <- quantile_dist(f, 0.5)

      return(density_integrate(med))
    }

    # Return input if `n_grid` is the same as number of present points or it is a
    # case of UPgridding a "discrete" pdqr-function
    n_f_x_tbl <- nrow(as_tbl(f))
    is_equal_size <- n_grid == n_f_x_tbl
    is_dis_increasing <-
      (attr(f, "type") == "discrete") && (n_grid > n_f_x_tbl)
    if (is_equal_size || is_dis_increasing) {
      return(f)
    }

    # If there are no early returns, return `NULL`
    NULL
  }
  compute_grid <- function(f, n_grid, method) {
    compute_grid_x <- function(f, n_grid) {
      seq_between(distfn_domain_num(f), length.out = n_grid)
    }
    compute_grid_q <- function(f, n_grid) {
      # Note that this might have duplicate values if `f` is of `type` "discrete"
      quantile_dist(f, seq(0, 1, length.out = n_grid))
    }

    switch(method,
           x = compute_grid_x(f, n_grid),
           q = compute_grid_q(f, n_grid)
    )
  }
  adjust_to_grid <- function(f, ref_grid) {
    switch(attr(f, "type"),
           discrete = adjust_to_grid_dis(f, ref_grid),
           continuous = adjust_to_grid_con(f, ref_grid)
    )
  }
  adjust_to_grid_dis <- function(f, ref_grid) {
    # If this function executes, it means that this is DOWNgridding (decreasing
    # granularity), i.e. `length(ref_grid)` is strictly less than
    # `nrow(meta_x_tbl(f))`.
    f_x_tbl <- as_tbl(f)[, c("x", "prob")]
    x <- f_x_tbl[["x"]]

    # Uniquely match `ref_grid` with `x` in terms of "nearest" elements
    res_x <- x[find_nearest_match(ref_grid, x)]

    # Collapse surplus of `x` into the nearest `res_x`
    f_x_tbl[["x"]] <- res_x[find_nearest_ind(x, res_x)]

    as_density_integrate(f_x_tbl)
  }
  adjust_to_grid_con <- function(f, ref_grid) {
    f_x_tbl <- as_tbl(f)[, c("x", "y")]
    x <- f_x_tbl[["x"]]
    n_grid_surplus <- length(ref_grid) - length(x)

    if (n_grid_surplus > 0) {
      # Case of UPgridding. Add to `f_x_tbl` rows with "x" equal to `ref_grid`
      # points furtherst to the set of `x`.
      furtherst_grid_inds <-
        find_neigh_subset(ref_grid, x, n_subset = n_grid_surplus, type = "max")
      furtherst_grid <- ref_grid[furtherst_grid_inds]

      d_f <- as_dens(f)

      # Here "x" column is not ordered, which should be imputed during `new_*()`
      x_tbl <- data.frame(
        x = c(x, furtherst_grid),
        y = c(f_x_tbl[["y"]], d_f(furtherst_grid))
      )
    } else {
      # Case of DOWNgridding. Uniquely match `ref_grid` with `x` in terms of
      # "nearest" elements.
      closest_x_inds <- find_nearest_match(ref_grid, x)

      x_tbl <- f_x_tbl[closest_x_inds, ]
    }

    # Special test to catch case when all y-values at new grid is zero. Other
    # violations of proper "x_tbl" structure are impossible because input `f` is
    # tested to be a proper pdqr-function.
    if (is_zero(sum(x_tbl[["y"]]))) {
      cat_stop(
        'All y-values in `regrid_distribution()` output\'s "x_tbl" are zero. ',
        "Try different `n_grid`."
      )
      return(NULL)
    }

    as_density_integrate(x_tbl)
  }
  find_nearest_ind <- function(x, set) {
    # Returns `length(x)` **indicies of `set`** which are closest to respective
    # `x` elements.
    if (length(set) == 1) {
      return(rep(1, length(x)))
    }

    if (is.unsorted(set)) {
      set_ord <- order(set)
    } else {
      set_ord <- seq_along(set)
    }
    set_sorted <- set[set_ord]

    # Find index of the biggest set point to the left
    x_ind <- findInterval(x, set_sorted, all.inside = TRUE)
    # Possibly correct found index to represent the closest point
    x_ind <-
      x_ind + (set_sorted[x_ind + 1] - x < x - set_sorted[x_ind])

    set_ord[x_ind]
  }
  find_nearest_match <- function(x, set) {
    # Returns `length(x)` **unique indicies of `set`** which are closest to
    # respective `x` elements. Uses naive iterative greedy search. For this to
    # work properly, `length(x)` should be not greater than `length(set)`.
    if (length(set) < length(x)) {
      cat_stop("Can't find unique matching because `set` has fewer elements than `x`.")
    }

    res <- integer(length(x))

    x_to_match <- seq_along(x)
    set_to_match <- seq_along(set)

    # Use greedy search of "matching nearest" values. Note that this might result
    # in very long distances between actual matches. That is why output should be
    # viewed as an unordered set of unique indicies of `set` that are "closest" to
    # `x`.
    while (length(x_to_match) > 0) {
      # Match "current" `x` and `set`
      x_matches <-
        find_nearest_ind(x[x_to_match], set[set_to_match])

      # Use only not duplicated matches, because output should be unique in terms
      # of `set` indicies
      good_match <- !duplicated(x_matches)
      set_matched <- set_to_match[x_matches[good_match]]

      # Update result (indicies of the nearest match) within "good matches"
      res[x_to_match[good_match]] <- set_matched

      # Update "current" `x` and `set` by removing already "used" elements
      x_to_match <- x_to_match[!good_match]
      set_to_match <- setdiff(set_to_match, set_matched)
    }

    res
  }
  find_neigh_subset <- function(x, set, n_subset, type = "min") {
    # Returns `n_subset` **indicies of `x`** which are closest to the `set` (as a
    # set).
    if (n_subset > length(x)) {
      cat_stop(
        "Can't find neighborhood subset because `n_subset` is bigger than ",
        "length of `x`."
      )
      return(NULL)
    }

    dist_x_set <- abs(x - set[find_nearest_ind(x, set)])

    order(dist_x_set, decreasing = type == "max")[seq_len(n_subset)]
  }


  method <- match.arg(method)
  n_grid <- as_int(n_grid)

  # Early regridding
  early_return <- early_regrid(f, n_grid)
  if (!is.null(early_return)) {
    return(early_return)
  }

  # Actual regridding
  ref_grid <- compute_grid(f, n_grid, method)

  adjust_to_grid(f, ref_grid)
}

#' @rdname distr_generic_n_ops
#' @export
mix_distribution <- function(f_list, weights = NULL) {
  impute_weights <- function(weights, n) {
    recycle_vec <- function(vec, n) {
      vec_name <- enbacktick(deparse(substitute(vec)))

      if (!(length(vec) %in% c(1, n))) {
        cat_stop("'{vec_name}' should have length 1 or '{n}'.")
      }

      rep(vec, length.out = n)
    }
    if (is.null(weights)) {
      weights <- rep(1, n) / n
    } else {
      weights <- recycle_vec(weights, n)
    }

    if (any(weights < 0)) {
      cat_stop("`weights` should not have negative elements")
    }
    if (sum(weights) <= 0) {
      cat_stop("`weights` should have positive sum.")
    }

    weights / sum(weights)
  }
  stack_mixtbl <- function(x_list, type) {
    stack_mixtbl_dis <- function(x_list) {
      x <- unlist(lapply(x_list, `[[`, i = "x"))
      prob <- unlist(lapply(x_list, `[[`, i = "prob"))
      if (anyDuplicated(x) != 0) {
        vals <- sort(unique(x))
        x_val_id <- match(x, vals)
        prob <- as_vct(tapply(prob, x_val_id, sum))
        x <- vals
      }
      data.frame(x = x, prob = prob)
    }

    stack_mixtbl_con <- function(x_list, type) {
      x_list_range <- lapply(x_list, function(x_tbl) {
        range(x_tbl[["x"]])
      })
      res_range <- range(unlist(x_list_range))
      ground_dir <- vapply(seq_along(x_list), function(i) {
        cur_range <- x_list_range[[i]]
        ground_left <- !is_near(cur_range[1], res_range[1])
        ground_right <- !is_near(cur_range[2], res_range[2])
        if (ground_left) {
          res <- if (ground_right) {
            "both"
          } else {
            "left"
          }
        } else {
          res <- if (ground_right) {
            "right"
          } else {
            "none"
          }
        }
        res
      }, character(1))
      x_list_grounded <- lapply(seq_along(x_list), function(i) {
        ground_distr_dir(x_list[[i]], direction = ground_dir[i], type = type)
      })
      x <- unlist(lapply(x_list_grounded, `[[`, i = "x"))
      x <- sort(unique(x))
      x_tbl_funs <- lapply(x_list_grounded, enfun_x)
      y_at_x <- lapply(x_tbl_funs, do.call, list(x))
      y_mat <- matrix(unlist(y_at_x), nrow = length(x))
      y <- rowSums(y_mat)
      data.frame(x = x, y = y)
    }

    res <- switch(type,
                  discrete = stack_mixtbl_dis(x_list),
                  continuous = stack_mixtbl_con(x_list, type)
    )
    row.names(res) <- NULL
    res
  }
  flist_type <- function(f_list) {
    # Note that it is assumed that `f_list` contains only pdqr-functions or single
    # numbers. Currently it should be pretested with
    # `assert_f_list(f_list, allow_numbers = TRUE)`
    # Main reason behind it is a wish to avoid usage of possibly "computantionally
    # expensive" `is_pdqr_fun()`.
    is_elem_number <- vapply(f_list, is_single_number, logical(1))
    is_elem_pdqr <- !is_elem_number
    type_vec <-
      vapply(f_list[is_elem_pdqr], attr, which = "type", character(1))
    # Combined type is "discrete" only if all inputs are "discrete"
    res_type <- if (all(type_vec == "discrete")) {
      "discrete"
    } else {
      "continuous"
    }
    res_type
  }
  weights <- impute_weights(weights, length(f_list))


  res_type <- flist_type(f_list)
  sec_col <- if (res_type == "discrete") {
    "prob"
  } else {
    "y"
  }

  x_list <- lapply(seq_along(f_list), function(i) {
    f_typed <- retype_distribution(f_list[[i]], res_type, method = "dirac")

    x_tbl <- as_tbl(f_typed)
    # Do weighting of distributions
    x_tbl[[sec_col]] <- x_tbl[[sec_col]] * weights[i]

    x_tbl
  })

  x_tbl <- stack_mixtbl(x_list, res_type)
  as_density_integrate(x_tbl)
}

#' @rdname distr_generic_n_ops
#' @export
smooth_distribution <-
  function(f,
           n_sample = 10000,
           args_new = list()) {
    f_x_tbl <- as_tbl(f)

    # Handle edge case of single point input (which is possible only if type of
    # `f` is "discrete")
    if (nrow(f_x_tbl) == 1) {
      return(f)
    }

    smpl <- random_dist(f, n_sample)

    # Smooth with `density()`

    call_args <-
      c_dedupl(list(x = smpl, type = "continuous"), args_new)
    con_d <- density_integrate(call_args$x[!is.na(call_args$x)])

    # Account for extra tails that appeared after using `density()`

    con_d <-
      resupport_distribution(con_d, support = distfn_domain_num(con_d), method = "reflect")
    f_x_tbl$y <-
      f_x_tbl$prob <- density_dist(dist = con_d, x = f_x_tbl[["x"]])

    as_density_integrate(f_x_tbl)
  }

#' @rdname distr_generic_n_ops
#' @export
estimate_distribution <- function(f,
                                  stat,
                                  sample_size,
                                  ...,
                                  n_sample = 10000,
                                  args_new = list()) {
  inv_num <- function(x) {
    x[!is_nonnum(x)]
  }
  r_f <- as_rand(f)
  est_smpl <- lapply(seq_len(n_sample), function(i) {
    stat(inv_num(r_f(sample_size), ...))
  })

  # Check outputs of `stat`
  est_smpl_is_number <-
    vapply(est_smpl, is_single_number, logical(1))
  est_smpl_is_bool <-
    vapply( # Not using `is_truefalse()` here because `NA` output is allowed
      est_smpl, function(x) {
        is.logical(x) && (length(x) == 1)
      }, logical(1)
    )
  if (!all(est_smpl_is_number | est_smpl_is_bool)) {
    cat_stop("All outputs of `stat` should be single numeric or logical values.")
    return(NULL)
  }
  est_smpl <- unlist(est_smpl)

  # Return boolean pdqr-function if all outputs are logical
  if (is.logical(est_smpl)) {
    prob_true <- mean(est_smpl, na.rm = TRUE)

    return(boolean_dist(prob_true))
  }

  density_integrate(est_smpl)
}

#' @rdname distr_generic_n_ops
#' @export
recenter_distribution <- function(f, to, method = "mean") {
  f + (to - distrstats_center(f, method))
}

#' @rdname distr_generic_n_ops
#' @export
respread_distribution <-
  function(f,
           to,
           method = c("var", "sd", "iqr", "mad", "range"),
           center_method = c("mean", "median", "mode", "midrange")) {
    method <- match.arg(method)
    center_method <- match.arg(center_method)
    center <- distrstats_center(f, center_method)

    if (to == 0) {
      recenter_distribution(f = f, to = 0, method = center_method)
    } else {
      cur_spread <- distrstats_spread(f, method)
      coef <- switch(method,
                     var = sqrt(to / cur_spread),
                     to / cur_spread
      )
      f_coef <- f * coef
      coef <- center * (1 - coef)

      f_coef + coef
    }
  }

#' @rdname distr_generic_n_ops
#' @export
tail_distribution <-
  function(f,
           level,
           method = c("trim", "winsor"),
           direction = "both") {
    method <- match.arg(method)
    switch(method,
           trim = tails_trim(f, level, direction),
           winsor = tails_winsor(f, level, direction)
    )
  }

#' @keywords internal
tails_trim <- function(f, level, direction = "both") {
  trim_all <- function(f, direction) {
    f_x_tbl <- as_tbl(f)

    res_x <- switch(direction,
                    left = max(f_x_tbl[["x"]]),
                    right = min(f_x_tbl[["x"]]),
                    both = quantile_dist(f, 0.5)
    )

    as_density_integrate(res_x)
  }
  is_all_trimmed <-
    ((direction %in% c("left", "right")) && (level == 1)) ||
    ((direction == "both") && (level == 0.5))
  if (is_all_trimmed) {
    return(trim_all(f, direction))
  }
  tails_trim_dis <- function(f, level, direction) {
    f_x_tbl <- as_tbl(f)

    new_supp <- compute_support_after_remove(f, level, direction)

    if (direction %in% c("left", "both")) {
      x_is_to_remove <- f_x_tbl[["x"]] < new_supp[1]
      tot_p_to_remove <- sum(f_x_tbl[["prob"]][x_is_to_remove])
      f_x_tbl <- f_x_tbl[!x_is_to_remove, ]

      # Delete "underremoved" probability from left "x" value. If all probability
      # is removed, the whole row is removed.
      f_x_tbl <- decrease_row_prob(f_x_tbl,
                                   row = 1,
                                   by_prob = level - tot_p_to_remove
      )
    }

    if (direction %in% c("right", "both")) {
      x_is_to_remove <- f_x_tbl[["x"]] > new_supp[2]
      tot_p_to_remove <- sum(f_x_tbl[["prob"]][x_is_to_remove])
      f_x_tbl <- f_x_tbl[!x_is_to_remove, ]

      # Delete "underremoved" probability from right "x" value. If all probability
      # is removed, the whole row is removed.
      f_x_tbl <- decrease_row_prob(f_x_tbl,
                                   row = nrow(f_x_tbl),
                                   by_prob = level - tot_p_to_remove
      )
    }

    as_density_integrate(f_x_tbl)
  }

  tails_trim_con <- function(f, level, direction) {
    compute_support_after_remove <- function(f, level, direction) {
      supp <- distfn_domain_num(f)
      q_f <- as_quan(f)

      if (direction %in% c("left", "both")) {
        left_val <- q_f(level)
      } else {
        left_val <- supp[1]
      }
      if (direction %in% c("right", "both")) {
        right_val <- q_f(1 - level)
      } else {
        right_val <- supp[2]
      }

      c(left_val, right_val)
    }

    new_supp <- compute_support_after_remove(f, level, direction)

    resupport_distribution(f, new_supp, method = "trim")
  }


  switch(attr(f, "type"),
         discrete = tails_trim_dis(f, level, direction),
         continuous = tails_trim_con(f, level, direction)
  )
}

#' @keywords internal
tails_winsor <- function(f, level, direction = "both") {
  new_supp <- compute_support_after_remove(f, level, direction)

  resupport_distribution(f, new_supp, method = "winsor")
}

#' @keywords internal
decrease_row_prob <- function(x_tbl, row, by_prob) {
  res_row_prob <- x_tbl[["prob"]][row] - by_prob
  if (res_row_prob == 0) {
    x_tbl <- x_tbl[-row, ]
  } else {
    x_tbl[["prob"]][row] <- res_row_prob
  }

  x_tbl
}

#' @keywords internal
compute_support_after_remove <- function(f, level, direction) {
  supp <- distfn_domain_num(f)
  q_f <- as_quan(f)

  if (direction %in% c("left", "both")) {
    left_val <- q_f(level)
  } else {
    left_val <- supp[1]
  }
  if (direction %in% c("right", "both")) {
    right_val <- q_f(1 - level)
  } else {
    right_val <- supp[2]
  }

  c(left_val, right_val)
}

#' @rdname distr_generic_n_ops
#' @export
distrstats_interval <- function(f,
                                level = 0.95,
                                method = c("minwidth", "percentile", "sigma"),
                                n_grid = 10001) {

  method <- match.arg(method)

  edge_probs <- 0.5 * c(1 - level, 1 + level)
  res <- switch(
    method,
    minwidth = interval_minwidth(f, level, n_grid),
    percentile = quantile_dist(f,edge_probs),
    sigma = distrstats_center(f,"mean") + stats::qnorm(edge_probs) * distrstats_spread(f,"sd")
  )
  f_supp <- distfn_domain_num(f)
  data.frame(left = max(f_supp[1], res[1]),right = min(f_supp[2], res[2]))
}
#' @keywords internal
interval_minwidth <- function(f, level = 0.95, n_grid = 10001) {
  if (level == 0) {
    mode <- distrstats_center(f, method = "mode",methods_mode = "global")

    return(c(mode, mode))
  }

  n_seq <- seq_len(n_grid)

  left_prob_seq <- seq(0, 1 - level, length.out = n_grid)
  prob_seq <- c(left_prob_seq, left_prob_seq + level)
  quants <- quantile_dist(f,prob_seq)

  width_vec <- quants[n_seq + n_grid] - quants[n_seq]
  # Rounding is needed to overcome numerical storage precision issues
  minwidth_ind <- which.min(round(width_vec, digits = 10))

  quants[minwidth_ind + c(0, n_grid)]
}



# statistics -

#' @rdname distr_generic_n_ops
#' @export
distrstats_center <-
  function(f,
           method = c("mean", "median", "mode", "midrange"),
           methods_mode = c("global", "local")) {
    distrstats_mean <- function(f) {
      # Not using `raw_moment(f, 1)` for speed reasons
      x_tbl <- as_tbl(f)
      dotprod <- function(x, y) {
        sum(x * y, na.rm = TRUE)
      }
      distrstats_mean_con <- function(x_tbl) {
        n <- nrow(x_tbl)
        x_left <- x_tbl[["x"]][-n]
        x_right <- x_tbl[["x"]][-1]
        dx <- x_right - x_left
        y_left <- x_tbl[["y"]][-n]
        y_right <- x_tbl[["y"]][-1]

        # Not putting `dx` out of brackets to be more sure about the case of
        # dirac-like functions
        sum(dx * (2 * y_left + y_right) * x_left + dx * (y_left + 2 * y_right) * x_right) / 6
      }

      switch(attr(f, "type"),
             discrete = dotprod(x_tbl[["x"]], x_tbl[["prob"]]),
             continuous = distrstats_mean_con(x_tbl)
      )
    }
    distrstats_median <- function(f) {
      quantile_dist(f, 0.5)
    }
    distrstats_mode <-
      function(f, methods_mode = c("global", "local")) {
        f_x_tbl <- as_tbl(f)
        x <- f_x_tbl[["x"]]
        col_name <-
          switch(attr(f, "type"),
                 discrete = "prob",
                 continuous = "y"
          )
        col <- f_x_tbl[[col_name]]

        if (method == "global") {
          # Returns the first (smallest) value if there are more than 1
          x[which.max(col)]
        } else {
          col_left <- col[-length(col)]
          col_right <- col[-1]
          col_geq_right <- c(col_left >= col_right, TRUE)
          col_geq_left <- c(TRUE, col_right >= col_left)

          x[col_geq_right & col_geq_left]
        }
      }
    distrstats_midrange <- function(f) {
      supp <- distfn_domain_num(f)

      (supp[1] + supp[2]) / 2
    }


    method <- match.arg(method)
    methods_mode <- match.arg(methods_mode)
    switch(method,
           mean = distrstats_mean(f),
           median = distrstats_median(f),
           mode = distrstats_mode(f, methods_mode = methods_mode),
           midrange = distrstats_midrange(f)
    )
  }
#' @rdname distr_generic_n_ops
#' @export
distrstats_metrics <- function(f, g, threshold, method = "F1") {
  compute_classmetric_vec <-
    function(f, g, threshold, method = "F1") {
      classmetric(
        p_f_t = as_prob(f)(threshold),
        p_g_t = as_prob(g)(threshold),
        method = classmetric_names(out = "aliases")[method]
      )
    }
  compute_classmetric_data <-
    function(f, g, threshold, method = "F1") {
      if (!all(method %in% names(classmetric_names(out = "aliases")))) {
        cat_stop("`method` should contain only values allowed in `distrstats_metrics()`.")
      }

      # Speed optimization (skips possibly expensive assertions)
      p_f_t <- as_prob(f)(threshold)
      p_g_t <- as_prob(g)(threshold)

      res <- data.frame(threshold = threshold)
      for (meth in method) {
        res[[meth]] <-
          classmetric(p_f_t, p_g_t, classmetric_names(out = "aliases")[meth])
      }

      res
    }
  classmetric <- function(p_f_t, p_g_t, method) {
    # In 'pdqr' setup, total amount of "negative" and "positive" values is 1 for
    # each. In classification terminology, it means that `P = 1` and `N = 1`.
    # This also implies that quantities "TP" (amount of "true positive"), "TN"
    # (amount of "true negative"), "FP" (amount of "false positive"), and "FN"
    # (amount of "false negative") are equal to corresponding "rates".
    classmetric_op <- function(p_f_t, p_g_t) {
      tpr <- classmetric(p_f_t, p_g_t, "TPR")
      tnr <- classmetric(p_f_t, p_g_t, "TNR")
      acc <- classmetric(p_f_t, p_g_t, "Acc")

      acc - abs(tpr - tnr) / (tpr + tnr)
    }
    classmetric_mcc <- function(p_f_t, p_g_t) {
      tp <- 1 - p_g_t
      tn <- p_f_t
      fp <- 1 - p_f_t
      fn <- p_g_t

      # TP + FN = 1; TN + FP = 1
      (tp * tn - fp * fn) / sqrt((tp + fp) * (tn + fn))
    }
    switch(method,
           # Simple metrics

           # TPR is a proportion of actual positives correctly identified
           TPR = 1 - p_g_t,
           # TNR is a proportion of actual negatives correctly identified
           TNR = p_f_t,
           # FPR is a proportion of actual negatives not correctly identified (as pos.)
           FPR = 1 - p_f_t,
           # FNR is a proportion of actual positives not correctly identified (as neg.)
           FNR = p_g_t,
           # PPV = TP / (TP + FP)
           PPV = (1 - p_g_t) / ((1 - p_g_t) + (1 - p_f_t)),
           # NPV = TN / (TN + FN)
           NPV = p_f_t / (p_f_t + p_g_t),
           # FDR = FP / (FP + TP)
           FDR = (1 - p_f_t) / ((1 - p_f_t) + (1 - p_g_t)),
           # FOR = FN / (FN + TN)
           FOR = p_g_t / (p_g_t + p_f_t),
           # LR+ = TPR / (1 - TNR)
           `LR+` = (1 - p_g_t) / (1 - p_f_t),
           # LR- = (1 - TPR) / TNR
           `LR-` = p_g_t / p_f_t,

           # Combined metrics

           # Ac = (TP + TN) / (TP + TN + FP + FN)
           Acc = ((1 - p_g_t) + p_f_t) / 2,
           # ER = (FP + FN) / (TP + TN + FP + FN)
           ER = ((1 - p_f_t) + p_g_t) / 2,
           # GM = sqrt(TPR * TNR)
           GM = sqrt((1 - p_g_t) * p_f_t),
           # F1 = 2*TP / (2*TP + FP + FN)
           F1 = 2 * (1 - p_g_t) / (2 * (1 - p_g_t) + (1 - p_f_t) + p_g_t),
           # OP = Acc - abs(TPR - TNR) / (TPR + TNR)
           OP = classmetric_op(p_f_t, p_g_t),
           # MCC is computed based on explicit formula in `classmetric_mcc()`
           MCC = classmetric_mcc(p_f_t, p_g_t),
           # YI = TPR + TNR - 1
           YI = (1 - p_g_t) + p_f_t - 1,
           # MK = PPV + NPV - 1
           MK = classmetric(p_f_t, p_g_t, "PPV") +
             classmetric(p_f_t, p_g_t, "NPV") - 1,
           # Jaccard = TP / (TP + FN + FP)
           Jaccard = (1 - p_g_t) / (1 + (1 - p_f_t)),
           # DOR = LR+ / LR-
           DOR = ((1 - p_g_t) / (1 - p_f_t)) / (p_g_t / p_f_t)
    )
  }
  classmetric_names <- function(out = c("aliases", "methods")) {
    aliases <- c(
      TPR = "TPR",
      TP = "TPR",
      sensitivity = "TPR",
      recall = "TPR",
      TNR = "TNR",
      TN = "TNR",
      specificity = "TNR",
      FPR = "FPR",
      FP = "FPR",
      `fall-out` = "FPR",
      FNR = "FNR",
      FN = "FNR",
      miss_rate = "FNR",
      PPV = "PPV",
      precision = "PPV",
      NPV = "NPV",
      FDR = "FDR",
      FOR = "FOR",
      `LR+` = "LR+",
      `LR-` = "LR-",
      Acc = "Acc",
      accuracy = "Acc",
      ER = "ER",
      error_rate = "ER",
      GM = "GM",
      F1 = "F1",
      OP = "OP",
      MCC = "MCC",
      corr = "MCC",
      YI = "YI",
      youden = "YI",
      informedness = "YI",
      MK = "MK",
      markedness = "MK",
      Jaccard = "Jaccard",
      DOR = "DOR",
      odds_ratio = "DOR"
    )
    methods <- names(aliases)
    switch(out,
           methods = methods,
           aliases = aliases
    )
  }


  if (!all(method %in% names(classmetric_names(out = "aliases")))) {
    cat_stop("`method` should contain only values allowed in `distrstats_metrics()`.")
    return(NULL)
  }
  if (!is_distrobj(f)) {
    cat_stop("{f} argument of should be distrobj-object")
    return(NULL)
  }
  if (!is_distrobj(g)) {
    cat_stop("{g} argument of should be distrobj-object")
    return(NULL)
  }

  if (length(method) > 1) {
    compute_classmetric_data(
      f = f,
      g = g,
      threshold = threshold,
      method = method
    )
  } else {
    compute_classmetric_vec(
      f = f,
      g = g,
      threshold = threshold,
      method = method
    )
  }
}

#' @rdname distr_generic_n_ops
#' @export
distrstats_roc <- function(f, g, n_grid = 1001) {
  # This is needed to achieve [0; 1] range of both `fpr` and `tpr` in case of
  # "discrete" type input
  #
  union_support <- function(f, g) {
    range(distfn_domain_num(f), distfn_domain_num(g))
  }
  t_range <- stretch_range(union_support(f, g))
  # Inversing range so that `t_grid` will be decreasing sequence. This is needed
  # to order points of ROC curve from left to right.
  t_grid <- seq_between(t_range[2:1], length.out = n_grid)

  fpr <- 1 - as_prob(f)(t_grid)
  tpr <- 1 - as_prob(g)(t_grid)

  data.frame(
    threshold = t_grid,
    fpr = fpr,
    tpr = tpr
  )
}

#' @rdname distr_generic_n_ops
#' @export
distrstats_rocauc <-
  function(f,
           g,
           method = c("expected", "pessimistic", "optimistic")) {
    # Speed optimization (skips possibly expensive assertions)
    method <- match.arg(method)
    method_coef <-
      switch(method,
             expected = 0.5,
             pessimistic = 0,
             optimistic = 1
      )

    prob_greater(g, f) + method_coef * prob_equal(g, f)
  }

#' @rdname distr_generic_n_ops
#' @export
distrstats_moment <- function(f, order) {
  distrstats_moment_con <- function(x_tbl, k) {
    # `E[X^k] = integral{x^k * f(x)} = sum(integral_i{x^k * (A_i*x + B-i)})`
    n <- nrow(x_tbl)
    x_l <- x_tbl[["x"]][-n]
    x_r <- x_tbl[["x"]][-1]

    # Powers are precomputed for speed reasons
    x_l_k <- x_l^k
    x_l_k1 <- x_l_k * x_l
    x_r_k <- x_r^k
    x_r_k1 <- x_r_k * x_r

    y_l <- x_tbl[["y"]][-n]
    y_r <- x_tbl[["y"]][-1]

    coeffs <-
      compute_piecelin_density_coeffs(x_tbl, seq_len(n - 1))

    piece_moments <-
      coeffs[["slope"]] * (x_r_k1 * x_r - x_l_k1 * x_l) / (k + 2) +
      coeffs[["intercept"]] * (x_r_k1 - x_l_k1) / (k + 1)

    # If `x_r-x_l << 1` and `pmax(y_l, y_r) >> 1` (which happens in case of
    # dirac-like approximations), above expression suffer from numerical
    # representation accuracy. In that case use approximate trapezoidal
    # integration.
    approx_piece_moments <-
      (x_r - x_l) * (x_l_k * y_l + x_r_k * y_r) / 2

    # Using third degree because it sufficiently describes "a lot bigger"
    # conditions
    diff_x_is_small <-
      (x_r - x_l <= 1e-3) & (pmax(y_l, y_r) >= 1e3)
    piece_moments[diff_x_is_small] <-
      approx_piece_moments[diff_x_is_small]

    sum(piece_moments)
  }

  x_tbl <- as_tbl(f)
  dotprod <- function(x, y) {
    sum(x * y, na.rm = TRUE)
  }
  switch(attr(f, "type"),
         discrete = dotprod(x_tbl[["x"]]^order, x_tbl[["prob"]]),
         continuous = distrstats_moment_con(x_tbl, order)
  )
}

#' @rdname distr_generic_n_ops
#' @export
distrstats_spread <- function(f, method=c("var","sd","iqr","mad","range")) {
  is_zero_tail <- function(vec, type) {
    n <- length(vec)
    is_zero <- vec == 0

    # Check for first element being zero is made for performance reasons
    if (is_zero[1]) {
      left_is_zero <- cumsum(is_zero) == 1:n

      if (type == "continuous") {
        # Exclude the "most center" zero
        left_is_zero <- left_is_zero &
          duplicated.default(left_is_zero, fromLast = TRUE)
      }
    } else {
      left_is_zero <- rep(FALSE, n)
    }

    # Check for last element being zero is made for performance reasons
    if (is_zero[n]) {
      right_is_zero <- (cumsum(is_zero[n:1]) == 1:n)[n:1]

      if (type == "continuous") {
        # Exclude the "most center" zero
        right_is_zero <-
          right_is_zero & duplicated.default(right_is_zero)
      }
    } else {
      right_is_zero <- rep(FALSE, n)
    }

    left_is_zero | right_is_zero
  }
  distrstats_mad <- function(f) {
    med <- distrstats_center(f, method = "median")
    f_med <- abs(f - med)
    distrstats_center(f_med, method = "median")
  }
  distrstats_var <- function(f) {
    max(
      -distrstats_center(f, method = "mean")^2 + distrstats_moment(f, order = 2),
      0
    )
  }
  distrstats_sd <- function(f) {
    sqrt(distrstats_var(f))
  }
  distrstats_range <- function(f) {
    x_tbl <- as_tbl(f)
    x <- x_tbl[["x"]]
    d_vals <- x_tbl[["y"]]

    # Note that this code assumes that "x_tbl" metadata is arranged in ascending
    # order of "x" column
    within_pos_prob <-
      !is_zero_tail(d_vals, type = attr(f, "type"))
    x_range <- range(x[within_pos_prob])

    x_range[2] - x_range[1]
  }
  distrstats_iqr <- function(f) {
    quarts <- quantile_dist(f, c(0.25, 0.75))

    quarts[2] - quarts[1]
  }
  method <- match.arg(method)
  switch(method,
         var = distrstats_var(f),
         sd = distrstats_sd(f),
         iqr = distrstats_iqr(f),
         mad = distrstats_mad(f),
         range = distrstats_range(f)
  )
}

#' @rdname distr_generic_n_ops
#' @export
roc_plot <- function(roc, ..., add_bisector = TRUE) {
  is_roc <- function(roc) {
    is.data.frame(roc) &&
      all(c("threshold", "fpr", "tpr") %in% names(roc)) &&
      is.numeric(roc[["fpr"]]) && is.numeric(roc[["tpr"]]) &&
      is.numeric(roc[["threshold"]])
  }
  assert_roc <- function(roc) {
    if (dont_assert()) {
      return(TRUE)
    }

    if (missing(roc)) {
      cat_missing("`roc`", "data frame for ROC curve")
    }
    if (!is_roc(roc)) {
      cat_stop("`roc` should represent ROC curve. See `summ_roc()`.")
    }

    TRUE
  }
  assert_roc(roc)

  roc_name <- deparse(substitute(roc))

  # Ensure ordering of points from left to right with respect to "fpr" using
  # inverse relationship between "threshold" and "fpr". This is needed to avoid
  # possible "zig-zags" in output line.
  if (is.unsorted(-roc[["threshold"]])) {
    roc <- roc[order(roc[["threshold"]], decreasing = TRUE), ]
  }

  plot_args <- c_dedupl(
    list(x = roc[["fpr"]], y = roc[["tpr"]]),
    ...,
    list(
      type = "l",
      xlim = c(0, 1),
      ylim = c(0, 1),
      xlab = "FPR or (1 - specificity)",
      ylab = "TPR or sensitivity",
      main = paste0("ROC curve for ", roc_name)
    )
  )
  do.call(graphics::plot, plot_args)

  if (add_bisector) {
    graphics::lines(c(0, 1), c(0, 1), lty = "dotted")
  }

  invisible()
}

#' @rdname distr_generic_n_ops
#' @export
roc_lines <- function(roc, ...) {
  is_roc <- function(roc) {
    is.data.frame(roc) &&
      all(c("threshold", "fpr", "tpr") %in% names(roc)) &&
      is.numeric(roc[["fpr"]]) && is.numeric(roc[["tpr"]]) &&
      is.numeric(roc[["threshold"]])
  }
  assert_roc <- function(roc) {
    if (dont_assert()) {
      return(TRUE)
    }

    if (missing(roc)) {
      cat_missing("roc", "data frame for ROC curve")
    }
    if (!is_roc(roc)) {
      cat_stop("roc should represent ROC curve. See `summ_roc()`.")
    }

    TRUE
  }

  assert_roc(roc)

  # Ensure ordering of points from left to right with respect to "fpr" using
  # inverse relationship between "threshold" and "fpr". This is needed to avoid
  # possible "zig-zags" in output line.
  if (is.unsorted(-roc[["threshold"]])) {
    roc <- roc[order(roc[["threshold"]], decreasing = TRUE), ]
  }

  lines_args <- c_dedupl(
    list(x = roc[["fpr"]], y = roc[["tpr"]]),
    ...,
    list(type = "l")
  )

  do.call(graphics::lines, lines_args)
}

#' @rdname distr_generic_n_ops
#' @export
distrstats_separation <- function(f,
                                  g,
                                  method = c("KS", "GM", "OP", "F1", "MCC"),
                                  n_grid = 10001) {
  separation_ks <- function(f, g) {
    f_type <- attr(f, "type")
    g_type <- attr(g, "type")

    if (f_type == "discrete") {
      if (g_type == "discrete") {
        separation_ks_two_dis(f, g)
      } else {
        separation_ks_mixed(dis = f, con = g)
      }
    } else {
      if (g_type == "discrete") {
        separation_ks_mixed(dis = g, con = f)
      } else {
        separation_ks_two_con(f, g)
      }
    }
  }

  separation_ks_two_dis <- function(f, g) {
    union_x <- function(f, g) {
      f_x <- as_tbl(f)[["x"]]
      g_x <- as_tbl(g)[["x"]]

      sort(union(f_x, g_x))
    }
    x_test <- union_x(f, g)
    p_f <- as_prob(f)
    p_g <- as_prob(g)
    max_ind <- which.max(abs(p_f(x_test) - p_g(x_test)))

    x_test[max_ind]
  }

  separation_ks_mixed <- function(dis, con) {
    p_dis <- as_prob(dis)

    p_con <- as_prob(con)

    dis_test <- as_tbl(dis)[["x"]]

    p_con_cumprob <- p_con(dis_test)

    p_dis_cumprob <- as_tbl(dis)[["cumprob"]]
    p_dis_left_cumprob <-
      c(0, p_dis_cumprob[-length(p_dis_cumprob)])

    cdf_absdiff_dis <- round(alternate(
      abs(p_con_cumprob - p_dis_cumprob),
      abs(p_con_cumprob - p_dis_left_cumprob)
    ), digits = 12)

    max_ind_dis <- which.max(cdf_absdiff_dis)
    sep_dis <- dis_test[ceiling(max_ind_dis / 2)]
    sep_dis_absdiff <- cdf_absdiff_dis[max_ind_dis]

    con_x_tbl <- as_tbl(con)
    con_x <- con_x_tbl[["x"]]
    cdf_absdiff_con <-
      round(abs(con_x_tbl[["cumprob"]] - p_dis(con_x)), digits = 12)

    max_ind_con <- which.max(cdf_absdiff_con)
    sep_con <- con_x[max_ind_con]
    sep_con_absdiff <- cdf_absdiff_con[max_ind_con]

    if (sep_con_absdiff >= sep_dis_absdiff) {
      min(sep_dis, sep_con)
    } else {
      sep_dis
    }
  }

  separation_ks_two_con <- function(f, g) {
    x_inters <- compute_density_crossings(f, g)
    x_test <-
      sort(c(distfn_domain_num(f), distfn_domain_num(g), x_inters))

    p_f <- as_prob(f)
    p_g <- as_prob(g)


    max_ind <- which.max(abs(p_f(x_test) - p_g(x_test)))

    x_test[max_ind]
  }


  # Early returns in cases of non-overlapping supports
  .separation <- function(f, g, method, threshold, n_grid = 10001) {
    union_support <- function(f, g) {
      range(distfn_domain_num(f), distfn_domain_num(g))
    }
    t_grid <-
      seq_between(union_support(f, g), length.out = n_grid)

    val01 <-
      distrstats_metrics(f, g, method = method, threshold = threshold)
    val02 <-
      distrstats_metrics(g, f, method = method, threshold = threshold)

    # `alternate()` is used to ensure that the smallest value is returned in case
    # of several alternatives
    metric <- alternate(val01, val02)

    target_ind <- which.max(metric)

    t_grid[ceiling(target_ind / 2)]
  }
  method <- match.arg(method)
  f_supp <- distfn_domain_num(f)
  g_supp <- distfn_domain_num(g)

  if (g_supp[1] >= f_supp[2]) {
    return((g_supp[1] + f_supp[2]) / 2)
  }
  if (f_supp[1] >= g_supp[2]) {
    return((f_supp[1] + g_supp[2]) / 2)
  }

  # Main cases
  switch(method,
         KS = separation_ks(f, g),
         .separation(f, g, method, n_grid)
  )
}
