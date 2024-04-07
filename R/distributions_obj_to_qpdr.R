#' @include util_env.R
NULL

# qpdr -

#' qpdr
#'
#' @name distr_qpdr
#' @rdname distr_qpdr
#' @keywords internal
#'
#' @examples
#'
#' set.seed(7)
#' vec_cont <- rnorm(n = 1000, mean = 20, sd = 3)
#' vec_cont <- safe_vec(vec_cont)
#' vec_dist <- rbinom(1000, 20:35, .5)
#'
#' ## density integrate
#'
#' dist_cont <- density_integrate(x = vec_cont)
#' dist_disc <- density_integrate(x = vec_dist)
#'
#' # continua
#'
#' q <- quantile_dist(dist = dist_cont, p = seq(.1, .9, .1))
#' q
#' p <- probability_dist(dist = dist_cont, q = q)
#' p
#' d <- density_dist(dist = dist_cont, x = q)
#' d
#' random_dist(dist = dist_cont, n = 10)
#'
#' f_q <- as_quan(dist = dist_cont)
#' f_p <- as_prob(dist = dist_cont)
#' f_d <- as_dens(dist = dist_cont)
#' f_r <- as_rand(dist = dist_cont)
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#' # discreta
#'
#' q <- quantile_dist(dist = dist_disc, p = seq(.1, .9, .1))
#' q
#' p <- probability_dist(dist = dist_disc, q = q)
#' p
#' d <- density_dist(dist = dist_disc, x = q)
#' d
#' random_dist(dist = dist_disc, n = 10)
#'
#' f_q <- as_quan(dist = dist_disc)
#' f_p <- as_prob(dist = dist_disc)
#' f_d <- as_dens(dist = dist_disc)
#' f_r <- as_rand(dist = dist_disc)
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#' ## fit dist
#'
#' #rm_ls(vec_cont, vec_dist)
#'
#' dist_cont <- distrcont(x = vec_cont)
#' dist_disc <- distrdiscr(x = vec_dist)
#'
#' # continua
#'
#' q <- quantile_dist(dist = dist_cont, p = seq(.1, .9, .1))
#' q
#' p <- probability_dist(dist = dist_cont, q = q)
#' p
#' d <- density_dist(dist = dist_cont, x = q)
#' d
#' random_dist(dist = dist_cont, n = 10)
#'
#' f_q <- as_quan(dist = dist_cont)
#' f_p <- as_prob(dist = dist_cont)
#' f_d <- as_dens(dist = dist_cont)
#' f_r <- as_rand(dist = dist_cont)
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#' # discreta
#'
#' q <- quantile_dist(dist = dist_disc, p = seq(.1, .9, .1))
#' q
#' p <- probability_dist(dist = dist_disc, q = q)
#' p
#' d <- density_dist(dist = dist_disc, x = q)
#' d
#' random_dist(dist = dist_disc, n = 10)
#'
#' f_q <- as_quan(dist = dist_disc)
#' f_p <- as_prob(dist = dist_disc)
#' f_d <- as_dens(dist = dist_disc)
#' f_r <- as_rand(dist = dist_disc)
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#'
#' ## fit_custom_distr
#'
#' #rm_ls(vec_cont, vec_dist)
#'
#' dist_cont <- fit_custom_distr(x = vec_cont)
#' dist_disc <- fit_custom_distr(x = vec_dist)
#'
#' # continua
#'
#' q <- quantile_dist(dist = dist_cont, p = seq(.1, .9, .1))
#' q
#' p <- probability_dist(dist = dist_cont, q = q)
#' p
#' d <- density_dist(dist = dist_cont, x = q)
#' d
#' random_dist(dist = dist_cont, n = 10)
#'
#' f_q <- as_quan(dist = dist_cont)
#' f_p <- as_prob(dist = dist_cont)
#' f_d <- as_dens(dist = dist_cont)
#' f_r <- as_rand(dist = dist_cont)
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#' # discreta
#'
#' q <- quantile_dist(dist = dist_disc, p = seq(.1, .9, .1))
#' q
#' p <- probability_dist(dist = dist_disc, q = q)
#' p
#' d <- density_dist(dist = dist_disc, x = q)
#' d
#' random_dist(dist = dist_disc, n = 10)
#'
#' f_q <- as_quan(dist = dist_disc)
#' f_p <- as_prob(dist = dist_disc)
#' f_d <- as_dens(dist = dist_disc)
#' f_r <- as_rand(dist = dist_disc)
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#'
#' ## truncate
#'
#' #rm_ls(vec_cont, vec_dist,dist_cont,dist_disc)
#'
#' dist_cont <-
#'    truncate_distr(
#'       dist = dist_cont,
#'       upper = quantile(vec_cont, .8),
#'       lower = quantile(vec_cont, .2)
#'    )
#' dist_disc <- truncate_distr(
#'    dist = dist_disc,
#'    upper = ceiling(quantile(vec_dist, .8)),
#'    lower = trunc(quantile(vec_dist, .2))
#' )
#'
#' # continua
#'
#' q <- quantile_dist(dist = dist_cont, p = seq(.1, .9, .1))
#' q
#' p <- probability_dist(dist = dist_cont, q = q)
#' p
#' d <- density_dist(dist = dist_cont, x = q)
#' d
#' random_dist(dist = dist_cont, n = 10)
#'
#' f_q <- as_quan(dist = dist_cont)
#' f_p <- as_prob(dist = dist_cont)
#' f_d <- as_dens(dist = dist_cont)
#' f_r <- as_rand(dist = dist_cont)
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#' # discreta
#'
#' q <- quantile_dist(dist = dist_disc, p = seq(.1, .9, .1))
#' q
#' p <- probability_dist(dist = dist_disc, q = q)
#' p
#' d <- density_dist(dist = dist_disc, x = q)
#' d
#' random_dist(dist = dist_disc, n = 10)
#'
#' f_q <- as_quan(dist = dist_disc)
#' f_p <- as_prob(dist = dist_disc)
#' f_d <- as_dens(dist = dist_disc)
#' f_r <- as_rand(dist = dist_disc)
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#' ## density
#'
#' #rm_ls(vec_cont, vec_dist)
#' dist_cont <- density(x = vec_cont)
#' dist_disc <- density(x = vec_dist)
#'
#' # continua
#'
#' q <- quantile_dist(dist = dist_cont, p = seq(.1, .9, .1),type = "continuous")
#' q
#' p <- probability_dist(dist = dist_cont, q = q,type = "continuous")
#' p
#' d <- density_dist(dist = dist_cont, x = q,type = "continuous")
#' d
#' random_dist(dist = dist_cont, n = 10,type = "continuous")
#'
#' f_q <- as_quan(dist = dist_cont,type = "continuous")
#' f_p <- as_prob(dist = dist_cont,type = "continuous")
#' f_d <- as_dens(dist = dist_cont,type = "continuous")
#' f_r <- as_rand(dist = dist_cont,type = "continuous")
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#' # discreta
#'
#' q <- quantile_dist(dist = dist_disc, p = seq(.1, .9, .1),type = "discrete")
#' q
#' p <- probability_dist(dist = dist_disc, q = q,type = "discrete")
#' p
#' d <- density_dist(dist = dist_disc, x = q,type = "discrete")
#' d
#' random_dist(dist = dist_disc, n = 10,type = "discrete")
#'
#' f_q <- as_quan(dist = dist_disc,type = "discrete")
#' f_p <- as_prob(dist = dist_disc,type = "discrete")
#' f_d <- as_dens(dist = dist_disc,type = "discrete")
#' f_r <- as_rand(dist = dist_disc,type = "discrete")
#'
#' f_q(p = seq(.1, .9, .1)) == q
#' f_p(q = q) == p
#' f_d(x = q) == d
#' f_r(n = 100)
#'
#'

# methods

#' @export
density_dist <- function(dist, ...) {
  UseMethod("density_dist")
}
#' @rdname distr_qpdr

#' @export
quantile_dist <- function(dist, ...) {
  UseMethod("quantile_dist")
}
#' @export
probability_dist <- function(dist, ...) {
  UseMethod("probability_dist")
}
#' @export
random_dist <- function(dist, ...) {
  UseMethod("random_dist")
}

# default

#' @export
density_dist.default <- function(dist, ...) {
  cat_message(
    "No method for { fn } is defined",
    "for { paste0(class(dist), collapse = ',') }.",
    .sep = "",
    fn = stringr::str_remove(
      string = deparse(match.call()[[1]]),
      pattern = stringr::fixed(".default")
    )
  )
}
#' @export
quantile_dist.default <- function(dist, ...) {
  cat_message(
    "No method for { fn } is defined",
    "for { paste0(class(dist), collapse = ',') }.",
    .sep = "",
    fn = stringr::str_remove(
      string = deparse(match.call()[[1]]),
      pattern = stringr::fixed(".default")
    )
  )
}
#' @export
probability_dist.default <- function(dist, ...) {
  cat_message(
    "No method for { fn } is defined",
    "for { paste0(class(dist), collapse = ',') }.",
    .sep = "",
    fn = stringr::str_remove(
      string = deparse(match.call()[[1]]),
      pattern = stringr::fixed(".default")
    )
  )
}
#' @export
random_dist.default <- function(dist, ...) {
  cat_message(
    "No method for { fn } is defined",
    "for { paste0(class(dist), collapse = ',') }.",
    .sep = "",
    fn = stringr::str_remove(
      string = deparse(match.call()[[1]]),
      pattern = stringr::fixed(".default")
    )
  )
}

# distrcont

#' @export
quantile_dist.distrcont <- function(dist, p) {
  fcall <-
    rlang::call2(
      .fn = dist$call_info$quantile[2],
      !!!dist$par,
      .ns = dist$call_info$quantile[1]
    )
  fcall$p <- p
  q<-ifelse(p > 1 | p < 0, NA, eval(fcall))
  q
}
#' @export
density_dist.distrcont <- function(dist, x) {
  if (!is_dblint(x)) {
    cat_stop("x no es un vector numerico")
  }

  fcall <-
    rlang::call2(
      .fn = dist$call_info$density[2],
      !!!dist$par,
      .ns = dist$call_info$density[1]
    )
  fcall$x <- x

  ifelse(!sapply(x, dist$valid_x), 0, eval(fcall))
}
#' @export
probability_dist.distrcont <- function(dist, q) {
  if (!is_dblint(q)) {
    cat_stop("x no es un vector numerico")
  }

  fcall <-
    rlang::call2(
      .fn = dist$call_info$probab[2],
      !!!dist$par,
      .ns = dist$call_info$probab[1]
    )


  fcall$q <- q
  eval(fcall)
}
#' @export
random_dist.distrcont <- function(dist, n) {
  if (length(n) > 1) {
    n <- length(n)
  }
  fcall <-
    rlang::call2(
      .fn = dist$call_info$random[2],
      !!!dist$par,
      .ns = dist$call_info$random[1]
    )

  fcall$n <- n

  r<-eval(fcall)

  r
}

# distrdiscr

#' @export
quantile_dist.distrdiscr <- function(dist, p) {
  fcall <-
    rlang::call2(
      .fn = dist$call_info$quantile[2],
      !!!dist$par,
      .ns = dist$call_info$quantile[1]
    )
  fcall$p <- p
  q<-ifelse(p > 1 | p < 0, NA, eval(fcall))
  q
}
#' @export
density_dist.distrdiscr <- function(dist, x) {
  if (!is_dblint(x)) {
    cat_stop("x no es un vector numerico")
  }

  fcall <-
    rlang::call2(
      .fn = dist$call_info$density[2],
      !!!dist$par,
      .ns = dist$call_info$density[1]
    )
  fcall$x <- x

  ifelse(!sapply(x, dist$valid_x), 0, eval(fcall))
}
#' @export
probability_dist.distrdiscr <- function(dist, q) {
  if (!is_dblint(q)) {
    cat_stop("x no es un vector numerico")
  }

  fcall <-
    rlang::call2(
      .fn = dist$call_info$probab[2],
      !!!dist$par,
      .ns = dist$call_info$probab[1]
    )


  fcall$q <- q
  eval(fcall)
}
#' @export
random_dist.distrdiscr <- function(dist, n) {
  if (length(n) > 1) {
    n <- length(n)
  }
  fcall <-
    rlang::call2(
      .fn = dist$call_info$random[2],
      !!!dist$par,
      .ns = dist$call_info$random[1]
    )

  fcall$n <- n

  r<-eval(fcall)

  r
}

# fit_custom_distr

#' @export
quantile_dist.fit_custom_distr <- function(dist, p) {

  ifelse(p > 1 | p < 0, NA, dist$quantile(p = p))
}
#' @export
density_dist.fit_custom_distr <- function(dist, x) {
  if(attr(dist,"type")=="discrete"){
    x <- round(x)
  }

  ifelse(x > attr(dist, "support")[2] |
           x < attr(dist, "support")[1],
         0,
         dist$density(x = x)
  )
}
#' @export
probability_dist.fit_custom_distr <- function(dist, q) {
  if(attr(dist,"type")=="discrete"){
    q <- round(q)
  }
  dist$probability(q = q)
}
#' @export
random_dist.fit_custom_distr <- function(dist, n) {
  p <- runif(n, 0, 1)
  dist$quantile(p = p)
}

# stats::density

#' @export
quantile_dist.density <- function(dist, type, p) {
  dist <- obj_density_integrate(dens = dist, type = type)

  quanfn <- build_custom_quantile(dist, type)


  ifelse(p > 1 | p < 0, NA, quanfn(p = p))
}
#' @export
density_dist.density <- function(dist, type, x) {
  dist <- obj_density_integrate(dens = dist, type = type)

  densfn <- build_custom_density(dist, type)

  densfn(x = x)
}
#' @export
probability_dist.density <- function(dist, type, q) {
  dist <- obj_density_integrate(dens = dist, type = type)

  probsfn <- build_custom_probability(dist, type)

  probsfn(q = q)
}
#' @export
random_dist.density <- function(dist, type, n) {
  p <- runif(n, 0, 1)
  as_quan(dist, type)(p)
}

# density_integrate
#' @export
quantile_dist.density_integrate <- function(dist, p) {

  f <- build_custom_quantile(dist)
  f(p)
}
#' @export
density_dist.density_integrate <- function(dist, x) {
  if(attr(dist,"type")=="discrete"){
    x <- round(x)
  }
  f <- build_custom_density(dist)
  f(x)
}
#' @export
probability_dist.density_integrate <- function(dist, q) {
  if(attr(dist,"type")=="discrete"){
    q <- round(q)
  }
  f <- build_custom_probability(dist)
  f(q)
}
#' @export
random_dist.density_integrate <- function(dist, n) {
  h <- .Machine$double.eps^.2
  p <- runif(n * 2, min(dist$cumprob,na.rm = T)+h, max(dist$cumprob,na.rm = T)-h)
  q <-quantile_dist(dist, p)
  q <- q[!is_nonnum(q)]
  sample(
    x = q,
    size = n,
    replace = n > length(q)
  )
}


# truncate_distr
#' @export
quantile_dist.truncate_distr <- function(dist, p) {
  dist$q(p)
}
#' @export
density_dist.truncate_distr <- function(dist, x) {
  dist$d(x)
}
#' @export
probability_dist.truncate_distr <- function(dist, q) {
  dist$p(q)
}
#' @export
random_dist.truncate_distr <- function(dist, n) {
  p <- runif(n, 0, 1)
  as_quan(dist)(p)
}


# numeric
#' @export
density_dist.numeric<- function(dist,x = dist){
  # if(is_discrete_distribution(dist)){
  #   x <- round(x)
  #   type<-"discrete"
  # }else {
  #   type <- "continuous"
  # }
  dist <- density_integrate(dist)
  density_dist(dist = dist,x)
}
#' @export
probability_dist.numeric<- function(dist,q = dist){
  if(is_discrete_distribution(dist)){
    #type<-"discrete"
    q <- round(q)
  } else {
    #type <- "continuous"
  }
  dist <- density_integrate(dist)
  probability_dist(dist = dist,q=q)
}
#' @export
quantile_dist.numeric<- function(dist,p){
  # if(is_discrete_distribution(dist)){
  #   type<-"discrete"
  # }else {
  #   type <- "continuous"
  # }
  dist <- density_integrate(dist)
  quantile_dist(dist = dist,p)
}
#' @export
random_dist.numeric<- function(dist,n){
  # if(is_discrete_distribution(dist)){
  #   type<-"discrete"
  # }else {
  #   type <- "continuous"
  # }
  dist <- density_integrate(dist)
  random_dist(dist = dist,n)
}


# as_  -

#' @keywords internal
#' @export
as_dens <- function(dist, ...) {
  UseMethod("as_dens")
}
#' @export
as_quan <- function(dist, ...) {
  UseMethod("as_quan")
}
#' @export
as_prob <- function(dist, ...) {
  UseMethod("as_prob")
}
#' @export
as_rand <- function(dist, ...) {
  UseMethod("as_rand")
}

# default
#' @export
as_dens.default <- function(dist, ...) {
  cat_message(
    "No method for { match.call()[[1]] } is defined",
    "for { paste0(class(dist), collapse = ',') }."
  )
}
#' @export
as_quan.default <- function(dist, ...) {
  cat_message(
    "No method for { match.call()[[1]] } is defined",
    "for { paste0(class(dist), collapse = ',') }."
  )
}
#' @export
as_prob.default <- function(dist, ...) {
  cat_message(
    "No method for { match.call()[[1]] } is defined",
    "for { paste0(class(dist), collapse = ',') }."
  )
}
#' @export
as_rand.default <- function(dist, ...) {
  cat_message(
    "No method for { match.call()[[1]] } is defined",
    "for { paste0(class(dist), collapse = ',') }."
  )
}

# fit_custom_distr
#' @export
as_dens.fit_custom_distr <- function(dist) {
  function(x) {
    density_dist(dist = dist, x = x)
  }
}
#' @export
as_quan.fit_custom_distr <- function(dist) {
  function(p) {
    quantile_dist(dist = dist, p = p)
  }
}
#' @export
as_prob.fit_custom_distr <- function(dist) {
  function(q) {
    probability_dist(dist = dist, q = q)
  }
}
#' @export
as_rand.fit_custom_distr <- function(dist) {
  function(n) {
    random_dist(dist = dist, n = n)
  }
}

# distrcont
#' @export
as_dens.distrcont <- function(dist) {
  function(x) {
    density_dist(dist = dist, x = x)
  }
}
#' @export
as_quan.distrcont <- function(dist) {
  function(p) {
    quantile_dist(dist = dist, p = p)
  }
}
#' @export
as_prob.distrcont <- function(dist) {
  function(q) {
    probability_dist(dist = dist, q = q)
  }
}
#' @export
as_rand.distrcont <- function(dist) {
  function(n) {
    random_dist(dist = dist, n = n)
  }
}

# distrdiscr
#' @export
as_dens.distrdiscr <- function(dist) {
  function(x) {
    density_dist(dist = dist, x = x)
  }
}
#' @export
as_quan.distrdiscr <- function(dist) {
  function(p) {
    quantile_dist(dist = dist, p = p)
  }
}
#' @export
as_prob.distrdiscr <- function(dist) {
  function(q) {
    probability_dist(dist = dist, q = q)
  }
}
#' @export
as_rand.distrdiscr <- function(dist) {
  function(n) {
    random_dist(dist = dist, n = n)
  }
}

# stats::density
#' @export
as_dens.density <- function(dist, type) {
  function(x) {
    density_dist(dist = dist, type = type, x = x)
  }
}
#' @export
as_quan.density <- function(dist, type) {
  function(p) {
    quantile_dist(dist = dist, type = type, p = p)
  }
}
#' @export
as_prob.density <- function(dist, type) {
  function(q) {
    probability_dist(dist = dist, type = type, q = q)
  }
}
#' @export
as_rand.density <- function(dist, type) {
  function(n) {
    random_dist(dist = dist, type = type, n = n)
  }
}


# density_integrate
#' @export
as_dens.density_integrate <- function(dist) {
  function(x) {
    density_dist(dist = dist, x = x)
  }
}
#' @export
as_quan.density_integrate <- function(dist) {
  function(p) {
    quantile_dist(dist = dist, p = p)
  }
}
#' @export
as_prob.density_integrate <- function(dist) {
  function(q) {
    probability_dist(dist = dist, q = q)
  }
}
#' @export
as_rand.density_integrate <- function(dist) {
  function(n) {
    random_dist(dist = dist, n = n)
  }
}


# truncate_distr
#' @export
as_dens.truncate_distr <- function(dist) {
  dist$d
}
#' @export
as_quan.truncate_distr <- function(dist) {
  dist$q
}
#' @export
as_prob.truncate_distr <- function(dist) {
  dist$p
}
#' @export
as_rand.truncate_distr <- function(dist) {
  function(n) {
    p <- runif(n, 0, 1)
    dist$q(p = p)
  }
}
