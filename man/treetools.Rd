% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treetools.R
\name{treetools}
\alias{treetools}
\alias{xtree}
\alias{xforest}
\alias{rpart_fn}
\title{fast tree implementation}
\usage{
xtree(data, ...)

xforest(data, ...)

rpart_fn(x)
}
\description{
fast tree implementation

fast forest implementation

rpart to case_when
}
\examples{


set.seed(7)
data <- datalearn$penguins_lite \%>\% tidyr::drop_na()
xt_obj <- xtree(data = data, formula = species ~ .)
predict(xt_obj, data)


set.seed(7)
data <- datalearn$penguins_lite \%>\% tidyr::drop_na()
xf_obj <- xforest(data = data, formula = species ~ ., trees = 25)
predict(xf_obj, data)

set.seed(7)
data <- datalearn$penguins_lite \%>\% tidyr::drop_na()

rpart_fit <- rpart::rpart(formula = species ~ ., data = data)
rpart_fn(rpart_fit)
}
