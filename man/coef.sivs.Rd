% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef.sivs.R
\name{coef.sivs}
\alias{coef.sivs}
\title{Extract Coefficients from sivs object}
\usage{
\method{coef}{sivs}(object, step = "iterative.res", ...)
}
\arguments{
\item{object}{An object of class "sivs"}

\item{step}{A a character string of length 1. It should either specify the
step ("iterative.res" or "rfe"), or step$subsetp (e.g "rfe$baseline").}

\item{...}{potential further arguments (required for Method/Generic reasons).}
}
\value{
The function returns a data.frame that has features as rows and
different runs as columns, with the addition of the first column which
contains the feature name.
}
\description{
A function to extract the coefficients of "iterative.res" step
or any part of "rfe" such as "sivs_object$rfe$baseline" from a sivs object.
}
\examples{
\dontrun{
# getting the coefficients of features for the baseline runs in rfe
coef(object = sivs_object, step = "rfe$baseline")
}

## WORKING EXAMPLE
## Note that this example does not logically make sense as iris data has only
## 4 columns and there is no need for SIVS to take care of feature selection
## therefore this example is only here for testing purposes.

tmp <- subset(x = iris, subset = Species != "setosa")

tmp <- varhandle::unfactor(tmp)

sivs_obj <- sivs(x = tmp[, c("Sepal.Length", "Sepal.Width",
                             "Petal.Length", "Petal.Width")],
                 y = factor(tmp$Species),
                 family = "binomial",
                 verbose = "detailed",
                 progressbar = FALSE,
                 nfolds = 3,
                 parallel.cores = FALSE,
                 iter.count = 20)

coef(sivs_obj)


}
