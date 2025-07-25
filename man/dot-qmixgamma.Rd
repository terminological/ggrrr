% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standalone-mixture-distributions.R
\name{.qmixgamma}
\alias{.qmixgamma}
\title{A quantile function for a mixture of gamma distributions}
\usage{
.qmixgamma(
  p,
  shapes,
  rates,
  weights = rep(1, length(shapes)),
  na.rm = FALSE,
  approx = TRUE,
  ...
)
}
\arguments{
\item{p}{vector of probabilities.}

\item{shapes}{a vector of gamma distribution shapes}

\item{rates}{a vector of gamma distribution rates}

\item{weights}{a vector of weights}

\item{na.rm}{remove distributions with NA values for mean or sd}

\item{approx}{use Cornish Fisher moment based approximation if available}

\item{...}{passed on to internal function, `samples=TRUE` to force random sampling}
}
\value{
the value of the `p`th quantile
}
\description{
A quantile function for a mixture of gamma distributions
}
\examples{
try({
  .qmixgamma(p=c(0.025,0.5,0.975), shapes=c(10,13,14), rates=c(1,1,2))
})
}
\concept{distributions}
\keyword{internal}
