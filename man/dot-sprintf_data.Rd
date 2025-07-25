% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standalone-text-formatting.R
\name{.sprintf_data}
\alias{.sprintf_data}
\title{`sprintf` with a list(ish) input}
\usage{
.sprintf_data(fmt, params, ..., na.replace = "―")
}
\arguments{
\item{fmt}{a sprintf format string}

\item{params}{the inputs as a list or a dataframe (rather than an enumeration of individual numbers)}

\item{...}{
  Named arguments passed on to \code{\link[=.sprintf_dp]{.sprintf_dp}}\describe{
    \item{\code{sep}}{a character to use a decimal point.}
}}

\item{na.replace}{a value to replace NA values with.}
}
\value{
the formatted string
}
\description{
A variant of sprintf that work well with inputs that are in the format of a list
or a dataframe. Examples of which are the quantile functions. This falls
in between `sprintf` and `glue::glue_data` in complexity.
}
\examples{
# generate an IQR from data
try({
iris \%>\%
  dplyr::group_by(Species) \%>\%
  dplyr::summarise(
    q.Sepal.Width = .sprintf_data(
      "\%1.2f [\%1.2f\u2013\%1.2f]",
      stats::quantile(Sepal.Width, probs = c(0.5,0.25,0.75))
    )
  )

# dataframe, lists or vectors should work. dataframe columns are interpreted
# in the order they are given.

.sprintf_data("\%1.2f - \%1.2f", tibble::tibble(low = c(1,2,NA,4,5), high = c(5,4,3,2,NA)))
.sprintf_data("\%1.2f - \%1.2f", tibble::tibble(low = c(1,2,NA,4,5), high = c(5,4,3,2,NA)), sep=",")
.sprintf_data("\%1.2f - \%1.2f", list(low = 1, high = 5))
.sprintf_data("\%1.2f - \%1.2f", c(1,5))
})
# this is an error because this is the sprintf syntax of enumerated items
try(.sprintf_data("\%1.2f - \%1.2f", 1, 5))
}
\keyword{internal}
