% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standalone-tidy-utils.R
\name{intersecting_group_by}
\alias{intersecting_group_by}
\title{Create a dataframe with groups mathing a range of predicates}
\usage{
intersecting_group_by(.data, ..., .colname)
}
\arguments{
\item{.data}{a data frame}

\item{...}{a set of predicates specified like case_whens syntax, such as mpg < 5 ~ "gas guzzlers"}

\item{.colname}{the name of the new group}
}
\value{
a new dataframe containing the overlapping groups which may create duplicates of individual rows.
}
\description{
Create a new data frame including duplicate rows where the rows fulfil a potentially overlapping set of conditions
specified as named predicates (as formulae)
}
\examples{
library(tidyverse)
iris \%>\% dplyr::group_by(Species) \%>\% intersecting_group_by(
  Sepal.Length > mean(Sepal.Length) ~ "Long",
  Sepal.Width > mean(Sepal.Width) ~ "Wide"
)
}
