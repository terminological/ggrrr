% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standalone-tidy-utils.R
\name{$.checked_list}
\alias{$.checked_list}
\title{Checked list accessor}
\usage{
\method{$}{checked_list}(x, y)
}
\arguments{
\item{x}{the list}

\item{y}{the item}
}
\value{
the value of the list item or an error if it does not exist
}
\description{
A checked list is a sub class of list access operator that throws an error if
you attempt to access a value that does not exist (rather than returning
NULL) the point of this is to throw errors early if the data changes.
}
