% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/font-utils.R
\name{reset_fonts}
\alias{reset_fonts}
\title{Reset any custom fonts}
\usage{
reset_fonts(
  confirm = utils::askYesNo(msg = "Are you sure?", default = FALSE),
  web = FALSE,
  fonts = FALSE
)
}
\arguments{
\item{confirm}{set to TRUE to automatically confirm}

\item{web}{also clear webfont cache? (default FALSE)}

\item{fonts}{also clear any downloaded fonts or converted afm files? (default FALSE)}
}
\value{
nothing
}
\description{
This wipes a lot of cached font data.
}
