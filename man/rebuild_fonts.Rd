% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/font-utils.R
\name{rebuild_fonts}
\alias{rebuild_fonts}
\title{Rebuild font caches}
\usage{
rebuild_fonts()
}
\value{
nothing
}
\description{
This repopulates `systemfonts` from the webfont cache and then `extrafont` from
`systemfonts`. This does a full rebuild and will be slow (depending a bit )
}
