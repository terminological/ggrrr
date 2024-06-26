% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standalone-cache.R
\name{.cache_download}
\alias{.cache_download}
\title{Download a file into a local cache.}
\usage{
.cache_download(
  url,
  ...,
  .nocache = getOption("cache.disable", default = FALSE),
  .cache = rappdirs::user_cache_dir(utils::packageName()),
  .stale = Inf,
  .extn = NULL
)
}
\arguments{
\item{url}{the url to download}

\item{...}{passed to `utils::download.file()`}

\item{.nocache}{if set to TRUE all caching is disabled}

\item{.cache}{the location of the downloaded files}

\item{.stale}{how long to leave this file before replacing it.}

\item{.extn}{the file name extension}
}
\value{
the path to the downloaded file
}
\description{
This function copies a remote file to a local cache once and
makes sure it is reused.
}
\concept{cache}
\keyword{internal}
