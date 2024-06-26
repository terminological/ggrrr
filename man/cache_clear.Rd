% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zz-api-cache.R
\name{cache_clear}
\alias{cache_clear}
\title{Clear data from the passthrough cache for complex or long running operations}
\usage{
cache_clear(
  .cache = getOption("cache.dir", default = rappdirs::user_cache_dir("ggrrr")),
  .prefix = ".*",
  interactive = TRUE
)
}
\arguments{
\item{.cache}{the location of the cache as a directory. May get its value from options("ggrrr.cache.dir") or the default value of rappdirs::user_cache_dir("ggrrr")}

\item{.prefix}{a regular expression matching the prefix of the cached item, so that do selective clean up operations. defaults to everything.}

\item{interactive}{suppress `are you sure?` warning with a FALSE value (defaults to TRUE)}
}
\value{
nothing. called for side effects
}
\description{
Clear data from the passthrough cache for complex or long running operations
}
\examples{
cache_clear(.prefix="example", .cache=tempdir(), interactive=FALSE)
}
\concept{cache}
