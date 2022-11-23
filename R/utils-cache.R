#.arear.cache <- new.env(parent=emptyenv())

#' A simple pass-through cache for complex or long running operations
#'
#' executes expr and saves the output as an RDS file indexed by has of code in expr
#' and the hash of input variables (which should contain any variable inputs)
#'
#' @param .expr the code the output of which requires caching. Other than a return value this should not create side effects or change global variables.
#' @param ... inputs that the code in expr depends on and changes in which require the code re-running, Could be Sys.Date()
#' @param .prefix a name of the operation so that you can namespace the cached files and do selective clean up operations on them
#' @param .nocache an option to defeat the caching which can be set globally as options("cache.disable"=TRUE)
#' @param .cache the location of the cache as a directory. May get its value from options("cache.dir") or the default value of rappdirs::user_cache_dir("ggrrr")
#' @param .stale the length of time in days to keep cached data before considering it as stale. can also be set by options("cache.stale")
#'
#' @return the output of .expr which will usually be a value
#' @export
#'
#' @examples
#' library(tidyverse)
#' colName = "Petal.Width"
#' {
#'   iris[[colName]]
#' } %>% cached(iris, colName, .prefix="example", .cache=tempdir())
cached = function (
  .expr,
  ...,
  .nocache = getOption("cache.disable", default=FALSE),
  .cache = getOption("cache.dir", default=rappdirs::user_cache_dir("ggrrr")),
  .prefix = getOption("cache.item.prefix", default="cached"),
  .stale = getOption("cache.stale", default=Inf))
{
   .cached(.expr,...,.nocache=.nocache,.cache=.cache,.prefix=.prefix,.stale=.stale)
}

#' Delete stale files in a cache
#'
#' Staleness is determined by the number of days from 2am on the current day in the current time-zone.
#' A item cached for only one day becomes stale at 2am the day after it is cached.
#' The time is configurable and option(cache.time_day_starts = 0) would be midnight.
#' Automated analysis using caches and updated data should ensure that analysis does not cross this time point otherwise
#' it may end up using old data.
#'
#' @param .prefix a name of the operation so that you can namespace the cached files and do selective clean up operations on them
#' @param .cache the location of the cache as a directory. May get its value from options("cache.dir") or the default value of rappdirs::user_cache_dir("ggrrr")
#' @param .stale the length of time in days to keep cached data before considering it as stale.
#'
#' @return nothing. called for side effects.
#' @export
cache_delete_stale = function(
  .cache = getOption("cache.dir", default=rappdirs::user_cache_dir("ggrrr")),
  .prefix = ".*",
  .stale = getOption("cache.stale", default=Inf)
) {

  .cache_delete_stale(.cache,.prefix,.stale)
}

# TODO:
# download and execute code e.g. unzip + read file
# generally filesystem abstraction functions from ukcovidtools

#' Clear data from the passthrough cache for complex or long running operations
#'
#' @param .prefix a regular expression matching the prefix of the cached item, so that do selective clean up operations. defaults to everything.
#' @param .cache the location of the cache as a directory. May get its value from options("ggrrr.cache.dir") or the default value of rappdirs::user_cache_dir("ggrrr")
#' @param interactive suppress `are you sure?` warning with a FALSE value (defaults to TRUE)
#'
#' @return nothing. called for side effects
#' @export
#'
#' @examples
#' library(tidyverse)
#' cache_clear(.prefix="example", .cache=tempdir(), interactive=FALSE)
cache_clear = function (
  .cache = getOption("cache.dir", default=rappdirs::user_cache_dir("ggrrr")),
  .prefix = ".*",
  interactive = TRUE
) {

  .cache_clear(.cache, .prefix, interactive)
}

#' Download a file into a local cache.
#'
#' This function copies a remote file to a local cache once and
#' makes sure it is reused.
#'
#' @param url the url to download
#' @param ... ignored
#' @param .nocache if set to TRUE all caching is disabled
#' @param .cache the location of the downloaded files
#' @param .stale how long to leave this file before replacing it.
#' @param .extn the file name extension
#'
#' @return the path to the downloaded file
#' @export
cache_download = function(
  url,
  ...,
  .nocache = getOption("cache.disable", default=FALSE),
  .cache = getOption("cache.download.dir", default=rappdirs::user_cache_dir("ggrrr-download")),
  .stale = getOption("cache.stale", default=Inf),
  .extn = NULL
) {

  .cache_download(url, ..., .nocache=.nocache, .cache=.cache, .stale=.stale, .extn=.extn)

}
