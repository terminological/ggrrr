#' @inherit .cached
#' @export
#'
#' @examples
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

#' @inherit .cache_delete_stale
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

#' @inherit .cache_clear
#' @export
#' @examples
#' cache_clear(.prefix="example", .cache=tempdir(), interactive=FALSE)
cache_clear = function (
  .cache = getOption("cache.dir", default=rappdirs::user_cache_dir("ggrrr")),
  .prefix = ".*",
  interactive = TRUE
) {

  .cache_clear(.cache, .prefix, interactive)
}

#' @inherit .cache_download
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
