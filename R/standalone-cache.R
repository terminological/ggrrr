# ---
# repo: terminological/ggrrr
# file: standalone-cache.R
# last-updated: 2023-11-08
# license: https://unlicense.org
# imports:
#    - usethis
#    - digest
#    - rappdirs
#    - utils
#    - rlang
#    - stringr
#    - fs
#    - dplyr
#    - tidyr
# ---

.md5obj = function(obj) {
  as.character(digest::digest(obj, algo="md5"))
}

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
#' @keywords internal
#' @concept cache
#'
#' @return the output of .expr which will usually be a value
.cached = function (
  .expr,
  ...,
  .nocache = getOption("cache.disable", default=FALSE),
  .cache = rappdirs::user_cache_dir(utils::packageName()),
  .prefix = "cached",
  .stale = Inf)
{

  # .expr2 = rlang::enquo(.expr)
  hash = rlang::list2(...)
  code = deparse(substitute(.expr))
  md5code = .md5obj(code)

  if(!stringr::str_ends(.cache,"/")) .cache = paste0(.cache,"/")

  dir.create(.cache, recursive = TRUE, showWarnings = FALSE)

  md5params = NULL
  if (!is.null(hash))
    md5params = .md5obj(hash)

  path = paste0(.cache,paste(.prefix,md5code,md5params,sep = "-"),".rda")

  if (.nocache) unlink(path)

  .cache_delete_stale(.cache = .cache, .prefix = .prefix, .stale = .stale)

  if (file.exists(path)) {
    message("using cached item: ",path)
    obj = readRDS(path)
    #assign(path, obj, envir=.arear.cache)
  } else {
    message("caching item: ",path)
    obj = .expr #eval(.expr2)
    attr(obj,"cache-path")=path
    attr(obj,"cache-date")=Sys.time()
    saveRDS(obj, path)
    #assign(path, obj, envir=.arear.cache)
  }
  return(obj)
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
#' @keywords internal
#' @concept cache
.cache_delete_stale = function(
  .cache = rappdirs::user_cache_dir(utils::packageName()),
  .prefix = ".*",
  .stale = Inf
) {

  modification_time = stale_time = path = NULL # remove global binding note

  if(!stringr::str_ends(.cache,"/")) .cache = paste0(.cache,"/")
  day_start = getOption("cache.time_day_starts", default=3)

  # if .stale==1 this is (by default) 2am on the current day.
  # something cached before 2am is deemed to be the previous day.

  fs::file_info(fs::dir_ls(.cache)) %>%
    dplyr::mutate(stale_time = as.POSIXct(as.Date(modification_time)+.stale-1)+day_start*60*60) %>%
    dplyr::filter(Sys.time() > stale_time) %>%
    dplyr::pull(path) %>%
    unlink()
}

#' Clear data from the passthrough cache for complex or long running operations
#'
#' @param .prefix a regular expression matching the prefix of the cached item, so that do selective clean up operations. defaults to everything.
#' @param .cache the location of the cache as a directory. May get its value from options("ggrrr.cache.dir") or the default value of rappdirs::user_cache_dir("ggrrr")
#' @param interactive suppress `are you sure?` warning with a FALSE value (defaults to TRUE)
#'
#' @return nothing. called for side effects
#' @keywords internal
#' @concept cache
.cache_clear = function (
  .cache = rappdirs::user_cache_dir(utils::packageName()),
  .prefix = ".*",
  interactive = TRUE
) {

  paths = filename = NULL # remove global binding note

  if (!fs::dir_exists(.cache)) {
    warning("cache does not exist (yet)")
  } else {
    files = tidyr::tibble(paths = fs::dir_ls(.cache,recurse=TRUE)) %>%
      dplyr::mutate( filename = fs::path_file(paths)) %>%
      dplyr::filter(stringr::str_starts(filename,.prefix))

    if(!interactive) {
      lapply(files$paths, unlink)
    } else {
      message("About to delete ",nrow(files)," cached files.")
      sure = utils::menu(c("Yes", "No"), title="Are you sure?")
      if(sure==1) lapply(files$paths, unlink)
      else message("operation aborted by the user")
    }
  }
  invisible(NULL)
}

#' Download a file into a local cache.
#'
#' This function copies a remote file to a local cache once and
#' makes sure it is reused.
#'
#' @param url the url to download
#' @param ... passed to `utils::download.file()`
#' @param .nocache if set to TRUE all caching is disabled
#' @param .cache the location of the downloaded files
#' @param .stale how long to leave this file before replacing it.
#' @param .extn the file name extension
#'
#' @return the path to the downloaded file
#' @keywords internal
#' @concept cache
.cache_download = function(
  url,
  ...,
  .nocache = getOption("cache.disable", default=FALSE),
  .cache = rappdirs::user_cache_dir(utils::packageName()),
  .stale = Inf,
  .extn = NULL
) {

  qualifier = basename(url) %>% stringr::str_extract("^[^?]*")
  if (!is.null(.extn)) {
    qualifier = qualifier %>% fs::path_ext_remove() %>% fs::path_ext_set(.extn)
  }
  md5 = .md5obj(url)
  fname = paste0(md5,"-",qualifier)

  if(!stringr::str_ends(.cache,"/")) .cache = paste0(.cache,"/")
  dir.create(.cache, recursive = TRUE, showWarnings = FALSE)
  path = normalizePath(paste0(.cache,fname), mustWork = FALSE)

  if (.nocache) unlink(path)

  .cache_delete_stale(.cache = .cache, .prefix = path, .stale = .stale)

  if (file.exists(path)) {
    message("using cached item: ",path)
    return(path)
    #assign(path, obj, envir=.arear.cache)
  } else {
    message("downloading item: ",qualifier)
    utils::download.file(url,path, ...)
    return(path)
  }

}
