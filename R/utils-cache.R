#
.md5obj = function(obj) {
  as.character(digest::digest(obj, algo="md5"))
}

#.arear.cache <- new.env(parent=emptyenv())

#' A simple passthrough cache for complex or long running operations
#'
#' executes expr and saves the output as an RDS file indexed by code in expr and the hash variable (which should contain any varying inputs)
#'
#' @param .expr the code the output of which requires caching. Other than a return value this should not create side effects or change global variables.
#' @param ... inputs that the code in expr depends on and changes in which require the code re-running, Could be Sys.Date()
#' @param .prefix a name of the operation so that you can identify the cached files and do clean up operations on them
#' @param .nocache an option to defeat the caching which can be set globally as options("ggrrr.disable.cache"=TRUE)
#' @param .cache the location of the cache as a directory. May get its value from options("ggrrr.cache.dir") or the default value of rappdirs::user_cache_dir("ggrrr")
#' @param .stale the length of time in days to keep cached data before considering it as stale. can also be set by options("ggrrr.cache.stale")
#'
#' @return the output of .expr which will usually be a value
#' @export
#'
#' @examples
cached = function (
  .expr = {},
  ...,
  .prefix = getOption("ggrrr.item.prefix", default="cached"),
  .nocache = getOption("ggrrr.disable.cache", default=FALSE),
  .cache = getOption("ggrrr.cache.dir", default=rappdirs::user_cache_dir("ggrrr")),
  .stale = getOption("ggrrr.cache.stale", default=Inf))
{

  hash = rlang::list2(...)
  code = deparse(substitute(.expr))
  md5code = .md5obj(code)

  if(!stringr::str_ends(.cache,"/"))
    .cache = paste0(.cache,"/")

  dir.create(.cache, recursive = TRUE, showWarnings = FALSE)

  md5params = NULL
  if (!is.null(hash))
    md5params = .md5obj(hash)

  path = paste0(.cache,paste(.prefix,md5code,md5params,sep = "-"),".rda")

  if (.nocache) unlink(path)
  if (file.exists(path)) {
    mtime = as.Date(file.info(path)$mtime)
    if (mtime < Sys.Date()-.stale+1) unlink(path)
  }

  if (file.exists(path)) {
    message("using cached item: ",path)
    readRDS(path)
    #assign(path, obj, envir=.arear.cache)
  } else {
    message("caching item: ",path)
    obj = .expr
    saveRDS(obj, path)
    #assign(path, obj, envir=.arear.cache)
    obj
  }
}


# TODO: clear caches
# download into cache
# download and execute code e.g. unzip + read file
# generally functions from the ukcovidtools

#' Clear data from the passthrough cache for complex or long running operations
#'
#' @param .prefix a name of the operation so that you can identify the cached files and do clean up operations on them
#' @param .cache the location of the cache as a directory. May get its value from options("ggrrr.cache.dir") or the default value of rappdirs::user_cache_dir("ggrrr")
#'
#' @return the output of .expr which will usually be a value
#' @export
#'
#' @examples
cache_clear = function (
  .prefix = getOption("ggrrr.item.prefix", default="cached"),
  .cache = getOption("ggrrr.cache.dir", default=rappdirs::user_cache_dir("ggrrr")),
  interactive = TRUE
) {
  if (!fs::dir_exists(.cache)) {
    warning("cache does not exist (yet)")
  } else {
    files = tibble(paths = fs::dir_ls(.cache,recurse=TRUE)) %>%
      mutate( filename = fs::path_file(paths)) %>%
      filter(stringr::str_starts(filename,.prefix))

    if(!interactive) {
      lapply(files$paths, unlink)
    } else {
      message("About to delete ",nrow(files)," cached files.")
      sure = menu(c("Yes", "No"), title="Are you sure?")
      if(sure==1) lapply(files$paths, unlink)
      else message("operation aborted by the user")
    }
  }
  invisible(NULL)
}

