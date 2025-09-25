# ---
# repo: terminological/ggrrr
# file: standalone-cache.R
# last-updated: 2025-09-23
# license: https://unlicense.org
# imports:
#    - rappdirs
#    - utils
#    - rlang
#    - stringr
#    - fs
#    - dplyr
# ---

# 2025-09-19: switched to rlang::hash - will probably invalidate existing caches

.cache_message = function(...) {
  if (getOption("cache.verbose", FALSE)) message(...)
}

.cache_loc = function() {
  if (!isFALSE(getOption("cache.path", FALSE))) {
    return(getOption("cache.path"))
  }
  rappdirs::user_cache_dir(
    if (is.null(utils::packageName())) "standalone" else utils::packageName()
  )
}

#' A simple pass-through filesystem cache for complex or long running operations
#'
#' executes `.expr` and saves the output as an RDS file indexed by hash of code in
#' `.expr` and the hash of specified input variables (in `...`,  which should
#' contain any inputs that influence `.expr`). The expression is evaluated in the
#' current environment and it is up to the user to ensure all significant
#' environmental factors are accounted for in `...`
#'
#' @param .expr the code the output of which requires caching. Other than a
#'   return value this should not create side effects or change global
#'   variables.
#' @param ... inputs that the code in `.expr` depends on and changes in which
#'   require the code re-running.
#' @param .prefix (optional) a name of the operation so that you can namespace
#'   the cached files and do selective clean up operations on them with
#'   .cache_clear()
#' @param .nocache an option to defeat the cacheing which can be set globally as
#'   `options("cache.disable"=TRUE)`
#' @param .cache the location of the cache as a directory. May get its value
#'   from `getOption("cache.path")` or the default value which is
#'   `rappdirs::user_cache_dir(<package_name>)`
#' @param .stale the length of time in days before considering cached data as
#'   stale.
#' @keywords internal
#' @concept cache
#'
#' @return the output of `.expr` which will usually be a value
#' @unit
#'
#' # we use `rnorm` here to prove that the cache is working but in reality
#' # you would normally only use something deterministic in `.expr`:
#' x = 100
#' tmp = .cached(stats::rnorm(x), x)
#' tmp2 = .cached(stats::rnorm(x), x)
#' testthat::expect_equal(tmp, tmp2)
#'
#' x = 200
#' tmp3 = .cached(stats::rnorm(x), x)
#' testthat::expect_equal(x, length(tmp3))
#'
.cached = function(
  .expr,
  ...,
  .nocache = getOption("cache.disable", default = FALSE),
  .cache = .cache_loc(),
  .prefix = "cached",
  .stale = Inf
) {
  # .expr2 = rlang::enquo(.expr)
  hash = rlang::list2(...)
  code = deparse(substitute(.expr))
  md5code = rlang::hash(code)

  if (!stringr::str_ends(.cache, "/")) {
    .cache = paste0(.cache, "/")
  }

  dir.create(.cache, recursive = TRUE, showWarnings = FALSE)

  md5params = NULL
  if (!is.null(hash)) {
    md5params = rlang::hash(hash)
  }

  path = paste0(.cache, paste(.prefix, md5code, md5params, sep = "-"), ".rda")

  if (.nocache) {
    unlink(path)
  }

  .cache_delete_stale(.cache = .cache, .prefix = .prefix, .stale = .stale)

  if (file.exists(path)) {
    .cache_message("using cached item: ", path)
    obj = readRDS(path)
    #assign(path, obj, envir=.arear.cache)
  } else {
    .cache_message("caching item: ", path)
    obj = .expr #eval(.expr2)
    attr(obj, "cache-path") = path
    attr(obj, "cache-date") = Sys.time()
    saveRDS(obj, path)
    #assign(path, obj, envir=.arear.cache)
  }
  return(obj)
}


#' Delete stale files in a cache
#'
#' Staleness is determined by the number of days from 2am on the current day in
#' the current time-zone. A item cached for only one day becomes stale at 2am
#' the day after it is cached. The time is configurable and
#' option(cache.time_day_starts = 0) would be midnight. Automated analysis using
#' caches and updated data should ensure that analysis does not cross this time
#' point otherwise it may end up using old data.
#'
#' @inheritParams .cached
#'
#' @return nothing. called for side effects.
#' @keywords internal
#' @concept cache
.cache_delete_stale = function(
  .cache = .cache_loc(),
  .prefix = ".*",
  .stale = Inf
) {
  modification_time = stale_time = path = NULL # remove global binding note

  if (!stringr::str_ends(.cache, "/")) {
    .cache = paste0(.cache, "/")
  }
  day_start = getOption("cache.time_day_starts", default = 3)

  # if .stale==1 this is (by default) 2am on the current day.
  # something cached before 2am is deemed to be the previous day.

  fs::file_info(fs::dir_ls(.cache)) %>%
    dplyr::mutate(
      stale_time = as.POSIXct(as.Date(modification_time) + .stale - 1) +
        day_start * 60 * 60
    ) %>%
    dplyr::filter(Sys.time() > stale_time) %>%
    dplyr::pull(path) %>%
    unlink()
}

#' Clear data from the passthrough cache for complex or long running operations
#'
#' @inheritParams .cached
#' @param interactive suppress `are you sure?` warning with a FALSE value
#'   (defaults to TRUE)
#'
#' @return nothing. called for side effects
#' @keywords internal
#' @concept cache
.cache_clear = function(
  .cache = .cache_loc(),
  .prefix = ".*",
  interactive = TRUE
) {
  paths = filename = NULL # remove global binding note

  if (!fs::dir_exists(.cache)) {
    .cache_message("cache does not exist (yet)")
  } else {
    files = dplyr::tibble(paths = fs::dir_ls(.cache, recurse = TRUE)) %>%
      dplyr::mutate(filename = fs::path_file(paths)) %>%
      dplyr::filter(stringr::str_starts(filename, .prefix))

    if (!interactive) {
      lapply(files$paths, unlink)
    } else {
      message("About to delete ", nrow(files), " cached files.")
      sure = utils::menu(c("Yes", "No"), title = "Are you sure?")
      if (sure == 1) {
        lapply(files$paths, unlink)
      } else {
        message("operation aborted by the user")
      }
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
#' @inheritDotParams utils::download.file -url -destfile
#' @inheritParams .cached
#' @param .extn the file name extension
#'
#' @return the path to the downloaded file
#' @keywords internal
#' @concept cache
.cache_download = function(
  url,
  ...,
  .nocache = getOption("cache.disable", default = FALSE),
  .cache = .cache_loc(),
  .stale = Inf,
  .extn = NULL
) {
  qualifier = basename(url) %>% stringr::str_extract("^[^?]*")
  if (!is.null(.extn)) {
    qualifier = qualifier %>% fs::path_ext_remove() %>% fs::path_ext_set(.extn)
  }
  md5 = rlang::hash(url)
  fname = paste0(md5, "-", qualifier)

  if (!stringr::str_ends(.cache, "/")) {
    .cache = paste0(.cache, "/")
  }
  dir.create(.cache, recursive = TRUE, showWarnings = FALSE)
  path = normalizePath(paste0(.cache, fname), mustWork = FALSE)

  if (.nocache) {
    unlink(path)
  }

  .cache_delete_stale(.cache = .cache, .prefix = path, .stale = .stale)

  if (file.exists(path)) {
    .cache_message("using cached item: ", path)
    return(path)
    #assign(path, obj, envir=.arear.cache)
  } else {
    .cache_message("downloading item: ", qualifier)
    utils::download.file(
      url,
      path,
      ...,
      quiet = !getOption("cache.verbose", FALSE)
    )
    return(path)
  }
}


#' Cache a post request
#'
#' @inherit .cache_download
#' @param body a named list of POST data
#' @param as how the response is delivered?
#' @inheritDotParams httr::POST -url
#'
#' @returns the result of the query
#' @keywords internal
#' @concept cache
.cache_post = function(
  url,
  body,
  as = c("text", "raw", "parsed", "httr"),
  ...,
  .nocache = getOption("cache.disable", default = FALSE),
  .cache = .cache_loc(),
  .stale = Inf
) {
  status = NULL
  as = match.arg(as)

  md5 = rlang::hash(url)
  md52 = rlang::hash(body)

  qualifier = basename(url) %>% stringr::str_extract("^[^?]*")
  qualifier = qualifier %>%
    fs::path_ext_remove() %>%
    fs::path_ext_set("Rdata")

  fname = paste0(md5, "-", md52, "-", qualifier)

  if (!stringr::str_ends(.cache, "/")) {
    .cache = paste0(.cache, "/")
  }
  dir.create(.cache, recursive = TRUE, showWarnings = FALSE)
  path = normalizePath(paste0(.cache, fname), mustWork = FALSE)

  if (.nocache) {
    unlink(path)
  }

  .cache_delete_stale(.cache = .cache, .prefix = path, .stale = .stale)

  if (file.exists(path)) {
    .cache_message("using cached item: ", path)
    res = readRDS(file = path)
  } else {
    res = httr::POST(url = url, body = body, ...)
    if (httr::status_code(res) == 200) {
      .cache_message("caching item: ", path)
      saveRDS(res, file = path)
    } else {
      warning("POST failed with status: ", status, ", returning raw response.")
      return(res)
    }
  }
  if (as == "httr") {
    return(res)
  }
  res = httr::content(res, as = as)
  return(res)
}
