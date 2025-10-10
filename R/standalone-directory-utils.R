# ---
# repo: terminological/ggrrr
# file: standalone-directory-utils.R
# last-updated: 2025-10-09
# license: https://unlicense.org
# imports:
# - fs
# - magrittr
# - rlang
# - stringr
# - testthat
# - utils
# ---

# Project output directory tools ----

#' Find a file in a data directory.
#'
#' This function generates a function that resolves a file path fragment to a
#' specific file location in an input directory.
#'
#' @param directory the root of the input
#'
#' @return a function that takes a relative path and returns the absolute path
#' of the input file. The function can take the same arguments as `fs::dir_ls`,
#' and particularly useful is `glob` and `type`. The function
#' @keywords internal
#' @concept output
#' @unit
#' inp = .inputter(tempdir())
#' for (i in 1:10) {
#'   fs::file_touch(fs::path(tempdir(), sprintf("test_%d.txt",i)))
#' }
#'
#' testthat::expect_true(fs::file_exists(inp("test_2.txt")))
#' testthat::expect_error(inp("test_0.txt"))
#'
#' testthat::expect_equal(length(inp(glob="*.txt")),10)
#'
.inputter = function(
  directory = .here("input")
) {
  directory = fs::path_expand(directory)
  if (!fs::dir_exists(directory)) {
    stop("The given input directory does not exist: ", directory)
  }
  #TODO: dated subdirectories. most recent version.
  message("finding input from: ", directory)
  return(function(filename = "", ..., type = "file") {
    if (filename == "") {
      tmp = fs::dir_ls(directory, recurse = TRUE, type = type, ...)
      # tmp = fs::path_rel(tmp, directory)
      return(tmp)
    }
    path = fs::path(directory, filename)
    if (!fs::file_exists(path)) {
      stop("Could not locate file: ", path)
    }
    return(path)
  })
}

#' Generate a versioned file name in a subdirectory.
#'
#' This function generates a function that resolves a file path fragment to a
#' specific file location, accounting for a versioning strategy involving
#' the current date. The defaults create a naming strategy that places an
#' file in the "output" sub-directory of the current project with a filename suffix including
#' the date.
#'
#' @param directory the root of the output
#' @param ... not used must be empty
#' @param datedFile do you want the filename to have the date appended?
#' @param datedSubdirectory do you want the files to be placed in a dated subdirectory?
#'
#' @return a function that takes a filename and boolean delete parameter.
#' When called with a filename component this function will return the absolute path of a file which is
#' versioned with date. If the file exists and delete=TRUE it is deleted.
#' (allowing for libraries that refuse to overwrite existing files)
#' @keywords internal
#' @concept output
#' @unit
#'
#' # default appends date to filename:
#' out = .outputter(tempdir())
#' tmp = out("test.pdf")
#' testthat::expect_match(tmp, "^.*/test-[0-9]{4}-[0-9]{2}-[0-9]{2}.pdf$")
#'
#' # default appends date to filename:
#' out2 = .outputter(tempdir(),datedSubdirectory=TRUE)
#' tmp2 = out2("test.pdf")
#' testthat::expect_match(tmp2, "^.*/[0-9]{4}-[0-9]{2}-[0-9]{2}/test.pdf$")
#'
.outputter = function(
  directory = .here("output"),
  ...,
  datedFile = !datedSubdirectory,
  datedSubdirectory = FALSE
) {
  rlang::check_dots_empty()
  directory = fs::path_norm(directory)
  fs::dir_create(directory)
  message("directing output to: ", directory)
  return(function(filename = "", delete = FALSE) {
    if (datedSubdirectory) {
      directory = fs::path(directory, Sys.Date())
      fs::dir_create(directory)
    }

    if (filename == "**BROWSE**") {
      utils::browseURL(directory)
      return(invisible(NULL))
    }

    ext = fs::path_ext(filename)
    if (datedFile) {
      filename = paste0(fs::path_ext_remove(filename), "-", Sys.Date()) %>%
        fs::path_ext_set(ext)
    }
    path = fs::path(directory, filename)
    if (delete & fs::file_exists(path)) {
      unlink(path)
    }

    return(path)
  })
}

#' Find the root of the current project
#'
#' gets a reverse directory listing and then finds the lowest directory
#' that contains an indication of there being a project in the directory
#' this should work where the `here` package does not
#' (particularly when knitting in CRAN checks for example, or running in a
#' non package project)
#'
#' @param inputFile a file to check the project root of.
#'
#' @return the file path of the root of the project
#' @keywords internal
#' @concept output
#'
#' @unit
#' .locate_project()
.locate_project = function(inputFile = NULL) {
  . = NULL
  if (is.null(inputFile)) {
    inputFile = tryCatch(.this_script(), error = function(e) getwd())
  }
  absPath = inputFile %>% fs::path_expand()
  parent = unique(ifelse(fs::is_dir(absPath), absPath, fs::path_dir(absPath)))
  current = parent
  # add parent directories up to /user/home
  repeat {
    grandparent = unique(fs::path_dir(current))
    grandparent = grandparent[!grandparent %in% parent]
    if (length(grandparent) == 0) {
      break
    }
    parent = c(parent, grandparent)
    current = grandparent[grandparent != fs::path_home()]
  }
  root = fs::path_dir(fs::dir_ls(parent, glob = "*.Rproj", type = "file"))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(parent, glob = "*/DESCRIPTION", type = "file"))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(parent, glob = "*/NAMESPACE", type = "file"))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(parent, glob = "*/R", type = "directory"))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(
    parent,
    glob = "*/vignettes",
    type = "directory"
  ))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(
    parent,
    glob = "*/.git",
    all = TRUE,
    type = "directory"
  ))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(
    parent,
    glob = "*/.Rproj.user",
    all = TRUE,
    type = "directory"
  ))
  if (length(root) == 1) {
    return(root)
  }
  # return the longest possible path with a .Rhistory file
  root = fs::path_dir(fs::dir_ls(
    parent,
    glob = "*/.Rhistory$",
    all = TRUE,
    type = "file"
  ))
  return(
    root %>%
      magrittr::extract(stringr::str_length(.) == max(stringr::str_length(.)))
  )
}

#' Drop in replacement for `here` (`here` pkg)
#'
#' @param ... the relative path within the project
#' @param projRoot the project root - defaults to `.locate_project()`
#'
#' @return a path
#' @keywords internal
#' @concept output
#'
#' @unit
#' .here("vignettes")
.here = function(..., projRoot = .locate_project()) {
  return(fs::path(projRoot, ...))
}
