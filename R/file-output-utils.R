## Project output directory tools ----


#' Generate a versioned file name in a subdirectory.
#'
#' This function generates a function that resolves a file path fragment to a
#' specific file location, accounting for a versioning strategy involving
#' the current date. The defaults create a naming strategy that places an
#' file in the "output" sub-directory of the current project with a filename suffix including
#' the date.
#'
#' @param directory the root of the output
#' @param datedFile do you want the filename to have the date appended?
#' @param datedSubdirectory do you want the files to be placed in a dated subdirectory?
#'
#' @return a function that takes a filename and boolean delete parameter.
#' When called with a filename component this function will return the absolute path of a file which is
#' versioned with date. If the file exists and delete=TRUE it is deleted
#' (allowing for libraries that refuse to overwrite existing files)
#' @export
#'
#' @examples
#' library(tidyverse)
#' out = outputter("~/output",datedSubdirectory=TRUE)
#' out("file.png")
outputter = function(directory = here::here("output"), datedFile=!datedSubdirectory, datedSubdirectory=FALSE) {
  directory = fs::path_norm(directory)
  fs::dir_create(directory)
  message("directing output to: ",directory)
  return(function(filename, delete=FALSE) {
    if(datedSubdirectory) {
      directory = fs::path(directory,Sys.Date())
      fs::dir_create(directory)
    }
    ext = fs::path_ext(filename)
    if(datedFile) filename = paste0(fs::path_ext_remove(filename),"-",Sys.Date()) %>% fs::path_ext_set(ext)
    path = fs::path(directory,filename)
    if (delete & fs::file_exists(path)) unlink(path)
    return(path)
  })
}


.this_script = function() {
  if (.is_knitting()) return(knitr::current_input(dir = TRUE))
  if (.is_running_in_chunk()) {
    return(fs::path_abs(rstudioapi::getSourceEditorContext()$pat))
  }
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(fs::path_abs(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    tryCatch({
      return(fs::path_abs(sys.frames()[[1]]$ofile))
    },error = function(e) return(fs::path_abs(getwd())))
  }
}

# inputFile = fs::dir_ls(recurse = TRUE)
# gets a reverse directory listing and then finds the lowest directory
# that contains an indication of there being a project in the directory.
# needed because here package does not always work (when knitting in CRAN checks for example)
.locate_project = function(inputFile = getwd()) {
  . = NULL
  absPath = inputFile %>% fs::path_abs() %>% magrittr::extract(stringr::str_starts(.,fs::path_home()))
  parent = unique(ifelse(fs::is_dir(absPath), absPath, fs::path_dir(absPath)))
  current = parent
  repeat {
    grandparent = unique(fs::path_dir(current))
    grandparent = grandparent[!grandparent %in% parent]
    if (length(grandparent) == 0) break
    parent = c(parent,grandparent)
    current = grandparent[grandparent != fs::path_home()]
  }
  root = fs::path_dir(fs::dir_ls(parent,glob="*.Rproj",type = "file"))
  if(length(root) == 1) return(root)
  root = fs::path_dir(fs::dir_ls(parent,glob="*/DESCRIPTION",type = "file"))
  if(length(root) == 1) return(root)
  root = fs::path_dir(fs::dir_ls(parent,glob="*/NAMESPACE",type = "file"))
  if(length(root) == 1) return(root)
  root = fs::path_dir(fs::dir_ls(parent,glob = "*/R",type="directory"))
  if(length(root) == 1) return(root)
  root = fs::path_dir(fs::dir_ls(parent,glob = "*/vignettes",type="directory"))
  if(length(root) == 1) return(root)
  root = fs::path_dir(fs::dir_ls(parent,glob="*/.git",all = TRUE,type = "directory"))
  if(length(root) == 1) return(root)
  root = fs::path_dir(fs::dir_ls(parent,glob="*/.Rproj.user",all = TRUE,type = "directory"))
  if(length(root) == 1) return(root)
  # return the longest possible path with a .Rhistory file
  root = fs::path_dir(fs::dir_ls(parent,glob="*/.Rhistory$",all = TRUE,type = "file"))
  return(root %>% magrittr::extract(stringr::str_length(.) == max(stringr::str_length(.))))
}

#' Knit to a versioned file in a sub-directory of the project
#'
#' used in a knitr preamble to direct the output to a subdirectory of the project
#' ---
#' title: "Analysis 1"
#' output: html_document
#' knit: ggrrr::knit_versioned("html","output/analysis-1")
#' ---
#'
#' @param ext the extension of file to target e.g. "pdf", "html" (TODO: multiple)
#' @param directory the root of the output - can be an absolute path or a relative path interpreted as relative to the root of the project.
#' @param datedFile do you want the filename to have the date appended?
#' @param datedSubdirectory do you want the files to be placed in a dated subdirectory?
#'
#' @return nothing. called for side effects
#' @export
knit_versioned = function(ext, directory = NULL, datedFile=!datedSubdirectory, datedSubdirectory=FALSE) {

  return(function(inputFile,encoding) {
    if (is.null(directory)) {
      directory = fs::path_dir(inputFile)
    } else if (!fs::is_absolute_path(directory)) {
      root = .locate_project(inputFile)
      directory = fs::path(root,directory)
    }
    if(datedSubdirectory) {
      directory = fs::path(directory,Sys.Date())
    }
    fs::dir_create(directory)
    filename = fs::path_file(inputFile)
    if(datedFile) filename = paste0(fs::path_ext_remove(filename),"-",Sys.Date()) %>% fs::path_ext_set(ext)
    path = fs::path(directory,filename)
    rmarkdown::render(inputFile, encoding = encoding, output_file = path)
    message("Output created: ",path)
  })

}
