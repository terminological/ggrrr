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


