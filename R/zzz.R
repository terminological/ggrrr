# cran(c("here","tidyverse","devtools","lubridate","huxtable","patchwork","openxlsx"))
#
# library(tidyverse)
# library(lubridate)
# library(patchwork)


.onLoad <- function(libname, pkgname) {

  # op <- options()
  # op.devtools <- list(
  #   devtools.path = "~/R-dev",
  #   devtools.install.args = "",
  #   devtools.name = "Your name goes here",
  #   devtools.desc.author = "First Last <first.last@example.com> [aut, cre]",
  #   devtools.desc.license = "What license is it under?",
  #   devtools.desc.suggests = NULL,
  #   devtools.desc = list()
  # )
  # toset <- !(names(op.devtools) %in% names(op))
  # if(any(toset)) options(op.devtools[toset])

  # manage non cran dependency loading here:
  non_cran("html2pdfr","terminological/html2pdfr",force=TRUE, subdir="r-library")


  invisible()
}
