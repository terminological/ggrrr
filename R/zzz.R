# cran(c("here","tidyverse","devtools","lubridate","huxtable","patchwork","openxlsx"))
#
# library(tidyverse)
# library(lubridate)
# library(patchwork)


.onAttach <- function(libname, pkgname) {

  # This is an unsanctioned use of onAttach which woudl not pass CRAN checks
  # actually I could do this with an Imports namespace directive if I was not developing html2pdfr locally.
  # manage non cran dependency loading here:
  non_cran("html2pdfr","terminological/html2pdfr",force=TRUE, subdir="r-library")
  invisible()

}
