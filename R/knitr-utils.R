
.is_html_output = knitr::is_html_output

.is_latex_output = knitr::is_latex_output

.is_document_output = function() {
  !.is_html_output() &
    !.is_latex_output() &
      .is_output(c("odt_document","word_document","bookdown::word_document2","bookdown::odt_document2"))
}

.is_unknown_output = function() {
  !.is_html_output() &
    !.is_latex_output() &
    !.is_document_output()
}

.is_output = function(outputFormats) {
  fmt = rmarkdown::default_output_format(knitr::current_input())$name
  return (fmt %in% outputFormats)
}

# TRUE if the whole document is being knitted.
# FALSE if running in chunk in RStudio, or not interactive, or 
.is_knitting = function() {
  isTRUE(getOption("knitr.in.progress"))
}

# TRUE is being knitted OR running in chunk in RStudio
# FALSE if not interactive or interactive but in console in RStudio
.is_running_in_chunk = function() {
  isTRUE(try(rstudioapi::getActiveDocumentContext()$id != "#console"))
}

# TRUE if in rstudio console
.is_running_in_console = function() {
  isTRUE(try(rstudioapi::getActiveDocumentContext()$id == "#console"))
}


.is_viewer_available = function() {
  viewer <- getOption("viewer")
  is.null(viewer)
}

# push png to plot window
# library(png)
# img <- readPNG(system.file("img", "Rlogo.png", package="png"))
# grid::grid.raster(img)

# open content in the viewer
# viewer <- getOption("viewer")
# if (!is.null(viewer))
#   viewer("http://localhost:8100")
# else
#   utils::browseURL("http://localhost:8100")
# but:
# html %>% htmltools::html_print()
# Will automatically invoke viewer (which is not what you want if running in a chunk, only good for console)