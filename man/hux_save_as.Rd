% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/huxtable-utils.R
\name{hux_save_as}
\alias{hux_save_as}
\title{Save a table to a variety of formats and depending on the context return the correct format for a document}
\usage{
hux_save_as(
  hux,
  filename,
  size = std_size$full,
  maxWidth = size$width,
  maxHeight = size$height,
  aspectRatio = maxWidth/maxHeight,
  formats = c("html", "png", "pdf"),
  sheetname = fs::path_ext_remove(fs::path_file(filename))
)
}
\arguments{
\item{hux}{the huxtable to save}

\item{filename}{the filename, which may omit the extension}

\item{size}{a ggrrr::std_size}

\item{maxWidth}{or the maximum width in inches}

\item{maxHeight}{and either the maximum height in inches}

\item{aspectRatio}{or the minimum allowed aspect ratio}

\item{formats}{if the extension is omitted all the extensions described here will be saved. Currently supported outputs are "html","png","pdf","docx","xlsx"}

\item{sheetname}{if saving as an xlsx then the sheetname}
}
\value{
the output depends on if the function is called in a knitr session. It maybe the HTML or a link to the pdf output for example.
}
\description{
The basic output here is to use HTML as an output if possible and convert it to an image or a PDF that can then
be included into a latex document for example.
}