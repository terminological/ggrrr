#' Knit to a versioned file in a sub-directory of the project
#'
#' used in a knitr preamble to direct the output to a subdirectory of the project
#' ---
#' title: "Analysis 1"
#' output: html_document
#' knit: ggrrr::knit_versioned("output/analysis-1")
#' ---
#'
#' This can only work when deployed as a library and hence no standalone version
#' of it exists, because the fully qualified packagename has to be used.
#'
#' @param directory the root of the output - can be an absolute path or a relative path interpreted as relative to the root of the project.
#' @param ... ignored
#' @param datedFile do you want the filename to have the date appended (defaults TRUE)?
#' @param datedSubdirectory do you want the files to be placed in a dated subdirectory  (defaults FALSE)?
#'
#' @return nothing. called for side effects
#' @export
knit_versioned = function(directory = NULL, ..., datedFile=!datedSubdirectory, datedSubdirectory=FALSE) {

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
    filename = fs::path_file(inputFile) %>% fs::path_ext_remove()
    if(datedFile) filename = paste0(filename,"-",Sys.Date())
    path = fs::path(directory,filename)
    rmarkdown::render(inputFile, encoding = encoding, output_format = "all", output_file = path, intermediates_dir=directory)
    message("Output created: ",path)
  })

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


# Why? I can't remember.
options(tinytex.engine_args = '-shell-escape')

#' Display a code snippet
#'
#' Pulls out a code snippet based on a vector of start
#' and end lines.
#'
#' @param type The code type (as understood by the minted latex plugin)
#' @param filename The source code filename
#' @param starts a vector of start indices ( as line numbers )
#' @param ends a vector of end indices ( as line numbers )
#' @param sep  a seperator
#'
#' @return a formatted string based on the file
#' @export
code_snip_by_line = function(type,filename,starts=1,ends=Inf,sep="\n...\n") {
  if (!fs::file_exists(filename)) {
    cat("```\nCANNOT FIND FILE - possibly if running in R-CMD-check\n```")
  } else {
    lines = readLines(filename)
    if (isTRUE(getOption("knitr.in.progress")) && knitr::is_latex_output()) {
      cat(
        paste0(
          "\\begin{minted}[linenos]{",type,"}\n",
          paste0(
            lapply(1:length(starts),function(i) paste0(lines[min(starts[[i]],length(lines)):min(ends[[i]],length(lines))],collapse = "\n")),
            collapse=sep),
          "\n\\end{minted}"
        ),
        sep="")
    } else {
      cat(
        paste0(
          "~~~ {.",type," .number-lines}\n",
          paste0(
            lapply(1:length(starts),function(i) paste0(lines[min(starts[[i]],length(lines)):min(ends[[i]],length(lines))],collapse = "\n")),
            collapse=sep),
          "\n~~~"
        ),
        sep="")
    }
  }
}

#' Display a code snippet
#'
#' Pulls out a code snippet based on a start and end
#' tags as comments within the code
#'
#' @param type the code type
#' @param filename the source code file
#' @param startMatches a regex that matched start lines
#' @param endMatches a regex that matches end lines
#' @param includeStart is the regex inclusive of the start line or not
#' @param includeEnd is the regex inclusive of the end line or not
#' @param sep a seperator
#'
#' @return a text string of the selected code
#' @export
code_snip = function(type,filename,startMatches="START",endMatches="END", includeStart=FALSE, includeEnd=FALSE,sep="\n...\n") {
  if (!fs::file_exists(filename)) {
    cat("```\nCANNOT FIND FILE - possibly if running in R-CMD-check\n```")
  } else {
    lines = readLines(filename)
    starts = stringr::str_detect(lines,startMatches)
    ends = stringr::str_detect(lines,endMatches)

    starts = (1:length(lines))[starts]
    ends = (1:length(lines))[ends]
    if (!includeStart) starts = starts+1
    if (!includeEnd) ends = ends-1

    if (length(starts) == 0) starts=c(1)
    if (length(ends) == 0) ends=c(Inf)
    if (length(starts) != length(ends)) stop("Mismatch in start and end markers, start:{}, end:{}",starts,ends)

    if (isTRUE(getOption("knitr.in.progress")) && knitr::opts_knit$get('rmarkdown.pandoc.to')=="html") {
      cat(
        paste0(
          "~~~ {.",type,"}\n", # .number-lines}\n",
          paste0(
            lapply(1:length(starts),function(i) paste0(lines[min(starts[[i]],length(lines)):min(ends[[i]],length(lines))],collapse = "\n")),
            collapse=sep),
          "\n~~~"
        ),
        sep="")
    } else {
      cat(
        paste0(
          "\\begin{minted}[linenos]{",type,"}\n",
          paste0(
            lapply(1:length(starts),function(i) paste0(lines[min(starts[[i]],length(lines)):min(ends[[i]],length(lines))],collapse = "\n")),
            collapse=sep),
          "\n\\end{minted}"
        ),
        sep="")
    }
  }
}
