# Why? I can't remember.
options(tinytex.engine_args = '-shell-escape')

# cran = function(pkg) {
#   if(knitr::is_html_output()) return(paste0("<em>",pkg,"</em>"))
#   else return(knitr::asis_output(paste0("\\CRANpkg{",pkg,"}")))
# }



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
