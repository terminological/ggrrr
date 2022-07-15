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


#' Create a function list that allows for supplementary tables (as huxtables) to be added to a XLSX output file.
#'
#' This function encapsulates a excel output file as a destination for data tables. With the output of this
#' function you can add extra data to the supplement as a new sheet, or you can write the spreadsheet to disk.
#' When each data table is written either the table can be written silently or returned so that it is included
#' in a knitr document. This is controlled by `option("hide.supplementary.tables"=TRUE)`.
#'
#' @param ... output location options will be passed to outputter(...) to define the location of the file
#' @param filename the xlsx filename
#' @param out an outputter (defaults to a default outputter )
#' @param nameGlue What will the tables be named
#'
#' @return a list of 2 functions.
#' $add_table(hux, caption, footnote, index), which takes a huxtable, caption, and index a writes the huxtable into a supplementary.
#' $write() which writes the collection of tables to the excel file.
#' @export
data_supplement = function(..., filename="supplementary-material.xlsx", out = outputter(...), nameGlue="Supplementary Table {index}") {
  e1 = new.env(parent = environment())
  with(e1, {

    .out = out
    .wb = openxlsx::createWorkbook()
    .indexes = integer()
    .default_filename = filename
    .catalog = tibble(table=character(),description=character())

    add_table = function(hux, caption, footnote = NULL, index=NULL, hide=getOption("hide.supplementary.tables",default=TRUE)) {
      wb = .wb
      indexes = .indexes
      if(is.null(index)) index = max(c(indexes,0))+1
      sheetname = glue::glue(nameGlue)
      if(sheetname %in% names(wb)) openxlsx::removeWorksheet(wb,sheet = sheetname)

      hux2 = hux %>% hux_auto_widths("xlsx") %>%
        huxtable::set_caption(paste0(sheetname," - ",caption)) %>%
        huxtable::set_caption_pos("topleft")

      if (!is.null(footnote)) {
        hux2 = hux2 %>% huxtable::insert_row(footnote, after=nrow(hux2), colspan = ncol(hux2), fill="",copy_cell_props = FALSE)
      }

      wb = hux2 %>% huxtable::as_Workbook(Workbook = wb, sheet = sheetname)
      .catalog <<- .catalog %>% filter(table != sheetname) %>% bind_rows(tibble(table=sheetname,description=caption))
      .wb <<- wb
      # indexes keeps track of order in which sheets added and overwritten
      .indexes <<- unique(c(indexes,index),fromLast = TRUE)
      if(!hide) return(hux2 %>% huxtable::set_width("auto"))
      invisible(sheetname)
    }

    write = function(filename = .out(.default_filename)) {

      cat = .catalog %>%
        mutate(.sort = dense_rank(.indexes)) %>%
        arrange(.sort) %>%
        select(-.sort) %>%
        huxtable::as_hux(add_colnames=FALSE) %>%
        huxtable::set_caption("Supplementary materials - table of contents") %>%
        huxtable::set_caption_pos("topleft") %>%
        huxtable::set_col_width(c(0.2,0.8)) %>%
        # 25 here is the desired with of the table of contents
        huxtable::set_width((25 / 0.206) / (20*ncol(.catalog)))
      # %>%
      # huxtable::set_height(nrow(.catalog)*20) %>%
      # huxtable::set_row_height(rep(1/(nrow(.catalog)+1),(nrow(.catalog)+1)))

      wb = .wb
      # add in new contents page
      if("Contents" %in% names(wb)) openxlsx::removeWorksheet(wb,"Contents")
      wb = cat %>% huxtable::as_Workbook(Workbook = wb, sheet = "Contents")

      # bring contents page to front. Order others by index.
      openxlsx::worksheetOrder(wb) = order(c(.indexes,0))
      openxlsx::saveWorkbook(wb,filename,overwrite = TRUE)
      invisible(filename)
    }
  })
  return(e1)
}
