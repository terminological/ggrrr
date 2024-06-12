# ggrrr package api

#' @inherit .outputter
#' @export
#'
#' @examples
#' out = outputter("~/output",datedSubdirectory=TRUE)
#' out("file.png")
outputter = .outputter

#' @inherit .here
#' @export
here = .here


#' @inherit .gg_save_as
#' @inheritDotParams .gg_save_as
#' @export
gg_save_as = function(...) {
  .gg_save_as(..., webfontFinder = gg_find_webfonts)
}

#' Find webfonts for a ggplot
#'
#' Resolves any missing fonts in a ggplot and tries to resolve them
#' against webfont providers (Brick and Google Fonts) locally cacheing
#' them. This function is the default for `gg_save_as`
#'
#' @param plot a ggplot
#'
#' @return a list of css webfont specifications
#' @export
gg_find_webfonts = function(plot) {
  fonts = .gg_used_fonts(plot)
  .get_font_face(fonts)
}

#' @inherit .hux_save_as
#' @inheritDotParams .hux_save_as
#' @export
hux_save_as = function(...) {
  .hux_save_as(..., pdfConverter = html_pdf_converter)
}

#' Convert html to pdf depending on what is available on the platform
#'
#' If html2pdfr is installed it will use that by default, if not it will fall
#' back to a headless chrome instance.
#'
#' @param html the html fragment
#' @param filename the pdf filename
#' @param maxWidth the maximum page width in inches
#' @param maxHeight the maximum page height in inches
#'
#' @return nothing called for side effects
#' @export
html_pdf_converter = function(html, filename, maxWidth, maxHeight) {

  if(rlang::is_installed("html2pdfr") && utils::packageVersion("html2pdfr") >= "0.4.0") {

    # get these functions without R CMD Check noticing that they might not be installed
    # since we check for these above we don't need to handle failure.
    html_converter = optional_fn("html2pdfr","html_converter")
    html_fragment_to_pdf = optional_fn("html2pdfr","html_fragment_to_pdf")
    conv = html_converter(
      unique(c(
        systemfonts::system_fonts()$path,
        systemfonts::registry_fonts()$path
      ))
    )
    html_fragment_to_pdf(
      htmlFragment = html,
      outFile = filename,
      formats = "pdf",
      maxWidthInches = maxWidth,
      maxHeightInches = maxHeight,
      xMarginInches = 0,
      yMarginInches = 0,
      pngDpi = 300,
      converter = conv
    )

    # try(
    #   grDevices::embedFonts(withExt("pdf")),
    #   silent=TRUE
    # );

  } else {

    .print_html_with_chrome(html, filename, maxWidth=maxWidth, maxHeight=maxHeight)

  }
  invisible(NULL)

}


# Collecting a data supplement ----

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
data_supplement = function(..., filename="supplementary-material.xlsx", out = ggrrr::outputter(...), nameGlue="Supplementary Table {index}") {
  e1 = new.env(parent = environment())
  with(e1, {

    .out = out
    .wb = openxlsx::createWorkbook()
    .indexes = integer()
    .default_filename = filename
    .catalog = tidyr::tibble(table=character(),description=character())

    add_table = function(hux, caption, footnote = NULL, index=NULL, hide=getOption("hide.supplementary.tables",default=TRUE)) {
      wb = .wb
      indexes = .indexes
      if(is.null(index)) index = max(c(indexes,0))+1
      sheetname = glue::glue(nameGlue)
      if(sheetname %in% names(wb)) openxlsx::removeWorksheet(wb,sheet = sheetname)

      hux2 = hux %>% ggrrr::hux_auto_widths("xlsx")

      hux2 = hux2 %>% ggrrr::hux_set_caption(paste0(sheetname," - ",caption))

      if (!is.null(footnote)) {
        hux2 = hux2 %>% hux_set_footer(footnote)
      }

      wb = hux2 %>% huxtable::as_Workbook(Workbook = wb, sheet = sheetname)
      openxlsx::setRowHeights(wb, sheetname, 1:nrow(hux2), heights=32)
      .catalog <<- .catalog %>% dplyr::filter(table != sheetname) %>% dplyr::bind_rows(tidyr::tibble(table=sheetname,description=caption))
      .wb <<- wb
      # indexes keeps track of order in which sheets added and overwritten
      .indexes <<- unique(c(indexes,index),fromLast = TRUE)
      if(!hide) return(hux2 %>% huxtable::set_width("auto"))
      invisible(sheetname)
    }

    write = function(filename = .out(.default_filename)) {

      cat = .catalog %>%
        dplyr::mutate(.sort = dplyr::dense_rank(.indexes)) %>%
        dplyr::arrange(.sort) %>%
        dplyr::select(-.sort) %>%
        huxtable::as_hux(add_colnames=TRUE) %>%
        ggrrr::hux_default_layout() %>%
        hux_set_caption("Supplementary materials - table of contents") %>%
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


