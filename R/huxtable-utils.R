## Huxtable utils ----

### hux_default_layout ----

#' A tidy article theme for huxtables
#'
#' The main aim is to get something that works with google docs when you copy and paste.
#'
#' @param hux a huxtable object
#' @param defaultFontSize default size of font in points (8)
#' @param defaultFont the font family name
#' @param headerRows the number of rows that are headers
#' @keywords huxtable
#' @return the formatted huxtable.
#' @export
#' @examples
#' library(tidyverse)
#' hux = iris %>% hux_default_layout()
hux_default_layout = .hux_default_layout

### hux_set_font ----

#' Set the font family and size in a huxtable globally
#'
#' @param hux a huxtable table
#' @param defaultFontSize the desired font size
#' @param defaultFont the desired font
#'
#' @return the altered huxtable
#' @export
hux_set_font = .hux_set_font

### hux_tidy ----

#' Convert a dataframe to a huxtable with nested rows and columns.
#'
#' The assumption here is that the input data is a long format tidy dataframe
#' with both rows and columns specified by values of the `rowGroupVars` and
#' `colGroupVars` columns. The long format (sparse) table is translated into a
#' nested tree of rows (using `rowGroupVars`) and a nested tree of columns (from
#' `colGroupVars`). Individual data items are placed in the cell intersecting
#' these two trees. If there are multiple matches an additional layer of grouing
#' is added to the columns.
#'
#' @param tidyDf A dataframe with row groupings (as a set of columns) and column
#'   groupings (as a set of columns) and data, where the data is in a tidy
#'   format with a row per "cell" or cell group.
#' @param rowGroupVars A dplyr::vars(...) column specification which will define how
#'   rows are grouped
#' @param colGroupVars A dplyr::vars(...) column specification with defines how columns
#'   will be grouped
#' @param missing If there is no content for a given rowGroup / colGroup
#'   combination then this character will be used as a placeholder
#' @param na If there are NA contents then this character will be used.
#' @param displayRedundantColumnNames if there is one column per column group
#'   the name of that column may be irrelevant (e.g. if there is a `col_name`,
#'   `value` fully tidy format) and `col_name` is inthe colGroupVars list then
#'   the name of the column `value` is redundant and not displayed by default.
#'   However sometimes you want to display this if you have named it as something specific
#'   e.g. including the units. If there is more than one column per `colGroup` the
#'   column titles are needed and kept.
#' @param ... passed onto hux_default_layout
#'
#' @return a huxtable table
#' @export
hux_tidy = .hux_tidy

### hux_nest_group ----

#' Make a huxtable narrower
#'
#' @param t the huxtable
#' @param col the column index you want to nest into the row above
#'
#' @return a narrower huxtable
#' @export
hux_nest_group = .hux_nest_group

#' Estimate column content widths
#'
#' Widths are based on dataframe or huxtable content ignoring rowspans and
#' potential for wrapping.
#'
#' @param table a table to get column content widths for.
#'
#' @return a vector of column widths
#' @export
#'
#' @examples
#' library(tidyverse)
#' iris %>% fit_col_widths()
fit_col_widths = function(table) {

  ar = NULL
  label= fontName = fontFace = colSpan = NULL # remove global binding note

  table %>% as.long_format_table() %>%
    dplyr::mutate(ar = .get_text_ar(label,font = fontName,face = fontFace) %>% dplyr::pull(ar)) %>%
    dplyr::filter(colSpan == 1) %>%
    dplyr::group_by(col) %>%
    dplyr::summarise(ar = max(ar)) %>%
    dplyr::arrange(col) %>% dplyr::pull(ar)
}

# get the aspect ratio of a bit of text - this gives the width/height of a bit of text
# TODO I think sysfonts or systemfonts might have something to help here
.get_text_ar = function(txt, font, face="plain", linespacing=1+1/8) {
  label = NULL # remove global binding note
  tmp = tidyr::tibble(label = txt, font=font, face=face)
  tmp = tmp %>% dplyr::mutate(
    lines = label %>% stringr::str_count("\n")+1,
    ar = purrr::pmap_dbl(list(l=label,f=font,ff=face, ll = lines),  function(l,f,ff,ll) {
      tryCatch({
        grob = suppressWarnings(grid::textGrob(l,gp=grid::gpar(fontfamily=f, fontface=ff, fontsize=8)))
        width = as.numeric(grid::widthDetails(grob)) # this is inches
        height = (ll+(ll-1)*(linespacing-1))*8/72 # this is inches
        return(width/height)
      }, error=function(e) {
        # browser()
        return(stringr::str_length(l) / (ll+(ll-1)*(linespacing-1)))
      })
    })
  )
  return(tmp)
}

# get the width of a bit of text based on the font size
.get_text_cms <- function(txt, font = "sans", font_size = 8, linespacing=1+1/8) {
  heightCms = widthCms = NULL # remove global binding note
  ret = .get_text_ar(txt,font)
  ret = ret %>%
    dplyr::mutate(
      heightCms = (lines+(lines-1)*(linespacing-1))*font_size/72*2.54,
      widthCms = heightCms*ar) %>%
    dplyr::pull(widthCms)

  return(ret)
}

#.get_text_cms(c("ABC","DEF","ABCDEF","ABC\nDEF"), font_size=c(12,16,12,12))
#.get_text_ar(c("ABC","DEF","ABCDEF","ABC\nDEF"),font = "Arial")




#' Calculate a sensible column and table width for a huxtable based on its content.
#'
#' @param hux the huxtable
#' @param target the expected output (could be "docx"/"odt", "xlsx") which are the only options that matter
#' @param including_headers Should we try and fit the header contents as well (TRUE) or let those wrap (FALSE).
#'
#' @return the huxtable with the width options set.
#' @export
hux_auto_widths = function(hux, target = "html", including_headers = FALSE) {

  value = minStrwidth = newwidth = NULL  # remove global binding note

  # TODO: this needs a bit of a tidy up.
  fontSize = stats::median(huxtable::font_size(hux))
  # graphics::par(family = "sans", ps = fontSize)
  merged_headers = sum(huxtable::header_rows(hux))-1
  # empirically determined for the default 8pt font size in excel:
  yourDevice = graphics::strwidth("mmmm",units="in")/0.5416667 #my device
  cmsPerChar = mean(c(5.01/7.461250,  4.22/6.297083, 0.68/0.714375, 3.45/5.000625,   0.68/0.714375, 3.49/5.000625, 2.1/2.963333,  0.95/1.031875))

  # Huxtable tries to guess appropriate widths and height for rows and columns; numeric huxtable::width() and huxtable::height() are treated as scaling factors:
  # cw <- huxtable::col_width(ht)
  # if (!is.numeric(cw) || anyNA(cw)) cw <- rep(1/ncol(ht), ncol(ht)) #cw defaults to 1/ncol
  # basic_width <- 20 * ncol(ht) # presume the 20 is in the basic excel unit
  # w <- huxtable::width(ht)
  # if (!is.numeric(w) || is.na(w))
  #   w <- 0.5 # the default table width = 0.5 is mysterious. This will scale the
  # widths = cw * w * basic_width
  # 8 columns results in row width of 2.06 cms => 8*2.06 is 0.5(w) * 20 * 8 => each unit is 0.206 cms
  # to make it do what we want. set width to be (desired_total_width_in_cms / 0.206) / (20*ncol(hux))
  # set col_width to be proportions that adds up to 1.

  if(including_headers) {
    tmp = hux
  } else {
    tmp = hux %>% dplyr::filter(dplyr::row_number()>merged_headers)
  }

  strwidths = tmp %>% dplyr::ungroup() %>%
    dplyr::summarise(dplyr::across(tidyr::everything(), .fns = ~ max(.get_text_cms(.x, font_size = fontSize),na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols=tidyr::everything()) %>% dplyr::rename(strwidth = value)

  rowspans = tmp %>%
    huxtable::rowspan() %>%
    apply(function(.x) mean(.x),MARGIN = 2) %>%
    purrr::as_vector() %>% tibble::enframe() %>%
    dplyr::rename(rowspan = value)

  words = tmp %>% dplyr::ungroup() %>%
    dplyr::summarise(dplyr::across(tidyr::everything(),
                  .fns = ~ stringr::str_split(stats::na.omit(.x),stringr::fixed(" ")) %>% lapply(length) %>% unlist() %>% max()
                  )) %>%
    tidyr::pivot_longer(cols=tidyr::everything()) %>% dplyr::rename(words = value)

  # a maximum single word length in a column.
  wordLength = tmp %>% dplyr::ungroup() %>%
    dplyr::summarise(dplyr::across(tidyr::everything(),
                     .fns = ~ stringr::str_split(stats::na.omit(.x),stringr::fixed(" ")) %>% lapply(function(.x) .get_text_cms(.x, font_size = fontSize)) %>% unlist() %>% max()
                     )) %>%
    tidyr::pivot_longer(cols=tidyr::everything()) %>% dplyr::rename(minStrwidth = value)
  # browser()
  charLen = strwidths %>% dplyr::inner_join(rowspans, by="name") %>% dplyr::inner_join(words, by="name") %>% dplyr::inner_join(wordLength, by="name") %>% dplyr::mutate(
    newwidth = pmax(
      minStrwidth,
      strwidth * dplyr::case_when(
        rowspan > 1 ~ 1/rowspan,
        words > 4 ~ 2/words,
        TRUE ~ 1)
    )
  ) %>% dplyr::pull(newwidth)

  charLen = charLen / yourDevice # correct any scaling issues due to device.
  # charLen = charLen * cmsPerChar # charLen is now in cms.
  charLen = charLen + 0.5 # enforce a minimum value for a column
  charLen = 1*ceiling(charLen/1) # discretise colwidths to units of 1 cms based on content and 1 header row.
  # TODO there would be better ways to discretise here - trying to find 4 levels that represent content for example e.g. 4 col widths based on quantiles of content width.
  desired_total_width_in_cms = sum(charLen) # + 0.3*(length(charLen)-1) # plus 0.3 cm per column for padding
  col_widths = charLen/sum(charLen) # normalise col widths to sum to 1

  hux2 = hux %>% huxtable::set_col_width(col_widths)

  # for Excel the overall width needs to be set to an absolute value which is in some arbitrary unit known only to excel itself
  if (target == "xlsx") hux2 = hux2 %>% huxtable::set_width((desired_total_width_in_cms / 0.206) / (20*ncol(hux)))
  # for officer the overall width is as a fraction of the page width.
  else if (target %in% c("docx","odt")) hux2 = hux2 %>% huxtable::set_width(1)
  # for everything else the target is automatically determined by size and HTML lays out the content.
  else hux2 = hux2 %>% huxtable::set_width("auto")

  hux2
}

.font_paths = function() {
  unique(c(
    systemfonts::system_fonts()$path,
    systemfonts::registry_fonts()$path
  ))
}

## Saving to pdf ----


#' Save a table to a variety of formats
#'
#' depending on the context return the correct format for a document.
#' The basic output here is to use HTML as an output if possible and convert it to an image or a PDF that can then
#' be included into a latex document for example.
#'
#' @param hux the huxtable to save
#' @param filename the filename, which may omit the extension
#' @param size a ggrrr::std_size
#' @param maxWidth or the maximum width in inches
#' @param maxHeight and either the maximum height in inches
#' @param aspectRatio or the minimum allowed aspect ratio
#' @param formats if the extension is omitted, all the formats described here will be saved. Currently supported outputs are "html","png","pdf","docx","xlsx"
#' @param defaultFontSize the default font size
#' @param sheetname if saving as an xlsx file.
#'
#' @return the output depends on if the function is called in a knitr session. It maybe the HTML or a link to the pdf output for example.
#' @export
hux_save_as = function(hux,filename,
      size = std_size$full, maxWidth = size$width, maxHeight = size$height,
      aspectRatio=maxWidth/maxHeight,
      formats = c("html","png","pdf"),
      defaultFontSize = 8,
      sheetname = fs::path_ext_remove(fs::path_file(filename))
  ) {

  html2 = NULL  # remove global binding note

  if (!huxtable::is_hux(hux)) hux = hux %>% ggrrr::hux_default_layout(defaultFontSize = defaultFontSize)
  if (.is_knitting() & .is_latex_output()) {
    formats = unique(c(formats,"pdf"))
  }

  if(rlang::is_installed("html2pdfr") && utils::packageVersion("html2pdfr") >= "0.4.0") {
    supported = c("html","png","pdf","docx","xlsx")
  } else {
    supported = c("html","docx","xlsx")
  }
  formats = formats[formats %in% supported]

  dir = fs::path_dir(filename)
  if(dir==".") stop("directory not given. filename must be a full path (use here::here function).")
  if(fs::path_ext(filename) %in% supported) formats = fs::path_ext(filename)
  if (!dir.exists(dir)) dir.create(dir,recursive = TRUE)
  filename = fs::path_ext_remove(filename)
  withExt = function(extn) {fs::path_ext_set(filename,extn)}
  matchedFiles = function(extns) {
    extns = paste0(extns,collapse="|")
    fs::dir_ls(fs::path_dir(filename),regexp = paste0(.escape(filename),"(_[0-9]+)?\\.(",extns,")"))
  }

  # clean up any possible outputs from previous run
  lapply(matchedFiles(supported), function(x) try(unlink(x)))

  fonts_used = hux %>% huxtable::font() %>% as.vector() %>% unique()
  non_local_fonts = fonts_used[!fonts_used %in% systemfonts::system_fonts()$family]

  out = list()
  out$hux = hux
  out$width = maxWidth
  out$height = maxHeight

  if ("docx" %in% formats) {
    doc = officer::read_docx()
    # browser()
    pageSize =
      if (maxWidth >= 21*2/2.54)
        officer::page_size(width = 29.7/2.54, height=21*2/2.54, orient="landscape") #A3 landscape
      else if (maxWidth >= 29.7/2.54)
        officer::page_size(width = 29.7/2.54, height=21*2/2.54, orient="portrait") #A3 portrait
      else if (maxWidth >= 21/2.54)
        officer::page_size(width = 21/2.54, height=29.7/2.54, orient="landscape") #A4 landscape
      else officer::page_size(width = 21/2.54, height=29.7/2.54, orient="portrait") #A4 portrait

    marginSize = (pageSize$width-maxWidth)/2

    doc = doc %>% officer::body_set_default_section(
      officer::prop_section(
        page_size=pageSize,
        page_margins = officer::page_mar(marginSize,marginSize,marginSize,marginSize,0.1,0.1,0.1),
        type = "continuous"
      )
    )
    ft = hux %>% huxtable::as_flextable() %>% flextable::autofit(add_h=0)
    for (i in 1:ncol(hux)) {
      ft = ft %>% flextable::width(j = i, width = huxtable::col_width(hux)[[i]]*maxWidth)
    }
    # ft = ft %>% #flextable::autofit(part = "body") %>%
    # flextable::fit_to_width(maxWidth)
    if (!is.null(ft$caption$value)) {
      if (utils::packageVersion("flextable") >= "0.5.5") {
        doc <- officer::body_add_par(doc, ft$caption$value, style = "table title")
      }
    }
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_add_par(doc, " ")
    print(doc, target = withExt("docx"))
    out$docx = withExt("docx")
  }

  if ("xlsx" %in% formats) {
    hux %>% huxtable::quick_xlsx(file = withExt("xlsx"),open=FALSE)
    out$xlsx = withExt("xlsx")
  }

  if (any(c("pdf","png","html") %in% formats)) {

    html = stringr::str_remove(
      hux %>% huxtable::to_html(),
      stringr::fixed("margin-bottom: 2em; margin-top: 2em;"))

    # stylesheet for google fonts
    # used_google_fonts = non_local_fonts[non_local_fonts %in% sysfonts::font_families_google()]
    # used_google_fonts = used_google_fonts %>% stringr::str_replace_all("\\s","+")
    # style_dec = paste0("<link rel='stylesheet' href='https://fonts.googleapis.com/css?family=",used_google_fonts,"'/>", collapse = "")

    if ("html" %in% formats) {
      style_dec = ""
      fonts = .hux_used_fonts(hux)
      webfonts = .get_font_face(fonts)
      if (length(webfonts) > 0) style_dec = sprintf("<style>%s</style>",paste0(webfonts, collapse = ""))
      write(sprintf("<html><head><meta charset='UTF-8'>%s</head><body>%s</body></html>",style_dec,html), withExt("html"))
      out$html = withExt("html")
    }

    if (any(c("pdf","png") %in% formats)) {

      if(rlang::is_installed("html2pdfr") && utils::packageVersion("html2pdfr") >= "0.4.0") {

        # get these functions without R CMD Check noticing that they might not be installed
        # since we check for these above we don't need to handle failure.
        html_converter = optional_fn("html2pdfr","html_converter")
        html_fragment_to_pdf = optional_fn("html2pdfr","html_fragment_to_pdf")

        conv = html_converter(.font_paths())

        tmp = c("pdf","png")[c("pdf","png") %in% formats]

        html_fragment_to_pdf(
          htmlFragment = html,
          outFile = filename,
          formats = tmp,
          maxWidthInches = maxWidth,
          maxHeightInches = maxHeight,
          xMarginInches = 0,
          yMarginInches = 0,
          pngDpi = 300,
          converter = conv
        )

        try(
          grDevices::embedFonts(withExt("pdf")),
          silent=TRUE
        );

        if("pdf" %in% tmp) out$pdf = withExt("pdf")
        if("png" %in% tmp) out$png = unname(matchedFiles("png"))

      } else if (rlang::is_installed("webshot")) {

        #TODO replace with printing with chrome?
        warning("html2pdfr (version >0.4.0) is not installed but webshot is. We'll use that but it will not respect page length constraints, there will only be 1 PNG for multiple pages, and output will be fixed width.")

        # get these functions without R CMD Check noticing that they might not be installed
        # since we check for these above we don't need to handle failure.
        is_phantomjs_installed = optional_fn("webshot","is_phantomjs_installed")
        webshot = optional_fn("webshot","webshot")

        if (is_phantomjs_installed()) {
          # webshot alternative
          if("png" %in% formats) {
            webshot(
              url = sprintf("file://%s",withExt("html")),
              file = withExt("png"),
              # webshots page width dimensions are messed up
              vwidth=maxWidth*96,
              vheight=10,
              zoom=300/96
            )
            out$png = unname(matchedFiles("png"))
          }

          if("pdf" %in% formats) {
            tmp2 = hux
            # for pdfs with webshot we need to adjust the font size by 2/3 to match png. This was determined empirically and it is really unclear why.
            attr(tmp2,"font_size") = ifelse(is.na(attr(tmp2,"font_size")),defaultFontSize,attr(tmp2,"font_size"))*2/3
            eahtml2 = stringr::str_remove(
              tmp2 %>% huxtable::to_html(),
              stringr::fixed("margin-bottom: 2em; margin-top: 2em;"))
            tmpHtml = tempfile(fileext = ".html")
            write(sprintf("<html><head><meta charset='UTF-8'>%s</head><body>%s</body></html>",style_dec,html2), tmpHtml)
            webshot(
              url = sprintf("file://%s",tmpHtml),
              file = withExt("pdf"),
              # webshots page width dimensions are messed up
              vwidth=maxWidth*72,
              vheight=10
            )
            out$pdf = withExt("pdf")
          }
        } else {
          # webshot is not set up.
          warning("skipping PNG and PDF generation as webshot is not properly set up. try `webshot::install_phantomjs()`")
        }
      } else {
        warning("skipping PNG and PDF generation as neither webshot nor html2pdfr is installed.")
      }

    }
  }

  return(structure(out, class="rendered_table"))
}

#' Knit a rendered_table object
#'
#' @param x the rendered_table
#' @param ... not used
#'
#' @return nothing - used for side effects
#' @export
knit_print.rendered_table = function(x,...) {
  hidetables = getOption("hide.tables",FALSE)
  if (hidetables) {
      # e.g. knitting to a word document
      return(knitr::asis_output(paste0("INSERT TABLE HERE: ",as.character(x),"\n\n")))
  } else {
    pngs = x$png
    if(.is_html_output()) {
      # knitting a html document
      return(knitr::knit_print(x$hux %>% huxtable::set_width("auto")))
    } else if (.is_document_output()) {
      # knitting a docx document
      # try and embed in the document but width must be scpecified with no
      # sensible default and paging is determined by word.
      return(knitr::knit_print(x$hux %>% huxtable::set_width(1)))
    } else {
      # most likely latex
      if (.is_latex_output() & !is.null(x$pdf)) {
        return(knitr::include_graphics(path = x$pdf))
      } else if (length(pngs)>0) {
        # this will be default output in many situations - a png with fallback to pdf if available.
        return(knitr::include_graphics(path = pngs, auto_pdf = TRUE, dpi=300))
      } else {
        # some non standard output.
        # just let huxtable decide what the best thing to do is.
        return(knitr::knit_print(x$hux))
      }
    }
  }
}

#' Convert a rendered_table object to a character
#'
#' @param x the rendered_table
#' @param ... not used
#'
#' @return a named vector
#' @export
#' @examples
#' hux = iris %>% hux_default_layout()
#' tmp = hux %>% hux_save_as(tempfile())
#' as.character(tmp)
as.character.rendered_table = function(x, ...) {
  tmp = x[!names(x) %in% c("hux","width","height")]
  out = sprintf("a huxtable with %d outputs:", length(tmp))
  if (length(tmp) > 0) {
    class(tmp)="list"
    out = c(out,sprintf("%s: %s", names(tmp), sapply(tmp, paste0, collapse=", ")))
  }
  return(out)
}

#' Print a rendered_table object
#'
#' @param x the rendered_table
#' @param ... not used
#'
#' @return nothing - used for side effects
#' @export
print.rendered_table = function(x,...) {

  if (interactive()) {
    v = getOption("viewer", utils::browseURL)
    if (!is.null(x$pdf)) v(x$pdf)
    else if (!is.null(x$png)) v(x$png)
    else if (!is.null(x$html)) v(x$html)
    else htmltools::html_print(htmltools::HTML(x$hux %>% huxtable::to_html()))
  }

  print(x$hux)

  # if (.is_running_in_chunk()) {
  #   # running interactively in an RMarkdown document
  #   pngs = x$png
  #   if (length(pngs)>0) {
  #     for (png in x$png) {
  #       img = png::readPNG(png)
  #       g = grid::rasterGrob(img, interpolate=TRUE)
  #       print(ggplot2::ggplot() +
  #         ggplot2::annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
  #     }
  #   } else {
  #     print(x$hux)
  #   }
  # } else {
  #   v = getOption("viewer")
  #   if (!is.null(x$pdf)) v(x$pdf)
  #   else if (!is.null(x$png)) v(x$png)
  #   else if (!is.null(x$html)) v(x$html)
  #   else htmltools::HTML(x$hux %>% huxtable::to_html()) %>% htmltools::html_print()
  # }
}

#' A sprintf alternative that handles NA values gracefully (ish)
#'
#' @param fmt sprintf format string
#' @param ... sprintf inputs
#' @param na.text an string to replace NA values with.
#'
#' @return a string value
#' @export
hux_sprintf = function(fmt, ..., na.text = "\u2014") {
  sprintf(fmt,...) %>% stringr::str_replace_all("NA",na.text)
}


## outputting as a ggplot object ----

#' Convert a huxtable to a ggplot object
#'
#' Useful if you need to include a formatted table in a figure with a plot
#'
#' @param hux the huxtable
#' @param width the desired ggplot width
#'
#' @return a ggplot object of the right width
#' @export
hux_to_ggplot = function(hux, width=5.9) {
  longFormatTable = hux %>% ggrrr::as.long_format_table(hux)
  ggrrr::gg_formatted_table(longFormatTable, width)
}



#' Bind rows for huxtables
#'
#' Sometimes vanilla bind_rows gets confused.
#'
#' @param ... a list of huxtables
#'
#' @return a single huxtable
#' @export
hux_bind_rows = function(...) {
  dots = rlang::list2(...)
  if (is.list(dots[[1]]) & length(dots)==1) dots = dots[[1]]
  out = dots[[1]]
  for (i in 2:length(dots)) {
    out = out %>% huxtable::add_rows(dots[[i]], after = nrow(out))
  }
  return(out)
}

#' Set a huxtable caption as a first row
#'
#' Keeps the same formatting as the rest of the table
#'
#' @param hux a huxtable
#' @param caption caption text
#'
#' @return a huxtable with first row caption
#' @export
hux_set_caption = function(hux, caption) {
  caption = paste0(caption,collapse="\n")
  hux %>% hux_insert_start(caption, colspan = ncol(hux)) %>%
    huxtable::set_top_border(1, huxtable::everywhere, 0) %>%
    huxtable::set_wrap(1, huxtable::everywhere, TRUE)
}

#' Insert row at start maintaining format
#'
#' @param hux a huxtable
#' @param ... stuff to insert
#' @param fill padding
#' @param colspan how far to span first inserted cell?
#'
#' @return a huxtable with row inserted at start in the same format
#' @export
hux_insert_start = function(hux, ..., fill="", colspan = 1) {
  hux = hux %>% huxtable::insert_row(
    ...,
    after=1, fill=fill,copy_cell_props = TRUE)
  tmp = hux[2,]
  hux[2,] = hux[1,]
  hux[1,] = tmp
  hux %>% huxtable::set_colspan(1, 1, colspan)
}

#' Set a huxtable caption as a first row
#'
#' Keeps the same formatting as the rest of the table
#'
#' @param hux a huxtable
#' @param footer footer text
#'
#' @return a huxtable with first row caption
#' @export
hux_set_footer = function(hux, footer) {
  footer = paste0(footer,collapse="\n")
  hux %>% huxtable::insert_row(
    footer,
    after=nrow(hux), colspan = ncol(hux), fill="",copy_cell_props = TRUE) %>%
    huxtable::set_bottom_border(huxtable::final(1), huxtable::everywhere, 0) %>%
    huxtable::set_wrap(huxtable::final(1), huxtable::everywhere, TRUE)
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
        .hux_default_layout() %>%
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

## Utilities ----

# from rex:::escape.character
.escape = function (x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[",
             "]", "{", "}", "\\")
  .sanitize(x, chars)
}

.sanitize = function (x, chars) {
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"),
       "\\\\\\1", x, perl = TRUE)
}
