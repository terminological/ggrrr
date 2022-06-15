## Huxtable utils ----

#' A tidy article theme for huxtables that works with google docs
#'
#' @param hux a huxtable object
#' @param defaultFontSize default size of font in points (8)
#' @param defaultFont the font name
#' @keywords huxtable
#' @import huxtable
#' @export
#' @examples
#' hux = huxtable(dataframe %>% select("col 1 title"=col1)) %>% defaultTableLayout()
hux_default_layout = function(hux, defaultFontSize=8, defaultFont = "Roboto", headerRows = 1) {
  if (!defaultFont %in% extrafont::fonts()) stop("Font not installed (choose something from extrafonts::fonts())")
  # TODO: load it from google if not.
  if(!huxtable::is_hux(hux)) hux = huxtable::as_hux(hux)
  return( hux %>%
            huxtable::set_font_size(huxtable::everywhere,huxtable::everywhere,defaultFontSize) %>%
            huxtable::set_font(huxtable::everywhere,huxtable::everywhere,defaultFont) %>%
            huxtable::set_top_border(1, huxtable::everywhere, 0.5) %>%
            huxtable::set_bottom_border(headerRows, huxtable::everywhere, 0.5) %>%
            huxtable::set_bottom_border(nrow(hux), huxtable::everywhere, 0.5) %>%
            huxtable::set_wrap(huxtable::everywhere, huxtable::everywhere, TRUE) %>%
            huxtable::set_top_padding(huxtable::everywhere,huxtable::everywhere,1) %>%
            huxtable::set_bottom_padding(huxtable::everywhere,huxtable::everywhere,0) %>%
            huxtable::set_valign(huxtable::everywhere,huxtable::everywhere,"top")
            )
}

#' Set the font in a huxtable globally
#'
#' @param hux a huxtable table
#' @param defaultFontSize the desired font size
#' @param defaultFont the desired font
#'
#' @return the altered huxtable
#' @export
hux_set_font = function(hux, defaultFontSize=8, defaultFont = "Roboto") {
  if (!defaultFont %in% extrafont::fonts()) stop("Font not installed (choose something from extrafonts::fonts())")
  # TODO: load it from google if not.
  hux %>%
    huxtable::set_font_size(huxtable::everywhere,huxtable::everywhere,defaultFontSize) %>%
    huxtable::set_font(huxtable::everywhere,huxtable::everywhere,defaultFont)
}


#' Convert a dataframe to a huxtable with nested rows and columns
#'
#' @param tidyDf A dataframe with row groupings (as a set of columns) and column groupings (as a set of columns) and data, where the data is in a tidy format with a row per "cell" or cell group.
#' @param rowGroupVars A vars(...) column specification which will define how rows are grouped
#' @param colGroupVars A vars(...) column specification with defines how columns will be grouped
#' @param missing If there is no content for a given rowGroup / colGroup combination then this character will be used as a placeholder
#' @param na If there are NA contents then this character will be used.
#'
#' @return a huxtable table
#' @export
hux_tidy = function(tidyDf, rowGroupVars, colGroupVars, missing="\u2014", na="\u2014") {

  if(tidyDf %>% group_by(!!!colGroupVars,!!!rowGroupVars) %>% count() %>% pull(n) %>% max() > 1) stop("rowGroupVars and colGroupVars do not define unique rows (did you forget to summarise?)")

  cols = lapply(colnames(tidyDf),as.symbol)
  data = colnames(tidyDf)[!colnames(tidyDf) %in% sapply(c(rowGroupVars, colGroupVars),as_label)]

  tmp = tidyDf %>%
    ungroup() %>%
    mutate(across(.cols = all_of(data), as.character)) %>%
    pivot_longer(cols = data) %>%
    mutate(name = factor(name,levels=data)) %>%
    #TODO formatters?
    ungroup() %>%
    group_by(!!!colGroupVars,name) %>%
    arrange(!!!rowGroupVars) %>%
    mutate(.x = cur_group_id()) %>%
    group_by(!!!rowGroupVars) %>%
    mutate(.y = cur_group_id())

  # browser()

  rowHeadings = tmp %>% ungroup() %>% select(!!!rowGroupVars,.y) %>% arrange(.y) %>% distinct()
  colHeadings = tmp %>% ungroup() %>% select(!!!colGroupVars,name,.x) %>% arrange(.x) %>% distinct()

  colHux = as.data.frame(unname(t(colHeadings %>% select(-.x))),stringsAsFactors = FALSE)
  colnames(colHux) = 1:length(colHux)

  hux = tmp %>% ungroup() %>% select(.y,.x,value) %>% mutate(value = ifelse(is.na(value), na, value)) %>%
    pivot_wider(names_from = .x, values_from = value, values_fill=missing) %>% arrange(.y) %>% select(-.y)

  rowHux = rowHeadings %>% select(-.y) %>% mutate(across(everything(), as.character))

  # browser()
  xOffset = length(colnames(rowHux))
  yOffset = nrow(colHux)
  topCornerHux = as.data.frame(t(matrix(c(rep("",(yOffset-1)*xOffset),colnames(rowHux)),nrow = xOffset,byrow = FALSE)),stringsAsFactors = FALSE)
  colnames(topCornerHux) = colnames(rowHux)
  #browser()
  fullHux = bind_cols(
    bind_rows(topCornerHux,rowHux),
    bind_rows(colHux,hux)
  )

  fullHux = fullHux %>% huxtable::hux(add_colnames = FALSE) %>%
    huxtable::set_header_rows(1:yOffset, TRUE) %>%
    huxtable::set_header_cols(1:xOffset, TRUE) %>%
    hux_default_layout(headerRows = yOffset)

  # do column merges
  tmpVars = colGroupVars
  while(length(tmpVars)>0) {
    for( mergeCols in colHeadings %>% group_by(!!!tmpVars) %>% summarise(cols = list(unique(.x))) %>% pull(cols)) {
      # mergeCols = colHeadings %>% group_by(!!!tmpVars) %>% group_data() %>% pull(.rows) %>% `[[`(1)
      rowIndex = length(tmpVars)
      l = min(mergeCols)+xOffset
      lr = c(min(mergeCols),max(mergeCols))+xOffset
      #fullHux = fullHux %>% huxtable::set_align(col=lr, row=rowIndex, "center")
      fullHux = fullHux %>% huxtable::merge_cells(col=lr, row=rowIndex)
      # column borders?
    }
    tmpVars = tmpVars %>% head(-1)
  }


  # do row merges
  tmpVars = rowGroupVars
  while(length(tmpVars)>0) {
    rowGroups = rowHeadings %>% group_by(!!!tmpVars) %>% summarise(rows = list(unique(.y)), count=length(unique(.y)))
    # do the merge if and only if there are multiple rows in at least one group.
    if(any(rowGroups$count > 1)) {
      for( mergeRows in rowGroups %>% pull(rows)) {
        # mergeCols = colHeadings %>% group_by(!!!tmpVars) %>% group_data() %>% pull(.rows) %>% `[[`(1)
        colIndex = length(tmpVars)
        l = min(mergeRows)+yOffset
        lr = c(min(mergeRows),max(mergeRows))+yOffset
        # fullHux = fullHux %>% huxtable::set_valign(lr,colindex,"middle")
        fullHux = fullHux %>% huxtable::merge_cells(row=lr, col=colIndex)
        fullHux = fullHux %>%
          huxtable::set_top_border(l, huxtable::final(ncol(fullHux)-colIndex+1), 0.5) %>%
          # This fills in the bottom border of a merged cell.
          huxtable::set_bottom_border(l, colIndex, 0.5)
        # column borders?
      }
    }
    tmpVars = tmpVars %>% head(-1)
  }

  # Fix merged borders.
  fullHux %>% huxtable::set_bottom_border(nrow(hux), huxtable::everywhere, 0.5)

  return(fullHux)
}


# get the aspect ratio of a bit of text - this gives the width/height of a bit of text
.get_text_ar = function(txt, font, face="plain", linespacing=1+1/8) {
  tmp = tibble(label = txt, font=font, face=face)
  tmp = tmp %>% mutate(
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
  ret = .get_text_ar(txt,font)
  ret = ret %>%
    mutate(
      heightCms = (lines+(lines-1)*(linespacing-1))*font_size/72*2.54,
      widthCms = heightCms*ar) %>%
    pull(widthCms)

  return(ret)
}

#.get_text_cms(c("ABC","DEF","ABCDEF","ABC\nDEF"), font_size=c(12,16,12,12))
#.get_text_ar(c("ABC","DEF","ABCDEF","ABC\nDEF"),font = "Arial")



#' Estimate column content widths based on dataframe or huxtable ignoring rowspans and potential for wrapping.
#'
#' @param table a table to get column content widths for.
#'
#' @return a vector od column widths
#' @export
#'
#' @examples
#' iris %>% fit_col_widths()
fit_col_widths = function(table) {
  table %>% as.long_format_table() %>%
    mutate(ar = ggrrr::.get_text_ar(label,font = fontName,face = fontFace) %>% pull(ar)) %>%
    filter(colSpan == 1) %>%
    group_by(col) %>%
    summarise(ar = max(ar)) %>%
    arrange(col) %>% pull(ar)
}

#' Calculate a sensible column and table width for a huxtable based on its content.
#'
#' @param hux the huxtable
#' @param target the expected output (could be "docx"/"odt", "xlsx") which are the only options that matter
#' @param including_headers Should we try and fit the header contents as well (TRUE) or let those wrap (FALSE).
#'
#' @return the huxtable with the width options set.
#' @export
hux_auto_widths = function(hux, target = "html", including_headers = FALSE) {
  # TODO: this needs a bit of a tidy up.
  fontSize = median(huxtable::font_size(hux))
  # par(family = "sans", ps = fontSize)
  merged_headers = sum(huxtable::header_rows(hux))-1
  # empirically determined for the default 8pt font size in excel:
  yourDevice = strwidth("mmmm",units="in")/0.5416667 #my device
  cmsPerChar = mean(c(5.01/7.461250,  4.22/6.297083, 0.68/0.714375, 3.45/5.000625,   0.68/0.714375, 3.49/5.000625, 2.1/2.963333,  0.95/1.031875))

  # Huxtable tries to guess appropriate widths and height for rows and columns; numeric width() and height() are treated as scaling factors:
  # cw <- col_width(ht)
  # if (!is.numeric(cw) || anyNA(cw)) cw <- rep(1/ncol(ht), ncol(ht)) #cw defaults to 1/ncol
  # basic_width <- 20 * ncol(ht) # presume the 20 is in the basic excel unit
  # w <- width(ht)
  # if (!is.numeric(w) || is.na(w))
  #   w <- 0.5 # the default table width = 0.5 is mysterious. This will scale the
  # widths = cw * w * basic_width
  # 8 columns results in row width of 2.06 cms => 8*2.06 is 0.5(w) * 20 * 8 => each unit is 0.206 cms
  # to make it do what we want. set width to be (desired_total_width_in_cms / 0.206) / (20*ncol(hux))
  # set col_width to be proportions that adds up to 1.

  if(including_headers) {
    tmp = hux
  } else {
    tmp = hux %>% filter(row_number()>merged_headers)
  }

  strwidths = tmp %>% ungroup() %>%
    summarise(across(everything(), .fns = ~ max(.get_text_cms(.x, font_size = fontSize),na.rm = TRUE))) %>%
    pivot_longer(cols=everything()) %>% rename(strwidth = value)

  rowspans = tmp %>%
    huxtable::rowspan() %>%
    apply(function(.x) mean(.x),MARGIN = 2) %>%
    as_vector() %>% enframe() %>%
    rename(rowspan = value)

  words = tmp %>% ungroup() %>%
    summarise(across(everything(),
                  .fns = ~ stringr::str_split(.x,fixed(" ")) %>% lapply(length) %>% unlist() %>% max()
                  ,na.rm = TRUE)) %>%
    pivot_longer(cols=everything()) %>% rename(words = value)

  # a maximum single word length in a column.
  wordLength = tmp %>% ungroup() %>%
    summarise(across(everything(),
                     .fns = ~ stringr::str_split(.x,fixed(" ")) %>% lapply(function(.x) .get_text_cms(.x, font_size = fontSize)) %>% unlist() %>% max()
                     ,na.rm = TRUE)) %>%
    pivot_longer(cols=everything()) %>% rename(minStrwidth = value)
  # browser()
  charLen = strwidths %>% inner_join(rowspans, by="name") %>% inner_join(words, by="name") %>% inner_join(wordLength, by="name") %>% mutate(
    newwidth = pmax(
      minStrwidth,
      strwidth * case_when(
        rowspan > 1 ~ 1/rowspan,
        words > 4 ~ 2/words,
        TRUE ~ 1)
    )
  ) %>% pull(newwidth)

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



## Saving to pdf ----


#' Save a table to a variety of formats and depending on the context return the correct format for a document
#'
#' The basic output here is to use HTML as an output if possible and convert it to an image or a PDF that can then
#' be included into a latex document for example.
#'
#' @param hux the huxtable to save
#' @param filename the filename, which may omit the extension
#' @param size a ggrrr::std_size
#' @param maxWidth or the maximum width in inches
#' @param maxHeight and either the maximum height in inches
#' @param aspectRatio or the minimum allowed aspect ratio
#' @param formats if the extension is omitted all the extensions described here will be saved. Currently supported outputs are "html","png","pdf","docx","xlsx"
#' @param sheetname if saving as an xlsx then the sheetname
#'
#' @return the output depends on if the function is called in a knitr session. It maybe the HTML or a link to the pdf output for example.
#' @export
hux_save_as = function(hux,filename,
                      size = std_size$full, maxWidth = size$width, maxHeight = size$height,
                      aspectRatio=maxWidth/maxHeight,
                      formats = c("html","png","pdf"),
                      sheetname = fs::path_ext_remove(fs::path_file(filename))
                      ) {
  if (!huxtable::is_hux(hux)) hux = hux %>% hux_default_layout()
  if (.is_knitting() & .is_latex_output()) {
    formats = unique(c(formats,"pdf"))
  }


  if(is_installed("html2pdfr")) {
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

  # Does not do anything with the width

  lapply(formats, function(x) try(unlink(withExt(x))))

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
  }

  if ("xlsx" %in% formats) hux %>% huxtable::quick_xlsx(file = withExt("xlsx"),open=FALSE)

  if (any(c("pdf","png","html") %in% formats)) {

    html = stringr::str_remove(
      hux %>% huxtable::to_html(),
      stringr::fixed("margin-bottom: 2em; margin-top: 2em;"))

    if ("html" %in% formats) write(html, withExt("html"))

    if (any(c("pdf","png") %in% formats)) {

      if(requireNamespace("html2pdfr", quietly=TRUE)) {
        J = html2pdfr::JavaApi$get()
        conv = J$HtmlConverter$new()
        outputFiles = conv$fitIntoPage(
          htmlFragment = html,
          outFile = filename,
          formats = c("pdf","png")[c("pdf","png") %in% formats],
          maxWidthInches = maxWidth,
          maxHeightInches = maxHeight,
          pngDpi = 300
        )
        conv$finalize()
        try(
          embedFonts(withExt("pdf")),
          silent=TRUE
        );
      }

    }
  }

  # TODO this is totally spurious. Need a much better way that is shared between this and gg_save_as of deciding what to actually display.

  if (.is_knitting()) {
    hidetables = getOption("hide.tables",FALSE)
    if (hidetables) {
      # e.g. knitting to a word document
      return(knitr::asis_output(paste0("INSERT TABLE HERE: ",fs::path_file(filename),"\n\n")))
    } else {
      if(.is_html_output()) {
        return(hux %>% huxtable::set_width("auto"))
      } else if (.is_document_output() & "docx" %in% formats) {
        # try and embed in the document
        return(hux %>% huxtable::set_width(1))
      } else {
        if ("png" %in% formats) {
          # this will be default output in many situations - a png with fallback to pdf if available.
          return(knitr::include_graphics(path = withExt("png"),auto_pdf = TRUE, dpi=NA))
        } else if (.is_latex_output() & "pdf" %in% formats) {
          return(knitr::include_graphics(path = withExt("pdf"),auto_pdf = TRUE, dpi=NA))
        } else {
          # just let huxtable decide
          return(hux)
        }
      }
    }
  } else {
    if (.is_running_in_chunk()) {
      return(htmltools::HTML(hux %>% huxtable::to_html()))
    } else {
      # on a console most likely: display html in the viewer.
      return(htmltools::HTML(hux %>% huxtable::to_html()) %>% htmltools::html_print())
    }
  }

}



hux_sprintf = function(fmt, ..., na.text = "\u2014") {
  sprintf(fmt,...) %>% stringr::str_replace_all("NA",na.text)
}


## outputting as a ggplot object ----

hux_to_ggplot = function(hux, width=5.9) {
  longFormatTable = hux %>% as.long_format_table(hux)
  gg_formatted_table(longFormatTable, width)
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
