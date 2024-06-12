## Huxtable utils ----

### hux_default_layout ----

#' @inherit .hux_default_layout
#' @export
#' @examples
#' library(tidyverse)
#' hux = iris %>% hux_default_layout()
hux_default_layout = .hux_default_layout

### rows footers headers ----

#' @inherit .hux_bind_rows
#' @export
hux_bind_rows = .hux_bind_rows

#' @inherit .hux_set_caption
#' @export
hux_set_caption = .hux_set_caption

#' @inherit .hux_insert_start
#' @export
hux_insert_start = .hux_insert_start

#' @inherit .hux_set_footer
#' @export
hux_set_footer = .hux_set_footer


### hux_set_font ----

#' @inherit .hux_set_font
#' @export
hux_set_font = .hux_set_font

### hux_tidy ----

#' @inherit .hux_tidy
#' @export
hux_tidy = .hux_tidy

### hux_nest_group ----

#' @inherit .hux_nest_group
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


