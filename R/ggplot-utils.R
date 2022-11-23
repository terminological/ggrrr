## ggplot defaults ----

#' A space saving ggplot theme
#'
#' A ggplot theme with minimal fluff and with the defaults set small.
#'
#' @param baseSize the size of the base font.
#' @param font the font family name
#'
#' @return a ggplot theme
#' @export
#'
#' @examples
#' library(tidyverse)
#' ggplot(diamonds,aes(x=carat,y=price,color=color))+geom_point()+gg_tiny_theme()
gg_tiny_theme = function(baseSize = 8, font = "Roboto") {
  font = ggrrr::check_font(font)
  ggplot2::theme_bw(base_size=baseSize)+
    ggplot2::theme(
      text = ggplot2::element_text(family = font),
      plot.title= ggplot2::element_text(size=baseSize,hjust=0.5),
      plot.subtitle = ggplot2::element_text(size=baseSize,hjust=0.5),
      axis.title = ggplot2::element_text(size=baseSize),
      axis.text = ggplot2::element_text(size=baseSize*0.75),
      axis.text.x.bottom = ggplot2::element_text(angle = 30, hjust = 1, vjust=1),
      axis.text.x.top = ggplot2::element_text(angle = 30, hjust = 0, vjust = 0),
      # shrink facet titles
      strip.text = ggplot2::element_text(margin = ggplot2::margin(.05, 0, .05, 0, "cm"), size=baseSize),
      strip.background = ggplot2::element_rect(fill = "#F8F8F8"),
      # shrink legend
      legend.title = ggplot2::element_text(size=baseSize),
      legend.text = ggplot2::element_text(size=baseSize*0.75),
      legend.key.size = ggplot2::unit(0.4, "lines"),
      legend.box.margin = ggplot2::margin(),
      legend.margin = ggplot2::margin(t = 0, unit='cm'),
      legend.justification = "left",
      legend.box.just = "left",
      # transparent background
      # plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      plot.background = ggplot2::element_rect(fill = "white", color = NA), # bg of the plot
      plot.tag = ggplot2::element_text(size = baseSize*1.2)
      # position plot annotation closer to plot
      # plot.tag = ggplot2::element_text(size = baseSize*1.2, hjust = 0, vjust=1),
      # plot.tag.position = c(0, 1)
    )
}

#' Set sizes in ggplot uniformly
#'
#' Set the default sizes of lines, points and fonts in ggplot geoms, and text labels in ggplot axes
#' to get a single consistent look and feel.
#'
#' @param lineSize the width of lines and size of points (the default size aesthetic)
#' @param fontSizePts the size of labels and other on plot text in pts.
#' @param font the font family name
#'
#' @return nothing
#' @export
#'
#' @examples
#' library(tidyverse)
#' gg_set_size_defaults(lineSize = 0.25)
gg_set_size_defaults = function(lineSize = 0.5, fontSizePts = 4+lineSize*8, font="Roboto") {
  font = ggrrr::check_font(font)
  # get all ggplot2 geoms
  geoms = ls(pattern = '^geom_', envir = as.environment('package:ggplot2')) %>% stringr::str_remove("geom_")
  for(geom in geoms) {
    try({
      if (geom %in% c("label","text","sf_label","sf_text")) {
        ggplot2::update_geom_defaults(geom, list(size = ggrrr::gg_label_size(fontSizePts), family=font))
      } else {
        ggplot2::update_geom_defaults(geom, list(size = lineSize))
      }
    },silent = TRUE)
  }
  invisible(NULL)
}


#' An opinionated set of defaults for plots
#'
#' This is a set of styles with a focus on making plots compact, and minimally fussy, and ensuring fonts are consistent between axes and labels.
#'
#' @param lineSize the default line and shape size in ggplot units
#' @param fontSize the base font size
#' @param font the default font name.
#'
#' @return nothing
#' @export
gg_pedantic = function(lineSize = 0.25, fontSize = 8, font="Roboto") {
  font = ggrrr::check_font(font)
  ggplot2::theme_set(ggrrr::gg_tiny_theme(fontSize, font))
  ggrrr::gg_set_size_defaults(lineSize,fontSize*0.75,font)
}

#' Convert a label size from points to ggplot units
#'
#' Labels like geom_text are in a random unit size which is only mysteriously connected to the size of text on axes
#'
#' @param pts label size in points
#'
#' @return the label size in ggplot units
#' @export
gg_label_size = .gg_label_size

#' Hide the x axis of a plot
#'
#' @return a theme
#' @export
gg_hide_X_axis = .gg_hide_X_axis

#' Hide the y axis of a plot
#'
#' @return a theme
#' @export
gg_hide_Y_axis = .gg_hide_Y_axis

#' Hide the legend of a plot
#'
#' @return a theme
#' @export
gg_hide_legend = .gg_hide_legend

#' Set the angle of the x axis labels of a plot
#'
#' @param ang the angle for the x labels
#'
#' @return a theme
#' @export
gg_set_X_angle = .gg_set_X_angle

#' Make a plot narrower
#'
#' @param ang the angle for the x labels
#'
#' @return a theme
#' @export
gg_narrow = .gg_narrow

#' Make the legend smaller
#'
#' @param pointSize - the ggplot size of lines or points
#' @param textSize - the size in pts of the text
#' @param spaceLegend - degree of spacing between items in the scale (defines overall size)
#'
#' @return a theme
#' @export
gg_resize_legend = .gg_resize_legend

# drawDetails.watermark <<- function(x, rot = 45, ...){
# does this need to be global to work or is package scope ok?

#' Internal function for drawing watermark on ggplots
#' @param x the grob
#' @param rot degrees of rotation
#' @param ... ignored
#'
#' @return a grid object
#' @export drawDetails.watermark
drawDetails.watermark = function(x, rot = 45, ...){
  cex <- min(
    grid::convertX(grid::unit(0.9,"npc"), "mm", valueOnly=TRUE) / grid::convertUnit(grid::unit(1,"grobwidth", grid::textGrob(x$lab, rot=rot)), "mm",valueOnly=TRUE),
    grid::convertY(grid::unit(0.9,"npc"), "mm", valueOnly=TRUE) / grid::convertUnit(grid::unit(1,"grobheight", grid::textGrob(x$lab, rot=rot)), "mm",valueOnly=TRUE)
  )
  return(grid::grid.text(x$lab,  rot=rot, gp=grid::gpar(cex = cex, col="black", fontface = "bold", alpha = 0.05)))
}

#' Add in a watermark to plots
#'
#' @param disable - global option to disable all watermarks options("ggrrr.disable.watermark"=TRUE)
#' @param lab the watermark label (DRAFT)
#'
#' @return a watermark layer
#' @export
gg_watermark = function(lab = "DRAFT", disable = getOption("ggrrr.disable.watermark",default=FALSE)) {
  if (!disable) {
    grb = grid::grob(lab=lab, cl="watermark")
    return(ggplot2::annotation_custom(grb,xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf))
  } else {
    return(ggplot2::geom_blank())
  }
}

## Custom scales ----

#' A scales breaks generator for log1p scales
#'
#' @param n the number of breaks
#' @param base the base for the breaks
#'
#' @return a function for ggplot scale breaks
#' @export
#'
#' @examples
#' library(tidyverse)
#' ggplot(diamonds, aes(x=price))+
#'   geom_density()+
#'   scale_x_continuous(trans="log1p", breaks=ggrrr::breaks_log1p())
breaks_log1p = function(n=5,base=10) {
  #scales::force_all(n, base)
  n_default = n
  function(x, n = n_default) {
    tmp = scales::breaks_log(n_default,base)(x+1,n)
    return(c(0,tmp[-1]))
  }
}

#' logit scale
#'
#' @description it perform logit scaling with right axis formatting. To not be used directly but with ggplot (e.g. scale_y_continuous(trans = "logit") )
#'
#' @return A scales object
#'
#' @examples
#'
#' library(ggplot2)
#' library(tibble)
#'
#' tibble(pvalue = c(0.001, 0.05, 0.1), fold_change = 1:3) %>%
#'  ggplot(aes(fold_change , pvalue)) +
#'  geom_point() +
#'  scale_y_continuous(trans = "logit")
#'
#' @export
logit_trans <- function() {

  trans <- stats::qlogis
  inv <- stats::plogis

  scales::trans_new("logit",
            transform = trans,
            inverse = inv,
            breaks = functional::Compose(trans, scales::extended_breaks(), inv),
            format = scales::label_scientific(digits = 2)
  )
}

## GGplot tables ----

#' A simple table as a ggplot patchwork object, no customisation allowed
#'
#' @keywords graph layout
#' @export
#' @param df the dataframe with the table data. Column names will become headings
#' @param pts text size in points
#' @param font the font family
#' @param unwrapped - set this to TRUE if you want to add to a patchwork and use wrap_plots(p,list(table))
#' @return A gtable object (i.e. a grob) optionally wrapped as a patchwork plot.
#' @examples
#' library(tidyverse)
#' gg_simple_table(tibble::tibble(x=c(1,2,3),y=c(5,4,3)),pts=10)
gg_simple_table = function(df, pts=8, font = "Roboto", unwrapped = FALSE) {
  font = ggrrr::check_font(font)
  p = suppressWarnings(suppressMessages({
    ttheme = gridExtra::ttheme_minimal(
      base_size = pts, base_colour = "black", base_family = font,
      parse = FALSE, padding = grid::unit(c(4, 1.5), "mm"),
      core=list(fg_params=list(hjust=0, x=0.1), bg_params = list(fill = "#FFFFFF", alpha=1, col=NA)),
      colhead=list(fg_params=list(hjust=0, x=0.1), bg_params = list(fill = "#FFFFFF", alpha=1, col=NA))
    )
    g = gridExtra::tableGrob(d = df,rows = NULL,theme = ttheme)
    g <- gtable::gtable_add_grob(g,
                                 grobs = grid::segmentsGrob( # line across the bottom
                                   x0 = grid::unit(0,"npc"),
                                   y0 = grid::unit(0,"npc"),
                                   x1 = grid::unit(1,"npc"),
                                   y1 = grid::unit(0,"npc"),
                                   gp = grid::gpar(lwd = 2.0)),
                                 t = nrow(g), l = 1, r = ncol(g))
    g <- gtable::gtable_add_grob(g,
                                 grobs = grid::grobTree(
                                   grid::segmentsGrob( # line across the top
                                     x0 = grid::unit(0,"npc"),
                                     y0 = grid::unit(1,"npc"),
                                     x1 = grid::unit(1,"npc"),
                                     y1 = grid::unit(1,"npc"),
                                     gp = grid::gpar(lwd = 2.0)),
                                   grid::segmentsGrob( # line across the bottom
                                     x0 = grid::unit(0,"npc"),
                                     y0 = grid::unit(0,"npc"),
                                     x1 = grid::unit(1,"npc"),
                                     y1 = grid::unit(0,"npc"),
                                     gp = grid::gpar(lwd = 1.0))
                                 ),
                                 t = 1, l = 1, r = ncol(g))
    #if(!unwrapped) return(patchwork::wrap_ggplot_grob(g))
    if(!unwrapped) return(patchwork::wrap_elements(g))
    g
  }))
  return(p)
}

#' Display a long format table as a ggplot object.
#'
#' This is useful if you want to combine a formatted table with a plot in a multi-panel patchwork.
#'
#' @param longFormatTable a table - usually converted using as.long_format_table()
#' @param colWidths (optional) the relative widths of the columns.
#' @param tableWidthInches the maximum desired width of the plot. Text will be scaled to fit this width.
#' @param font the default font family
#' @param ... passed to as.long_format_table if and only if the input is not already in that format.
#'
#' @return a ggplot object containing the table as a ggplot.
#' @export
gg_formatted_table = function(longFormatTable, colWidths = NULL, tableWidthInches=5.9, font="Roboto", ...) {

  hux = colSpan = rowSpan = y1 = y0 = x0 = x1 = ay0 = ay1 = label = fontName = fontSize = textwidth = width =
    textheight = height = topBorderWeight = size = bottomBorderWeight = leftBorderWeight = rightBorderWeight =
    xpos = ypos = vjust = hjust = fontFace = x = y = xend = yend = NULL  # remove global binding note

  font = ggrrr::check_font(font)
  if (!("long_format_table" %in%  class(longFormatTable))) longFormatTable = ggrrr::as.long_format_table(longFormatTable, colWidths = colWidths, fontName=font, ...)
  if(is.null(colWidths)) colWidths = attr(longFormatTable,"colWidths")
  colWidths = colWidths/sum(colWidths)
  if (any(is.na(colWidths))) colWidths = rep(1/ncol(hux), ncol(hux))

  ncol = max(longFormatTable$col+longFormatTable$colSpan-1)
  nrow = max(longFormatTable$row+longFormatTable$rowSpan-1)


  # NB: ignores any kind of wrapping.
  rowHeights = rep(1/nrow, nrow)

  tidy2 = longFormatTable %>%
    dplyr::mutate(
      colMax = col+colSpan-1,
      rowMax = row+rowSpan-1,
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(col=1:(length(colWidths)+1), x0=c(0,cumsum(colWidths))),
      by="col"
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(colMax=0:(length(colWidths)), x1=c(0,cumsum(colWidths))),
      by="colMax"
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(row=1:(length(rowHeights)+1), y0=c(0,cumsum(rowHeights))),
      by="row"
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(rowMax=0:(length(rowHeights)), y1=c(0,cumsum(rowHeights))),
      by="rowMax"
    ) %>% dplyr::mutate(
      # flip y axis
      ay0 = 1-y1,
      ay1 = 1-y0,
      cx = (x0+x1)/2,
      cy = (ay0+ay1)/2
    ) %>% dplyr::mutate(
      xpos = dplyr::case_when(alignment=="START"~x0,alignment=="CENTER"~cx,alignment=="RIGHT"~x1,TRUE~cx),
      hjust = dplyr::case_when(alignment=="START"~0,alignment=="CENTER"~0.5,alignment=="RIGHT"~1,TRUE~0.5),
      ypos = dplyr::case_when(valignment=="TOP"~ay1,valignment=="MIDDLE"~cy,valignment=="BOTTOM"~ay0,TRUE~cy),
      vjust = dplyr::case_when(valignment=="TOP"~1,valignment=="MIDDLE"~0.5,valignment=="BOTTOM"~0,TRUE~0.5),
    )

  table_width = tidy2 %>% dplyr::mutate(
    textwidth = .get_text_cms(label, font = fontName,font_size = fontSize)/2.54*72+2,
  ) %>% dplyr::group_by(row) %>% dplyr::summarise(
    width = max(textwidth*1/(x1-x0)) #* (.is_header_col == FALSE))
  ) %>% dplyr::pull(width) %>% max()

  if (table_width > tableWidthInches*72) {
    scale = tableWidthInches*72/table_width # for scaling text to fit desired width
  } else {
    scale = 1
  }

  table_height = tidy2 %>% dplyr::mutate(
    textheight = fontSize+2,
  ) %>% dplyr::group_by(col) %>% dplyr::summarise(
    height = sum(textheight)
  ) %>% dplyr::ungroup() %>% dplyr::pull(height) %>% max()


  borders = dplyr::bind_rows(
    tidy2 %>% dplyr::mutate(size=ggrrr::gg_label_size(topBorderWeight)) %>% dplyr::select(x=x0,y=ay1,xend=x1,yend=ay1,size),
    tidy2 %>% dplyr::mutate(size=ggrrr::gg_label_size(bottomBorderWeight)) %>% dplyr::select(x=x0,y=ay0,xend=x1,yend=ay0,size),
    tidy2 %>% dplyr::mutate(size=ggrrr::gg_label_size(leftBorderWeight)) %>% dplyr::select(x=x0,y=ay0,xend=x0,yend=ay1,size),
    tidy2 %>% dplyr::mutate(size=ggrrr::gg_label_size(rightBorderWeight)) %>% dplyr::select(x=x1,y=ay0,xend=x1,yend=ay1,size)
  ) %>% dplyr::filter(size>0) %>% dplyr::distinct()

  g = ggplot2::ggplot(tidy2)+
    ggplot2::geom_label(mapping = ggplot2::aes(
      x=xpos,y=ypos,vjust=vjust,hjust=hjust,label=label,
      # TODO: for some massivley obscure reason this does not work.
      # This is despite it ought to work and
      # ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars))) + geom_label(aes(family=c("Times New Roman", "Roboto")[am+1],fontface=c("bold", "italic")[am+1]))
      # Does do what it is supposed to
      family=fontName,fontface=fontFace,
      size=ggrrr::gg_label_size(fontSize)*scale), # Shrink the label to ensure the table fits the max table width
      label.size=0, label.padding = grid::unit(2*scale,"pt")
    )+
    ggplot2::theme(
      line = ggplot2::element_blank(), rect = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "pt"), axis.ticks.length.x = NULL,
      axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL,
      axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL,
      axis.ticks.length.y.right = NULL, legend.box = NULL,
      axis.text = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.y.left = ggplot2::element_blank(),
      axis.text.y.right = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0,0,0,0), units = "pt")
    )+
    ggplot2::coord_fixed(ratio = table_height/table_width,xlim = c(0,1),ylim = c(0,1))+
    ggplot2::geom_segment(data = borders, mapping=ggplot2::aes(x=x,y=y,xend=xend,yend=yend,size=size), inherit.aes = FALSE)+
    ggplot2::scale_size_identity()+
    # scale_discrete_identity(c("family","fontface"))+
    ggplot2::scale_x_continuous(expand = c(0,0))+
    ggplot2::scale_y_continuous(expand = c(0,0))

  attr(g,"target.width")=table_width*scale/72
  class(g) = c("formatted.table",class(g))
  return(g)
}


#' Standard image and paper sizes
#'
#' The width and height of images to fit scientific publication standards.
#'
#' @docType data
#' @name std_size
#' @format A list with width and height in inches
#' @export
std_size = list(
  A4 = list(width=8.25,height=11.75,rot=0),
  A5 = list(width=5+7/8,height=8.25,rot=0),
  full =  list(width=5.9,height=8,rot=0),
  landscape =  list(width=9.75,height=5.9,rot=0),
  half =  list(width=5.9,height=4,rot=0),
  third =  list(width=5.9,height=3,rot=0),
  two_third = list(width=5.9,height=6,rot=0),
  quarter = list(width=5.9,height=2,rot=0),
  quarter_portrait = list(width=3,height=4,rot=0),
  sixth = list(width=3,height=3,rot=0),
  slide = list(width=12,height=6,rot=0)
)

# Outputs ----

#' Save a plot to multiple formats
#'
#' Saves a ggplot object to disk at a set physical size. Allows specific maximum dimensions
#' with an optional target aspect ratio to fit into specific configurations for publication.
#' e.g. a half page plot or a third of a 2 column page. Allows output in pdf for journal
#' publication or png for inclusion in documents, and makes sure that the outputs are
#' near identical.
#'
#' @param filename base of target filename (excluding extension).
#' @param plot a GGplot object or none
#' @param size a standard size see `std_size`
#' @param maxWidth maximum width in inches
#' @param maxHeight maximum height in inches
#' @param aspectRatio defaults to maxWidth/maxHeight
#' @param formats some of png, pdf, Rdata
#'
#' @keywords plot
#' @return the output is an sensible default object that can be displayed given the context it is called in,
#' for example if knitting an RMarkdown document a link to the png file for embedding, if latex
#' a link to the pdf file.
#' @export
#' @examples
#' library(tidyverse)
#' p = ggplot(mtcars, aes(mpg, wt, colour=as.factor(cyl))) + geom_point()
#' p %>% gg_save_as(filename=tempfile(),maxWidth=4,maxHeight=4)
gg_save_as = function(plot,filename = tempfile(),
                      size = std_size$half, maxWidth = size$width, maxHeight = size$height,
                      aspectRatio=maxWidth/maxHeight,
                      formats = c("png","pdf","Rdata")) {


  if ("formatted.table"==class(plot)[[1]]) {
    maxWidth = attr(plot,"target.width")
    # plot comes with an aspect ratio which is expressed as height/width
    # this is geenrally true if the coords_fixed has been used.
    plotAr = tryCatch({plot$coordinates$ratio}, error = function(e) NULL)
    if(!is.null(plotAr)) {
      aspectRatio = 1/plotAr
      if (maxWidth/aspectRatio > maxHeight) maxWidth = maxHeight*aspectRatio
      if (maxHeight*aspectRatio > maxWidth) maxHeight = maxWidth/aspectRatio
    }
  }
  # else just better to let ggplot lay it out

  dir = fs::path_dir(filename)
  if(dir==".") stop("directory not given. filename must be a full path (use here::here function).")
  if(fs::path_ext(filename) %in% c("png","pdf","Rdata")) formats = fs::path_ext(filename)
  if (!dir.exists(dir)) dir.create(dir,recursive = TRUE)
  filename = fs::path_ext_remove(filename)
  withExt = function(extn) {fs::path_ext_set(filename,extn)}

  if ("Rdata" %in% formats) saveRDS(plot, withExt("Rdata"))

  if ("pdf" %in% formats) {
    if (!capabilities()["cairo"] ) {

      ggplot2::ggsave(
        withExt("pdf"),
        plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), bg = "transparent");
      try(
        grDevices::embedFonts(withExt("pdf")),
        silent=TRUE
      );

    } else {

      ggplot2::ggsave(
        withExt("pdf"),
        plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), bg = "transparent",device = cairo_pdf);
      try(
        grDevices::embedFonts(withExt("pdf")),
        silent=TRUE
      );

      # Cairo::CairoPDF(
      #   file = withExt("pdf"),
      #   width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), bg = "transparent")
      # print(plot)
      # dev.off()


    }

  }

  if ("png" %in% formats) {
    op = showtext::showtext_opts(dpi=300)
    if (!capabilities()["cairo"] ) {
      ggplot2::ggsave(
        withExt("png"),
        plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), units="in", dpi=300, bg = "transparent", device = grDevices::png, res=300);
    } else {
      ggplot2::ggsave(
        withExt("png"),
        plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), units="in", dpi=300, bg = "transparent", device = grDevices::png, type="cairo", res=300);
      # Cairo::CairoPNG(
      #   file = withExt("png"),
      #   width = min(maxWidth,maxHeight*aspectRatio)*300, height = min(maxHeight,maxWidth/aspectRatio)*300, dpi=300, bg = "transparent")
      # print(plot)
      # dev.off()
    }
    showtext::showtext_opts(op)
  }

  if (.is_knitting()) {
    hidefigs = getOption("hide.figures",FALSE)
    if (hidefigs) {
      return(knitr::asis_output(paste0("INSERT FIGURE HERE: ",fs::path_file(filename),"\n\n")))
    } else {
      if (.is_html_output() & "png" %in% formats) {
        return(knitr::asis_output(sprintf("<img src='%s'></img>", base64enc::dataURI(file = fs::path_ext_set(filename,"png"), mime = "image/png"))))
      } else {
        return(knitr::include_graphics(path = fs::path_ext_set(filename,formats[1]),auto_pdf = TRUE))
      }
    }
  } else {
    if ("png" %in% formats) {
      if (.is_html_output()) {
        return(knitr::asis_output(sprintf("<img src='%s'></img>", base64enc::dataURI(file = fs::path_ext_set(filename,"png"), mime = "image/png"))))
      } else {
        return(knitr::include_graphics(path = fs::path_ext_set(filename,"png"),auto_pdf = TRUE, dpi=300))
      }
    } else {
      return(plot)
    }
  }
}

# TODO: Show text haas an issue with some UTF8 characters: e.g.
# library(showtext)
# ## Loading Google fonts (https://fonts.google.com/)
# font_add_google("Gochi Hand", "gochi")
# font_add_google("Schoolbell", "bell")
#
# ## Automatically use showtext to render text
# showtext_auto()
# #extrafont::loadfonts()
#
# set.seed(123)
# hist(rnorm(1000), breaks = 30, col = "steelblue", border = "white",
#      main = "", xlab = "", ylab = "")
# title("Histogram of Normal Random Numbers \u00B2 \u2074 \u2081 \u00B1 \u2463", family = "Roboto", cex.main = 2)
# # title(ylab = "Frequency₂", family = "Arial", cex.lab = 2)
# title(ylab = latex2exp::TeX("FiO_2"), family = "Arial", cex.lab = 2)
# text(2, 70, latex2exp::TeX("FiO_2 = 1000 \u2014 \u2074 \u2082 \u00B1"), family = "bell", cex = 2.5)

# These work when using extrafonts, but there are other issues with extrafonts in compatibility with
# certain google ttf I think. extrafonts also only supports pdf.
# showtext is good but has another issue in that the text is embedded not as a font but as a darwing object which makes it
# difficult to edit. Not sure whether this is an issue for journals. If so we may need to revert to extrafonts, or use it as an option
# at least. In this case we can use pdftools to generate a png file from the pdf to get consistency.


## Colour scales ----

.subtype_pal = function(x,...) {
  UseMethod(".subtype_pal",x)
}

# undefined as grey
# scales::show_col(.subtype_pal(scales::viridis_pal,direction=-1,subclasses = c(3,4,2))(12))
# coninuous palette
# scales::show_col(.subtype_pal(scales::hue_pal, h=c(-40,70) ,subclasses = c(3,4,2))(12))
# no undefined
# scales::show_col(.subtype_pal(scales::viridis_pal, h=c(-40,70) ,subclasses = c(3,4,2))(9))
# shorter that specification
# scales::show_col(.subtype_pal(scales::viridis_pal, h=c(-40,70) ,subclasses = c(3,4,2))(6))
.subtype_pal.function = function(x, ..., subclasses, undefined="#606060", lighten=NA) {
  dots = rlang::list2(...)
  dots = dots[names(dots) %in% names(formals(x))]
  pal_fun = rlang::exec(x, !!!dots)
  continuous = isTRUE(is.na(pal_fun(2)))
  return(function(n) {
    majors = if (n > sum(subclasses)) length(subclasses)+1 else min((1:length(subclasses))[cumsum(subclasses)>=n])
    tmp = subclasses[1:(majors-1)]
    minors = c(tmp,n-sum(tmp))
    if (n > sum(subclasses)) {
      # undefined items
      pal_input = if(continuous) seq(0,1,length.out = majors-1) else (majors-1)
      major_pal = c(pal_fun(pal_input),undefined)
    } else {
      # no undefined items
      pal_input = if(continuous) seq(0,1,length.out = majors) else (majors)
      major_pal = pal_fun(pal_input)
    }
    out = purrr::map2(major_pal, minors, function(.x,.y) {
      if(is.na(lighten)) lighten = 1/.y
      fade = 1-(1-lighten)^((1:.y)-1)
      return(colorspace::lighten(.x,fade))
    }) %>% unlist()
    return(out)
  })
}

#scales::show_col(.subtype_pal(c("#FF0000","#00FF00","#0000FF"),subclasses = c(3,4,2))(12))
.subtype_pal.character = function(x, ..., subclasses, undefined="#606060", lighten=NA) {
  if (length(x) != length(subclasses)) stop("palette length and subclass length must be the same")
  major_pal = c(x,undefined)
  return(function(n) {
    majors = if (n > sum(subclasses)) length(subclasses)+1 else min((1:length(subclasses))[cumsum(subclasses)>=n])
    tmp = subclasses[1:(majors-1)]
    minors = c(tmp,n-sum(tmp))
    out = purrr::map2(major_pal, minors, function(.x,.y) {
      if(is.na(lighten)) lighten = 1/.y
      fade = 1-(1-lighten)^((1:.y)-1)
      return(colorspace::lighten(.x,fade))
    }) %>% unlist()
    return(out)
  })
}

#' Discrete fill or colour scale where there is a natural ordered subgrouping
#'
#' If you have a categorical variable defining colour or fill and
#' it has a natural grouping you can use this to have a colour scale involving
#' major colors defining the major groupings, and these are progressively
#' lightened for each of the subcategories.
#'
#' @param .palette the palette for the major groupings, either as a function e.g. 'scales::viridis_pal', or as a manual set of colors e.g. 'c("#FF0000","#00FF00","#0000FF")'. if a function can be either discrete or continuous palette.
#' @param subclasses a vector containing the count of the subcategories, e.g. c(2,3,4) defines 3 major categories and a total of 9 sub-categories
#' @param ... additional options to be passed to the major palette function, e.g. 'option="magma"', or to 'discrete_scale()', e.g. 'alpha=0.5'
#' @param undefined If the number of sub-categories in the data is longer than defined in 'subclasses', the extra categories are assumed to be an set of "other" categories, which will be coloured using this base colour
#' @param lighten The factor by which to lighten the colour at each step of the subgrouping. If left blank this will calculate a fraction based on the number of levels of the subgroup.
#' Otherwise if, e.g. 0.5 the first sub category will be the full saturation, the second 0.5 saturation, the third 0.25 saturation, the fourth 0.125 and so on.
#' @param na.value what colour for NA values.
#' @param aesthetics this is a fill scale by default but can be used for colour by setting this to "color" or both as c("fill","color")
#'
#' @return a ggplot scale
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' # prep some data:
#' data = diamonds %>%
#'   mutate(color_cut = sprintf("%s (%s)",color,cut)) %>%
#'   group_by(color,cut,color_cut) %>%
#'   count() %>%
#'   ungroup() %>%
#'   mutate(color_cut = ordered(color_cut))
#'
#' # work out the number of subgroups for each group:
#' subgroups = data %>%
#'   select(color,cut) %>%
#'   distinct %>%
#'   group_by(color) %>%
#'   count() %>%
#'   pull(n)
#'
#' # plot as a horizontal stacked bar chart using color brewer as the main
#' # colour axis. N.b. having enough different colours here is important
#' ggplot(data, aes(y=1,x=n, fill=color_cut, color=color_cut))+
#'   geom_bar(stat="identity",orientation = "y")+
#'   ggrrr::scale_fill_subtype(.palette = scales::brewer_pal,
#'     palette="Accent", subclasses = subgroups)+
#'   ggrrr::scale_colour_subtype(subclasses=subgroups)+
#'   ggrrr::gg_hide_Y_axis()+
#'   ggrrr::gg_narrow()
scale_fill_subtype = function (.palette, subclasses, ..., undefined="#606060", lighten=NA,  na.value = "grey50", aesthetics = "fill") {
  dots = rlang::list2(...)
  discrete_scale_opts = dots[names(dots) %in% names(formals(ggplot2::discrete_scale)) & !names(dots) %in% c("palette","scale_name")]
  p = .subtype_pal(.palette, ..., subclasses=subclasses, undefined = undefined, lighten = lighten)
  discrete_scale_opts = c(aesthetics=aesthetics, scale_name="subtype", palette=p, na.value = na.value, discrete_scale_opts)
  return(rlang::exec(discrete_scale, !!!discrete_scale_opts))
}


#' A discrete colour scale for dividing where there is a natural ordered subgrouping into groups and subgroups
#'
#' This is intended to combine with 'scale_fill_subtype' when we want to divide major groupings differently to minor groups
#'
#' @param subclasses a vector containing the count of the subcategories, e.g. c(2,3,4) defines 3 major categories and a total of 9 sub-categories
#' @param class_colour  the colour for major group divisions
#' @param subclass_colour the colour for sub group divisions
#' @param na.value missing value colour
#' @param aesthetics this only really makes sense for color scales.
#' @param ... passed on to discrete_scale()
#'
#' @return a ggplot scale
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' # prep some data:
#' data = diamonds %>%
#'   mutate(color_cut = sprintf("%s (%s)",color,cut)) %>%
#'   group_by(color,cut,color_cut) %>%
#'   count() %>%
#'   ungroup() %>%
#'   mutate(color_cut = ordered(color_cut))
#'
#' # work out the number of subgroups for each group:
#' subgroups = data %>%
#'   select(color,cut) %>%
#'   distinct %>%
#'   group_by(color) %>%
#'   count() %>%
#'   pull(n)
#'
#' # plot as a horizontal stacked bar chart using color brewer as the main
#' # colour axis. N.b. having enough different colours here is important
#' ggplot(data, aes(y=1,x=n, fill=color_cut, color=color_cut))+
#'   geom_bar(stat="identity",orientation = "y")+
#'   ggrrr::scale_fill_subtype(.palette = scales::brewer_pal,
#'     palette="Accent", subclasses = subgroups)+
#'   ggrrr::scale_colour_subtype(subclasses=subgroups)+
#'   ggrrr::gg_hide_Y_axis()+
#'   ggrrr::gg_narrow()
scale_colour_subtype = function( subclasses, class_colour = "black", subclass_colour = "grey50",  na.value = "grey50", aesthetics = "color", ...) {
  discrete_scale(
    aesthetics=aesthetics,
    scale_name="subtype",
    palette=function(n) {return(ifelse( c(TRUE,2:n %in% (cumsum(subclasses)+1)), class_colour, subclass_colour))},
    na.value = na.value,
    ...
  )
}
