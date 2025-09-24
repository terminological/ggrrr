## ggplot defaults ----

#' @inherit .gg_tiny_theme
#' @export
#' @concept ggplot
#'
#' @examples
#' if (interactive()) {
#'   ggplot2::ggplot(ggplot2::diamonds,
#'     ggplot2::aes(x=carat,y=price,color=color))+
#'     ggplot2::geom_point()+
#'     gg_tiny_theme()
#' }
gg_tiny_theme = .gg_tiny_theme

#' @inherit .gg_set_size_defaults
#' @export
#' @concept ggplot
#'
#' @examples
#' library(tidyverse)
#' gg_set_size_defaults(lineSize = 0.25)
gg_set_size_defaults = .gg_set_size_defaults

#' @inherit .gg_pedantic
#' @export
#' @concept ggplot
gg_pedantic = .gg_pedantic

#' @inherit .gg_label_size
#' @export
#' @concept ggplot
gg_label_size = .gg_label_size

#' @inherit .gg_hide_X_axis
#' @export
#' @concept ggplot
gg_hide_X_axis = .gg_hide_X_axis

#' @inherit .gg_hide_Y_axis
#' @export
#' @concept ggplot
gg_hide_Y_axis = .gg_hide_Y_axis

#' @inherit .gg_hide_legend
#' @export
#' @concept ggplot
gg_hide_legend = .gg_hide_legend

#' @inherit .gg_set_X_angle
#' @export
#' @concept ggplot
gg_set_X_angle = .gg_set_X_angle

#' @inherit .gg_narrow
#' @export
#' @concept ggplot
gg_narrow = .gg_narrow

#' @inherit .gg_resize_legend
#' @export
#' @concept ggplot
gg_resize_legend = .gg_resize_legend


#' @inherit .gg_layer
#' @concept ggplot
#' @export
#' @examples
#' # top level function contains `...` and `mapping` extensions points:
#' myPlot = function(data, formula, ..., mapping = .gg_check_for_aes(...)) {
#'     xCol = rlang::f_lhs(formula)
#'     yCol = rlang::f_rhs(formula)
#'     ggplot2::ggplot(data)+
#'     gg_layer(
#'       ggplot2::GeomPoint,
#'       data = data,
#'       mapping=ggplot2::aes(x=!!xCol, y=!!yCol, !!!mapping),
#'       ...,
#'       .default = list(size=10)
#'      )
#' }
#' myPlot(iris, Sepal.Length~Sepal.Width, mapping = ggplot2::aes(colour=Species))
#' myPlot(iris, Sepal.Length~Petal.Length, mapping = ggplot2::aes(colour=Species), shape="+", size=5)
#' myPlot(mtcars, mpg~wt, mapping = ggplot2::aes(colour=as.factor(cyl), size=hp))
gg_layer = .gg_layer

# drawDetails.watermark <<- function(x, rot = 45, ...){
# does this need to be global to work or is package scope ok?

#' Internal function for drawing watermark on ggplots
#'
#' @inheritParams grid::drawDetails
#'
#' @return a grid object
#' @importFrom grid drawDetails
#' @export
#' @concept ggplot
drawDetails.watermark = function(x, recording) {
  rot = 45
  alpha = 0.1
  cex <- min(
    grid::convertX(grid::unit(0.9, "npc"), "mm", valueOnly = TRUE) /
      grid::convertUnit(
        grid::unit(1, "grobwidth", grid::textGrob(x$lab, rot = rot)),
        "mm",
        valueOnly = TRUE
      ),
    grid::convertY(grid::unit(0.9, "npc"), "mm", valueOnly = TRUE) /
      grid::convertUnit(
        grid::unit(1, "grobheight", grid::textGrob(x$lab, rot = rot)),
        "mm",
        valueOnly = TRUE
      )
  )
  return(grid::grid.text(
    x$lab,
    rot = rot,
    gp = grid::gpar(cex = cex, col = "black", fontface = "bold", alpha = alpha)
  ))
}

#' Add in a watermark to plots
#'
#' @param disable - global option to disable all watermarks options("ggrrr.disable.watermark"=TRUE)
#' @param lab the watermark label (DRAFT)
#'
#' @return a watermark layer
#' @export
#' @concept ggplot
gg_watermark = function(
  lab = "DRAFT",
  disable = getOption("ggrrr.disable.watermark", default = FALSE)
) {
  if (!disable) {
    grb = grid::grob(lab = lab, cl = "watermark")
    return(ggplot2::annotation_custom(
      grb,
      xmin = -Inf,
      ymin = -Inf,
      xmax = Inf,
      ymax = Inf
    ))
  } else {
    return(ggplot2::geom_blank())
  }
}

## Custom scales ----

#' @inherit .gg_breaks_log1p
#' @export
#' @concept ggplot
#'
#' @examples
#' library(tidyverse)
#' ggplot2::ggplot(diamonds, ggplot2::aes(x=price))+
#'   ggplot2::geom_density()+
#'   ggplot2::scale_x_continuous(trans="log1p", breaks=ggrrr::breaks_log1p())
breaks_log1p = .gg_breaks_log1p

#' percentage scale
#'
#' `r lifecycle::badge(stage = "deprecated")`
#' @description display a 0-1 scale as 0-100%
#' @return A scales object
#' @export
#' @concept ggplot
percent_trans <- function() {
  lifecycle::deprecate_stop(
    "0.0.0.9024",
    "percent_trans()",
    "scale_y_percent()"
  )
  #
  #   trans <- function(x) return(x*100)
  #   inv <- function(y) return(y/100)
  #
  #   scales::trans_new("percent",
  #             transform = trans,
  #             inverse = inv,
  #             breaks = functional::Compose(trans, scales::extended_breaks(), inv),
  #             format = scales::label_scientific(digits = 2)
  #   )
}

#' @inherit .gg_transform_logit
#' @examples
#'
#' tibble::tibble(pvalue = c(0.001, 0.05, 0.1), fold_change = 1:3) %>%
#'   ggplot2::ggplot(ggplot2::aes(fold_change , pvalue)) +
#'   ggplot2::geom_point() +
#'   ggplot2::scale_y_continuous(trans = "logit")
#'
#' @export
#' @concept ggplot
logit_trans = .gg_transform_logit


#' @inherit .gg_scale_x_log1p
#' @export
#' @concept ggplot
scale_x_log1p = .gg_scale_x_log1p

#' @inherit .gg_scale_y_log1p
#' @export
#' @concept ggplot
scale_y_log1p = .gg_scale_y_log1p

#' @inherit .gg_scale_x_logit
#' @export
#' @concept ggplot
scale_x_logit = .gg_scale_x_logit

#' @inherit .gg_scale_y_logit
#' @export
#' @concept ggplot
scale_y_logit = .gg_scale_y_logit

#' @inherit .gg_scale_x_percent
#' @export
#' @concept ggplot
scale_x_percent = .gg_scale_x_percent

#' @inherit .gg_scale_y_percent
#' @export
#' @concept ggplot
scale_y_percent = .gg_scale_y_percent

#' @inherit .gg_scale_consistent
#' @export
#' @concept ggplot
scale_fill_consistent = function(
  plot,
  original_aesthetic = "fill",
  target_aesthetic = "fill",
  ...
) {
  .gg_scale_consistent(plot, original_aesthetic, target_aesthetic, ...)
}

#' @inherit .gg_scale_consistent
#' @export
#' @concept ggplot
scale_colour_consistent = function(
  plot,
  original_aesthetic = "colour",
  target_aesthetic = "colour",
  ...
) {
  .gg_scale_consistent(plot, original_aesthetic, target_aesthetic, ...)
}

## GGplot tables ----

#' A simple table as a ggplot patchwork object, no customisation allowed
#'
#' @keywords graph layout
#' @export
#' @concept ggplot
#' @param df the dataframe with the table data. Column names will become headings
#' @param pts text size in points
#' @param font the font family
#' @param unwrapped - set this to TRUE if you want to add to a patchwork and use patchwork::wrap_plots(p,list(table))
#' @return A gtable object (i.e. a grob) optionally wrapped as a patchwork plot.
#' @examples
#' if (FALSE) {
#'   gg_simple_table(tibble::tibble(x=c(1,2,3),y=c(5,4,3)),pts=10)
#' }
gg_simple_table = function(df, pts = 8, font = "sans", unwrapped = FALSE) {
  font = ggrrr::check_font(font)
  p = suppressWarnings(suppressMessages({
    ttheme = gridExtra::ttheme_minimal(
      base_size = pts,
      base_colour = "black",
      base_family = font,
      parse = FALSE,
      padding = grid::unit(c(4, 1.5), "mm"),
      core = list(
        fg_params = list(hjust = 0, x = 0.1),
        bg_params = list(fill = "#FFFFFF", alpha = 1, col = NA)
      ),
      colhead = list(
        fg_params = list(hjust = 0, x = 0.1),
        bg_params = list(fill = "#FFFFFF", alpha = 1, col = NA)
      )
    )
    g = gridExtra::tableGrob(d = df, rows = NULL, theme = ttheme)
    g <- gtable::gtable_add_grob(
      g,
      grobs = grid::segmentsGrob(
        # line across the bottom
        x0 = grid::unit(0, "npc"),
        y0 = grid::unit(0, "npc"),
        x1 = grid::unit(1, "npc"),
        y1 = grid::unit(0, "npc"),
        gp = grid::gpar(lwd = 2.0)
      ),
      t = nrow(g),
      l = 1,
      r = ncol(g)
    )
    g <- gtable::gtable_add_grob(
      g,
      grobs = grid::grobTree(
        grid::segmentsGrob(
          # line across the top
          x0 = grid::unit(0, "npc"),
          y0 = grid::unit(1, "npc"),
          x1 = grid::unit(1, "npc"),
          y1 = grid::unit(1, "npc"),
          gp = grid::gpar(lwd = 2.0)
        ),
        grid::segmentsGrob(
          # line across the bottom
          x0 = grid::unit(0, "npc"),
          y0 = grid::unit(0, "npc"),
          x1 = grid::unit(1, "npc"),
          y1 = grid::unit(0, "npc"),
          gp = grid::gpar(lwd = 1.0)
        )
      ),
      t = 1,
      l = 1,
      r = ncol(g)
    )
    #if(!unwrapped) return(patchwork::wrap_ggplot_grob(g))
    if (!unwrapped) {
      return(patchwork::wrap_elements(g))
    }
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
#' @concept ggplot
gg_formatted_table = function(
  longFormatTable,
  colWidths = NULL,
  tableWidthInches = 5.9,
  font = "Roboto",
  ...
) {
  hux = colSpan = rowSpan = y1 = y0 = x0 = x1 = ay0 = ay1 = label = fontName = fontSize = textwidth = width =
    textheight = height = topBorderWeight = size = bottomBorderWeight = leftBorderWeight = rightBorderWeight =
      xpos = ypos = vjust = hjust = fontFace = x = y = xend = yend = NULL # remove global binding note

  lines = ar = NULL

  font = ggrrr::check_font(font)
  if (!("long_format_table" %in% class(longFormatTable))) {
    longFormatTable = ggrrr::as.long_format_table(
      longFormatTable,
      colWidths = colWidths,
      fontName = font,
      ...
    )
  }
  if (is.null(colWidths)) {
    colWidths = attr(longFormatTable, "colWidths")
  }
  colWidths = colWidths / sum(colWidths)
  if (any(is.na(colWidths))) {
    colWidths = rep(1 / ncol(hux), ncol(hux))
  }

  ncol = max(longFormatTable$col + longFormatTable$colSpan - 1)
  nrow = max(longFormatTable$row + longFormatTable$rowSpan - 1)

  # NB: ignores any kind of wrapping.
  rowHeights = rep(1 / nrow, nrow)

  tidy2 = longFormatTable %>%
    dplyr::mutate(
      colMax = col + colSpan - 1,
      rowMax = row + rowSpan - 1,
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(
        col = 1:(length(colWidths) + 1),
        x0 = c(0, cumsum(colWidths))
      ),
      by = "col"
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(
        colMax = 0:(length(colWidths)),
        x1 = c(0, cumsum(colWidths))
      ),
      by = "colMax"
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(
        row = 1:(length(rowHeights) + 1),
        y0 = c(0, cumsum(rowHeights))
      ),
      by = "row"
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(
        rowMax = 0:(length(rowHeights)),
        y1 = c(0, cumsum(rowHeights))
      ),
      by = "rowMax"
    ) %>%
    dplyr::mutate(
      # flip y axis
      ay0 = 1 - y1,
      ay1 = 1 - y0,
      cx = (x0 + x1) / 2,
      cy = (ay0 + ay1) / 2
    ) %>%
    dplyr::mutate(
      xpos = dplyr::case_when(
        alignment == "START" ~ x0,
        alignment == "CENTER" ~ cx,
        alignment == "RIGHT" ~ x1,
        TRUE ~ cx
      ),
      hjust = dplyr::case_when(
        alignment == "START" ~ 0,
        alignment == "CENTER" ~ 0.5,
        alignment == "RIGHT" ~ 1,
        TRUE ~ 0.5
      ),
      ypos = dplyr::case_when(
        valignment == "TOP" ~ ay1,
        valignment == "MIDDLE" ~ cy,
        valignment == "BOTTOM" ~ ay0,
        TRUE ~ cy
      ),
      vjust = dplyr::case_when(
        valignment == "TOP" ~ 1,
        valignment == "MIDDLE" ~ 0.5,
        valignment == "BOTTOM" ~ 0,
        TRUE ~ 0.5
      ),
    )

  table_width = tidy2 %>%
    dplyr::mutate(
      textwidth = .get_text_cms(label, font = fontName, font_size = fontSize) /
        2.54 *
        72 +
        2,
    ) %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(
      width = max(textwidth * 1 / (x1 - x0)) #* (.is_header_col == FALSE))
    ) %>%
    dplyr::pull(width) %>%
    max()

  if (table_width > tableWidthInches * 72) {
    scale = tableWidthInches * 72 / table_width # for scaling text to fit desired width
  } else {
    scale = 1
  }

  table_height = tidy2 %>%
    dplyr::mutate(
      textheight = fontSize + 2,
    ) %>%
    dplyr::group_by(col) %>%
    dplyr::summarise(
      height = sum(textheight)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::pull(height) %>%
    max()

  borders = dplyr::bind_rows(
    tidy2 %>%
      dplyr::mutate(size = .gg_label_size(topBorderWeight)) %>%
      dplyr::select(x = x0, y = ay1, xend = x1, yend = ay1, size),
    tidy2 %>%
      dplyr::mutate(size = .gg_label_size(bottomBorderWeight)) %>%
      dplyr::select(x = x0, y = ay0, xend = x1, yend = ay0, size),
    tidy2 %>%
      dplyr::mutate(size = .gg_label_size(leftBorderWeight)) %>%
      dplyr::select(x = x0, y = ay0, xend = x0, yend = ay1, size),
    tidy2 %>%
      dplyr::mutate(size = .gg_label_size(rightBorderWeight)) %>%
      dplyr::select(x = x1, y = ay0, xend = x1, yend = ay1, size)
  ) %>%
    dplyr::filter(size > 0) %>%
    dplyr::distinct()

  g = ggplot2::ggplot(tidy2) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        x = xpos,
        y = ypos,
        vjust = vjust,
        hjust = hjust,
        label = label,
        # TODO: for some massivley obscure reason this does not work.
        # This is despite it ought to work and
        # ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg, label=rownames(mtcars))) + ggplot2::geom_label(ggplot2::aes(family=c("Times New Roman", "Roboto")[am+1],fontface=c("bold", "italic")[am+1]))
        # Does do what it is supposed to
        family = fontName,
        fontface = fontFace,
        size = .gg_label_size(fontSize) * scale
      ), # Shrink the label to ensure the table fits the max table width
      label.size = 0,
      label.padding = grid::unit(2 * scale, "pt")
    ) +
    ggplot2::theme(
      line = ggplot2::element_blank(),
      rect = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "pt"),
      axis.ticks.length.x = NULL,
      axis.ticks.length.x.top = NULL,
      axis.ticks.length.x.bottom = NULL,
      axis.ticks.length.y = NULL,
      axis.ticks.length.y.left = NULL,
      axis.ticks.length.y.right = NULL,
      legend.box = NULL,
      axis.text = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.y.left = ggplot2::element_blank(),
      axis.text.y.right = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0, 0, 0, 0), units = "pt")
    ) +
    ggplot2::coord_fixed(
      ratio = table_height / table_width,
      xlim = c(0, 1),
      ylim = c(0, 1)
    ) +
    ggplot2::geom_segment(
      data = borders,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        size = size
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_size_identity() +
    # ggplot2::scale_discrete_identity(c("family","fontface"))+
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0))

  attr(g, "target.width") = table_width * scale / 72
  class(g) = c("formatted.table", class(g))
  return(g)
}


## Colour scales ----
#TODO: this needs a little rethink to maybe extend a scale properly

.subtype_pal = function(x, ...) {
  UseMethod(".subtype_pal", x)
}

# undefined as grey
# scales::show_col(.subtype_pal(scales::viridis_pal,direction=-1,subclasses = c(3,4,2))(12))
# coninuous palette
# scales::show_col(.subtype_pal(scales::hue_pal, h=c(-40,70) ,subclasses = c(3,4,2))(12))
# no undefined
# scales::show_col(.subtype_pal(scales::viridis_pal, h=c(-40,70) ,subclasses = c(3,4,2))(9))
# shorter that specification
# scales::show_col(.subtype_pal(scales::viridis_pal, h=c(-40,70) ,subclasses = c(3,4,2))(6))
.subtype_pal.function = function(
  x,
  ...,
  subclasses,
  undefined = "#606060",
  lighten = NA
) {
  dots = rlang::list2(...)
  dots = dots[names(dots) %in% names(formals(x))]
  pal_fun = rlang::exec(x, !!!dots)
  continuous = isTRUE(is.na(pal_fun(2)))
  return(function(n) {
    majors = if (n > sum(subclasses)) {
      length(subclasses) + 1
    } else {
      min((1:length(subclasses))[cumsum(subclasses) >= n])
    }
    tmp = subclasses[1:(majors - 1)]
    minors = c(tmp, n - sum(tmp))
    if (n > sum(subclasses)) {
      # undefined items
      pal_input = if (continuous) {
        seq(0, 1, length.out = majors - 1)
      } else {
        (majors - 1)
      }
      major_pal = c(pal_fun(pal_input), undefined)
    } else {
      # no undefined items
      pal_input = if (continuous) seq(0, 1, length.out = majors) else (majors)
      major_pal = pal_fun(pal_input)
    }
    out = purrr::map2(major_pal, minors, function(.x, .y) {
      if (is.na(lighten)) {
        lighten = 1 / .y
      }
      fade = 1 - (1 - lighten)^((1:.y) - 1)
      return(colorspace::lighten(.x, fade))
    }) %>%
      unlist()
    return(out)
  })
}

#scales::show_col(.subtype_pal(c("#FF0000","#00FF00","#0000FF"),subclasses = c(3,4,2))(12))
.subtype_pal.character = function(
  x,
  ...,
  subclasses,
  undefined = "#606060",
  lighten = NA
) {
  if (length(x) != length(subclasses)) {
    stop("palette length and subclass length must be the same")
  }
  major_pal = c(x, undefined)
  return(function(n) {
    majors = if (n > sum(subclasses)) {
      length(subclasses) + 1
    } else {
      min((1:length(subclasses))[cumsum(subclasses) >= n])
    }
    tmp = subclasses[1:(majors - 1)]
    minors = c(tmp, n - sum(tmp))
    out = purrr::map2(major_pal, minors, function(.x, .y) {
      if (is.na(lighten)) {
        lighten = 1 / .y
      }
      fade = 1 - (1 - lighten)^((1:.y) - 1)
      return(colorspace::lighten(.x, fade))
    }) %>%
      unlist()
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
#' @param ... additional options to be passed to the major palette function, e.g. 'option="magma"', or to 'ggplot2::discrete_scale()', e.g. 'alpha=0.5'
#' @param undefined If the number of sub-categories in the data is longer than defined in 'subclasses', the extra categories are assumed to be an set of "other" categories, which will be coloured using this base colour
#' @param lighten The factor by which to lighten the colour at each step of the subgrouping. If left blank this will calculate a fraction based on the number of levels of the subgroup.
#' Otherwise if, e.g. 0.5 the first sub category will be the full saturation, the second 0.5 saturation, the third 0.25 saturation, the fourth 0.125 and so on.
#' @param na.value what colour for NA values.
#' @param aesthetics this is a fill scale by default but can be used for colour by setting this to "color" or both as c("fill","color")
#'
#' @return a ggplot scale
#' @export
#' @concept ggplot
#'
#' @examples
#' library(tidyverse)
#'
#' # prep some data:
#' data = ggplot2::diamonds %>%
#'   dplyr::mutate(color_cut = sprintf("%s (%s)",color,cut)) %>%
#'   dplyr::group_by(color,cut,color_cut) %>%
#'   dplyr::count() %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(color_cut = ordered(color_cut))
#'
#' # work out the number of subgroups for each group:
#' subgroups = data %>%
#'   dplyr::select(color,cut) %>%
#'   dplyr::distinct() %>%
#'   dplyr::group_by(color) %>%
#'   dplyr::count() %>%
#'   dplyr::pull(n)
#'
#' # plot as a horizontal stacked bar chart using color brewer as the main
#' # colour axis. N.b. having enough different colours here is important
#' ggplot2::ggplot(data, ggplot2::aes(y=1,x=n, fill=color_cut, color=color_cut))+
#'   ggplot2::geom_bar(stat="identity",orientation = "y")+
#'   scale_fill_subtype(.palette = scales::brewer_pal,
#'     palette="Accent", subclasses = subgroups)+
#'   scale_colour_subtype(subclasses=subgroups)+
#'   gg_hide_Y_axis()+
#'   gg_narrow()
scale_fill_subtype = function(
  .palette,
  subclasses,
  ...,
  undefined = "#606060",
  lighten = NA,
  na.value = "grey50",
  aesthetics = "fill"
) {
  dots = rlang::list2(...)
  discrete_scale_opts = dots[
    names(dots) %in%
      names(formals(ggplot2::discrete_scale)) &
      !names(dots) %in% c("palette", "scale_name")
  ]
  p = .subtype_pal(
    .palette,
    ...,
    subclasses = subclasses,
    undefined = undefined,
    lighten = lighten
  )
  discrete_scale_opts = c(
    list(
      aesthetics = aesthetics,
      scale_name = "subtype",
      palette = p,
      na.value = na.value
    ),
    discrete_scale_opts
  )
  return(rlang::exec(ggplot2::discrete_scale, !!!discrete_scale_opts))
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
#' @param ... passed on to ggplot2::discrete_scale()
#'
#' @return a ggplot scale
#' @export
#' @concept ggplot
#'
#' @examples
#' library(tidyverse)
#'
#' # prep some data:
#' data = ggplot2::diamonds %>%
#'   dplyr::mutate(color_cut = sprintf("%s (%s)",color,cut)) %>%
#'   dplyr::group_by(color,cut,color_cut) %>%
#'   dplyr::count() %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(color_cut = ordered(color_cut))
#'
#' # work out the number of subgroups for each group:
#' subgroups = data %>%
#'   dplyr::select(color,cut) %>%
#'   dplyr::distinct() %>%
#'   dplyr::group_by(color) %>%
#'   dplyr::count() %>%
#'   dplyr::pull(n)
#'
#' # plot as a horizontal stacked bar chart using color brewer as the main
#' # colour axis. N.b. having enough different colours here is important
#' ggplot2::ggplot(data, ggplot2::aes(y=1,x=n, fill=color_cut, color=color_cut))+
#'   ggplot2::geom_bar(stat="identity",orientation = "y")+
#'   ggrrr::scale_fill_subtype(.palette = scales::brewer_pal,
#'     palette="Accent", subclasses = subgroups)+
#'   ggrrr::scale_colour_subtype(subclasses=subgroups)+
#'   ggrrr::gg_hide_Y_axis()+
#'   ggrrr::gg_narrow()
scale_colour_subtype = function(
  subclasses,
  class_colour = "black",
  subclass_colour = "grey50",
  na.value = "grey50",
  aesthetics = "color",
  ...
) {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    scale_name = "subtype",
    palette = function(n) {
      return(ifelse(
        c(TRUE, 2:n %in% (cumsum(subclasses) + 1)),
        class_colour,
        subclass_colour
      ))
    },
    na.value = na.value,
    ...
  )
}
