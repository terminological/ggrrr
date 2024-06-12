# ---
# repo: terminological/ggrrr
# file: standalone-ggplot-utils.R
# last-updated: 2024-06-06
# license: https://unlicense.org
# imports:
#    - dplyr
#    - ggplot2
#    - ggrrr
#    - knitr
#    - purrr
#    - ragg
#    - rlang
#    - scales
#    - stats
#    - stringr
#    - systemfonts
#    - tibble
# ---


# Axes, legends etc ----

#' Convert a label size from points to ggplot units
#'
#' Labels like geom_text are in a random unit size which is only mysteriously connected to the size of text on axes
#'
#' @keywords internal
#' @concept ggplot
#' @param pts label size in points
#' @return a ggplot size aesthetic for labels
.gg_label_size = function(pts) {
  return (pts/ggplot2::.pt) #/(96/72))
}

#' Hide the x axis of a plot
#'
#' @keywords internal
#' @concept ggplot
#' @return a theme
.gg_hide_X_axis = function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.x.bottom = ggplot2::element_blank(),
    axis.text.x.top = ggplot2::element_blank()
  );
}

#' Hide the y axis of a plot
#'
#' @keywords internal
#' @concept ggplot
#' @return a theme
.gg_hide_Y_axis = function() {
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.text.y.left = ggplot2::element_blank(),
    axis.text.y.right = ggplot2::element_blank()
  );
}

#' Hide the legend of a plot
#'
#' @keywords internal
#' @concept ggplot
#' @return a theme
.gg_hide_legend = function() {
  ggplot2::theme(legend.position = "none")
}

#' Set the angle of the x axis labels of a plot
#'
#' Also sets horizontal and vertical alignment correctly, and does top and
#' bottom axes.
#'
#' @param ang the angle for the x labels
#'
#' @keywords internal
#' @concept ggplot
#' @return a theme
.gg_set_X_angle = function(ang = 60) {
  hj = dplyr::case_when(
    ang == 0 ~ 0.5,
    TRUE ~ 1
  )
  vj = dplyr::case_when(
    ang > 90 ~ 0,
    ang == 90 ~ 0.5,
    TRUE ~ 1
  )
  ggplot2::theme(
    axis.text.x.top = ggplot2::element_text(angle = ang, hjust = 1-hj, vjust = 1-vj),
    axis.text.x.bottom = ggplot2::element_text(angle = ang, hjust = hj, vjust = vj)
  )
}

#' Make a plot narrower
#'
#' @param ang the angle for the x labels
#'
#' @keywords internal
#' @concept ggplot
#' @return a theme
.gg_narrow = function(ang = 90) {
  list(
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box="vertical",
      legend.justification = "center"
    ),
    .gg_set_X_angle(ang = ang)
  )
}

#' Make the legend smaller
#'
#' @param pointSize - the ggplot size of lines or points
#' @param textSize - the size in pts of the text
#' @param spaceLegend - degree of spacing between items in the scale (defines overall size)
#'
#' @keywords internal
#' @concept ggplot
#' @return a theme
.gg_resize_legend = function(pointSize = 0.75, textSize = 6, spaceLegend = 0.75) {
  return(list(
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = pointSize)),
                    color = ggplot2::guide_legend(override.aes = list(size = pointSize))),
    ggplot2::theme(legend.title = ggplot2::element_text(size = textSize),
                   legend.text  = ggplot2::element_text(size = textSize),
                   legend.key.size = ggplot2::unit(spaceLegend, "lines"),
                   legend.box.margin = ggplot2::margin())
  ))
}


# Formatting and themes ----

#' A space saving ggplot theme
#'
#' A ggplot theme with minimal fluff and with the defaults set small.
#'
#' @param baseSize the size of the base font.
#' @param font the font family name
#'
#' @return a ggplot theme
#' @keywords internal
#' @concept ggplot
#'
#' @examples
#' try({
#'   ggplot2::ggplot(ggplot2::diamonds,
#'     ggplot2::aes(x=carat,y=price,color=color))+
#'     ggplot2::geom_point()+
#'     .gg_tiny_theme()
#' })
.gg_tiny_theme = function(baseSize = 8, font = "Roboto") {
  font = .gg_substitute_fonts(font)
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
      # plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
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
#' @param lineSize the width of lines
#' @param fontSizePts the size of labels and other on plot text in pts.
#' @param font the font family name
#' @param size the size of points (the default size aesthetic)
#'
#' @return nothing
#' @keywords internal
#' @concept ggplot
#'
#' @examples
#' gg_set_size_defaults(lineSize = 0.25)
.gg_set_size_defaults = function(lineSize = 0.5, fontSizePts = 4+lineSize*8, font="Roboto", size=lineSize*2) {
  font = .gg_substitute_fonts(font)
  # get all ggplot2 geoms
  try(attachNamespace("ggplot2"),silent = TRUE)
  geoms = ls(pattern = '^geom_', envir = as.environment('package:ggplot2')) %>% stringr::str_remove("geom_")
  for(geom in geoms) {
    try({
      if (geom %in% c("label","text","sf_label","sf_text")) {
        ggplot2::update_geom_defaults(geom, list(size = ggrrr::gg_label_size(fontSizePts), family=font))
      } else {
        ggplot2::update_geom_defaults(geom, list(size = size, linewidth=lineSize))
      }
    },silent = TRUE)
  }
  ggplot2::update_geom_defaults(ggplot2::GeomPoint, list(stroke=0))
  invisible(NULL)
}


#' An opinionated set of defaults for plots
#'
#' This is a set of styles with a focus on making plots compact, and minimally
#' fussy, and ensuring fonts are consistent between axes and labels. It sets
#' default sizes for line widths and point sizes. It also
#' switched the default png renderer in knitr to `ragg::ragg_png` to allow for
#' modern font support.
#'
#' @param lineSize the default line and shape size in ggplot units
#' @param fontSize the base font size
#' @param font the default font name.
#' @param size the size of points (the default size aesthetic)
#' @param ... passed to `ggplot2::theme`
#'
#' @return nothing
#' @keywords internal
#' @concept ggplot
.gg_pedantic = function(lineSize = 0.25, fontSize = 8, font="Roboto", size=lineSize*2, ...) {

  ggplot2::theme_set(.gg_tiny_theme(fontSize, font)+ggplot2::theme(...))
  .gg_set_size_defaults(lineSize,fontSize*0.75,font,size)

  if (is.null(knitr::opts_chunk$get("dev")))
    knitr::opts_chunk$set(dev = "ragg_png")

}

# Fonts ----

#' Pick a locally installed font family that matches requested
#'
#' @param family the font family requested
#'
#' @return a mapping as a named list of font families that are present on the
#'   system (names are the requested font family)
#' @keywords internal
#' @concept ggplot
#'
#' @examples
#' try({
#' .gg_substitute_fonts(c("Roboto","Arial","Kings","Unmatched"))
#' })
.gg_substitute_fonts = function(family) {

  weight = path = NULL

  sys_fonts_list = dplyr::bind_rows(
    systemfonts::registry_fonts() %>% dplyr::mutate(weight = as.character(weight)),
    systemfonts::system_fonts() %>% dplyr::mutate(weight = as.character(weight))
  ) %>% dplyr::select(
    path, sub=family
  ) %>% dplyr::distinct()

  tmp = tibble::tibble(
    family = family,
    path = systemfonts::match_fonts(family)$path
  ) %>% dplyr::inner_join(
    sys_fonts_list, by="path"
  ) %>%
    dplyr::select(family, sub) %>%
    dplyr::distinct()

  if (any(tmp$family != tmp$sub)) {
    missing = tmp %>% filter(family != sub) %>% pull(family) %>% paste0(collapse = ", ")
    rlang::warn(
      sprintf("The requested font(s): [%s], are not present on the system. Alternatives will be used.", missing),
      .frequency = "once",
      .frequency_id = missing
    )
  }
  names(tmp$sub) = tmp$family
  return(tmp$sub)
}

#' Check if any fonts listed are missing
#'
#' @param family the font family
#'
#' @return `TRUE` if missing fonts detected
#' @keywords internal
#' @concept ggplot
#'
#' @examples
#' try({
#'   .gg_fonts_missing("Arial")
#'   .gg_fonts_missing(c("Roboto","Kings","ASDASDAS"))
#' })
.gg_fonts_missing = function(family) {
  return(suppressWarnings({
    tmp = .gg_substitute_fonts(family)
    !all(unname(tmp[family]) == family)
  }))
}

#' @noRd
#' @examples
#' plot = ggplot2::ggplot(ggplot2::diamonds, ggplot2::aes(x=carat,y=price,color = color))+
#'   ggplot2::theme_minimal(base_family=check_font("Roboto"))+
#'   ggplot2::geom_point()+
#'   ggplot2::annotate("label",x=2,y=10000,label="Hello \u2014 world", family=check_font("Kings"))+
#'   ggplot2::labs(tag = "A")+
#'   ggplot2::xlab("Carat\u2082")+
#'   ggplot2::ylab("price\u2265")
#'
#' .gg_used_fonts(plot)
.gg_used_fonts = function(plot) {
  theme = purrr::possibly(~.x$theme$text$family)(plot)
  geoms = plot$layers %>% purrr::map(purrr::possibly(~ .x$computed_geom_params$family)) %>% purrr::list_c()
  return(c(theme,geoms))
}




# Transforms and breaks ----

#' A scales breaks generator for log1p scales
#'
#' @param n the number of breaks
#' @param base the base for the breaks
#'
#' @return a function for ggplot scale breaks
#' @keywords internal
#' @concept ggplot
#'
#' @examples
#' try({
#' ggplot2::ggplot(ggplot2::diamonds, ggplot2::aes(x=price))+
#'   ggplot2::geom_density()+
#'   ggplot2::scale_x_continuous(trans="log1p", breaks=.gg_breaks_log1p())
#' })
.gg_breaks_log1p = function(n=5,base=10) {
  n_default = n
  function(x, n = n_default) {
    tmp = scales::breaks_log(n_default,base)(x+1,n)
    return(c(0,tmp[-1]))
  }
}

#' logit scale
#'
#' Perform logit scaling with right axis formatting. To not be
#' used directly but with ggplot (e.g. ggplot2::scale_y_continuous(trans =
#' "logit")
#'
#' @param n number of breas
#' @param ... not used
#'
#' @return A scales object
#'
#' @keywords internal
#' @concept ggplot
#' @examples
#' try({
#' tibble::tibble(pvalue = c(0.001, 0.05, 0.1), fold_change = 1:3) %>%
#'  ggplot2::ggplot(ggplot2::aes(fold_change , pvalue)) +
#'  ggplot2::geom_point() +
#'  ggplot2::scale_y_continuous(transform= .gg_transform_logit())
#' })
.gg_transform_logit = function(n = 5, ...) {

  trans = stats::qlogis
  inv = stats::plogis
  n_default = n
  tmp_fn = scales::extended_breaks(n=n)
  breaks_fn = function(x, n=n_default) {
    x %>% trans() %>% tmp_fn(n=n) %>% inv()
  }

  scales::new_transform("logit",
                    transform = trans,
                    inverse = inv,
                    breaks = breaks_fn,
                    format = scales::label_scientific(digits = 2)
  )
}

# Scales ----

#' Use a colour scale from one plot in another
#'
#' Multiple factor levels with custom labels are difficult to sync between
#' graphs if some levels are missing in the second plot. This copies the
#' palette from one plot to another. It is sometimes the case that we want to reuse
#' the fill from one plot as the color for another.
#'
#' @keywords internal
#' @concept ggplot
#'
#' @param plot a ggplot with a colour scale to clone
#' @param original_aesthetic the original aesthetic we are cloning (fill or color)
#' @param target_aesthetic the aesthetic in the new plot we want to match.
#' @inheritDotParams ggplot2::scale_fill_manual
#'
.gg_scale_consistent = function(plot, original_aesthetic = c("fill","color"), target_aesthetic = original_aesthetic, ... ) {
  original_aesthetic = match.arg(original_aesthetic)
  # get the palette
  tmp = ggplot2::ggplot_build(plot)
  fill = tmp$plot$scales$scales %>% purrr::discard(~ !original_aesthetic %in% .x$aesthetics) %>% `[[`(1)
  man = fill$palette.cache
  names(man) = fill$range$range
  return(ggplot2::scale_fill_manual(values = man, aesthetics = target_aesthetic, ...))
}

#' A log1p x scale
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @param sf significant figures
#' @param base the base for the logarithm
#' @param n the number of major breaks
#'
#' @return a ggplot scale
#' @keywords internal
#' @concept ggplot
.gg_scale_x_log1p = function(..., n=5, base=10, sf=2) {
  return(ggplot2::scale_x_continuous(trans="log1p",
             breaks = .gg_breaks_log1p(n,base),
             labels = ~ sprintf("%.*g",sf,.x), ...))
}

#' A log1p y scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param sf significant figures
#' @param base the base for the logarithm
#' @param n the number of major breaks
#'
#' @return a ggplot scale
#' @keywords internal
#' @concept ggplot
.gg_scale_y_log1p = function(..., n=5, base=10, sf=2) {
  return(ggplot2::scale_y_continuous(trans="log1p",
    breaks = .gg_breaks_log1p(n,base),
    labels = ~ sprintf("%.*g",sf,.x),
    ...))
}

#' A logit y scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param sf significant figures
#' @param n  the number of major breaks
#'
#' @return a ggplot scale
#' @keywords internal
#' @concept ggplot
#' @examples
#' tibble::tibble(pvalue = c(0.001, 0.05, 0.1), fold_change = 1:3) %>%
#'  ggplot2::ggplot(ggplot2::aes(fold_change , pvalue)) +
#'  ggplot2::geom_point() +
#'  scale_y_logit(n=8)
.gg_scale_y_logit = function(..., n=5, sf=2) {
  return(ggplot2::scale_y_continuous(
    trans=.gg_transform_logit(n),
    labels = ~ sprintf("%.*g",sf,.x),
    ...))
}



#' A logit x scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param sf significant figures
#' @param n  the number of major breaks
#'
#' @return a ggplot scale
#' @keywords internal
#' @concept ggplot
.gg_scale_x_logit = function(..., n=5, sf=2) {
  return(ggplot2::scale_x_continuous(
    trans=.gg_transform_logit(n),
    labels = ~ sprintf("%.*g",sf,.x),
    ...))
}

#' A percentage y scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param sf significant figures
#'
#' @return a ggplot scale
#' @keywords internal
#' @concept ggplot
#' @examples
#'
#' tibble::tibble(pvalue = c(0.001, 0.05, 0.1), fold_change = 1:3) %>%
#'  ggplot2::ggplot(ggplot2::aes(fold_change , pvalue)) +
#'  ggplot2::geom_point() +
#'  scale_y_percent()
.gg_scale_y_percent = function(..., sf=2) {
  return(ggplot2::scale_y_continuous(
    labels = ~ sprintf("%.*g%%",sf,.x*100),
    ...))
}

#' A percentage x scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param sf significant figures
#'
#' @return a ggplot scale
#' @keywords internal
#' @concept ggplot
.gg_scale_x_percent = function(..., sf=2) {
  return(ggplot2::scale_y_continuous(
    labels = ~ sprintf("%.*g%%",sf,.x*100),
    ...))
}
