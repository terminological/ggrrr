#' Convert a label size from points to ggplot units
#'
#' Labels like geom_text are in a random unit size which is only mysteriously connected to the size of text on axes
#'
#' @param pts label size in points
.gg_label_size = function(pts) {
  return (pts/ggplot2::.pt) #/(96/72))
}

#' Hide the x axis of a plot
#'
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
#' @return a theme
.gg_hide_legend = function() {
  ggplot2::theme(legend.position = "none")
}

#' Set the angle of the x axis labels of a plot
#'
#' @param ang the angle for the x labels
#'
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
