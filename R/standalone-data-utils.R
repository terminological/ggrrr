# ---
# repo: terminological/ggrrr
# file: standalone-data-utils.R
# last-updated: 2025-10-09
# license: https://unlicense.org
# imports:
# - dplyr
# - glue
# - stats
# - tibble
# - utils
# ---

#' Cut and label an integer valued quantity
#'
#' Deals with some annoying issues classifying integer data sets, such as ages, into groups. where you want to
#' specify just the change over points as integers and clearly label the resulting ordered factor.
#'
#' @param x a vector of integer valued numbers, e.g. ages, counts
#' @param cut_points a vector of integer valued cut points which define the lower boundaries of conditions
#' @param glue a glue spec that may be used to generate a label. It can use \{low\}, \{high\}, \{next_low\}, or \{label\} as values.
#' @param lower_limit the minimum value we should include (this is inclusive for the bottom category) (default -Inf)
#' @param upper_limit the maximum value we should include (this is also inclusive for the top category) (default Inf)
#' @param ordered should the result be an ordered factor? default is TRUE
#' @param ... not used
#'
#' @return an ordered factor of the integer
#' @export
#' @concept tidydata
#'
#' @examples
#' cut_integer(stats::rbinom(20,20,0.5), c(5,10,15))
#' cut_integer(floor(stats::runif(100,-10,10)), cut_points = c(2,3,4,6), lower_limit=0, upper_limit=10)
cut_integer = function(
  x,
  cut_points,
  glue = "{label}",
  lower_limit = -Inf,
  upper_limit = Inf,
  ordered = TRUE,
  ...
) {
  next_low = NULL # remove global binding note

  if (!all(as.integer(x) == x, na.rm = TRUE)) {
    warning("input to cut_integer(...) has been coerced to integer values")
  }
  x = floor(x)
  if (!all(as.integer(cut_points) == cut_points)) {
    stop(
      "cut_points must be integer valued, and define the lower end of each category."
    )
  }
  if (any(cut_points <= lower_limit | cut_points >= upper_limit)) {
    warning(
      "cut_point values should be between lower_limit (",
      lower_limit,
      ") and upper_limit (",
      upper_limit,
      ")."
    )
  }
  # make sure the limits are not included.
  cut_points = cut_points[cut_points > lower_limit & cut_points < upper_limit]
  # sort and uniquify
  #
  breaks = unique(sort(c(lower_limit, cut_points, upper_limit + 1)))
  labels = tibble::tibble(
    low = utils::head(breaks, -1),
    next_low = utils::head(dplyr::lead(breaks, n = 1), -1),
    high = ifelse(next_low != upper_limit, next_low - 1, upper_limit)
  ) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        low == high ~ sprintf("%1.0f", low),
        low == -Inf ~ sprintf("<%1.0f", next_low),
        high == Inf ~ sprintf("\u2265%1.0f", low),
        TRUE ~ sprintf("%1.0f\u2012%1.0f", low, high)
      )
    ) %>%
    dplyr::mutate(
      label2 = glue::glue(glue)
    )

  return(
    cut(
      x,
      include.lowest = TRUE,
      breaks = breaks,
      labels = labels$label2,
      ordered_result = ordered,
      right = FALSE
    )
  )
}

#' The maximum date as a date
#'
#' @param x a date vector
#' @param ... not used
#' @param na.rm remove NAs?
#'
#' @returns a date
#' @keywords internal
#' @concept tidydata
max_date = function(x, ..., na.rm = TRUE) {
  tmp = as.Date(suppressWarnings(max(x, ..., na.rm = na.rm)), "1970-01-01")
  if (!is.finite(tmp)) {
    tmp = as.Date("0001-01-01")
  }
  return(tmp)
}

#' The minimum date as a date
#'
#' @param x a date vector
#' @param ... not used
#' @param na.rm remove NAs
#'
#' @returns a date
#' @keywords internal
#' @concept tidydata
min_date = function(x, ..., na.rm = TRUE) {
  tmp = as.Date(suppressWarnings(min(x, ..., na.rm = na.rm)), "1970-01-01")
  if (!is.finite(tmp)) {
    tmp = as.Date("9999-12-31")
  }
  return(tmp)
}
