# ---
# repo: terminological/ggrrr
# file: standalone-text-formatting.R
# last-updated: 2024-06-06
# license: https://unlicense.org
# imports:
#    - base
#    - dplyr
#    - ggplot2
#    - glue
#    - grDevices
#    - rlang
#    - stats
#    - stringr
#    - tibble
# ---
# This set of functions involve formatting

#' `sprintf` with a list(ish) input
#'
#' A variant of sprintf that work well with inputs that are in the format of a list
#' or a dataframe. Examples of which are the quantile functions. This falls
#' in between `sprintf` and `glue::glue_data` in complexity.
#'
#' @param fmt a sprintf format string
#' @param params the inputs as a list or a dataframe (rather than an enumeration of individual numbers)
#' @inheritDotParams .sprintf_dp
#' @param na.replace a value to replace NA values with.
#'
#' @return the formatted string
#' @keywords internal
#'
#'
#' @unit
#' # generate an IQR from data
#' iris %>%
#'   dplyr::group_by(Species) %>%
#'   dplyr::summarise(
#'     q.Sepal.Width = .sprintf_data(
#'       "%1.2f [%1.2f\u2013%1.2f]",
#'       stats::quantile(Sepal.Width, probs = c(0.5,0.25,0.75))
#'     )
#'   )
#'
#' # dataframe, lists or vectors should work. dataframe columns are interpreted
#' # in the order they are given.
#'
#' .sprintf_data("%1.2f - %1.2f", tibble::tibble(low = c(1,2,NA,4,5), high = c(5,4,3,2,NA)))
#' .sprintf_data("%1.2f - %1.2f", tibble::tibble(low = c(1,2,NA,4,5), high = c(5,4,3,2,NA)), sep=",")
#' .sprintf_data("%1.2f - %1.2f", list(low = 1, high = 5))
#' .sprintf_data("%1.2f - %1.2f", c(1,5))
#' # this is an error because this is the sprintf syntax of enumerated items
#' try(.sprintf_data("%1.2f - %1.2f", 1, 5))
.sprintf_data = function(fmt, params, ..., na.replace = "\u2015") {
  dots = rlang::list2(...)
  anyNa = Reduce(`|`, lapply(params, is.na))
  tmp = do.call(.sprintf_dp, c(as.list(fmt), as.list(params), dots))
  ifelse(anyNa, na.replace, tmp)
}


#' Format a value replacing NA values
#'
#' @param x a vector of values
#' @inheritDotParams base::format
#' @param na.replace a value to replace NA values with
#'
#' @return a vector of character values
#' @keywords internal
#'
#'
#' @unit
#' .if_na( c(1,2,NA,4,5)/3, digits=2 )
.if_na = function(x, na.replace = "\u2015", ...) {
  if (is.null(x)) {
    return(character())
  }
  dots = rlang::dots_list(...)
  if (any(names(dots) == "")) {
    stop("extra `...` parameters must be named")
  }
  ifelse(is.na(x), na.replace, format(x, ...))
}


#' Format a value omitting NA values
#'
#' @param x a vector of values
#' @inheritDotParams base::format
#'
#' @return a vector of character values
#' @keywords internal
#'
#'
#' @unit
#' .if_present( c(1,2,NA,4,5)/3, digits=2 )
.if_present = function(x, ...) {
  return(.if_na(x, na.replace = "", ...))
}

#' Replaces decimal points in sprintf output.
#'
#' `sprintf` will respect decimal point conventions only if you switch locale.
#' This is unwieldy and not in line with `format`. This wrapper around `sprintf`
#' replaces decimal points with a `sep` character of your choice, sensible choices
#' for which are `,` or `UTF8+00B7`. It will not try and replace things that are
#' not syntactially numbers.
#'
#' @param fmt a sprintf format string
#' @inheritDotParams base::sprintf
#' @param sep a character to use a decimal point.
#'
#' @return the formatted string with decimal points as specified
#' @keywords internal
#'
#'
#' @unit
#' .sprintf_dp("%1.2f",1:3/3, sep="\u00B7")
#' .sprintf_dp("%1.2f-%1.2f", 1:3/3, 1:3, sep="\u00B7")
#' .sprintf_dp("%s %1.2f-%1.2f", "A.1.2", 1:3/3, 1:3, sep="\u00B7")
.sprintf_dp = function(fmt, ..., sep = getOption("OutDec", ".")) {
  c = sprintf(fmt, ...)
  if (sep == ".") {
    return(c)
  }
  c %>%
    stringr::str_replace_all(
      "([^A-Za-z0-9.]|\\s|^)([0-9]*)\\.([0-9]+)(?!\\.)",
      paste0("\\1\\2", sep, "\\3")
    )
}


#' Switch UTF-8 into plain text when using the pdf device
#'
#' The plain `grDevices::pdf()` device is the default when running examples via CRAN R CMD
#' check. It throws warnings and sometimes errors that other devices do not when
#' encountering UTF-8 characters in plot objects (due to a warning from `grid`).
#' The resulting behaviour is R version dependent. It is basically impossible to
#' get round these in your function examples and you either decide not to run
#' them or only run them with non UTF-8 content. This will make that decision at
#' runtime and provide a transliterated alternative for the `pdf` device in the
#' context of a function example in a CRAN check.
#'
#' @param label a UTF8 label
#' @param alt alternative for the pdf device
#'
#' @return the same string or a non UTF alternative if currently using the
#'   legacy pdf device
#' @keywords internal
#'
#'
#' @unit
#' .pdf_safe("test")
#' .pdf_safe("\u00B1\u221E")
#' ggplot2::ggplot()+ggplot2::xlab(.pdf_safe("\u00B1\u221E"))
.pdf_safe = function(label, alt = label) {
  if (names(grDevices::dev.cur()) == "pdf") {
    alt = iconv(alt, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    return(alt)
  }
  return(label)
}
