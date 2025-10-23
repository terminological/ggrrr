# ---
# repo: terminological/ggrrr
# file: standalone-text-formatting.R
# last-updated: 2025-10-10
# license: https://unlicense.org
# imports:
# - base
# - dplyr
# - ggplot2
# - glue
# - grDevices
# - rlang
# - stats
# - stringr
# - testthat
# - tibble
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
#' # N.B. see `.sprintf_quant()` for a better version of this:
#'
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
#' testthat::expect_equal(
#'   .sprintf_data(
#'     "%1.2f - %1.2f",
#'     tibble::tibble(low = c(1, 2, NA, 4, 5), high = c(5, 4, 3, 2, NA)),
#'     sep = ","
#'   ),
#'   c("1,00 - 5,00", "2,00 - 4,00", "―", "4,00 - 2,00", "―")
#' )
#'
#' testthat::expect_equal(
#'   .sprintf_data("%1.2f - %1.2f", list(low = 1, high = 5)),
#'   "1.00 - 5.00"
#' )
#'
#' testthat::expect_equal(
#'   .sprintf_data("%1.2f - %1.2f", c(1, 5)),
#'   "1.00 - 5.00"
#' )
#' # this is the sprintf syntax of enumerated items
#' testthat::expect_warning(.sprintf_data("%1.2f - %1.2f", c(1,2), 5))
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
#' testthat::expect_equal(
#'   .if_na(c(1, 2, NA, 4, 5) / 3, digits = 2),
#'   c("0.33", "0.67", "―", "1.33", "1.67")
#' )
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
#' @unit
#' testthat::expect_equal(
#'   .sprintf_dp("%1.2f", 1:3 / 3, sep = "\u00B7"),
#'   c("0·33", "0·67", "1·00")
#' )
#'
#' testthat::expect_equal(
#'   .sprintf_dp("%1.2f-%1.2f", 1:3 / 3, 1:3, sep = "\u00B7"),
#'   c("0·33-1·00", "0·67-2·00", "1·00-3·00")
#' )
#'
#' testthat::expect_equal(
#'   .sprintf_dp("%s %1.2f-%1.2f", "A.1.2", 1:3 / 3, 1:3, sep = "\u00B7"),
#'   c("A.1.2 0·33-1·00", "A.1.2 0·67-2·00", "A.1.2 1·00-3·00")
#' )
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


#' Escape a string for use in a regular expression
#'
#' Adapted from rex:::escape.character
#'
#' @param x A string to use in regex
#'
#' @returns An regex that matches the literal string x
#' @keywords internal
.escape_regex = function(x) {
  chars <- c(
    "*",
    ".",
    "?",
    "^",
    "+",
    "$",
    "|",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    "\\"
  )
  gsub(
    paste0("([\\", paste0(collapse = "\\", chars), "])"),
    "\\\\\\1",
    x,
    perl = TRUE
  )
}


#' Sprintf a value for regex
#'
#' Usually this will be used with strings as
#'
#' @param fmt a sprintf format that contains templated regex
#' @param ... sprintf inputs. Usually this will be string data
#'
#' @returns a regular expression (or set of) that match the fixed strings given
#'   and any regex given in the fmt string.
#' @keywords internal
#'
#' @unit
#'
#' literal_capture = "[[1]]"
#' literal_extn = "..."
#' regex = .sprintf_regex("(%s).*%s",literal_capture, literal_extn)
#'
#' testthat::expect_equal(regex, "(\\[\\[1\\]\\]).*\\.\\.\\.")
#'
#' # should match all of:
#' testthat::expect_equal(
#'   grepl(regex, c("[[1]]asdas...", "[[1]]...", "[[1]]$...")),
#'   c(TRUE, TRUE, TRUE)
#' )
#'
.sprintf_regex = function(fmt, ...) {
  dots = rlang::list2(...)
  dots = lapply(dots, function(x) if (is.character(x)) .escape_regex(x) else x)
  do.call(sprintf, c(fmt, dots))
}


#' A formatted confidence interval from data
#'
#' Can be used in a summarise with grouped data or in a mutate with nested data
#'
#' @param x a vector of data to summarise or a list column of data.
#' @inheritDotParams .sprintf_data
#' @param fmt a sprintf format string expecting exactly 3 numbers
#' @param ci the confidence interval width
#' @param na.rm remove NAs?
#' @param dp number of decimal places
#'
#' @returns a formatted CI string, or a set of formatted strings ig a list was
#'   supplied
#' @keywords internal
#' @unit
#' testthat::expect_equal(
#'   .sprintf_quant(iris$Petal.Length),
#'   "4.35 [1.27 – 6.46]"
#' )
#'
#' # decimal point
#' testthat::expect_equal(
#'   .sprintf_quant(iris$Petal.Length, dp = 4),
#'   "4.3500 [1.2725 – 6.4550]"
#' )
#'
#' # works for list columns:
#' testthat::expect_equal(
#'   .sprintf_quant(lapply(1:5, seq, length.out = 50)),
#'   c(
#'     "25.50 [2.23 – 48.77]",
#'     "26.50 [3.23 – 49.77]",
#'     "27.50 [4.23 – 50.77]",
#'     "28.50 [5.23 – 51.77]",
#'     "29.50 [6.23 – 52.77]"
#'   )
#' )
#'
#' testthat::expect_equal(
#'   rlang::hash(iris %>% dplyr::group_by(Species) %>% .sprintf_quant()),
#'   "21fda821e7c7b1005517706f1c9a619a"
#' )
.sprintf_quant = function(
  x,
  ...,
  fmt = "%1.*f [%1.*f \u2013 %1.*f]",
  ci = 0.95,
  na.rm = FALSE,
  dp = 2
) {
  if (is.data.frame(x)) {
    return(
      dplyr::summarise(
        x,
        dplyr::across(
          dplyr::where(is.numeric),
          function(cx) {
            .sprintf_quant(cx, ..., fmt = fmt, ci = ci, na.rm = na.rm, dp = dp)
          }
        )
      )
    )
  }
  if (is.list(x)) {
    return(sapply(x, function(cx) {
      .sprintf_quant(cx, ..., fmt = fmt, ci = ci, na.rm = na.rm, dp = dp)
    }))
  }

  fmt = gsub("*", floor(dp), fmt, fixed = TRUE)
  .sprintf_data(
    fmt = fmt,
    params = stats::quantile(
      x,
      probs = c(0.5, 0.5 - ci / 2, 0.5 + ci / 2),
      na.rm = na.rm,
      names = FALSE
    ),
    ...
  )
}
