# ---
# repo: terminological/ggrrr
# file: standalone-recycle.R
# last-updated: '2025-07-10'
# license: https://unlicense.org
# imports: rlang
# ---

#' Strictly recycle function parameters
#'
#' `.recycle` is called within a function and ensures the parameters in the
#' calling function are all the same length by repeating them using `rep`. This
#' function alters the environment from which it is called. It is stricter than
#' R recycling in that it will not repeat vectors other than length one to match
#' the longer ones, and it throws more informative errors.
#'
#' NULL values are not recycled, missing values are ignored.
#'
#' @param ... the variables to recycle
#' @param .min the minimum length of the results (defaults to 1)
#' @param .env the environment to recycle within.
#'
#' @return the length of the longest variable
#'
#' @concept parameter_checks
#' @keywords internal
#'
#'
#' @unit
#' testfn = function(a, b, c) {
#'   n = .recycle(a,b,c)
#'   return(list(
#'     a=a, b=b, c=c, n=n
#'   ))
#' }
#'
#' tmp = testfn(a=c(1,2,3), b="needs recycling", c=NULL)
#'
#' testthat::expect_equal(tmp$n, 3)
#' testthat::expect_null(tmp$c)
#' testthat::expect_equal(length(tmp$a), length(tmp$b))
#'
#' # no parameter
#' testthat::expect_error(testfn(a=c(1,2,3), c=NULL))
#'
#'
#' tmp = testfn(a=character(), b=integer(), c=NULL)
#' testthat::expect_equal(tmp$n,0)
#'
#' # inconsistent to have a zero length and a non zero length
#' testthat::expect_error(testfn(a=c("a","b"), b=integer(), c=NULL))
#'
.recycle = function(..., .min = 1, .env = rlang::caller_env()) {
  names = sapply(rlang::ensyms(...), rlang::as_label)
  dots = rlang::enexprs(...)
  env = .env

  missing = sapply(names, function(x) {
    !(exists(x, envir = env, inherits = FALSE)) || rlang::is_missing(env[[x]])
  })
  # if (any(missing)) warning( paste0(names[missing],collapse=","), " "
  names = names[!missing]
  dots = dots[!missing]
  # browser()

  lengths = sapply(names, function(x) length(env[[x]]))
  nulls = sapply(names, function(x) is.null(env[[x]]))
  lengths[nulls] = NA_integer_

  if (all(is.na(lengths))) {
    return(0)
  } # all parameters are null

  if (all(lengths == 0, na.rm = TRUE)) {
    ml = 0
  } else {
    ml = max(c(lengths, .min), na.rm = TRUE)
  }

  if (!all(lengths %in% c(NA_integer_, 1, ml))) {
    names = names[!lengths %in% c(NA_integer_, 1, ml)]
    stop(
      sprintf(
        "Parameter %s is/are the wrong lengths. They should be length %d%s",
        paste0("`", names, "`", collapse = ","),
        ml,
        if (ml != 1) " (or 1)" else ""
      ),
      call. = FALSE
    )
  }

  if (ml > 1) {
    for (i in seq_along(names)) {
      name = names[[i]]
      x = env[[name]]
      if (length(x) == 1) {
        env[[name]] = rep(x, ml)
      }
    }
  }

  return(ml)
}
