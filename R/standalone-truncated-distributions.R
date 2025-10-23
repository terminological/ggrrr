# ---
# repo: terminological/ggrrr
# file: standalone-truncated-distributions.R
# last-updated: '2025-10-14'
# license: https://unlicense.org
# depends:
# - standalone-recycle.R
# imports:
# - rlang
# - stats
# - testthat
# - withr
# ---

# .distfn(stats::rlnorm2, "p")
# .distfn("norm", "p")
# distr must be an expression from rlang::enexpr
.distfn = function(distr, type = c("p", "q", "r")) {
  if (is.character(distr)) {
    distname <- distr
    distname <- paste(type, distname, sep = "")
    pkg = rlang::caller_env()
  } else {
    if (!is.function(eval(distr))) {
      stop("distr must be a function name or a function.")
    }
    pkg = environment(eval(distr))

    distr = rlang::as_label(distr)
    if (grepl("::", distr)) {
      distname = gsub("^[^:]+::[dpqr]", type, distr, perl = TRUE)
    } else {
      distname = gsub("^[dpqr]", type, distr, perl = TRUE)
    }
  }

  if (!exists(distname, mode = "function", envir = pkg)) {
    stop(
      "The ",
      distname,
      " function must be defined in ",
      environmentName(pkg)
    )
  }
  return(get(distname, mode = "function", envir = pkg))
}

#' Sample from an interval of a distribution
#'
#' @param n The number of samples or a vector the same length as the number of samples
#' @param x_left The lower end of the interval or NA for open
#' @param x_right The upper end of the interval or NA for open
#' @param ... parameters for the underlying distribution.
#' @param distr a name e.g. (`"norm"`) or statistical function (e.g. `rnorm`).
#'   `p` and `q` equivalents must exist
#' @param simplify instead of a list return a matrix with `n` columns and
#'   `length(x_left)` rows, or a plain vector of length `n`.
#'
#' @returns a list of samples the same length as `x_left` and `x_right`
#' @keywords internal
#'
#' @unit
#'
#' testthat::expect_equal(
#'   withr::with_seed(123, .rtrunc(5, 0, NA, rnorm, simplify = TRUE)),
#'   c(
#'     0.36860458186587,
#'     1.24891855422508,
#'     0.537354048150005,
#'     1.56756538386253,
#'     1.88423854662693
#'   )
#' )
#'
#' testthat::expect_equal(
#'   withr::with_seed(
#'     123,
#'     .rtrunc(5, 100, 101, "norm", mean = 0, sd = 2, simplify = TRUE)
#'   ),
#'   c(
#'     100.287577520125,
#'     100.788305135444,
#'     100.408976921812,
#'     100.883017404005,
#'     100.940467284294
#'   )
#' )
#'
#' testthat::expect_equal(
#'   withr::with_seed(
#'     123,
#'     .rtrunc(5, 0, 0, "norm", mean = 0, sd = 2, simplify = TRUE)
#'   ),
#'   c(0, 0, 0, 0, 0)
#' )
#'
#' testthat::expect_equal(
#'   withr::with_seed(123, .rtrunc(5, 0.2, 0.3, stats::qexp, r = 2, simplify = TRUE)),
#'   c(
#'     0.226768410134133,
#'     0.277097702724268,
#'     0.238513388505999,
#'     0.28721473064001,
#'     0.29345270111699
#'   )
#' )
#'
#' # Some part of range is possible => works and only produces positives.
#' testthat::expect_equal(
#'   withr::with_seed(
#'     123,
#'     .rtrunc(5, -1, 1, "gamma", shape = 1, simplify = TRUE)
#'   ),
#'   c(
#'     0.200628506352649,
#'     0.689760686697004,
#'     0.29911075895734,
#'     0.81683790576972,
#'     0.902606552073028
#'   )
#' )
#'
#' # No part of range is possible => only produces NAs.
#' testthat::expect_equal(
#'   withr::with_seed(
#'     123,
#'     .rtrunc(5, -2, -1, "gamma", shape = 1, simplify = TRUE)
#'   ),
#'   c(NA, NA, NA, NA, NA)
#' )
#'
#' testthat::expect_error(
#'   {
#'     .rtrunc(5, 2, 1, "gamma", shape = 1, simplify = TRUE)
#'   },
#'   "all `x_left` must be smaller than `x_right`",
#'   fixed = TRUE
#' )
#'
.rtrunc = function(n, x_left, x_right, distr, ..., simplify = FALSE) {
  distr = rlang::enexpr(distr)
  pfn = .distfn(distr, "p")
  qfn = .distfn(distr, "q")

  nx = .recycle(x_left, x_right)

  if (any(x_left > x_right, na.rm = TRUE)) {
    stop("all `x_left` must be smaller than `x_right`")
  }

  q_left = pfn(x_left, ...)
  q_right = pfn(x_right, ...)

  q_left = ifelse(is.na(x_left), 0, q_left)
  q_right = ifelse(is.na(x_right), 1, q_right)

  low = qfn(0, ...)
  high = qfn(1, ...)

  samples = lapply(seq_len(nx), function(i) {
    if (q_left[i] == q_right[i]) {
      if (x_right[i] < low || x_left[i] > high) {
        rep(NA, n)
      } else if (x_left[i] == x_right[i]) {
        rep(x_left[i], n)
      } else {
        runif(n, x_left[i], x_right[i])
      }
    } else {
      qfn(runif(n, q_left[i], q_right[i]), ...)
    }
  })

  if (simplify) {
    if (nx == 1) {
      return(unlist(samples))
    }
    return(matrix(unlist(samples), nrow = nx, byrow = TRUE))
  }

  return(samples)
}
