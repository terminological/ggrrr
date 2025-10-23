# ---
# repo: terminological/ggrrr
# file: standalone-empirical.R
# last-updated: '2025-10-15'
# license: https://unlicense.org
# imports:
# - rlang
# - stats
# ---

#' Fit a piecewise logit transformed linear model to cumulative data
#'
#' This fits a CDF and quantile function to data in a transformed space. X value
#' transformation is specified in `trans_fn` and `inv_fn` and is either something
#' like log / exp, logit / expit etc or can also be specified as a
#' cdf / quantile function pair if `link_cdf` is true. The distribution is a
#' piecewise linear fit in transformed X and logit Y space. The end points are
#' linearly interpolated in this space to the `tail_p`th quantile. The function
#' can either be used to fit samples as a vector of `x`, or fit quantiles
#' specified as `x, P(X<=x)` pairs.
#'
#' This function imputes tails of distributions. Given perfect data as samples
#' or as quantiles it should recover
#'
#' @param x either a vector of samples from a distribution `X` or cut-offs for
#'   cumulative probabilities when combined with `probs`
#' @param probs (optional) If present a vector the same length as x giving
#'   `P(X <= x)`.
#' @param trans_fn A function which maps `X` to +/- Infinity, or if `link_cdf`
#'  is true the to 0-1 (i.e. a CDF)
#' @param inv_fn A function which maps +/- Infinity to the support of `X`, or if
#'  `link_cdf` is true them maps 0-1 to the support of `X` (i.e. a quantile
#'  function)
#' @param link_cdf Should the link function be treated as a CDF/QF pair? This
#'   supports the use of a prior to define the support of the empirical function,
#'   and is designed to prevent tail truncation. Support for the
#'   updated quantile function will be the same as the provided prior.
#' @param knots for distributions from data how many points do we use to model the
#'  cdf?
#' @param tail_p what is the minimum tail probability modelled.
#' @param weights if the distribution is from data then an importance weighting
#'   for each point can be supplied here. This will be normalised, so it is a
#'   relative importance with respect to the mean.
#'
#' @returns a list of 3 functions `p()` for CDF, `q()` for quantile, and `r()`
#'  for a RNG. There is no density function as it is not stable around the knot
#'  points.
#' @keywords internal
#'
#' @unit
#'
#' #from cdf:
#' xs = c(2,3,6,9)
#' ps = c(0.1,0.4,0.6,0.95)
#' e = .empirical(xs, ps, trans_fn = log, inv_fn = exp)
#'
#' testthat::expect_equal(e$p(xs), ps)
#' testthat::expect_equal(e$q(ps), xs)
#'
#' # from samples:
#' withr::with_seed(123,{
#'  e2 = .empirical(rnorm(10000),knots = 20)
#'  testthat::expect_equal(e2$p(-5:5), pnorm(-5:5), tolerance=0.01)
#'  testthat::expect_equal(e2$q(seq(0,1,0.1)), qnorm(seq(0,1,0.1)), tolerance=0.025)
#' })
#'
#' p2 = seq(0,1,0.1)
#' testthat::expect_equal( e2$p(e2$q(p2)), p2)
#'
#' # quantiles:
#' p = c(0.025,0.05,0.10,0.25,0.5,0.75,0.9,0.95,0.975)
#' q = stats::qgamma(p, shape=2)
#' gemp = .empirical(q,p,trans_fn = ~ pgamma(.x,2), inv_fn = ~ qgamma(.x,2), link_cdf= TRUE)
#' withr::with_seed(123, {
#'   testthat::expect_equal(mean(gemp$r(100000)),2, tolerance=0.01)
#'   testthat::expect_equal(sd(gemp$r(100000)), sqrt(2), tolerance=0.01)
#' })
#'
#' # With perfect input can recover the underlying distribution including tails:
#' tmp = .empirical(x=seq(0.01,0.99,0.01),trans_fn = ~ punif(.x,0,1), inv_fn = ~ qunif(.x,0,1),link_cdf = TRUE, knots = 100)
#' testthat::expect_equal(
#'   tmp$q(c(0.01, 0.1, 0.25, 0.75, 0.9, 0.99)),
#'   c(0.01, 0.1, 0.25, 0.75, 0.9, 0.99)
#' )
#'
#'
#'
.empirical = function(
  x,
  probs = NULL,
  trans_fn = ~.x,
  inv_fn = ~.x,
  link_cdf = FALSE,
  knots = length(x),
  tail_p = 0.00001,
  weights = rep(1, length(x))
) {
  # fast
  .logit = qlogis

  # slow (but faster that plogis)
  .expit = function(x) {
    return(1 / (1 + exp(-x)))
  }

  if (link_cdf) {
    trans_fn_int = rlang::as_function(trans_fn)
    inv_fn_int = rlang::as_function(inv_fn)
    trans_fn = function(x) .logit(trans_fn_int(x))
    inv_fn = function(y) inv_fn_int(.expit(y))
  } else {
    trans_fn = rlang::as_function(trans_fn)
    inv_fn = rlang::as_function(inv_fn)
  }

  if (is.null(probs)) {
    weights = weights / sum(weights) * length(x)
    x1 = sort(unique(x))
    y1 = sapply(x1, function(xi) sum(weights[which(xi == x)]))
    x = x1
    y = cumsum(y1) / (sum(y1) + 1)
    # alpha = cumsum(y) #= r
    # alpha_plus_beta = sum(y) + 1 #(=n+1)
    # y = alpha / alpha_plus_beta
    # y = (alpha - 1 / 3) / (alpha_plus_beta - 2 / 3) #median of beta
  } else {
    y = probs[order(x)]
    x = x[order(x)]
  }

  x2 = trans_fn(x)
  y2 = .logit(y)

  y2_eps = .logit(tail_p)

  window = length(x2) / (knots - 2)
  predict_with = if (is.null(probs)) {
    2
  } else {
    min(c(10, ceiling(window), length(x2)))
  }

  if (min(y2) > y2_eps) {
    # We need to infer a tail value to prevent truncation
    if (link_cdf) {
      # All the transformation has been done already.
      x2_eps = y2_eps
      # x2_eps = trans_fn(.expit(y2_eps))
    } else {
      x2_eps = .predict_lm_1d(
        head(x2, predict_with),
        head(y2, predict_with),
        y2_eps
      )
    }
    x2 = c(x2_eps, x2)
    y2 = c(y2_eps, y2)
  }

  y2_eps = -y2_eps
  if (min(y2) < y2_eps) {
    # We need to infer a tail value to prevent truncation
    if (link_cdf) {
      # All the transformation has been done already.
      x2_eps = y2_eps
      # x2_eps = trans_fn(.expit(y2_eps))
    } else {
      x2_eps = .predict_lm_1d(
        tail(x2, predict_with),
        tail(y2, predict_with),
        y2_eps
      )
    }
    x2 = c(x2, x2_eps)
    y2 = c(y2, y2_eps)
  }

  # x2 = 1:55
  # y2 = 1:55
  # knots = 20
  if (is.null(probs)) {
    if (window > 2) {
      starts = c(2 + floor((seq_len(knots - 2) - 1) * window))
      ends = c(starts[-1] - 1, length(x2) - 1)
      x3 = sapply(seq_along(starts), function(i) {
        mean(x2[starts[i]:ends[i]])
      })
      y3 = sapply(seq_along(starts), function(i) {
        mean(y2[starts[i]:ends[i]])
      })
      x2 = c(x2[1], x3, x2[length(x2)])
      y2 = c(y2[1], y3, y2[length(y2)])
    }
  }

  x = y = y2_eps = window = predict_with = NULL
  return(
    structure(
      list(
        p = function(x1) {
          .expit(stats::approxfun(x2, y2, yleft = -Inf, yright = Inf)(trans_fn(
            x1
          )))
        },
        q = function(p1) {
          inv_fn(stats::approxfun(
            y2,
            x2,
            yleft = -Inf,
            yright = Inf
          )(.logit(p1)))
        },
        r = function(n) {
          inv_fn(stats::approxfun(
            y2,
            x2,
            # prevent sampler from giving infinite values outside of tail_p limits:
            yleft = min(y2),
            yright = max(y2)
          )(.logit(runif(n))))
        }
      ),
      class = c("dist_fns", "list")
    )
  )
}


#' S3 object wrapper for statistical distributions
#'
#' @param distr a name of or function from a statistical distribution family.
#'   e.g. `"norm"` or `rnorm`
#' @param ... parameters to the distribution.
#'
#' @returns a list of the `p`, `q` and `r` functions of this distribution with
#'   the supplied parameters.
#' @keywords internal
.as.dist_fns = function(distr, ...) {
  distr = rlang::enexpr(distr)
  dots = rlang::list2(...)
  rawfns = lapply(c("p", "q", "r"), function(type) {
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
  })

  browser()
  return(
    structure(
      list(
        p = function(q1) {
          do.call(rawfns[[1]], args = c(q = q1, dots))
        },
        q = function(p1) {
          do.call(rawfns[[2]], args = c(p = p1, dots))
        },
        r = function(n) {
          do.call(rawfns[[3]], args = c(n = n, dots))
        }
      ),
      class = c("dist_fns", "list")
    )
  )
}

#' @export
print.dist_fns = function(x, ...) {
  cat(format.dist_fns(x, ...))
}

#' @export
format.dist_fns = function(x, ..., digits = 3) {
  return(sprintf(
    "Distribution: Median %s [IQR %s \u2014 %s]",
    format(x$q(0.5), ..., digits = digits),
    format(x$q(0.25), ..., digits = digits),
    format(x$q(0.75), ..., digits = digits)
  ))
}

#' Interpolate from 2 coordinates and predict output
#'
#' @param y the y values. length 2.
#' @param x the x values. length 2.
#' @param new_x the values to predict at (vectorised). If null will return a
#'   function that can predict new values.
#'
#' @returns the interpolated value or a prediction function
#' @keywords internal
#'
#' @unit
#' .interpolate(c(0,1),c(0,1),5)
.interpolate = function(y, x, new_x = NULL) {
  if (length(unique(x)) != length(unique(y)) || length(unique(y)) != 2) {
    stop("coordinates must be length 2")
  }
  fn = function(x2) {
    (y[2] - y[1]) / (x[2] - x[1]) * (x2 - x[1]) + y[1]
  }
  if (is.null(new_x)) {
    return(fn)
  }
  return(fn(new_x))
}

#' Fit a 1D linear model and predict output
#'
#' @param y the y values. At least 2.
#' @param x the x values. At least 2.
#' @param new_x the values to predict at (vectorised). If null will return a
#'   function that can predict new values.
#'
#' @returns the predicted value or a prediction function
#' @keywords internal
#'
#' @unit
#' .predict_lm_1d(c(6,5,7,10), c(1,2,3,4), c(0,1))
#' .predict_lm_1d(c(0,1), c(0,1), -2:2)
.predict_lm_1d = function(y, x, new_x = NULL) {
  if (length(y) == 2 && length(x) == 2) {
    return(.interpolate(y, x, new_x))
  }
  if (length(x) != length(y)) {
    stop("unequal length LM coordinates")
  }
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    stop("not enough data in LM")
  }
  missing = is.na(x) | is.na(y)
  x = x[!missing]
  y = y[!missing]
  x = matrix(c(rep(1, length(x)), x), ncol = 2)
  y = matrix(y, ncol = 1)
  beta = solve(t(x) %*% x) %*% t(x) %*% y
  fn = function(x2) return(beta[1, 1] + beta[2, 1] * x2)
  if (is.null(new_x)) {
    return(fn)
  }
  return(fn(new_x))
}
