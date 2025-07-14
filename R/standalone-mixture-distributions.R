# ---
# repo: terminological/ggrrr
# file: standalone-mixture-distributions.R
# last-updated: '2025-07-10'
# license: https://unlicense.org
# imports:
# - purrr
# - stats
# - interfacer
# ---
# This set of functions are stats distributions and an anomaly here in ggrrr.

# mix normals ----

#' The cumulative density function of a mixture of normal distributions
#'
#' @param q vector of quantiles.
#' @param means a vector of normal distribution means
#' @param sds  a vector of normal distribution sds
#' @param weights  a vector of weights
#' @param na.rm remove distributions which have NA for mean or sd
#'
#' @return the pdf of the mixture distribution.
#' @keywords internal
#' @concept distributions
#'
#' @examples
#' try({
#' .pmixnorm(q=c(2,20), means=c(10,13,14), sds=c(1,1,2), weights=c(2,2,3))
#' })
.pmixnorm = function(q, means, sds, weights=rep(1,length(means)), na.rm=FALSE) {
  return(.pmix("norm",q,means,sds,weights,na.rm))
}

#' A quantile function for a mixture of normal distributions
#'
#' @param p vector of probabilities.
#' @param means a vector of normal distribution means
#' @param sds a vector of normal distribution sds
#' @param weights  a vector of weights
#' @param na.rm remove distributions with NA values for mean or sd
#' @param approx use Cornish Fisher moment based approximation if available
#' @param ... passed on to internal function, `samples=TRUE` to force random sampling
#'
#' @return the value of the `p`th quantile
#' @keywords internal
#' @concept distributions
#'
#' @examples
#' try({
#' .qmixnorm(p=c(0.025,0.5,0.975), means=c(10,13,14), sds=c(1,1,2))
#' })
.qmixnorm = function(p, means, sds, weights=rep(1,length(means)), na.rm=FALSE, approx = TRUE,...) {
  return(.qmix("norm",p, means,sds,weights,na.rm,approx,...))
}

# mix log normals ----

#' The cumulative density function of a mixture of log normal distributions
#'
#' @param q vector of quantiles.
#' @param meanlogs a vector of normal distribution means
#' @param sdlogs  a vector of normal distribution sds
#' @param weights  a vector of weights
#' @param na.rm remove distributions which have NA for mean or sd
#'
#' @return the pdf of the mixture distribution.
#' @keywords internal
#' @concept distributions
#'
#' @examples
#' try({
#' .pmixlnorm(q=c(2,20), meanlogs=c(1.0,1.3,1.4), sdlogs=c(1,1,2), weights=c(2,2,3))
#' })
.pmixlnorm = function(q, meanlogs, sdlogs, weights=rep(1,length(meanlogs)), na.rm=FALSE) {
  return(.pmix("lnorm",q,meanlogs,sdlogs,weights,na.rm))
}

#' A quantile function for a mixture of log normal distributions
#'
#' @param p vector of probabilities.
#' @param meanlogs a vector of log normal distribution means
#' @param sdlogs  a vector of log normal distribution sds
#' @param weights  a vector of weights
#' @param na.rm remove distributions with NA values for mean or sd
#' @param approx use Cornish Fisher moment based approximation if available
#' @param ... passed on to internal function, `samples=TRUE` to force random sampling
#'
#' @return the value of the `p`th quantile
#' @keywords internal
#' @concept distributions
#'
#' @examples
#' try({
#'   .qmixlnorm(p=c(0.025,0.5,0.975), meanlogs=c(1,1.3,1.4), sdlogs=c(0.1,0.1,0.2))
#'   .qmixlnorm(p=c(0.025,0.5,0.975), meanlogs=c(1,1.3,1.4), sdlogs=c(0.1,0.1,0.2), approx=FALSE)
#'   .qmixlnorm(p=c(0.025,0.5,0.975), meanlogs=c(1,1.3,1.4), sdlogs=c(1,1,2), approx=FALSE)
#' })
.qmixlnorm = function(p, meanlogs, sdlogs, weights=rep(1,length(meanlogs)), na.rm=FALSE, approx = TRUE,...) {
  return(.qmix("lnorm",p, meanlogs,sdlogs,weights,na.rm,approx,...))
}


# mix gammas ----

#' The cumulative density function of a mixture of gamma distributions
#'
#' @param q vector of quantiles.
#' @param shapes a vector of gamma distribution shapes
#' @param rates  a vector of gamma distribution rates
#' @param weights  a vector of weights
#' @param na.rm remove distributions which have NA for shape or rate
#'
#' @return the pdf of the mixture distribution.
#' @keywords internal
#' @concept distributions
#'
#' @examples
#' try({
#'   .pmixgamma(q=c(2,20), shapes=c(10,13,14), rates=c(1,1,1), weights=c(2,2,3))
#' })
.pmixgamma = function(q, shapes, rates, weights=rep(1,length(shapes)), na.rm=FALSE) {
  return(.pmix("gamma",q,shapes,rates,weights,na.rm))
}

#' A quantile function for a mixture of gamma distributions
#'
#' @param p vector of probabilities.
#' @param shapes a vector of gamma distribution shapes
#' @param rates  a vector of gamma distribution rates
#' @param weights  a vector of weights
#' @param na.rm remove distributions with NA values for mean or sd
#' @param approx use Cornish Fisher moment based approximation if available
#' @param ... passed on to internal function, `samples=TRUE` to force random sampling
#'
#' @return the value of the `p`th quantile
#' @keywords internal
#' @concept distributions
#'
#' @examples
#' try({
#'   .qmixgamma(p=c(0.025,0.5,0.975), shapes=c(10,13,14), rates=c(1,1,2))
#' })
.qmixgamma = function(p, shapes, rates, weights=rep(1,length(shapes)), na.rm=FALSE, approx = TRUE,...) {
  return(.qmix("gamma",p, shapes,rates,weights,na.rm,approx,...))
}


# mix betas ----

#' The cumulative density function of a mixture of beta distributions
#'
#' @param q vector of quantiles.
#' @param alphas a vector of beta distribution alphas
#' @param betas  a vector of gamma distribution betas
#' @param weights  a vector of weights
#' @param na.rm remove distributions which have NA for shape or rate
#'
#' @return the pdf of the mixture distribution.
#' @keywords internal
#' @concept distributions
#'
#' @examples
#' try({
#'   .pmixbeta(q=c(2,20), alphas=c(10,13,14), betas=c(1,1,1), weights=c(2,2,3))
#' })
.pmixbeta = function(q, alphas, betas, weights=rep(1,length(alphas)), na.rm=FALSE) {
  return(.pmix("beta",q,alphas,betas,weights,na.rm))
}

#' A quantile function for a mixture of gamma distributions
#'
#' @param p vector of probabilities.
#' @param alphas a vector of gamma distribution shapes
#' @param betas  a vector of gamma distribution rates
#' @param weights  a vector of weights
#' @param na.rm remove distributions with NA values for mean or sd
#' @param approx use Cornish Fisher moment based approximation if available
#' @param ... passed on to internal function, `samples=TRUE` to force random sampling
#'
#' @return the value of the `p`th quantile
#' @keywords internal
#' @concept distributions
#'
#' @examples
#' try({
#'   .qmixbeta(p=c(0.025,0.5,0.975), alphas=c(10,13,14), betas=c(1,1,2))
#' })
.qmixbeta = function(p, alphas, betas, weights=rep(1,length(alphas)), na.rm=FALSE, approx = TRUE,...) {
  return(.qmix("beta",p, alphas,betas,weights,na.rm,approx,...))
}

# internal utilities ----

.allEqual = function(means, tol = 10^{-10}) {
  if (any(is.na(means))) return(all(is.na(means)))
  return((max(means)-min(means))<tol)
}

# scope of this function is for a single type of distribution. a vector of x
# inputs and a probably different length vector of param1,param2,weights describing the
# (single) mixture distribution. (N.B. the multiple inputs is required for the
# solver.)
#' Title
#'
#' @param dist either a function e.g. `pnorm` or a character e.g. `"norm"`
#' @param x a vector of values for assess for `P(X<x)`
#' @param param1 the first parameter of `dist`
#' @param param2 the second parameter of `dist`
#' @param weights the weighting of the mixture (defaults to uniform)
#' @param na.rm remove `NA` values from the distributions (in `param1`, `param2`, or `weights`)
#'
#' @return a vector of probabilities either the same length as input `x` or if
#'   `x` is length 1 and `param1` is a list, then length of `param1`
#' @keywords internal
#' @noRd
#'
#' @examples
#' # a single mixture with vectorised input:
#' .pmix("norm", seq(-5,5,1), param1=c(-1,0,1), param2=c(1,1,1))
#' # a dataframe of mixture distributions
#' tmp = tibble(
#'   x = 1:3,
#'   param1 = list(c(1,2),c(3,4,5),c(6,7,8,9)),
#'   param2 = list(1,2,3)
#' )
#' tmp %>% mutate(
#'   pX = .pmix(pnorm, x, param1, param2),
#'   qX = .qmixnorm(pX, param1,param2)
#' ) %>% glimpse()
#'
#' # same as last row above
#' .pmix("norm", 3, param1=c(6,7,8,9), param2=3)
#'
.pmix = function(dist, x, param1, param2, weights=1, na.rm=FALSE) {

  if (is.function(dist)) {
    pfn=dist
  } else {
    pfn = switch(dist,
                 norm = stats::pnorm,
                 lnorm = stats::plnorm,
                 gamma = stats::pgamma,
                 beta = stats::pbeta,
                 # default:
                 get0(sprintf("p%s",dist),mode = "function")
    )
  }

  if (is.list(param1)) {
    # if param1 is a list then this is a set of mixture distributions.
    # this is OK if the length of x is 1 or the same as param1,param2,weights
    interfacer::recycle(x, param1, param2, weights)
    return(sapply(seq_along(x), function(i) {
      .pmix(dist, x[i], param1[[i]], param2[[i]], weights[[i]], na.rm)
    }))
  }

  # so param1, param2, and weights are expected to be numeric vectors at this point.
  # x could be any length. weights might be length 1 or the
  # same length as param1
  n = interfacer::recycle(param1,param2,weights)
  filt = is.na(param1) | is.na(param2) | is.na(weights)
  if (na.rm) {
    param1 = param1[filt]
    param2 = param2[filt]
    weights = weights[filt]
  } else {
    if (any(filt)) return(rep(NA_real_,length(x)))
  }
  weights = weights/sum(weights)

  if (
    # only one distribution in the mixture:
    length(param1) == 1 || (
    # all the distributions in the mixture are the same:
    .allEqual(param1) && .allEqual(param2)
  )) {
    return(pfn(x,param1[1],param2[1]))
  }

  tmp = sapply(
    seq_along(param1),
    FUN = function(i) {
      return(pfn(x, param1[i], param2[i]))
    }
  )

  if (is.matrix(tmp)) {
    tmp = apply(tmp,MARGIN=1,mean)
  } else {
    tmp = mean(tmp)
  }
  return(tmp)
}


# solve
# This function is vectorised on p and parameters. as mixture parameters
# must be a list of vectors.
.qmix = function(dist, p, param1, param2, weights=1, na.rm=FALSE, approx=TRUE, samples=FALSE) {

  if (is.function(dist)) {
    qfn=dist
  } else {
    qfn = switch(dist,
                 norm = stats::qnorm,
                 lnorm = stats::qlnorm,
                 gamma = stats::qgamma,
                 beta = stats::qbeta,
                 # default:
                 get0(sprintf("q%s",dist),mode = "function")
    )
  }

  if (is.list(param1)) {
    # input is a list of mixtures. p should be length 1 or the same as param1
    # this supports the dataframe use case.
    interfacer::recycle(p,param1,param2,weights)
    return(sapply(seq_along(p), function(i) {
      .qmix(dist, p[i], param1[[i]], param2[[i]], weights[[i]], na.rm)
    }))
  }

  # This is the vectorised single mixture use case
  # so param1, param2, and weights are expected to be numeric vectors at this point.
  # p could be any length. weights might be length 1 or the
  # same length as param1
  n = interfacer::recycle(param1,param2,weights)
  weights = weights/sum(weights)

  filt = is.na(param1) | is.na(param2) | is.na(weights)
  if (na.rm) {
    param1 = param1[filt]
    param2 = param2[filt]
    weights = weights[filt]
  } else {
    if (any(filt)) return(rep(NA_real_,length(p)))
  }

  # fall back to direct evaluation if `mixture` of 1 distribution.
  if (length(param1) == 1 || (
    .allEqual(param1) && .allEqual(param2)
  )) {
    return(qfn(p,param1,param2))
  }

  # so we need to do the quantile(s) here for the single mixture with params1/2
  # Options are: solve using uniroot for each quantile (non vectorised)
  # calculate moments and use PDQutils to estimate with Cornish-Fisher expansion

  if (!approx) {
    # solve the mixture cdf for the quantiles
    out = sapply(p, function(p1) {
      minmax = range(qfn(p1,param1,param2))
      cdf = function(x) .pmix(dist, x, param1, param2) - p1
      stats::uniroot(
        cdf,
        lower=minmax[1],
        upper = minmax[2],
        tol = 0.01
      )$root
    })
  } else {
    if (.is_analytical(dist) && !samples) {
      # moment based approach.
      # calculate analytical moments for sum:
      moments = .mixture_moments_vectorised(dist,param1,param2, weights)
      skewness = (moments[3] - 3*moments[1]*moments[2] + 2*moments[1]^3) / (moments[2] - moments[1]^2)^1.5
      # kurtosis = (moments[4] + 4*moments[3]*moments[1] + 6*moments[2]*moments[1]^2-3*moments[1]^4) / (moments[2] - moments[1]^2)^2
      if(abs(skewness) > 3) {
        # warn(paste0("falling back to slow quantiles due to excess skewness: ",skewness))
        return(.qmix(dist,p,param1,param2,weights,na.rm,approx=TRUE,samples=TRUE))
      }
      # if(abs(kurtosis) > 5) {
      #   warn(paste0("falling back to slow quantiles due to excess kurtosis: ",kurtosis))
      #   return(.qmix(dist,p,param1,param2,weights,na.rm,approx=FALSE))
      # }
      raw_cumulants = PDQutils::moment2cumulant(moments)
      out = PDQutils::qapx_cf(p, raw.cumulants = raw_cumulants, support=.support(dist))
      if (!identical(order(out), order(p)))
        return(.qmix(dist,p,param1,param2,weights,na.rm,approx=FALSE,samples=TRUE))
    } else {
      # sampling.
      samples = numeric(length = 50000)
      pointer = 1
      for (i in seq_along(param1)) {
        n_sample = floor(50000*weights[[i]])
        rsample = qfn(stats::runif(n_sample), param1[[i]], param2[[i]])
        samples[pointer:(pointer+n_sample-1)] = rsample
        pointer = pointer+n_sample
      }
      out = stats::quantile(samples[1:(pointer-1)], probs=p)
    }
  }

  names(out) = paste0("q.",p)
  return(out)
}

.is_analytical = function(dist) {
  if (!is.character(dist)) return(FALSE)
  return(dist %in% c("norm","lnorm","gamma","beta"))
}

.support = function(dist) {
  switch(dist,
         norm = c(-Inf,Inf),
         lnorm = c(0,Inf),
         gamma = c(0,Inf),
         beta = c(0,1)
  )
}

# .mixture_moments_vectorised("norm", c(-1,0,1), c(1,1,1), c(1,1,1)/3)
# .mixture_moments_vectorised("lnorm", c(-1,0,1), c(1,1,1), c(1,1,1)/3)
# .mixture_moments_vectorised("gamma", c(1,2,3), c(1,1,1), c(1,1,1)/3)
.mixture_moments_vectorised = function(dist_type, param1, param2, weights) {
  if(dist_type == "norm") {
    # Normal distribution moments
    mu <- param1
    sigma <- param2

    arr = cbind(
        mu,
        mu^2 + sigma^2,
        mu^3 + 3*mu*sigma^2,
        mu^4 + 6*mu^2*sigma^2 + 3*sigma^4,
        mu^5 + 10*mu^3*sigma^2 + 15*mu*sigma^4,
        mu^6 + 15*mu^4*sigma^2 + 45*mu^2*sigma^4 + 15*sigma^6
    )

  } else if(dist_type == "gamma") {
    # Gamma distribution moments
    shape <- param1
    rate <- param2
    scale <- 1/rate

    # E[X^k] = scale^k * Gamma(shape + k) / Gamma(shape)
    arr = sapply(1:6, \(k) scale^k * gamma(shape + k) / gamma(shape))

  } else if(dist_type == "lnorm") {
    # Log-normal distribution moments
    meanlog <- param1
    sdlog <- param2

    # E[X^k] = exp(k*meanlog + k^2*sdlog^2/2)
    arr = sapply(1:6, \(k) exp(k * meanlog + k^2 * sdlog^2 / 2))

  } else if(dist_type == "beta") {
    # Beta distribution moments (assuming support [0,1])
    alpha <- param1
    beta <- param2

    # E[X^k] = B(alpha+k, beta) / B(alpha, beta)
    arr = sapply(1:6, \(k) beta(alpha + k, beta) / beta(alpha, beta))

  } else if(dist_type == "weibull") {
    # Weibull distribution moments
    shape <- param1
    scale <- param2

    # E[X^k] = scale^k * Gamma(1 + k/shape)
    arr = sapply(1:6, \(k) scale^k * gamma(1 + k/shape))

  } else {
    # Numerical integration fallback for unknown distributions
    stop("Analytical moments not defined: ", dist_type)
  }

  arr = apply(arr, MARGIN = 2, \(x) sum(x*weights))
  return(arr)
}
