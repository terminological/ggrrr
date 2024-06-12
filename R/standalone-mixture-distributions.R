# ---
# repo: terminological/ggrrr
# file: standalone-mixture-distributions.R
# last-updated: 2024-06-06
# license: https://unlicense.org
# imports:
#   - stats
# ---
# This set of functions are stats distributions and an anomaly here in ggrrr.

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

  r = p = NULL  # remove global binding note

  if(length(sds) != length(means)) stop("means and sds vectors must be the same length")

  if (na.rm == TRUE) {
    filt = is.na(means) | is.na(sds)
    means = means[filt]
    sds = sds[filt]
  }

  if (any(is.na(means)) | length(means)==0 | any(is.na(sds)) | length(sds)==0 ) {
    return(rep(NA_real_, length(r)))
  }

  if (.allEqual(means) & .allEqual(sds)) {
    return(stats::pnorm(p,means[1],sds[1]))
  }

  m = weights/sum(weights) * sapply(q, function(x) stats::pnorm(x, means, sds))
  apply(m,MARGIN=2,sum)

}

.allEqual = function(means, tol = 10^{-10}) all(sapply(means, function(x) abs(means - x) < tol))

#' A quantile function for a mixture of normal distributions
#'
#' @param p vector of probabilities.
#' @param means a vector of normal distribution means
#' @param sds  a vector of normal distribution sds
#' @param weights  a vector of weights
#' @param na.rm remove distributions with NA values for mean or sd
#'
#' @return the value of the yth quantile
#' @keywords internal
#' @concept distributions
#'
#' @examples
#' try({
#' .qmixnorm(p=c(0.025,0.5,0.975), means=c(10,13,14), sds=c(1,1,2))
#' })
.qmixnorm = function(p, means, sds, weights=rep(1,length(means)), na.rm=FALSE) {

  if(length(sds) != length(means)) stop("means and sds vectors must be the same length")

  if (na.rm == TRUE) {
    filt = is.na(means) | is.na(sds)
    means = means[filt]
    sds = sds[filt]
  }

  if (any(is.na(means)) | length(means)==0 | any(is.na(sds)) | length(sds)==0 ) {
    solve = rep(NA_real_, length(p))
    names(solve) = paste0("q.",p)
    return(solve)
  }

  if (.allEqual(means) & .allEqual(sds)) {
    return(stats::qnorm(p,means[1],sds[1]))
  }
  # the min minmax below is computed to supply a range to the solver
  # the solution must be between the min and max
  # quantile of the mixed distributions
  minmax <- range(sapply(p, function(x) stats::qnorm(x,means,sds)))
  solve = tryCatch({
    sapply(p, function(qPrime) stats::uniroot(function(x) .pmixnorm(x,means,sds,weights)-qPrime,interval = minmax,tol = 10^{-16})$root)
  }, error = browser)
  names(solve) = paste0("q.",p)
  return(solve)
}


