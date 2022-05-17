

#' The cumulative density function of a mixture of normal distributions
#'
#' @param q vector of quantiles.
#' @param means a vector of normal distribution means
#' @param sds  a vector of normal distribution sds
#' @param weights  a vector of weights
#'
#' @return the pdf of the mixture distribution.
#' @export
#' @examples
#' pmixnorm(q=c(2,20), means=c(10,13,14), sds=c(1,1,2), weights=c(2,2,3))
pmixnorm = function(q, means, sds, weights=rep(1,length(means))) {
  m = weights/sum(weights) * sapply(q, function(x) pnorm(x, means, sds))
  apply(m,MARGIN=2,sum)
}

.allEqual = function(means, tol = 10^{-10}) all(sapply(means, function(x) abs(means - x) < tol))



#' A quantile function for a mixture of normal distributions
#'
#' @param p vector of probabilities.
#' @param means a vector of normal distribution means
#' @param sds  a vector of normal distribution sds
#' @param weights  a vector of weights
#'
#' @return the value of the yth quantile
#' @export
#'
#' @examples
#' qmixnorm(p=c(0.025,0.5,0.975), means=c(10,13,14), sds=c(1,1,2))
qmixnorm = function(p, means, sds, weights=rep(1,length(means))) {
  if (.allEqual(means) & .allEqual(sds)) {
    return(qnorm(p,means[1],sds[1]))
  }
  # the min minmax below is computed to supply a range to the solver
  # the solution must be between the min and max
  # quantile of the mixed distributions
  minmax <- range(sapply(p, function(x) qnorm(x,means,sds)))
  solve = tryCatch({
    sapply(p, function(qPrime) uniroot(function(x) pmixnorm(x,means,sds,weights)-qPrime,interval = minmax,tol = 10^{-16})$root)
  }, error = browser)
  names(solve) = paste0("Q.",p)
  return(solve)
}


#' Sprintf with a list input
#'
#' @param format the format string
#' @param params the inputs as a list (rather than as a set of individual numbers)
#'
#' @return the formatted string
#' @export
#'
#' @examples
#' # generate a mixture confidence interval from a set of distributions
#' sprintf_list("%1.2f [%1.2f\u2013%1.2f]",qmixnorm(p=c(0.5,0.025,0.975), means=c(10,13,14), sds=c(1,1,2)))
sprintf_list = function(format, params, na.replace="\u2015") {
  if(any(is.na(params))) return(na.replace)
  do.call(sprintf, c(as.list(format), as.list(params)))
}

