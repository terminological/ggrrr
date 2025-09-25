#' Mixture of normal distribution PDFs
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @inherit .pmixnorm
#' @export
#'
#' @examples
#' pmixnorm(q=c(2,20), means=c(10,13,14), sds=c(1,1,2), weights=c(2,2,3))
pmixnorm = .pmixnorm

#' Mixture of normal distribution quantiles
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @inherit .qmixnorm
#' @export
#'
#'
#' @examples
#' qmixnorm(p=c(0.025,0.5,0.975), means=c(10,13,14), sds=c(1,1,2))
qmixnorm = .qmixnorm

#' Sprintf with a list input
#'
#' A variant of sprintf that work well with inputs that are in the format of a list.
#' Good examples of which are the quantile functions
#'
#' @param format the format string
#' @param params the inputs as a list (rather than as a set of individual numbers)
#' @param na.replace a value to replace NA values with.
#'
#' @return the formatted string
#' @export
#'
#'
#' @examples
#' # generate a mixture confidence interval from a set of distributions
#' sprintf_list("%1.2f [%1.2f\u2013%1.2f]",
#'  qmixnorm(p=c(0.5,0.025,0.975),
#'  means=c(10,13,14), sds=c(1,1,2)))
sprintf_list = function(format, params, na.replace = "\u2015") {
  if (any(is.na(params))) {
    return(na.replace)
  }
  do.call(sprintf, c(as.list(format), as.list(params)))
}
