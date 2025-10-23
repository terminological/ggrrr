#' # #' Quantile Function Estimator Using Logit-Beta Kernels
#' #'
#' #' Constructs a smooth quantile function estimator based on a weighted sum of
#' #' logit-Beta kernels centered at empirical quantiles. For a data vector `x`,
#' #' this returns a function `q(p)` that estimates the inverse CDF (quantile
#' #' function) at probability `p` using Bayesian-smoothed ranks.
#' #'
#' #' The method treats the rank-\code{r} order statistic \eqn{x_{(r)}} as being
#' #' associated with a Beta posterior on its quantile level:
#' #' \deqn{\alpha_r = r + 1,\quad \beta_r = n - r + 1}
#' #' (Laplace smoothing / uniform prior).
#' #'
#' #' In logit space (\eqn{z = \log(p / (1 - p))}), the kernel density for rank r is:
#' #' \deqn{k_r(z) = \frac{e^{\alpha_r z}}{(1 + e^z)^{\alpha_r + \beta_r} B(\alpha_r, \beta_r)}}
#' #' which is the PDF of the logit-transformed Beta(\eqn{\alpha_r, \beta_r}) distribution.
#' #'
#' #' The quantile estimate at logit-quantile \eqn{z} is then a Nadaraya-Watson-type
#' #' local average:
#' #' \deqn{\hat{Q}(z) = \frac{\sum_{r=1}^n x_{(r)} \cdot k_r(z)}{\sum_{r=1}^n k_r(z)}}
#' #'
#' #' @param x A numeric vector of observed data (will be sorted internally).
#' #' @return A function `q(p)` where `p` is a numeric vector in (0,1). The function
#' #'   returns estimated quantiles using the logit-Beta kernel smoother.
#' #' @examples
#' #' set.seed(123)
#' #' x <- rnorm(50)
#' #' qfun <- quantile_logit_beta(x)
#' #' p <- seq(0.01, 0.99, length.out = 100)
#' #' q_est <- qfun(p)
#' #' plot(p, q_est, type = "l", main = "Logit-Beta Quantile Estimate")
#' #' points((1:50)/51, sort(x), col = "red", pch = 16, cex = 0.6) # plotting positions
#' #' @export
#' quantile_logit_beta <- function(x) {
#'   if (!is.numeric(x) || length(x) < 2) {
#'     stop("Input 'x' must be a numeric vector with at least 2 elements.")
#'   }
#'
#'   x <- sort(x)
#'   n <- length(x)
#'   r <- seq_len(n) # ranks 1 to n
#'
#'   # Smoothed Beta parameters (Laplace: +1 to both)
#'   alpha <- r + 1
#'   beta <- n - r + 1
#'
#'   # Precompute log(B(alpha, beta)) for stability
#'   log_B_ab <- lbeta(alpha, beta)
#'
#'   # Return quantile function
#'   qfun <- function(p) {
#'     if (any(p <= 0 | p >= 1)) {
#'       stop("p must be in (0, 1)")
#'     }
#'
#'     z <- qlogis(p) # logit(p) = log(p / (1 - p))
#'
#'     # Compute log-kernel values to avoid overflow
#'     # log(k_r(z)) = alpha_r * z - (alpha_r + beta_r) * log(1 + exp(z)) - log(B(alpha_r, beta_r))
#'     # Use log1p(exp(z)) for stability when z is large
#'     log1p_exp_z <- ifelse(
#'       z > 0,
#'       z + log1p(exp(-z)), # stable for z >> 0
#'       log1p(exp(z))
#'     ) # stable for z << 0
#'
#'     log_kernel <- alpha * z - (alpha + beta) * log1p_exp_z - log_B_ab
#'
#'     # Normalize in log-space for numerical stability
#'     log_kernel_max <- max(log_kernel)
#'     w_norm <- exp(log_kernel - log_kernel_max) # proportional weights
#'     weights <- w_norm / sum(w_norm)
#'
#'     # Weighted average of order statistics
#'     as.numeric(crossprod(weights, x))
#'   }
#'
#'   return(qfun)
#' }
