#' Maximum Likelihood Estimation for Beta Distribution
#'
#' This function computes the maximum likelihood estimates for the parameters
#' \eqn{alpha} and \eqn{beta} of the Beta distribution based on resampled data.
#'
#' @param x A numeric vector containing the sample data.
#' @importFrom stats dbeta
#' @param alpha A numeric vector of possible \eqn{alpha} values.
#' @param beta A numeric vector of possible \eqn{beta} values.
#' @param ... Additional arguments passed to the plot function.
#'
#' @return A list containing the maximum likelihood estimates for \eqn{alpha} and \eqn{beta},
#' the log-likelihood values, and the plot of the likelihood surface.
#'
#' @export
estimate_beta_mle <- function(x, alpha, beta, ...) {
  na = length(alpha)
  nb = length(beta)
  n = length(x)
  zz = matrix(NA, nrow = na, ncol = nb)

  lfun = function(x, a, b) log(dbeta(x, shape1 = a, shape2 = b))

  for (j in 1:nb) {
    z = outer(x, alpha, lfun, b = beta[j])
    y = apply(z, 2, sum)
    zz[, j] = y
  }

  maxl = max(exp(zz), na.rm = TRUE)
  coord = which(exp(zz) == maxl, arr.ind = TRUE)

  aest = alpha[coord[1]]
  best = beta[coord[2]]

  return(list(x = x, coord = coord, maxl = maxl, maxalpha = aest, maxbeta = best, zz = zz))
}
