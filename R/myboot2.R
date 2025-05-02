#' Bootstrapping function
#'
#' Performs bootstrap sampling of a statistic from a numeric vector.
#'
#' @param x A numeric vector.
#' @param fun A function to apply to each bootstrap sample.
#' @param nboot Number of bootstrap samples. Default is 1000.
#' @param ... Additional arguments passed to \code{fun}.
#'
#' @return A vector of bootstrap estimates.
#' @examples
#' x <- rnorm(100)
#' boot_means <- myboot2(x, mean)
#' hist(boot_means)
#' @importFrom stats quantile
#' @export

myboot2 <- function(x, fun, nboot = 1000, ...) {
  n <- length(x)
  boot_estimates <- numeric(nboot)
  for (i in 1:nboot) {
    resample <- sample(x, size = n, replace = TRUE)
    boot_estimates[i] <- fun(resample, ...)
  }
  return(boot_estimates)
}
