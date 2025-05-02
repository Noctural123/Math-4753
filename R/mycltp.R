#' Simulate sample means from a binomial distribution
#'
#' This function simulates sample means from a binomial distribution
#' and plots their histogram.
#'
#' @param n Sample size
#' @param iter Number of iterations
#' @param p Probability of success in each trial
#'
#' @return A histogram of the sample means is plotted. Invisibly returns the sample means as a numeric vector.
#'
#' @examples
#' mycltb(n = 10, iter = 1000, p = 0.5)
#'
#' @importFrom graphics hist
#' @importFrom stats rbinom
#' @export
mycltb <- function(n, iter, p) {
  y <- rbinom(n * iter, size = n, prob = p)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  w <- apply(data, 2, mean)
  hist(w, freq = FALSE,
       main = paste("Binomial Sample Mean\nSample Size =", n, "p =", p),
       xlab = "Sample Mean")
  invisible(w)
}
