utils::globalVariables("x")

#' Normal Curve with Shaded Area
#'
#' Plots a normal curve with mean `mu` and standard deviation `sigma`,
#' shades the area from `-∞` to `a`, and returns the probability P(X ≤ a).
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation.
#' @param a Shading cutoff point.
#'
#' @return A list with `mu`, `sigma`, and `probability`.
#'
#' @examples
#' # myncurve(mu = 10, sigma = 5, a = 6)
#'
#' @importFrom stats dnorm pnorm
#' @importFrom graphics curve polygon
#' @export
#'
myncurve <- function(mu, sigma, a) {
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        main = paste("Normal Curve (mu =", mu, ", sigma =", sigma, ")"),
        ylab = "Density", col = "blue", lwd = 2)

  x_vals <- seq(mu - 3 * sigma, a, length = 1000)
  y_vals <- dnorm(x_vals, mean = mu, sd = sigma)
  polygon(c(min(x_vals), x_vals, a), c(0, y_vals, 0), col = "lightblue", border = "blue")

  probability <- pnorm(a, mean = mu, sd = sigma)

  return(list(mu = mu, sigma = sigma, probability = probability))
}
