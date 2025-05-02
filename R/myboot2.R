#' Bootstrap Confidence Interval Function
#'
#' Generates bootstrap confidence intervals for a given statistic.
#'
#' @param x A numeric vector of data to bootstrap.
#' @param fun The function to compute the statistic (default: `"mean"`).
#' @param alpha Confidence level (default: `0.05` for 95% CI).
#' @param nboot Number of bootstrap samples (default: `1000`).
#' @param ... Additional arguments passed to `fun`.
#'
#' @return A list containing:
#'   - `statistic`: Original statistic.
#'   - `CI`: Bootstrap confidence interval.
#'   - `boot_stats`: Bootstrap distribution.
#'
#' @examples
#' myboot2(x = rnorm(100))
#' @export
myboot2 <- function(x, fun = "mean", alpha = 0.05, nboot = 1000, ...) {
  # Calculate original statistic
  stat <- match.fun(fun)(x, ...)

  # Generate bootstrap samples
  boot_stats <- replicate(nboot, {
    x_sample <- sample(x, replace = TRUE)
    match.fun(fun)(x_sample, ...)
  })

  # Compute confidence interval
  ci <- quantile(boot_stats, c(alpha/2, 1 - alpha/2))

  # Return results
  list(
    statistic = stat,
    CI = ci,
    boot_stats = boot_stats
  )
}
