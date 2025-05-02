#' Generate a Sample and Create a Bar Plot
#'
#' This function randomly selects numbers from 1 to 10 with replacement
#' and creates a bar plot showing the relative frequencies.
#'
#' @param n Sample size.
#' @param iter Number of iterations (default: 1).
#' @param time Pause time between iterations in seconds (default: 1).
#'
#' @return A bar plot of relative frequencies.
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#' @export
#'
#' @examples
#' SPRING254753nguy0700::mysample(n = 1000, iter = 30)
mysample <- function(n, iter = 1, time = 1) {
  for (i in 1:iter) {
    s <- sample(1:10, n, replace = TRUE)
    sf <- factor(s, levels = 1:10)
    barplot(table(sf) / n, beside = TRUE, col = rainbow(10),
            main = paste("Sample Iteration:", i, "n =", n),
            ylim = c(0, 0.2))
    Sys.sleep(time)
  }
}
