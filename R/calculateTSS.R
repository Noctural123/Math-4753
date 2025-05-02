#' Calcuatling TSS
#'
#' This function is for task 7 on lab 4.
#'
#' @param data A dataframe containing column 'Height'
#'
#' @return TSS Value
#'
#' @examples
#' # calculateTSS(spruce)
#'
#' @export

calculateTSS <- function(data) {
  TSS <- sum((data$Height - mean(data$Height))^2)
  return(TSS)
}
