#' Mode
#' @description Calculate the arithmetic mode of a vector. If multiple elements 
#' have the same frequency, all of them will be displayed.
#' @param x (numeric vector) vector from which we'll obtain the mode
#' @param w (numeric vector) vector of weights. If not provided, it defaults to non-weighted mode.
#'
#' @return (numeric vector) one or multiple elements that will be the arithmetic mode
#' @export
#'
#' @examples
#' weighted.mode(c(1,2,3,4,5,4,3,4,5,3))
#' weighted.mode(c(NA,1,3,NA))
weighted.mode <- function(x, w = rep(1,length(x))) {
  df <- data.frame(x, w)
  res <- aggregate(w ~ x, df, sum)
  res$x[res$w==max(res$w)]
}