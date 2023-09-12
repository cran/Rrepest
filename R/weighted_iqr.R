#' Weighted Interquantile Range
#'
#' @description Compute interquantile range
#' @param x (numeric vector) variable from where to get quantiles
#' @param w (numeric vector) vector of weights
#' @param rang (numeric vector) two numbers indicating the range of the quantiles
#'
#' @return Interquantile range
#' @export
#'
#' @examples
#' weighted.iqr(x = mtcars$mpg, w = mtcars$wt,  rang = c(.5,.9))
#'   
weighted.iqr <- function(x, w = rep(1, length(x)), rang = c(0.25,0.75)) {
  # Goal: Get weighted interquantile range
  # ------ INPUTS ------.
  # x: (vector) variable from where to get quantiles
  # w : (vector) vector of weights
  # rang : (vector) two numbers indicating the range
  
  res <- weighted.quant(x, q=max(rang), w) - 
    weighted.quant(x, q=min(rang), w)
  
  return(res)
}
