#' Weighted inter-quantile range
#'
#' @description Computes the weighted inter-quantile range of a numeric vector.
#' @param x (numeric vector) Variable of interest for which to compute the inter-quantile range
#' @param w (numeric vector) Vector with the weights
#' @param rang (numeric vector) Two numbers between 0 and 1 indicating the desired inter-quantile range
#'
#' @return Scalar containing the inter-quantile range
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
