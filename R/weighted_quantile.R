#' Weighted quantile
#' 
#' @description Computes weighted quantiles of a numeric vector.
#' @param x (numeric vector) Variable of interest for which to compute the quantile
#' @param w (numeric vector) Vector with the weights
#' @param q (numeric vector) A number between 0 and less than 1 indicating the desired quantile
#'
#' @return Scalar containing the quantile
#' @export
#'
#' @examples
#' weighted.quant(x = mtcars$mpg, w = mtcars$wt,  q = seq(.1,.9,.1))
#' 

weighted.quant <- function(x, w =  rep(1, length(x)), q = 0.5) {
  # Goal: Get weighted quantiles
  # ------ INPUTS ------.
  # x: (numeric vector) variable from where to get quantiles
  # w : (numeric vector) vector of weights
  # q : (numeric vector) [0,1] for the quantile to get
  
  # Apply function over vector of quants
  res.v <- sapply(q, function(q_i){
    #Only non NA data
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
    #Order ascending both
    ord <- order(x)
    w <- w[ord]
    x <- x[ord]
    #Running sum
    w.sum <- cumsum(w)
    
    #Get x_i s.t. max Σw_i <= W*q
    res <- head(x[!(w.sum <= sum(w)*q_i)], n=1)
    
    return(res)
  })
  
  names(res.v) <- q
  
  return(res.v)
  
}
