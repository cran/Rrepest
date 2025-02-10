#' Weighted bivariate correlation
#' 
#' @description Computes the weighted Pearson correlation coefficient of two numeric vectors.
#' @param x (numeric vector) Variable of interest x for computing the correlation
#' @param y (numeric vector) Variable of interest y for computing the correlation
#' @param w (numeric vector) Vector with the weights
#' @param na.rm (bool) if TRUE: Excludes missing values before computing the correlation
#'
#' @return Scalar containing the Pearson correlation coefficient
#' @export
#'
#' @examples
#' data(df_talis18) 
#' 
#' weighted.corr(x = df_talis18$t3stake, y = df_talis18$t3team, w = df_talis18$tchwgt)
weighted.corr <- function(x, y, w, na.rm = TRUE) {
  # Goal: Get weighted correlation
  # ------ INPUTS ------.
  # x: (vector) variable from where to get correlation
  # y: (vector) variable from where to get correlation
  # w : (vector) vector of weights
  
  # Use non NAs in both x and y
  use <- !is.na(x) & !is.na(y)
  x <- x[use]
  y <- y[use]
  w <- w[use]
  
  # Calculate variance and covariance of x and y
  w.mean.x <- weighted.mean(x, w, na.rm = na.rm)
  w.mean.y <- weighted.mean(y, w, na.rm = na.rm)
  cov.w <- sum(w*(x-w.mean.x)*(y-w.mean.y), na.rm = na.rm)
  var.w.x <- sum(w*(x-w.mean.x)^2, na.rm = na.rm)
  var.w.y <- sum(w*(y-w.mean.y)^2, na.rm = na.rm)
  
  return(cov.w / sqrt(var.w.x * var.w.y))
}
