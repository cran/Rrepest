#' Weighted Bivariate Correlation
#' @description Compute weighted pearson correlation coefficient of two numeric vectors
#' @param x (numeric vector) variable from where to get correlation
#' @param y (numeric vector) variable from where to get correlation
#' @param w (numeric vector) vector of weights
#' @param na.rm (bool) True: NAs be stripped before computation proceeds
#'
#' @return Pearson correlation coefficient
#' @export
#'
#' @examples
#' data(df_talis18) 
#' 
#' weighted.corr(x = df_talis18$T3STAKE, y = df_talis18$T3TEAM, w = df_talis18$TCHWGT)
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
