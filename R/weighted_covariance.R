#' Weighted Bivariate Covariance
#' @description Compute weighted covariance coefficient of two numeric vectors
#' @param x (numeric vector) variable from where to get covariance
#' @param y (numeric vector) variable from where to get covariance
#' @param w (numeric vector) vector of weights
#' @param na.rm (bool) True: NAs be stripped before computation proceeds
#'
#' @return Pearson correlation coefficient
#' @export
#'
#' @examples
#' data(df_talis18) 
#' 
#' weighted.cov(x = df_talis18$T3STAKE, y = df_talis18$T3TEAM, w = df_talis18$TCHWGT)
weighted.cov <- function(x, y, w, na.rm = TRUE) {
  # Goal: Get weighted covariance
  # ------ INPUTS ------.
  # x: (vector) variable from where to get correlation
  # y: (vector) variable from where to get correlation
  # w : (vector) vector of weights
  
  # Use non NAs in both x and y
  use <- !is.na(x) & !is.na(y)
  x <- x[use]
  y <- y[use]
  w <- w[use]
  
  # Covariance
  w.mean <- weighted.mean(x, w, na.rm = na.rm)
  cov.w <- sum(w * (x - w.mean) * (y - w.mean), na.rm = na.rm)/sum(w, na.rm = na.rm)
  return(cov.w)
}
