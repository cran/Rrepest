#' Weighted bivariate covariance
#' 
#' @description Computes the weighted covariance coefficient of two numeric vectors.
#' @param x (numeric vector) Variable of interest x for computing the covariance
#' @param y (numeric vector) Variable of interest y for computing the covariance
#' @param w (numeric vector) Vector with the weights
#' @param na.rm (bool) if TRUE: Excludes missing values before computing the covariance
#'
#' @return Scalar containing the covariance
#' @export
#'
#' @examples
#' data(df_talis18) 
#' 
#' weighted.cov(x = df_talis18$t3stake, y = df_talis18$t3team, w = df_talis18$tchwgt)
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
