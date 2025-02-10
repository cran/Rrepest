#' Weighted variance
#' 
#' @description Computes the weighted variance of a numeric vector.
#' @param x (numeric vector) Variable of interest for which to compute the variance
#' @param w (numeric vector) Vector with weights
#' @param na.rm (bool) if TRUE: Excludes missing values before computing the variance
#'
#' @return Scalar containing the variance
#' @export
#'
#' @examples
#' data(df_talis18) 
#' 
#' weighted.var(df_talis18$TT3G02, df_talis18$TRWGT1)
weighted.var <- function(x, w, na.rm = TRUE) {
  # Goal: Obtained weighted variance de4of a vector
  # ------ INPUTS ------.
  # x : (vector) variable to analyze
  # w : (vector) vector of weights
  # na.rm : (bool) if TRUE (default) remove missing values.
  
  w.var <- sum(w*(x-weighted.mean(x,w,na.rm = na.rm))^2, na.rm = na.rm)/sum(w,na.rm = na.rm)
  return(w.var)
}
