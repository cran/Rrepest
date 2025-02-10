#' Weighted standard deviation
#' 
#' @description Computes the weighted standard deviation of a numeric vector.
#' @param x (numeric vector) Variable of interest for which to compute the standard deviation.
#' @param w (numeric vector) Vector with the weights.
#' @param na.rm (bool) if TRUE: Excludes missing values before computing the standard deviation
#'
#' @return Scalar containing the standard deviation
#' @export
#'
#' @examples 
#' data(df_talis18)
#' 
#' weighted.std(df_talis18$TT3G02, df_talis18$TRWGT1)

weighted.std <- function(x, w, na.rm = TRUE) {
  # Goal: Obtained weighted standard deviation of a vector
  # ------ INPUTS ------.
  # x : (vector) variable to analyze
  # w : (vector) vector of weights
  # na.rm : (bool) if TRUE (default) remove missing values.
  
  w.std <- (sum(w*(x-weighted.mean(x,w,na.rm = na.rm))^2, na.rm = na.rm)/sum(w,na.rm = na.rm))^(1/2)
  return(w.std)
}
