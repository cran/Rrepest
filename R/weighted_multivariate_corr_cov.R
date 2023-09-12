#' Multivariate Correlation and Covariance
#'
#' @param data (dataframe) data to analyze
#' @param x (vector string) variables names from where to get correlation/covariance
#' @param w (string) weight name
#' @param corr (bool) True: get correlation. False: get covariance
#' @param na.rm (bool) True: NAs be stripped before computation proceeds
#'
#' @return Dataframe containing 2 Choose length(x) columns with each bivariate correlation/covariance
#' @export
#'
#' @examples
#' data(df_talis18)
#' 
#' weighted.corr.cov.n(df_talis18,c("T3STAKE","T3TEAM","T3STUD"),"TCHWGT")
#' 
weighted.corr.cov.n <- function(data, x, w =  rep(1, length(data[x[1]])), corr = TRUE, na.rm = TRUE) {
  # Goal: Get weighted correlation or covariance of n variables
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # x: (vector string) variables from where to get correlation/covariance
  # w : (string) weight name
  # corr : (bool) T → get correlation, F → get covariance
  
  # Get dataframe of correlation
  vec.res <- apply(as.data.frame(combn(x,2)), 2, function(x.i) {
    if (corr) {
      res <- weighted.corr(x = data[x.i[1]],
                           y = data[x.i[2]], 
                           w = data[w],
                           na.rm = na.rm)
    }else{
      res <- weighted.cov(x = data[x.i[1]],
                          y = data[x.i[2]], 
                          w = data[w],
                          na.rm = na.rm)
    }
    
    return(res)
  })
  # Convert vector to dataframe if 1 by n
  vec.res <- data.frame(t(vec.res))
  
  # Get column names
  vec.names <- apply(as.data.frame(combn(x,2)), 2, function(x.i) {
    if (corr) {
      res <- paste0("corr.",x.i[1],"&",x.i[2])
    }else{
      res <- paste0("cov.",x.i[1],"&",x.i[2])
    }
    return(as.vector(res))
  })
  colnames(vec.res) <- vec.names
  
  return(vec.res)
}
