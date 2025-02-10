#' Estimate list
#'
#' @description Obtains a list with the arguments for the est() function used within the Rrepest() function.
#' @param statistic (string vector) Statistics of interest that can include
#' mean ("mean"), variance ("var"), standard deviation ("std"), quantile ("quant"), inter-quantile range ("iqr"), 
#' frequency count ("freq"), correlation ("corr"), linear regression ("lm"), covariance ("cov") and 
#' any other statistics that are not pre-programmed into Rrepest but take a data frame and weights as parameters ("gen")
#' @param target (string vector) Variable(s) of interest of the estimation
#' @param regressor (string vector) Independent variable(s) to be included in a linear regression
#'
#' @return List containing the arguments for the est() function used within Rrepest() function
#' @export
#'
#' @examples
#' est(c("mean","quant",.5,"corr"),c("pv1math","pv1read","pv1scie"))
#' 

est <- function(statistic, target, regressor = NULL) {
  # statistic : (string vector) accepts "mean","var","std", "quant", "iqr", "freq", "lm", "corr", "cov", "gen"
  # target : (string vector) variable from where to get estimation
  # regressor : (string vector) independant variable for regression (1+)
  if ("gen" %in% statistic) {
    target <- deparse(substitute(target)) %>% 
      paste0(collapse = "")
    }
  return(list(what = statistic, tgt = target, rgr = regressor))
}


