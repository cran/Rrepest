#' Estimate list
#'
#' @description Input the statistic wanted, target variable, and (optional) list of regressors
#' @param statistic (string vector) accepts "mean","var","std", "quant", "iqr", "freq", "lm", "corr", "cov"
#' @param target (string vector) variable from where to get estimation
#' @param regressor (string vector) independent variable for regression (1+)
#'
#' @return list of components to estimate for repest
#' @export
#'
#' @examples
#' est(c("mean","quant",.5,"corr"),c("pv1math","pv1read","Pv1SCIE"))
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


