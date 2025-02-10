#' Coverage percentage  (1 - mean(is.na)) * 100
#'
#' @description Cpmputes teh coverage percentage for the column/variable of interest.
#' @param df (data frame) Data to analyse
#' @param by (string vector) Variable(s) used for tabulating the variable of interest
#' @param x (string) Variable of interest for which to compute the number of valid (i.e. non-missing) observations
#' @param w (string) Vector of weights
#' @param limit (numeric) Threshold at which, if lower, value will be TRUE
#' @return Data frame containing the number of valid (i.e. non-missing) observations for the variable of interest
#' @export
#'
#' @examples
#' data(df_pisa18)
#' data(df_talis18) 
#' 
#' coverage_pct(df = df_pisa18, by = "cnt",x = "wb173q03ha")
#' coverage_pct(df = df_talis18, by = "cntry",x = "tt3g01", w = "TCHWGT")
coverage_pct <- function(df, by, x, w = NULL, limit = NULL) {
  # Goal: Number of schools and teachers calculation is performed on
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze
  # by : (string) column in which we'll break down results
  # x : (string) variable from where to get means
  # w : (string) vector of weights
  # limit : (numeric) threshold at which if lower, value will be Inf
  
  #All lower caps column names
  colnames(df) <- colnames(df) %>% tolower()
  by <- by %>% tolower()
  x <- x %>% tolower()
  
  if(!is.null(w)) {
    w <- tolower(w)
  }else{
    df <- df %>% mutate(weight..column = 1)
    w <- "weight..column"
  }
  
  #Convert desired variable to is.na
  df[x] <- is.na(df[x])
  
  tot.n <- df %>% 
    group_by(across(all_of(by))) %>%
    summarise(cvge = 100 - weighted.mean(get(x),as.vector(get(w))) * 100) %>%
    unite("by.group", all_of(by), sep = "|")
  
  #add x colanames
  colnames(tot.n)[-1] <- paste(colnames(tot.n)[-1], tolower(x), sep = ".")
  
  if (!is.null(limit)) {
    tot.n[ncol(tot.n)] <- tot.n[ncol(tot.n)] <= limit
  }
  
  return(tot.n)
}
