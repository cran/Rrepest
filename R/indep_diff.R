#' Paired independent differences
#' @description
#' Get differences and standard errors from two columns starting with b. and se. assuming independence.
#' The second column will be subtracted from the first.
#' 
#' @param df (dataframe) Dataframe that contains the columns with b. and se. on their name to get the difference from
#' @param col1 (numeric vector) Column name, not including "b.". Must have a column in dataframe also including "se.".
#' @param col2 (numeric vector) Column name, not including "b.". Must have a column in dataframe also including "se.".
#'
#' @return Dataframe with extra columns containing the difference of the columns along with their standard error.
#' @export
#'
#' @examples
#' paired_indep_diff(rrepest_pisa_age_gender,"mean.age..Male","mean.age..Female")
#' 
paired_indep_diff <- function(df, col1, col2){
  # GOAL: Get differences from two columns starting with b. and se. assuming independence
  # Will do the difference of col2 - col1
  # Parameters col1 and col2 are without b. and se.
  
  
  
  b_col1 <- paste0("b.",col1)
  b_col2 <- paste0("b.",col2)
  b_col2_col1 <- paste0("b.(",col2,")-(",col1,")")
  
  se_col1 <- paste0("se.",col1)
  se_col2 <- paste0("se.",col2)
  se_col2_col1 <- paste0("se.(",col2,")-(",col1,")")
  
  res <- df %>% 
    as_tibble() %>% 
    mutate(!!b_col2_col1:=get(b_col2) - get(b_col1),
           !!se_col2_col1:= sqrt(get(se_col2)^2 + get(se_col1)^2))
  
  return(res)
}

#' Independent Differences of columns
#' @description
#' Get in a dataframe all bivariate combinations of differences and standard errors from columns starting with b. and se. assuming independence.
#' For a time series the vector must be order from oldest to newest.
#' 
#' @param df (dataframe) Dataframe that contains the columns with b. and se. on their name to get the difference from
#' @param vec_series (numeric vector) Column names to get difference from, not including "b." at the start. Must have a column in dataframe also including "se.".
#'
#' @return Dataframe with extra columns containing the difference of the columns along with their standard error.
#' @export
#'
#' @examples
#' indep_diff(rrepest_pisa_age_isced, paste0("mean.age..ISCED level ",c("1","3A","2")))
#' 
indep_diff <- function(df, vec_series){
  # GOAL: Get in a dataframe all bivariate combinations of differences and standard errors
  # of the vector vec_series
  # - ARGUMENTS -
  # vec_series (string vector): names of all time period variables without b. and se. from the oldest to the newest
  
  vec_series <- rev(vec_series)
  
  bivariate_time_periods <- lapply(1:choose(length(vec_series),2), function(x_i){
    all_combs <- combn(vec_series,2,simplify = TRUE)
    return(all_combs[,x_i])
  })
  
  res_df <- df
  for(time_periods_i in bivariate_time_periods){
    res_df <- paired_indep_diff(res_df,time_periods_i[1],time_periods_i[2])
  }
  
  return(res_df)
}