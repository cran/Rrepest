#' Format categorical variables as factor for Rrepest
#'
#' @param df (data frame) Data to analyse.
#' @param categ.vars (string vector) Categorical variables for analysis.
#' @param show_na (bool) Keeps NAs as categories to get frequency from.
#'
#' @return Data frame with categorical variables as factors for frequencies or over variables.
#' @export
#'
#' @examples
#' format_data_categ_vars(df = mtcars, categ.vars = "cyl") 
format_data_categ_vars <- function(df, categ.vars, show_na = F) {
  # Goal: Converts to numeric all variables for a continuous analysis (means, regression, etc.)
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # cont.vars : (string vector) continuous variables for analysis
  
  if (show_na) {
    df.res <- df %>% 
      mutate_at(categ.vars,to_factor,drop_unused_labels = TRUE ,user_na_to_na = TRUE) 
  }else{
    df.res <- df %>% 
      mutate_at(categ.vars,to_factor,drop_unused_labels = TRUE ,user_na_to_na = TRUE) %>% 
      drop_na(all_of(categ.vars))
  }
  
  return(df.res)
}