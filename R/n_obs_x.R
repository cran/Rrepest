#' Number of observations valid for column x
#'
#' @param df (dataframe) data to analyze
#' @param by (string vector) column by which we'll break down results
#' @param x (string) variable from where to get means
#' @param svy (string) Possible projects to analyse.must be equal to ALL, IALS, 
#' IELS, PIAAC, PISA, PISA2015, PISAOOS, TALISSCH, TALISTCH 
#'
#' @return Dataframe containing the number of observations valid for the target variable x
#' @export
#'
#' @examples
#' data(df_pisa18)
#' data(df_talis18) 
#' 
#' n_obs_x(df = df_pisa18, by = "cnt",x = "wb173q03ha", svy = "PISA2015")
#' n_obs_x(df = df_talis18, by = "cntry",x = "tt3g01", svy = "TALISTCH")
n_obs_x <- function(df, by, x, svy = NULL) {
  # Goal: Number of schools and teachers calculation is performed on
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze
  # by : (string) column in which we'll break down results
  # x : (string) variable from where to get means
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  
  #All lower caps column names
  colnames(df) <- colnames(df) %>% tolower()
  by <- by %>% tolower()
  x <- x %>% tolower()
  
  tot.n <- df %>%
    group_by(across(all_of(by))) %>%
    {if (svy == "TALISSCH") summarise(., n.obs = sum(!is.na(get(x)))) 
      else if (svy == "TALISTCH") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, idschool)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy == "PISA2015") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, cntschid)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy == "PISA") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, schoolid)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy == "SSES") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, schid)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else summarise(., n.obs = sum(!is.na(get(x))))
    } %>%
    unite("by.group", all_of(by), sep = "|")
  
  #add x colanames
  colnames(tot.n)[-1] <- paste(colnames(tot.n)[-1], tolower(x), sep = ".")
  
  return(tot.n)
}
