#' Number of valid (i.e. non-missing) observations for column/variable x
#'
#' @description Computes the number of valid (i.e. non-missing) observations for the column/variable of interest.
#' @param df (data frame) Data to analyse with lowercase column names.
#' @param by (string vector) Variable(s) used for tabulating the variable of interest
#' @param x (string) Variable of interest for which to compute the number of valid (i.e. non-missing) observations
#' @param svy (string) Survey settings that must be equal to one of the following: ALL, IALS, ICCS, ICILS, IELS,
#' PBTS, PIAAC, PIRLS, PISA, PISAOOS, PISA2015, SSES, SSES2023, SVY, TALISSCH, TALISTCH, TALISEC_LEADER, TALISEC_STAFF, TIMSS
#' @return Data frame containing the number of valid (i.e. non-missing) observations for the variable of interest
#' @export
#'
#' @examples
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
      else if (svy == "TALISTCH") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$idschool)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy == "TALISEC_STAFF") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$idcentre)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy == "PISA2015") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$cntschid)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy == "PISA") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$schoolid)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy %in% c("SSES","SSES2023")) {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$schid)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else summarise(., n.obs = sum(!is.na(get(x))))
    } %>%
    unite("by.group", all_of(by), sep = "|")
  
  #add x colanames
  colnames(tot.n)[-1] <- paste(colnames(tot.n)[-1], tolower(x), sep = ".")
  
  return(tot.n)
}
