
#' Group Averages
#'
#' @param data (dataframe) df from which to get replicated weights
#' @param res (dataframe) df of results with b. and se. to average
#' @param group (grp function) that takes arguments group.name, column, cases to create averages at the end of dataframe
#' @param by (string vector) column in which we'll break down results
#' @param over (vector string) columns over which to do analysis
#' @param est (est function) that takes arguments what = estimate, tgt = target, rgr = regressor
#' @param svy (string) name of possible projects to analyse TALISSCH and TALISTCH
#' @param user_na (bool) TRUE: show nature of user defined missing values for by.var
#' @param ... Additional arguments such as na_to_zero : (Bool) TRUE → will take NA as zero for the simple average calculation
#'
#' @return Dataframe with avergas or weighted averages (totals) in last rows for the selected cases
#'
average_groups <- function(res, data = NULL, group, by = NULL, over = NULL, est = NULL, svy = NULL, user_na = FALSE, ...) {
  # Goal: Average across sub samples of results assuming same size
  # ------ INPUTS ------.
  # data : (dataframe) df from which to get replicated weights
  # res : (dataframe) df of results with b. and se. to average
  # group: (grp function) that takes arguments group.name, column, cases to create averages at the end of dataframe
  # by: (string vector) column in which we'll break down results
  # est : (est function) that takes arguments what = estimate, tgt = target, rgr = regressor
      # what : (string vector) accepts "mean", "mean pct","var","std", "quant", "iqr", "freq", "lm", "corr", "cov", "gen"
      # tgt : (string vector) variable from where to get frequencies or R script if selected "gen"
      # rgr : (string vector) independant variable for regression (1+)
  # over : (vector string) columns over which to do analysis
  # user_na : (Bool) TRUE: show nature of user defined missing values for by.var
  # ...
  # na_to_zero : (Bool) TRUE → will take NA as zero for the simple average calculation
  
  # Example:
  #grp("ARE-AUS","CNTRY",c("ARE","AUS"))
  #grp("OECD Average","CNTRY",c("HUN","MEX"))
  
  #Get na_to_zero from ...
  arguments <- list(...)
  
  if (("." %in% by) | (is.null(by))){
    warning("Average without a 'by' variable is redundant")
    return(res)
  }
  
  #Iterate over the names of each group
  res.avgs <- lapply(names(group), function(g_i){
    # Replacing all NAs with zeros to consider for simple average
    if (!is.null(is.there(arguments$na_to_zero))) if(arguments$na_to_zero) res <- res %>% replace(is.na(.),0)
    
    
    

    # SIMPLE AVERAGE ----------------------------------------------------------
    # If not weighted average
    if(!group[[g_i]][["full_weight"]]){
      # Create a df with only the "cases" in group and renamed by the group "name"
      res.df <- res %>% 
        mutate(!!group[[g_i]][["column"]] := 
                 ifelse(get(group[[g_i]][["column"]]) %in%
                          group[[g_i]][["cases"]], 
                        g_i, # if YES
                        get(group[[g_i]][["column"]]))) %>% # if NO
        # filter from selected variable the group needed
        filter(!!as.name(group[[g_i]][["column"]]) == g_i) 
      
      # Average for b -----------------------------------------------------------
      res.b <- res.df %>% 
        #get by and b.
        select(by, starts_with("b.")) %>% 
        group_by(across(all_of(by))) %>% 
        summarise(across(starts_with("b."),\(x) mean(x, na.rm = TRUE)))
      
      # calculate √(Σ(se))/n for se. --------------------------------------------
      res.se <- res.df %>%
        select(by,starts_with("se.")) %>% 
        group_by(across(all_of(by))) %>% 
        summarise(across(starts_with("se."),\(x) (x^2 %>% sum(na.rm = TRUE) %>% sqrt())/
                           sum(!is.na(x))))
    } else {
    # -------------------------------------------------------------------------
    # WEIGHTED AVERAGE --------------------------------------------------------
    # -------------------------------------------------------------------------
      
      # Same as initial Repest formating
      # Argument renaming to be adapted in v2 -----------------------------------
      what <- est$what
      tgt <- est$tgt
      rgr <- est$rgr
      
      # Plausible Values DIGITS--------------------------------------------------
      # If tgt or rgr has an @ then get digits of PVs
      if (any(grepl("@", c(tgt,rgr,by,over)))) {
        # Get all PV digits
        pv.digits <- digits.pvs(svy = svy, ...)
      } else {
        # if no @ the just iterate over 1 value
        pv.digits <- "@"
      }
      
      # Get optional arguments
      extra.args <- list(...)
      
      # Formatting depending on continuous or categorical tgt (assuming categorical target for all)
      df_weights <- format_data_repest(df = data,
                               svy = svy,
                               x = NULL, 
                               # all arguments with an @ get converted into pvs
                               by.over = get.pv.arguments(pv.digits, tolower(c(by,over,tgt))),
                               user_na = user_na, 
                               isced = is.there(extra.args$isced),
                               ...)
      

      # Group by BY and OVER but not FREQ the sum of the final weight (1)
      res_weights <- df_weights %>% 
        group_by(across(tolower(by))) %>% 
        summarise(weights = sum(get(replicated_w_names(svy = svy, ...)[1])))
      
      # Rename variables to match upper and lower case
      names(res_weights) <- c(by,"weights")
      
      #Join results and weights and select only relevant cases
      # Create a df with only the "cases" in group and renamed by the group "name"
      res.df <- left_join(res, res_weights, by = by) %>% 
        mutate(!!group[[g_i]][["column"]] := 
                 ifelse(get(group[[g_i]][["column"]]) %in%
                          group[[g_i]][["cases"]], 
                        g_i, # if YES
                        get(group[[g_i]][["column"]]))) %>% # if NO
        # filter from selected variable the group needed
        filter(!!as.name(group[[g_i]][["column"]]) == g_i) 
      
      # Average for WEIGHTED b -----------------------------------------------------------
      res.b <- res.df %>% 
        #get by and b.
        select(by, starts_with("b."), "weights") %>% 
        group_by(across(all_of(by))) %>% 
        summarise(across(starts_with("b."),\(x) weighted.mean(x = x, w = .data$weights, na.rm = TRUE)))
      
      # calculate √(Σ(N^2 * se^2))/ ΣN. --------------------------------------------
      res.se <- res.df %>%
        select(by,starts_with("se."), "weights") %>% 
        group_by(across(all_of(by))) %>% 
        summarise(across(starts_with("se."),\(x) ((.data$weights^2 * x^2) %>% sum(na.rm = TRUE) %>% sqrt())/
                           sum(.data$weights * x/x, na.rm = TRUE)))
      
    }
    
    
    # Join b. and se. together
    res.df <- inner_join(x = res.b, y = res.se, by = by) %>% 
      #reorder according to original database
      select(names(res))

    
    
    return(res.df)
  })
  
  #Join all averages to the original results
  res.avgs <- append(list(res), res.avgs) %>% reduce(rbind)
  
  return(res.avgs)
}


