
#grp("ARE-AUS","CNTRY",c("ARE","AUS"))
#grp("OECD Average","CNTRY",c("HUN","MEX"))

average_groups <- function(res, group, by, ...) {
  # Goal: Average across sub samples of results assuming same size
  # ------ INPUTS ------.
  # res : (dataframe) df of results with b. and se. to average
  # group: (grp function) that takes arguments group.name, column, cases to create averages at the end of dataframe
  # by: (string vector) column in which we'll break down results
  # ...
  # na_to_zero : (Bool) TRUE → will take NA as zero for the simple average calculation
  
  #Get na_to_zero from ...
  arguments <- list(...)
  
  #Iterate over the names of of each group
  res.avgs <- lapply(names(group), function(g_i){
    # Replacing all NAs with zeros to consider for simple average
    if (!is.null(is.there(arguments$na_to_zero))) if(arguments$na_to_zero) res <- res %>% replace(is.na(.),0)
    
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


