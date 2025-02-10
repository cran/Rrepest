coverage_column <- function(data, res, by = NULL, over = NULL, est, svy, user_na = FALSE, pct = NULL, ...){
  # Goal: Add a column of coverage_pct to the right of all se, not including those for tests, and show the coverage of the estimation
  # ------ INPUTS ------.
  # data : (dataframe) df from which to get replicated weights
  # res : (dataframe) df of results with b. and se. to average
  # by: (string vector) column in which we'll break down results
  # est : (est function) that takes arguments what = estimate, tgt = target, rgr = regressor
  # what : (string vector) accepts "mean", "mean pct","var","std", "quant", "iqr", "freq", "lm", "corr", "cov", "gen"
  # tgt : (string vector) variable from where to get frequencies or R script if selected "gen"
  # rgr : (string vector) independant variable for regression (1+)
  # over : (vector string) columns over which to do analysis
  # user_na : (Bool) TRUE: show nature of user defined missing values for by.var
  # pct: (numeric) NULL: shows column next to se. Numeric: Shows estimation (NA) [Coverage] if bellow the set coverage
  
  
  #Get na_to_zero from ...
  arguments <- list(...)
  
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
  

  # Formatting depending on continuous or categorical tgt (assuming categorical target for all)
  df_covg <- format_data_repest(df = data,
                                   svy = svy,
                                   x = NULL, 
                                   # all arguments with an @ get converted into pvs
                                   by.over = get.pv.arguments(pv.digits, tolower(c(by,over,tgt))),
                                   user_na = user_na, 
                                   isced = is.there(arguments$isced),
                                   ...)
  
  #If no by provided
  if(is.null(by)) by <- "."

  # Get coverage vector
  if (any(what %in% "freq")){
    # For freq coverage is always 100%
    message("The coverage column/condition is not shown as it is 100% for all columns when calculating frequencies.")
  } else {
    # If lm, log or odds the coverage is from the regressors
    if(any(what %in% c("lm","log","odr"))){
      tgt_coverage <- rgr
    } else {
      tgt_coverage <- tgt
    }
    # If multiple  targets (correlation or covariance) we need all their coverage options
    res_cvg_l <- lapply(tgt_coverage, function(tgt_x){
      coverage_pct(df = df_covg, by = c(by, over) ,x = tgt_x, w = replicated_w_names(svy = svy, ...)[1]) %>% 
        separate(col = "by.group", sep = "\\|", into = c(by,over)) 
    } )
    
    # If there's an over
    if(!is.null(over)){
      res_cvg_l <- lapply(res_cvg_l, function(cvg_x){
        cvg_x %>% 
          unite(col = "over.cols", over, sep = "..") %>%
          # Make all column names have similar format to res
          pivot_wider(names_from = "over.cols", values_from = rev(names(.))[1], names_prefix = paste0(rev(names(.))[1],".."))
      })
    }
    
    # Give the same names as TGT
    names(res_cvg_l) <- tolower(tgt_coverage)
    
    # Join all dataframes from (possibly) multiple coverages into one
    res_cvg <- res_cvg_l %>% 
      reduce(inner_join, by = by)
    
    # If lm, log or odds add Intercept (and Rsqr) columns with 100 and targets for search in res_cvg
    if(any(what %in% c("lm","log","odr"))) {
      res_cvg[["cvge.intercept"]] <- 100
      tgt_coverage <- c(tgt_coverage, "intercept") # Must be lowercase for the search
      res_cvg_l[["intercept"]] <- res_cvg %>% select(!starts_with("cvge"),"cvge.intercept")
    }
    if(any(what %in% c("lm"))){
      res_cvg[["cvge.rsqr"]] <- 100
      tgt_coverage <- c(tgt_coverage, "rsqr") # Must be lowercase for the search
      res_cvg_l[["rsqr"]] <- res_cvg %>% select(!starts_with("cvge"),"cvge.rsqr")
    } 
  }
  
  
  
  
  # We have the dataframe from where to start extracting coverage
  # !!!!!IMPORTANT!!!!!: Coverage must be done in Rrepest separately for either statistics, or frequencies, or corr/cov, or lm
  if(!any(what %in% c("freq","gen"))){


    # -------------------------------------------------------------------------
    # Part 1: GETTING NAMES
    # Get all column names b, se, cvge in final Rrepest result --------
    
    # Only statistics and regressions will apply
    new_columns <- lapply(names(res),function(x){

        #Go over all names of the res df and if it finds se..
        if (startsWith(x,"se")){
          #Get the 3rd component in the column name (after the dots. and before the two dots..)
          x_tgt <- strsplit(x,split = "\\.{1,2}")[[1]][3]
          
          #Check which target variables are in this name component
          tgts_in_column <- tolower(tgt_coverage)[sapply(tolower(tgt_coverage), grepl,x_tgt, USE.NAMES = FALSE)]
          
          # For each of these names we need to grab cvge components
          sapply(tgts_in_column, function(tgt_i){
            
            # If has se and structure nn..(x-y)
            if (grepl(".*\\.{2}\\(.*\\-.*\\)", x)){
              # Seperate left and right side of test 
              names_x <- separate_test_name(x) %>% 
                sapply(function(y){
                  # and remove the 1st two components of the column name (b/se.what.)
                  ending <- paste(strsplit(y,"\\.{1,2}")[[1]][-c(1,2,3)],collapse = "..")
                  ending <- paste(c(tgt_i,ending),collapse = "..")
                  # return only the names that come from the coverage information database that are in the test part of the name
                  return(names(res_cvg_l[[tgt_i]])[endsWith(names(res_cvg_l[[tgt_i]]),ending)])
                },USE.NAMES = FALSE)
              
              
              # two coverage columns with information for the test
              new_x <- c(names_x)
            } else {
              # Grab the one from res_cvg, the one tha finishes the same way after the "what" in the name
              ending <- paste(strsplit(x,"\\.{1,2}")[[1]][-c(1,2,3)],collapse = "..")
              ending <- paste(c(tgt_i,ending),collapse = "..")
              new_x <- c(names(res_cvg_l[[tgt_i]])[endsWith(names(res_cvg_l[[tgt_i]]),ending)])
            }
            return(new_x)
          },USE.NAMES = FALSE) %>% c()
          
          
          
        } else {
          NULL
        }

      
    })
    names(new_columns) <- names(res)

    # Part 2: NEW COLUMNS in CVGE DB -----------------------------------------
    
    # Go over new_columns and check if they empty
    new_res_columns <- c()
    for (i in names(res)) {
      if (!is.null(new_columns[[i]])){
        
        # Get the ending of the column
        ending <- paste(strsplit(i,".",fixed = T)[[1]][-c(1,2)],collapse = ".")
        
        # if there are 2+ cvge columns to choose from
        if(length(new_columns[[i]]) > 1){
          res_cvg <- res_cvg %>% 
            rowwise() %>%  
            mutate(Min = min(c_across(c(new_columns[[i]]))))
          
          #Rename Min column to cvg
          names(res_cvg)[names(res_cvg) == 'Min'] <- paste0("cvge.",ending)
        }
        
        
        #Add the name and cvge column name to new_res_columns
        new_res_columns <- c(new_res_columns, c(i,paste0("cvge.",ending)))
        } else {
        # If no cvge column to add just get the name from res column
        new_res_columns <- c(new_res_columns,i)
        }
    }
    
     
    # Part 3: MERGE AND SELECT NAMES FROM PART 1 --------------------------------------
    res <- left_join(res,res_cvg, by = by) %>% 
      select(new_res_columns)

    # If a minimum coverage was provided
    if (!is.null(pct)){
  
      #go over cvge columns
      for(i in names(res)[startsWith(names(res),"cvge.")]){
        # Grab the ending of the names
        ending <- substr(i,6,nchar(i))
        for(j in names(res)[endsWith(names(res),ending) & !startsWith(names(res),"cvge.")]){
          # Assign a NaN to estimations under the minimum coverage
          res <- res %>% 
            mutate(!!j := ifelse(get(i) < pct, NaN, get(j)))
          
        }
      } 
      # Remove al cvge columns
      res <- res[!startsWith(names(res),"cvge.")]
    }
    
    
    
  }
  
  
  
    
  return(res)
}