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
  # if (any(what %in% "freq")){
  #   tgt_coverage <- tgt
  # } else {
    # If lm, log or odds the coverage is from the regressors
    if(any(what %in% c("lm","log","odr"))){
      tgt_coverage <- rgr
    } else {
      tgt_coverage <- tgt
    }
  
    # If any element has @ in it replace it with a 1 in df_covg, PVs must have the same valid values = Coverage done on the 1st PV
    if(any(grepl(pattern = "@", x = tgt_coverage))){
      for(tgt_i in tgt_coverage){
        # Look for the 1st pv and replace the 1 with an @
        tgt2find <- sub(pattern = "@", replacement = "1",tgt_i)
        names(df_covg)[which(names(df_covg) == tgt2find)] <- tgt_i
      }
    }
    # Likewise on the over, do it on the 1st pv
    if(any(grepl(pattern = "@", x = over))){
      for(over_i in over){
        # Look for the 1st pv and replace the 1 with an @
        over2find <- sub(pattern = "@", replacement = "1",over_i)
        names(df_covg)[which(names(df_covg) == over2find)] <- over_i
      }
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
  # } OLD: Frequencies not included in coverage
  
  # Check if names are similar for FREQUENCIES and add them to the res_cvg data
  if(any(what %in% c("freq"))){
    # Get the names of se after first dot
    names0res <- names(res)[grep("^se\\.",names(res))] %>% substring(4)
    # Get the names of cvge after first dot
    names0res_cvge <- names(res_cvg)[grep("^cvge\\.",names(res_cvg))] %>% substring(6)
    
    for(n_cvge_i in names0res_cvge){
      for(n_i in names0res){
        # Split both names and subtract 2nd component from n_i
        n_cvge_i_check <- paste(strsplit(n_cvge_i,"\\.{1,2}")[[1]],collapse = "..")
        n_i_check <- paste(strsplit(n_i,"\\.{1,2}")[[1]][-2],collapse = "..")
        if(n_cvge_i_check == n_i_check){
          # the name of 
          res_cvg[[paste0("cvge.",n_i)]] <- res_cvg[[paste0("cvge.",n_cvge_i)]]
        }
      }
    }
  }
  
  # We have the dataframe from where to start extracting coverage
  # !!!!!IMPORTANT!!!!!: Coverage must be done in Rrepest separately for either statistics, or frequencies, or corr/cov, or lm
  if(!any(what %in% c("gen"))){


    # -------------------------------------------------------------------------
    # Part 1: GETTING NAMES
    # Get all column names b, se, cvge in final Rrepest result --------
    
    # Only statistics and regressions will apply
    new_columns <- lapply(names(res),function(x){

        #Go over all names of the res df and if it finds se..
        if (startsWith(x,"se")){
          #Get the 3rd component in the column name (after the dots. and before the two dots..), the second if frequencies
          nth_component <- ifelse(any(what %in% c("freq")), 2 , 3)
          x_tgt <- strsplit(x,split = "\\.{1,2}")[[1]][nth_component]
          
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
        
        
        # Removing 1st and 2nd component in all cases and 1st only for frequencies
        name_components2remove <- if(any(what %in% c("freq"))) 1 else c(1,2)
        # Get the ending of the column
        ending <- paste(strsplit(i,".",fixed = T)[[1]][-name_components2remove],collapse = ".")
        
        # if there are 2+ cvge columns to choose from
        if(length(new_columns[[i]]) > 1){
          # If the name of the cvge column to calculculate is already in the res_cvg don't calculate it
          if(! paste0("cvge.",ending) %in% names(res_cvg)){
            res_cvg <- res_cvg %>% 
              rowwise() %>%  
              mutate(Min = min(c_across(c(new_columns[[i]]))))
            
            #Rename Min column to cvg
            names(res_cvg)[names(res_cvg) == 'Min'] <- paste0("cvge.",ending)
          }
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