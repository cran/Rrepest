#Rrepest Isolated for one database (group)

rrepest_base <- function(data, svy, est, by = NULL, over = NULL,
                    test = FALSE, user_na= FALSE, show_na= FALSE, flag = FALSE, fast = FALSE, ...) {
  # Goal: Dataframe with statistics for TALIS
  # ------ INPUTS ------.
  ######### WHO ######### 
  # data : (dataframe) df to analyze
  # svy : List of possible projects to analyse TALISSCH and TALISTCH
  ######### WHAT ######### 
  # est : (est function) that takes arguments what = estimate, tgt = target, rgr = regressor
  # what : (string vector) accepts "mean","var","std", "quant", "iqr", "freq", "lm", "corr", "cov"
  # tgt : (string vector) variable from where to get frequencies
  # rgr : (string vector) independant variable for regression (1+)
  ######### WHERE ######### 
  # by.var : (string vector) column in which we'll break down results
  # over : (vector string) columns over which to do analysis
  ######### HOW ######### 
  # test : (bool) If TRUE will calculate the difference between over variables
  # user_na : (Bool) TRUE → show nature of user defined missing values for by.var
  # show_na : (Bool) TRUE → include na in frequencies of x 
  # flag : (Bool) TRUE → Show NaN when there is not enough cases (or schools)
  # fast : (Bool) TRUE → Only do 6 replicated weights
  # cool : (Bool) TRUE → Creates a flextable with all examples
  # ...
  # isced : (number) isced level to analyze
  
  
  # Argument renaming to be adapted in v2 -----------------------------------
  
  by.var <- by
  what <- est$what
  tgt <- est$tgt
  rgr <- est$rgr
  
  
  # Groups ------------------------------------------------------------------
  # if (!is.null(group)) {
  #   data <- do_group(data = data, grp.l = group)
  # }
  
  
  
  
  # Plausible Values DIGITS--------------------------------------------------
  # If tgt or rgr has an @ then get digits of PVs
  if (any(grepl("@", c(tgt,rgr,by.var,over)))) {
    # Get all PV digits
    pv.digits <- digits.pvs(svy = svy, ...)
    pv <- T #To know when to parallelize
  } else {
    # if no @ the just iterate over 1 value
    pv.digits <- "@"
    pv <- F #To know when to parallelize
  }
  
  # FORMATING ---------------------------------------------------------------.
  # Get optional arguments
  extra.args <- list(...)
  
  # If no by.var do it in all df
  if (is.null(by.var)) {
    data["tot.df"] <- "Total"
    by.var <- "tot.df"
  }
  
  
  #GENERAL option: If what is not "gen" convert to lowercase (might be functions in upper case)
  if (!("gen" %in% what))  {
    # All variables in dataframe must be LOWERCASE
    over <- tolower(over)
    by.var <- tolower(by.var)
    tgt <- tolower(tgt)
    rgr <- tolower(rgr)
    }
  
  # Formatting depending on continuous or categorical tgt
  if ("freq" %in% what) {
    # Data formatting
    df <- format.data.repest(df = data,
                             svy = svy,
                             x = NULL, 
                             # all arguments with an @ get converted into pvs
                             by.over = get.pv.arguments(pv.digits, c(by.var,over,tgt)),
                             user_na = user_na, 
                             isced = is.there(extra.args$isced),
                             ...)
    
    # # If there is an OVER variable remove NAs from OVER vars
    # if (length(get.pv.arguments(pv.digits,over)) > 0) {
    #   if (!any(show_na,user_na)) {
    #     for (i in get.pv.arguments(pv.digits,over)) {
    #       df <- df %>% drop_na(i)
    #     }
    #   }
    # }
    
    
    # # X is categorical in a frequency, must be formatted accordingly
    # df <- format.data.categ.vars(df,
    #                              get.pv.arguments(pv.digits, tgt),
    #                              show_na)
  } 
  else if (!("gen" %in% what)) {
    # Data formatting
    df <- format.data.repest(df = data,
                             svy = svy,
                             x = NULL, 
                             # all arguments with an @ get converted into pvs
                             by.over = get.pv.arguments(pv.digits, c(by.var,over)),
                             user_na = user_na, 
                             isced = is.there(extra.args$isced),
                             ...)
    
    # If there is an OVER variable remove NAs from OVER vars
    if (length(get.pv.arguments(pv.digits,over)) > 0) {
      for (i in get.pv.arguments(pv.digits,over)) {
        df %>% drop_na(i)
      }
    }
    
    # Format continuous vars
    df <- format.data.cont.vars(df, cont.vars = get.pv.arguments(pv.digits, c(tgt,rgr)))
  }
  
  
  # CLUSTER -----------------------------------------------------------------.
  # If pvs, set up cluster
  if (pv) {
    #Get repweights
    rep_weights <- replicated_w_names(svy = svy, ...)
    
    # Select necessary data and convert to to datable
    # GENERAL option: Assumes only essential data is preselected by user
    if (!("gen" %in% what)) {data.par <- as.data.table(df[,get.pv.arguments(pv.digits,c(tgt,rgr,rep_weights,by.var,over))])}
    else {data.par <- as.data.table(data)}
    
    # Set up cluster working nodes and what they have to know
    cl <- makeCluster(max(detectCores()-1,1))
    clusterExport(cl, "data.par",envir = environment())
    doParallel::registerDoParallel(cl)
  }
  
  # PLAUSIBLE VALUES LOOP -------------------------------------------------
  pv.l <- lapply(pv.digits, function(pv.d.i) {
    if (pv) message(as.character(pv.d.i)) #counter of pvs
    # PARALLEL UNIVERSES ---------------------------------------------------.
    # replace @ with that digit in tgt and rgr
    tgt <- gsub("@", as.character(pv.d.i), tgt)
    rgr <- gsub("@", as.character(pv.d.i), rgr)
    # Also for over variables in the over and by.vars if categories created from pvs
    by.var <- gsub("@", as.character(pv.d.i), by.var)
    over <- gsub("@", as.character(pv.d.i), over)
    
    # REPLICATED WEIGHTS ---------------------------------------------------.
    res.l <- lapply(tgt, function(tgt.i){
      res.tgt.i <- list()
      #-------------- STATISTICS --------------.
      # If there is something else than freq
      if(any(c("mean","var","std", "sd", "quant", "iqr") %in% what )){
        # remove "lm" and "freq"
        what.statistic <- what[! what %in% c("lm","freq","corr","cov")]
        
        # if flag show.n as "flag"
        if (flag) {show.n = "flag"} else {show.n = "n"}
        
        # Do statistics
        res.stats <- 
          pv.rrepest.statistics(data = df, svy = svy, statistic = what.statistic,
                                x = tgt.i, by.var = by.var,over = over, test = test,
                                flag = flag, user_na = user_na, fast = fast, pv = pv, ...)
        
        # Rename to by.var first column to merge all results
        colnames(res.stats)[1] <- "by.var"
        # Append result of statistics to list of results
        res.tgt.i <- append(res.tgt.i, list(res.stats))
        
      }
      #---------------------------------------.
      
      #-------------- FREQUENCIES --------------.
      if ("freq" %in% what) {
        # Do frequencies
        res.freq <- 
          pv.rrepest.frequencies(data = df, svy = svy, x = tgt.i, by.var = by.var,
                                 over = over, test = test, user_na = user_na,
                                 show_na = show_na, flag = flag, fast = fast, pv = pv, 
                                 pvdigits = pv.digits, # Insert pv.digits to erase NAs from over variables 
                                 #pcluster = ifelse(pv,cl,c()), # Cluster of workers in case pv exists
                                 ...)
        
        # Append result of frequencies to list of results
        res.tgt.i <- append(res.tgt.i, list(res.freq))
        
      }
      
      #---------------------------------------.
      
      #-------------- LINEAR REGRESSION --------------.
      if ("lm" %in% what) {
        # Do regression
        res.lm <- 
          pv.rrepest.lm(data = df, svy = svy, x = rgr, y = tgt.i, by.var = by.var,
                        over = over, test = test, user_na = user_na, flag = flag,
                        fast = fast, pv = pv, ...)
        
        # Append result of regression to list of results
        res.tgt.i <- append(res.tgt.i, list(res.lm))
      }
      #---------------------------------------.
      
      #-------------- GENERAL OPTION --------------.
      if ("gen" %in% what) {
        # Do general calculation
        res.gen <- # Here the argument for data is data and not df as no data preparation is done in "gen"
          pv.rrepest.gen(data = data, svy = svy, y = tgt.i, by.var = by.var,
                        over = over, test = test, user_na = user_na, flag = flag,
                        fast = fast, pv = pv, ...)
        
        # Append result of regression to list of results
        res.tgt.i <- append(res.tgt.i, list(res.gen))
        
      }
      
      #---------------------------------------.
      
      # Merge all in list of results if there is a result
      if (length(res.tgt.i) > 0) {
        res.tgt.i <- reduce(res.tgt.i, full_join, by = "by.var")
      }
      
      return(res.tgt.i)
    })
    
    # If there is nothing on res.l initialize it
    res.l <- Filter(length, res.l)
    
    #-------------- CORRELATION AND COVARIANCE --------------.
    # Since corr will use all the variables in tgt we must do a separate loop
    if (any(c("corr","cov","cor") %in% what)) {
      # Leave what with only cor,corr,cov
      what.cov.cor <- what[what %in% c("corr","cov","cor")]
      # Loop over all left
      res.l.corr <- lapply(what.cov.cor, function(what.i){
        corr <- !(what.i == "cov")
        res.corr <- pv.rrepest.corr.cov(data = df, svy = svy, x = tgt, by.var = by.var,
                                        over = over, test = test, user_na=user_na, pv = pv,
                                        show_na=show_na, flag = F, fast = fast, corr = corr,
                                        ...) #Can't accept flags since many variables
        
        return(res.corr)
      })
      res.l <- append(res.l,res.l.corr)
    }
    #---------------------------------------.
    
    # return all results merged into one dataframe
    return(reduce(res.l, full_join, by = "by.var"))
    
  })
  
  # Stop the cluster if pvs
  if(pv) stopCluster(cl)
  
  
  
  # If there are no PVs return first element of the list
  if (!pv) {
    result <- pv.l[[1]] %>% as_tibble()
  } else {
    # Check which inputs have an @
    pv.inputs <- c(tgt,rgr,by.var,over)[grepl("@", c(tgt,rgr,by.var,over))]
    # If there are PVs Calculate B and Imputation and Sampling Variance for SE
    result <- b.se.pv(res.l = pv.l, pv.inputs = pv.inputs, statistic = what) %>% as_tibble()
  }
  
  return(result)
}

