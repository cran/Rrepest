################# Multiple statistics on Continuous variables #################
# 22 Mar 2023
# Rodolfo Ilizaliturri
#############################################################
# Goal: Obtain several statistics using Fay's BRR model


# ------------------.
# --------- WORK FUNCTIONS
# ------------------.

pv.brr.stat.by <- function(df, statistic = "mean",x, w, by.var, over, test, n.df = list(), svy,...) {
  # Goal: Statistics from final weight
  # ------ INPUTS ------.
  # df : (dataframe) df to analize
  # x: (string) variable from where to get means
  # w : (string) weighting variable
  # by.var : (string vector) variables to break analysis by
  # over : (vector string) columns over which to do analysis
  
  arg <- list(...)
  options(dplyr.summarise.inform = FALSE)
  
  res.col <- df %>% 
    group_by(across(all_of(by.var))) 
  
  # STATISTICS --------------------------------------------------------------.
  if(statistic == "quant") {res.col <- summarise(res.col, w.quant = weighted.quant(get(x),get(w),arg$q.iqr))}
  else if(statistic == "iqr") {res.col <- summarise(res.col, w.iqr = weighted.iqr(get(x),get(w),arg$q.iqr))}
  else if(statistic %in% c("mean","means")) {res.col <- summarise(res.col, w.mean = weighted.mean(get(x),get(w),na.rm = T))}
  else if(statistic %in% c("meanpct","meanspct")) {res.col <- summarise(res.col, w.mean = weighted.mean(get(x),get(w),na.rm = T)*100)}
  else if(statistic == "var") {res.col <- summarise(res.col, w.var = weighted.var(get(x),get(w),na.rm = T))}
  else if(statistic %in% c("std","sd")) {res.col <- summarise(res.col, w.std = weighted.std(get(x),get(w),na.rm = T))}

  # FLAG --------------------------------------------------------------------.
  #If flag attach n.df a nd remove based on condition
  if (length(n.df) > 0) {
    # make into one column over and by var
    res.col <- res.col %>% 
      unite("by.group", all_of(c(by.var, over)), sep = "|")
    
    # Merge both dfs together n and freqs
    res.col <- left_join(res.col, n.df, by = "by.group")
    
    #Assign NaN when not enough coverage for estimation
    res.col <- flags(data = res.col, svy = svy) %>% 
      separate("by.group",into = all_of(by.var), sep = "\\|" )
  }
  

  
  
  #------ If test for over add tests to res.col
  if (test) {
    res.col <- over.test(res.col, over) %>% 
      column_to_rownames(var= "by.group")
  }else{
    res.col <- res.col %>% 
      remove_rownames %>%
      unite("by.group", all_of(by.var), sep = "|") %>% 
      column_to_rownames(var= "by.group")
  }
  
  
  
  # -- Track weight number
  colnames(res.col) <- paste("w.",
                             sub("RWGT","",w),
                             sep = "")
  
  return(res.col)
}

pv.brr.stat.by.PAR <- function(df, statistic = "mean",x, w, by.var, over, test, n.df = list(), svy,...) {
  # Goal: Statistics from final weight in for each since in packages the info can only go 1 enviroment deep
  # ------ INPUTS ------.
  # df : (dataframe) df to analize
  # x: (string) variable from where to get means
  # w : (string) weighting variable
  # by.var : (string vector) variables to break analysis by
  # over : (vector string) columns over which to do analysis
  # ...
  #(functions) weighted.iqr, weighted.quant, weighted.var, weighted.std
  
  arg <- list(...)
  options(dplyr.summarise.inform = FALSE)
  
  res.col <- df %>% 
    group_by(across(all_of(by.var))) 
  
  # STATISTICS --------------------------------------------------------------.
  if(statistic == "quant") {res.col <- summarise(res.col, w.quant = arg$weighted.quant(get(x),get(w),arg$q.iqr))}
  else if(statistic == "iqr") {res.col <- summarise(res.col, w.iqr = arg$weighted.iqr(get(x),get(w),arg$q.iqr))}
  else if(statistic %in% c("mean","means")) {res.col <- summarise(res.col, w.mean = weighted.mean(get(x),get(w),na.rm = T))}
  else if(statistic %in% c("meanpct","meanspct")) {res.col <- summarise(res.col, w.mean = weighted.mean(get(x),get(w),na.rm = T)*100)}
  else if(statistic == "var") {res.col <- summarise(res.col, w.var = arg$weighted.var(get(x),get(w),na.rm = T))}
  else if(statistic %in% c("std","sd")) {res.col <- summarise(res.col, w.std = arg$weighted.std(get(x),get(w),na.rm = T))}
  
  # FLAG --------------------------------------------------------------------.
  #If flag attach n.df a nd remove based on condition
  if (length(n.df) > 0) {
    # make into one column over and by var
    res.col <- res.col %>% 
      unite("by.group", all_of(c(by.var, over)), sep = "|")
    
    # Merge both dfs together n and freqs
    res.col <- left_join(res.col, n.df, by = "by.group")
    
    #Assign NaN when not enough coverage for estimation
    res.col <- arg$flags(data = res.col, svy = svy) %>% 
      separate("by.group",into = all_of(by.var), sep = "\\|" )
  }
  
  
  #------ If test for over add tests to res.col
  if (test) {
    res.col <- arg$over.test(res.col, over) %>% 
      column_to_rownames(var= "by.group")
  }else{
    res.col <- res.col %>% 
      remove_rownames %>%
      unite("by.group", all_of(by.var), sep = "|") %>% 
      column_to_rownames(var= "by.group")
  }
  
  
  
  # -- Track weight number
  colnames(res.col) <- paste("w.",
                             sub("RWGT","",w),
                             sep = "")
  
  return(res.col)
}

pv.loop.stat.on.weights <- function (data, statistic = "mean", rep_weights,
                                     x, by.var, over, test, n.df = list(),
                                     svy, pv = F, ...) {
  # Goal: Means from final weight
  # ------ INPUTS ------.
  # data : (dataframe) df to analize
  # rep_weights : (string vector) names of replicated weight vars
  # over : (vector string) columns over which to do analysis
  # x: (string) variable from where to get means
  # pv : (Bool) TRUE → We are in plausible values and must parallelize

  
  if (!pv) {
    # NON PARALLEL ------------------------------------------------------------.
    #Filter on only non NA values move here due to performance
    df <- data %>% filter(!is.na(get(x)))
    
    rep.w.stat <- lapply(rep_weights, function(i) pv.brr.stat.by(df = df,
                                                              statistic = statistic,
                                                              x = x,
                                                              w = i,
                                                              by.var = by.var,
                                                              over = over,
                                                              test = test,
                                                              n.df = n.df,
                                                              svy = svy,
                                                              ...))
  } else {
    # PARALLEL ---------------------------------------------------------------.
    rep.w.stat <- foreach(w.i = rep_weights,
                          .packages = c("dplyr","tidyr","purrr","tibble", "data.table"),
                          .export = c("pv.brr.stat.by.PAR",
                                      "weighted.std","weighted.var",
                                      "weighted.quant","weighted.iqr",
                                      "flags","over.test","...")) %dopar% {
                                        
                                        #Filter on only non NA values move here due to performance
                                        data.par <- na.omit(data.par, cols = x)
                                        #NOTE: na.omit uses a data.table since it is 7 times faster
                                        pv.brr.stat.by.PAR(df = data.par,
                                                    statistic = statistic,
                                                    x = x,
                                                    w = w.i,
                                                    by.var = by.var,
                                                    over = over,
                                                    test = test,
                                                    n.df = n.df,
                                                    svy = svy,
                                                    weighted.std = weighted.std,
                                                    weighted.var = weighted.var,
                                                    weighted.quant = weighted.quant,
                                                    weighted.iqr = weighted.iqr,
                                                    flags = flags,
                                                    over.test = over.test,
                                                    ... = ...)
                                      }
    
    
  }
  
  
  
  return(reduce(rep.w.stat, cbind))
  }
  

pv.brr.gstats <- function(data, svy, x, by.var, statistic = "mean",
                          over, test, flag, fast, pv = F, ...) {
  # Note: Same as rrepest.gstats without the number of schools and teachers
  # Goal: Dataframe with statistic and SE by Fay's BRR model
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # statistic : (string) accepts "mean","var","std"
  # q.iqr : (vector numeric) of length 1 if quantile, of length 2 if interquantile range 
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # by.var : (string) column in which we'll break down results !IOP!: several variables
  # x : (string) variable from where to get statistics
  # pv : (Bool) TRUE → We are in plausible values and must parallelize
  # user_na : (Bool) TRUE → show nature of user defined missing values in by.var
  
  eclass <- list()
  arg <- list(...)
  
  
  # Get weight names
  weight.names <- replicated_w_names(svy, ...)
  # If fast then less replicated weights
  if (fast) {
    weight.names <- weight.names[1:6]
  }
  
  # Get n data if flag needed
  if (flag) {
    n.df <-  n.obs.x(df = data, by.var = c(by.var, over), x = x, svy = svy)
  } else {
    n.df <- list()
  }
  
  
  # Loop over weights
  brr.res <- pv.loop.stat.on.weights(data = data,
                                  statistic = statistic,
                                  rep_weights = weight.names,
                                  x = x,
                                  by.var = by.var, 
                                  over = over,
                                  test = test,
                                  n.df = n.df,
                                  svy = svy,
                                  pv = pv,
                                  ...)
  
  #----------statistic
  m <- brr.res[1] %>% 
    set_names(nm = paste0("b.",statistic,
                          if_else(statistic %in% c("quant","iqr"),
                                  str_replace_all(
                                    paste0(arg$q.iqr,collapse = "-"),
                                    "0.","0"),
                                  ""),
                          ".", tolower(x) )) %>% 
    rownames_to_column(var = "by.group")
  
  eclass <- append(eclass,list("m"=m))
  
  #----------se
  se <- apply(brr.res, 1, rrvar.pv, svy=svy, pv=pv, ... = ...) %>% 
    as.data.frame() %>% 
    set_names(nm = paste0("se.",statistic,
                          if_else(statistic %in% c("quant","iqr"),
                                  str_replace_all(
                                    paste0(arg$q.iqr,collapse = "-"),
                                    "0.","0"),
                                  ""),
                          ".", tolower(x) )) %>% 
    rownames_to_column(var = "by.group")
  
  eclass <- append(eclass,list("se"=se))
  
  
  return(reduce(eclass, full_join, by = "by.group"))
}


# --------- FINAL FUNCTION



pv.rrepest.statistics <- function(data, svy, statistic = "mean", x, by.var = NULL,
                               over = NULL, test = FALSE, flag = FALSE, user_na=FALSE, 
                               fast = FALSE, pv = FALSE, ...) {
  # Goal: Dataframe with statistic and SE by Fay's BRR model
  # Note: All formatting depends on higher function
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # statistic : (string vector) accepts "mean","var","std", "quant", "iqr"
  # q.iqr : (vector numeric) of length 1 if quantile, of length 2 if interquantile range 
  # by.var : (vector string) column in which we'll break down results
  # over : (vector string) columns over which to do analysis
  # test : (bool) If TRUE will calculate the difference between over variables
  # x : (string) variable from where to get statistics
  # user_na : (Bool) TRUE → show nature of user defined missing values in by.var
  # flag : (Bool) TRUE → Show NaN when there is not enough observations (or schools)
  # pv : (Bool) TRUE → We are in plausible values and must parallelize
  # ...
  # isced : (number) isced level to analyze
  
  #Store resulting DFs
  results <- list()
  
  stats_rr <- statistic
  
  # Data formatting
  by.over <- c(by.var,over)
  
  # In multiple statistics vector, skipping iterations if "quant" or "iqr"
  skip_iter <- 0
  for (i in seq_along(stats_rr)) {
    if (skip_iter > 0) {
      skip_iter <- skip_iter-1
      next}
    
    if (stats_rr[i] == "quant") {
      skip_iter <- 1
      
      #add brr statistics to original results
      results <- append(results, list(
        pv.brr.gstats(data = data, svy = svy, x = x,by.var = by.over, 
                   statistic = "quant", test = test, over = over, flag = flag,
                   fast = fast, pv = pv, 
                   q.iqr = stats_rr[i+1] %>% as.numeric(), ...) )
      )
      
    }else if (stats_rr[i] == "iqr"){
      skip_iter <- 2
      
      #add brr statistics to original results
      results <- append(results, list(
        pv.brr.gstats(data = data, svy = svy, x = x, by.var = by.over, 
                   statistic = "iqr", test = test, over = over, flag = flag,
                   fast = fast, pv = pv, 
                   q.iqr = c(stats_rr[i+1] %>% as.numeric(),
                             stats_rr[i+2] %>% as.numeric()),...) )
      )
      
    }else{
      #add brr statistics to original results
      results <- append(results, list(
        pv.brr.gstats(data = data, svy = svy, x = x, by.var = by.over,
                   test = test, over = over, flag = flag,
                   fast = fast, pv = pv, 
                   statistic = stats_rr[i],...))
      )
    }
    
  }
  
  # Merge all results together
  df.results <- reduce(results, full_join, by = "by.group")
  
  if (length(over) > 0) {
    #Separate all of by.group by | and unite only by by.var, leaving columns from over
    df.results <- df.results %>% 
      separate(by.group, into = by.over, sep = "\\|") %>% 
      unite("by.group", all_of(by.var), sep = "|") 
    
    #Pivot on the columns from over
    df.results <- df.results %>% 
      pivot_wider(names_from = all_of(over),
                  values_from = colnames(df.results)[! colnames(df.results) %in% c("by.group",over)],
                  names_sep = "..")
  }
  
  #If there is an over reorder results to have se following b
  if (length(over) > 0) {
    
    #reorder df with intercalating columns
    n.betas <- (ncol(df.results) - 1)/2
    betas.order <- lapply(1:n.betas,function(x) c(x,x+n.betas)) %>% 
      reduce(function (x,y) c(x,y))
    df.results <- df.results[,c(1,betas.order + 1)]
  }
  
  
  return(df.results)
}

