################# Functionalize General option and PVs #################
# 19 Jun 2023
# Rodolfo Ilizaliturri
#############################################################
# Goal: Obtain a dataframe with any R like input set into Rrepest
# REQUIREMENTS:
#   - Have at least one argument where you input weights as w_i
#   - Have an argument where you input the data as d_i
#   - Must return a named data frame
#     - Extreme case: If no over or by arguments, it would return a 1Xn named df
#   - Optional: A flextable with a column name prefix indication what you are doing deparated by a point (Ex. reg_age.(Intercept))
#   - PVs: intidace with an @ what you would like to be evaluated as a Plausible and indicate what you are doing in pv at begining of column name

pv.do.gen <- function(df, y, by.var, w, ...){
  # Goal: Get coefficients and r2 from linear regression
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze previosly formated
  # by.var : (string) column in which we'll break down results !IOP!: several variables
  # y : (string) Deparsed method to include in normal r script
  # w : (string) weighting variable (just 1)
  
  # Only colnames of the same # of characters as my replicated weight name
  col_names <- subset(names(df), nchar(names(df)) == nchar(w))
  # Grab closest non case sensitive weight name
  w <- col_names[grepl(w, col_names,ignore.case = T)]
  
  message(as.character(w)) #indicate wich w you are in
  
  # Replace what is w_gen with the replicated weight needed
  y <- sub("w_gen",w,y)
  #model only fitting values. Out: Coeff and R2 ONLY
  res.df <- df %>% 
    group_by(across(all_of(by.var))) %>% 
    do({
      #Parsed script model REQUIREMENTS 
      eval(parse(text = y))
    }
    ) %>%
    ungroup()
  
  return(res.df)
}
pv.do.gen.PAR <- function(df, y, by.var, w, ...){
  # Goal: Get coefficients and r2 from linear regression
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze previosly formated
  # by.var : (string) column in which we'll break down results !IOP!: several variables
  # y : (string) Deparsed method to include in normal r script
  # w : (string) weighting variable (just 1)
  # ...
  
  # get ... arguments
  arg <- list(...)

  # Only colnames of the same # of characters as my replicated weight name
  col_names <- subset(names(df), nchar(names(df)) == nchar(w))
  # Grab closest non case sensitive weight name
  w <- col_names[grepl(w, col_names,ignore.case = T)]
  
  message(as.character(w)) #indicate which w you are in
  
  # Replace what is w_gen with the replicated weight needed
  y <- sub("w_gen",w,y)
  
  #model only fitting values. Out: Coeff and R2 ONLY
  res.df <- df %>% 
    group_by(across(all_of(by.var))) %>% 
    do({
      #Parsed script model REQUIREMENTS 
      eval(parse(text = y))
    }
    ) %>%
    ungroup()
  
  return(res.df)
}
pv.loop.gen.on.weights <- function (data, x, y, by.var, over, test = F, flag = F,
                                   svy, rep_weights, fast = F, pv = F) {
  # Goal: Regression model results for each weight
  # ------ INPUTS ------.
  # data : (dataframe) df to analize
  # rep_weights : (string vector) names of replicated weight vars
  # reg.model : (expression) linear model to implement
  # by.var : (string vector) variables to break analysis by
  # fast : (bool) TRUE → Only do 6 replicated weights
  # pv : (Bool) TRUE → We are in plausible values and must parallelize
  
  # If fast then less replicated weights
  if (fast) {
    rep_weights <- rep_weights[1:6]
  }
  
  if (!pv) {
    # NON PARALLEL ------------------------------------------------------------.
    
    res.l <- lapply(rep_weights, function(w.i){
      #Do gen, add weight to colnames, append new data of results to list
      res.df <- pv.do.gen(data, 
                          y, 
                          all_of(c(by.var,over)), 
                          w.i) %>% 
        unite("by.var", all_of(c(by.var,over)), sep = "|")
      
      # Change col names to include weight name
      # colnames(res.df)[-1] <- paste0(colnames(res.df)[-1], paste0("|",w.i))
      
      return(res.df)
    })
    # PARALLEL ---------------------------------------------------------------.
  }else{
    res.l <- foreach(w.i = rep_weights,
                     .packages = c("dplyr","tidyr","data.table","tibble"),
                     .export = c("pv.do.gen.PAR","n.obs.x",
                                 "weighted.var")) %dopar% {
                                   
                                   res.df <- pv.do.gen.PAR(df = data.par,
                                                           y = y,
                                                           by.var = all_of(c(by.var,over)),
                                                           w = w.i) %>% 
                                     unite("by.var", all_of(c(by.var,over)), sep = "|")
                                   return(res.df)
                                 }
    
  }
  
  
  # ---------------- FLAGS
  if (flag) {
    # Get n for flags and separate column
    # NOTE!!!!!!: Here x = y since the target variable in regressions is the y
    n.df <- n.obs.x(df = data, by.var = c(by.var, over), x = y, svy = svy)
    
    res.l <- lapply(res.l, function(res.i){
      # Merge both dfs together n and freqs
      res.df <- left_join(res.i, n.df, by = c("by.var"="by.group"))
      
      #Assign NaN when not enough coverage for estimation (flags.nan is deprecated)
      res.df <- flags(data = res.df, svy = svy)
      
      return(res.df)
    })
  }
  # ---------------- .
  
  # ---------------- TEST
  if (test) {
    # Create extra rows with test differences
    res.l <- lapply(res.l, function(res.i){
      # Split by.var into c(by.var, over, x) by |
      res.df <- res.i %>% 
        separate(col = "by.var", into = all_of(c(by.var, over)), sep = "\\|") %>% 
        over.test(over = c(over)) %>% 
        rename("by.var"="by.group")
      
      return(res.df)
    })
  } 
  # ---------------- .
  # ---------------- FORMAT DF to have over on the columns
  if (length(over)>0) {
    res.l <- lapply(res.l, function(res.i){
      res.df <- res.i %>% 
        # Split by.var leaving only the variables in "by.var" united 
        separate(col = "by.var", into = all_of(c(by.var, over)), sep = "\\|") %>% 
        unite(col = "by.var", all_of(by.var), sep = "|") %>% 
        # Pivot to columns over
        pivot_wider(names_from = all_of(c(over)),
                    values_from = colnames(select_if(.,is.numeric)),
                    names_sep = "..")
      
      return(res.df)
    })
  }
  # ---------------- .
  # ---------------- ASSIGN reg_{name.of.y} to each dataframe
  # res.l <- lapply(res.l, function(res.i){
  #   res.i <- res.i
  #   colnames(res.i)[-1] <- paste0("reg_", y,".", colnames(res.i)[-1])
  #   return(res.i)
  # })
  
  return(res.l)
}


# --------- FINAL LINEAR MODEL FUNCTION

pv.rrepest.gen <- function(data,  svy, y, by.var = NULL, over = NULL, test = F, 
                          user_na=F, flag = F, fast = F, pv = F, ...) {
  # Goal: Dataframe with β and SE by Fay's BRR model for linear regression
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # isced : (number) isced level to analyze
  # by.var : (string) column in which we'll break down results
  # y : (string) method to include in normal r script
  # user_na : (Bool) TRUE → show nature of user defined missing values in by.var
  # over : (vector string) columns over which to do analysis
  # test : (bool) If TRUE will calculate the difference between over variables
  # flag : (Bool) TRUE → Show NaN when there is not enough cases (or schools)
  # fast : (bool) TRUE → Only do 6 replicated weights
  # pv : (Bool) TRUE → We are in plausible values and must parallelize
  # ...
  # isced : (number) isced level to analyze
  
  # Get optional arguments
  extra.args <- list(...)
  
  # If there is an OVER variable remove NAs from OVER vars
  # if (length(over) > 0) {
  #   for (i in over) {
  #     df <- df %>% drop_na(i)
  #   }
  # }
  
  # Get weight names
  weight.names <- replicated_w_names(svy, ...)
  
  # Loop over each variable
  brr.res <- pv.loop.gen.on.weights(data = data,
                                   y = y,
                                   svy = svy,
                                   by.var = by.var,
                                   over = over,
                                   test = test,
                                   flag = flag,
                                   rep_weights = weight.names,
                                   fast = fast,
                                   pv = pv)
  
  # Reorder data to have b and se intertwined
  res <- pv.get.se.reorder(brr.res, svy = svy, pv = pv, ... = ...)
  
  return(res)
}
