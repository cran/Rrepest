################# Functionalize Linear regression an PVs #################
# 4 April 2023
# Rodolfo Ilizaliturri
#############################################################
# Goal: Obtain coefficients and R2 drom regression model with greater speed

#' Format continuous variables as numeric for Rrepest
#'
#' @param df (data frame) Data to be analysed.
#' @param cont.vars (string vector) continuous variables for analysis
#'
#' @return Data frame with cantinuous variables converted to numeric for a continuous analysis (means, regression, etc.)
#' @export
#'
#' @examples format_data_cont_vars(mtcars,"hp")
format_data_cont_vars <- function(df, cont.vars) {
  # Goal: Converts to numeric all variables for a continuous analysis (means, regression, etc.)
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # cont.vars : (string vector) continuous variables for analysis
  
  df.res <- df %>% 
    mutate(across(contains(cont.vars) & ( where(is.labelled)), remove_user_na, user_na_to_na = T)) %>%
    mutate(across(contains(cont.vars), as.numeric))
  
  return(df.res)
}
pv.do.lm <- function(df, x, y, by.var, w){
  # Goal: Get coefficients and r2 from linear regression
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze previosly formated
  # by.var : (string) column in which we'll break down results !IOP!: several variables
  # x : (string vector) independant variable (1+)
  # y : (string) dependant variable (just 1)
  # w : (string) weighting variable (just 1)
  
  
  
  if (is.data.table(df)) {
    # Faster drop of NAs if it is a data.table
    df.reg <- na.omit(df, cols = c(x,y)) %>% 
      mutate(intercept = 1)
  } else {
    # Selecting data and preparation
    df.reg <- df[c(x,y,by.var,w)] %>% 
      mutate(intercept = 1)
    
    df.reg <- df.reg %>% drop_na(all_of(c(x,y)))
  }
  
  
  
  
  #model only fitting values. Out: Coeff and R2 ONLY
  res.df <- df.reg %>% 
    group_by(across(all_of(by.var))) %>% 
    do({
      model <- lm.wfit(x = .[c('intercept', x)] %>% as.matrix(),
                       y = .[y] %>% as.matrix(),
                       w = .[w] %>% as.matrix() %>% as.vector())
      
      #R2
      Y <- .[y] %>% as.matrix()
      W <- .[w] %>% as.matrix() %>% as.vector()
      
      ssr <- weighted.var(Y-resid(model),W)
      sst <- weighted.var(Y,W)
      r2 <- ssr/sst
      
      model %>% 
        coefficients() %>% 
        t() %>% 
        as.data.frame() %>% 
        add_column(rsqr = r2)
      
    }
    ) %>%
    ungroup()
  
  return(res.df)
}
pv.do.lm.PAR <- function(df, x, y, by.var, w, weighted.var){
  # Goal: Get coefficients and r2 from linear regression
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze previosly formated
  # by.var : (string) column in which we'll break down results !IOP!: several variables
  # x : (string vector) independant variable (1+)
  # y : (string) dependant variable (just 1)
  # w : (string) weighting variable (just 1)
  # ...
  #(functions) weighted.var
  
  # get ... arguments
  # arg <- list(...)
  
  
  if (is.data.table(df)) {
    # Faster drop of NAs if it is a data.table
    df.reg <- na.omit(df, cols = c(x,y)) %>% 
      mutate(intercept = 1)
  } else {
    # Selecting data and preparation
    df.reg <- df[c(x,y,by.var,w)] %>% 
      mutate(intercept = 1)
    
    df.reg <- df.reg %>% drop_na(all_of(c(x,y)))
  }
  
  
  
  
  #model only fitting values. Out: Coeff and R2 ONLY
  res.df <- df.reg %>% 
    group_by(across(all_of(by.var))) %>% 
    do({
      model <- lm.wfit(x = .[c('intercept', x)] %>% as.matrix(),
                       y = .[y] %>% as.matrix(),
                       w = .[w] %>% as.matrix() %>% as.vector())
      
      #R2
      Y <- .[y] %>% as.matrix()
      W <- .[w] %>% as.matrix() %>% as.vector()
      
      ssr <- weighted.var(Y-resid(model),W)
      sst <- weighted.var(Y,W)
      r2 <- ssr/sst
      
      model %>% 
        coefficients() %>% 
        t() %>% 
        as.data.frame() %>% 
        add_column(rsqr = r2)
      
    }
    ) %>%
    ungroup()
  
  return(res.df)
}
pv.loop.lm.on.weights <- function (data, x, y, by.var, over, test = F, flag = F,
                                   svy, rep_weights, fast = F, pv = F, ...) {
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
      #Do lm, add weight to colnames, append new data of results to list
      res.df <- pv.do.lm(data, x, y, all_of(c(by.var,over)), w.i) %>% 
        unite("by.var", all_of(c(by.var,over)), sep = "|")
      
      # Change col names to include weight name
      # colnames(res.df)[-1] <- paste0(colnames(res.df)[-1], paste0("|",w.i))
      
      return(res.df)
    })
    # PARALLEL ---------------------------------------------------------------.
  }else{
    res.l <- foreach(w.i = rep_weights,
                     .packages = c("dplyr","tidyr","data.table","tibble"),
                     .export = c("pv.do.lm.PAR","n.obs.x",
                                 "weighted.var")) %dopar% {
                                   
                                   res.df <- pv.do.lm.PAR(data.par,
                                                          x,
                                                          y,
                                                          all_of(c(by.var,over)),
                                                          w.i,
                                                          weighted.var = weighted.var) %>% 
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
  res.l <- lapply(res.l, function(res.i){
    res.i <- res.i
    colnames(res.i)[-1] <- paste0("reg_", y,".", colnames(res.i)[-1])
    return(res.i)
  })
  
  return(res.l)
}


# --------- FINAL LINEAR MODEL FUNCTION

pv.rrepest.lm <- function(data,  svy, x, y, by.var = NULL, over = NULL, test = F, 
                       user_na=F, flag = F, fast = F, pv = F, ...) {
  # Goal: Dataframe with β and SE by Fay's BRR model for linear regression
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # isced : (number) isced level to analyze
  # by.var : (string) column in which we'll break down results
  # x : (string vector) independant variable (1+)
  # y : (string) dependant variable (just 1)
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
  brr.res <- pv.loop.lm.on.weights(data = data,
                                x = x,
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
