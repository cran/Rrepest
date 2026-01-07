################# Functionalize Quantile Table #################
# 8 March 2025
# Rodolfo Ilizaliturri
#############################################################
# Goal: Get table of the mean at each quantile for n quantiles


pv.do.quantiletable <- function(df, quant_n, y, by.var, w, ...){
  # Goal: Get table of the mean at each quantile for n quantiles of equal length according to the weight
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze previosly formated
  # by.var : (string) column in which we'll break down results !IOP!: several variables
  # quant_n : (numeric) number of quantiles to do. If not integer, will get difference of last quantile minus first
  # y : (string) Target variable (just 1). If two variables inputed in the format "var1|var2", 
  #     it will order and get same length quantiles according to the first and get the mean of the second
  # w : (string) weighting variable (just 1)
  
  # Search pattern "var1__var2" in y
  if(grepl("__",y)){
    # If found separete element into "y for order" and "y for mean""
    y_vars <- strsplit(y,"__")[[1]]
    y_4order <- y_vars[1]
    y_4mean <- y_vars[2]
    # Get the name of y for columns
    y_name <- paste0(y_4order,"__",y_4mean)
  } else {
    # If not found they are both the same
    y_4order <- y
    y_4mean <- y
    # Get the name of y for columns
    y_name <- y_4order
  }
  
  diff_quant <- FALSE
  # If quant_n has decimals floor the number
  if(quant_n %% 1 != 0){
    quant_n <- floor(quant_n)
    diff_quant <- TRUE
  }
  
  if (is.data.table(df)) {
    # Faster drop of NAs if it is a data.table
    df.qtable <- na.omit(df, cols = c(y_4order))
  } else {
    # Selecting data and preparation
    df.qtable <- df[c(unique(c(y_4order,y_4mean)),by.var,w)]
    
    df.qtable <- df.qtable %>% drop_na(all_of(c(y_4order)))
  }
  

  # Get quantiles -----------------------------------------------------------

  # Get average by each quantile
  res.df <- df.qtable %>% 
    group_by(across(all_of(c(by.var)))) %>% 
    do({
      # Set seed
      set.seed(5094) #For random order of values
      
      # Create dataframe for organization and noise
      df_quantiletable <- .data[c(unique(c(y_4order,y_4mean)),w)]
      df_quantiletable[["noise"]] <- runif(nrow(df_quantiletable))
      
      # Weight accounting for NA in y
      df_quantiletable[[w]] <- ifelse(is.na(df_quantiletable[[y_4order]]),NA,1) * df_quantiletable[[w]]
      
      # Value of quantiles
      quantile_breaks <- seq(0,sum(df_quantiletable[[w]],na.rm = TRUE),length.out = quant_n + 1)
      # Force last element to be avobe breaks to make them all fall into a bin
      quantile_breaks[length(quantile_breaks)] <- quantile_breaks[length(quantile_breaks)] + 1
      
      # Add noise, arrange and do cumsum
      df_quantiletable <- df_quantiletable %>% 
        mutate("y_order" := !!rlang::sym(y_4order) + 0.0001*.$noise) %>% 
        arrange(.$y_order) %>% 
        mutate("w_cumsum" := cumsum(!!rlang::sym(w)))
      
      # Assign value of y to each bin of quants
      df_quantiletable[["quantile_table_cuts"]] <- cut(
        x = df_quantiletable[["w_cumsum"]], 
        breaks = quantile_breaks, 
        include.lowest = TRUE, right = TRUE,
        labels = paste0("q",1:quant_n,".",y_name))
      
      # Weighted mean grouped by cut
      res_quintiletable <- df_quantiletable %>% 
        group_by("quantile_table_cuts" = .$quantile_table_cuts) %>% #groupby done like that to avoid  pv.do.quantiletable: no visible binding for global variable 'quantile_table_cuts'
        summarise(q_table = weighted.mean(x = .data[[y_4mean]], w = .data[[w]], na.rm = TRUE)) %>% 
        pivot_wider(values_from = "q_table", names_from = "quantile_table_cuts")
      
      # If quant_n has decimals subtract last from first
      if(diff_quant){
        res_quintiletable <- res_quintiletable %>% 
          mutate(!!rlang::sym(paste0("q",quant_n,"_1.",y_name)) := .[[quant_n]] - .[[1]])
      }
      
      res_quintiletable
    }) %>% 
    ungroup()
  
  return(res.df)
}
pv.do.quantiletable.PAR <- function(df, quant_n, y, by.var, w, ...){
  # Goal: Get table of the mean at each quantile for n quantiles
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze previosly formated
  # by.var : (string) column in which we'll break down results !IOP!: several variables
  # quant_n : (numeric) number of quantiles to do.
  # y : (string) target variable (just 1).
  # w : (string) weighting variable (just 1)
  # ...
  #(functions) weighted.var
  
  # get ... arguments
  arg <- list(...)
  
  # Search pattern "var1|var2" in y
  if(grepl("__",y)){
    # If found separete element into "y for order" and "y for mean""
    y_vars <- strsplit(y,"__")[[1]]
    y_4order <- y_vars[1]
    y_4mean <- y_vars[2]
    # Get the name of y for columns
    y_name <- paste0(y_4order,"__",y_4mean)
  } else {
    # If not found they are both the same
    y_4order <- y
    y_4mean <- y
    # Get the name of y for columns
    y_name <- y_4order
  }
  
  diff_quant <- FALSE
  # If quant_n has decimals floor the number
  if(quant_n %% 1 != 0){
    quant_n <- floor(quant_n)
    diff_quant <- TRUE
  }
  
  if (is.data.table(df)) {
    # Faster drop of NAs if it is a data.table
    df.qtable <- na.omit(df, cols = c(y_4order,y_4mean))
  } else {
    # Selecting data and preparation
    df.qtable <- df[c(y_4order,y_4mean,by.var,w)]
    
    df.qtable <- df.qtable %>% drop_na(all_of(c(y_4order,y_4mean)))
  }
  
  
  # Get quantiles -----------------------------------------------------------
  
  # Get average by each quantile
  res.df <- df.qtable %>% 
    group_by(across(all_of(c(by.var)))) %>% 
    do({
      # Set seed
      set.seed(5094) #For random order of values
      
      # Create dataframe for organization and noise
      df_quantiletable <- .data[c(y_4order,y_4mean,w)]
      df_quantiletable[["noise"]] <- runif(nrow(df_quantiletable))
      
      # Weight accounting for NA in y
      df_quantiletable[[w]] <- ifelse(is.na(df_quantiletable[[y_4order]]),NA,1) * df_quantiletable[[w]]
      
      # Value of quantiles
      quantile_breaks <- seq(0,sum(df_quantiletable[[w]],na.rm = TRUE),length.out = quant_n + 1)
      # Force last element to be avobe breaks to make them all fall into a bin
      quantile_breaks[length(quantile_breaks)] <- quantile_breaks[length(quantile_breaks)] + 1
      
      # Add noise, arrange and do cumsum
      df_quantiletable <- df_quantiletable %>% 
        mutate("y_order" := !!rlang::sym(y_4order) + 0.0001*.$noise) %>% 
        arrange(.$y_order) %>% 
        mutate("w_cumsum" := cumsum(!!rlang::sym(w)))
      
      # Assign value of y to each bin of quants
      df_quantiletable[["quantile_table_cuts"]] <- cut(
        x = df_quantiletable[["w_cumsum"]], 
        breaks = quantile_breaks, 
        include.lowest = TRUE, right = TRUE,
        labels = paste0("q",1:quant_n,".",y_name))
      
      # Weighted mean grouped by cut
      res_quintiletable <- df_quantiletable %>% 
        group_by("quantile_table_cuts" = .$quantile_table_cuts) %>% 
        summarise(q_table = weighted.mean(x = .data[[y_4mean]], w = .data[[w]], na.rm = TRUE)) %>% 
        pivot_wider(values_from = "q_table", names_from = "quantile_table_cuts")
      
      # If quant_n has decimals subtract last from first
      if(diff_quant){
        res_quintiletable <- res_quintiletable %>% 
          mutate(!!rlang::sym(paste0("q",quant_n,"_1.",y_name)) := .[[quant_n]] - .[[1]])
      }
      
      res_quintiletable
    }) %>% 
    ungroup()
  
  return(res.df)
}
pv.loop.quantiletable.on.weights <- function (data, quant_n, y, by.var, over, test = F, flag = F,
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
      res.df <- pv.do.quantiletable(data, quant_n, y, all_of(c(by.var,over)), w.i) %>% 
        unite("by.var", all_of(c(by.var,over)), sep = "|")
      
      # Change col names to include weight name
      # colnames(res.df)[-1] <- paste0(colnames(res.df)[-1], paste0("|",w.i))
      
      return(res.df)
    })
    # PARALLEL ---------------------------------------------------------------.
  }else{
    res.l <- foreach(w.i = rep_weights,
                     .packages = c("dplyr","tidyr","data.table","tibble"),
                     .export = c("pv.do.quantiletable.PAR","n.obs.x",
                                 "weighted.quant","...")) %dopar% {
                                   
                                   res.df <- pv.do.quantiletable.PAR(data.par,
                                                                     quant_n,
                                                              y,
                                                              all_of(c(by.var,over)),
                                                              w.i,
                                                              weighted.quant = weighted.quant,
                                                              ...=...) %>% 
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
  # ---------------- ASSIGN qtableq#.{name.of.y} to each dataframe
  res.l <- lapply(res.l, function(res.i){
    res.i <- res.i
    colnames(res.i)[-1] <- paste0("qtable", colnames(res.i)[-1])
    return(res.i)
  })
  
  return(res.l)
}


# --------- FINAL LOGISTIC MODEL FUNCTION

pv.rrepest.quantiletable <- function(data,  svy, quant_n, y, by.var = NULL, over = NULL, test = F, 
                              user_na=F, flag = F, fast = F, pv = F, ...) {
  # Goal: Dataframe with β and SE by Fay's BRR model for logistic regression
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # isced : (number) isced level to analyze
  # by.var : (string) column in which we'll break down results
  # quant_n : (numeric) number of quantiles to do.
  # y : (string) Target variable (just 1).
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
  brr.res <- pv.loop.quantiletable.on.weights(data = data,
                                              quant_n = quant_n,
                                              y = y,
                                              svy = svy,
                                              by.var = by.var,
                                              over = over,
                                              test = test,
                                              flag = flag,
                                              rep_weights = weight.names,
                                              fast = fast,
                                              pv = pv,
                                              ... = ...)
  
  # Reorder data to have b and se intertwined
  res <- pv.get.se.reorder(brr.res, svy = svy, pv = pv, ... = ...)
  
  return(res)
}
