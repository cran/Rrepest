################# Frequencies on categoric variables PVs #################
# 1 April 2023
# Rodolfo Ilizaliturri
#############################################################
# Goal: Obtain frequencies using for replicated weights and/or plausible values


# --------- WORK FUNCTIONS
# ------------------.

pv.get.se.reorder <- function(brr.res, svy, pv = F, ...) {
  # Goal: Dataframe with coefficients and standard error of statistics from model
  # ------ INPUTS ------.
  # brr.res: (list of DFs) A list of dataframes in tidy format with 1st colum by.var
  # and next columns all wanted statistics from model. Each element is statistics for
  # each weight
  # svy : (string) List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # pv : (Bool) TRUE → We are in plausible values and keep calculation as variance, not SE
  
  betas <- brr.res[[1]] 
  #Get indexes of columns minus by.var
  cols.data <- seq_along( colnames(brr.res[[1]][-1]) ) + 1 
  #Get raw column names
  se.c.names <- sapply(colnames(brr.res[[1]]), function (x) strsplit(x,
                                                                     split = "[|]")[[1]][1])
  #Paste colnames and .b on betas
  colnames(betas)[-1] <- paste0("b.",se.c.names[-1])
  
  
  #Empty list > for all columns but by.bar > extract same col on all dfs 
  #> join them and store them . Each element of list is statistic on T and i weights
  brr.beta = list()
  for (i in cols.data) {
    res <- brr.res %>% 
      lapply(function(x) x[c(1,i)]) %>% 
      reduce(full_join, by = "by.var")
    
    brr.beta[[i-1]] <- res
  }
  #1st col to rnames> rrvar on each element > rnames to by.var > join them all
  se.beta <- lapply(brr.beta, function(x) {
    rowname.x <- pull(x[1]) 
    x <- data.frame(x[,-1])
    rownames(x) <- rowname.x
    apply(x,1,rrvar.pv, svy = svy, pv = pv, ... = ...) %>% 
      as.data.frame() %>% 
      rownames_to_column("by.var")
  }) %>% 
    reduce(full_join, by = "by.var")
  
  #rename se.beta colnames to include .se and join
  colnames(se.beta)[-1] <- paste0("se.",se.c.names[-1])
  res <- full_join(betas, se.beta, by="by.var")
  
  #reorder df with intercalating columns
  n.betas <- (ncol(res) - 1)/2
  betas.order <- lapply(1:n.betas,function(x) c(x,x+n.betas)) %>% 
    reduce(function (x,y) c(x,y))
  res <- res[,c(1,betas.order + 1)]
  
  return(res)
}

#' Within group frequencies of categories in a column.
#'
#' @param data (data frame) Data to analyze.
#' @param small.level (string vector) all variables to get grouped sum.
#' @param big.level (string vector) Must be fully contained in variables from small.level
#' @param w (string) Numeric variable from which to get weights (if NULL then 1).
#'
#' @return Data frame with frequencies from the grouped sum of small.level and big.level used for getting percentages.
#' @export
#'
#' @examples grouped_sum_freqs(data = mtcars,small.level = c("cyl","gear"),big.level = c("cyl"))
grouped_sum_freqs <- function(data, small.level, big.level, w = NULL) {
  # Goal: Get a dataframe with frequencies from the grouped sum of small.level and big.level used for getting percentages
  # Example: cnt gender wealth 
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # w : (string) numeric variable from which to get weights (if NULL then 1)
  # small.level : (string vector) all variables to get grouped sum
  # big.level : (string vector) Must be fully contained in variables from small.level 
  
  # If no weights then add 1s
  if(is.null(w)) {
    data$weight <- 1
    w <-"weight"
  }
  
  #Small group
  small.group <- data %>% 
    group_by(across(all_of(small.level))) %>% 
    summarise(small.sum = sum(get(w), na.rm = T))
  
  #Bigger group with variable list fully contained in small group
  big.group <- data %>% 
    group_by(across(all_of(big.level))) %>% 
    summarise(big.sum = sum(get(w), na.rm = T))
  
  # Do frequencies in 100% format. Resulting variable is "freq"
  res <- full_join(small.group,big.group,by = big.level) %>%
    mutate(freq = small.sum / big.sum * 100) %>% 
    select(-small.sum,-big.sum)
  
  return(res)
}


row.pct.byvar.x <- function(data, x, by.var, over, test = F, flag = F, w, ...) {
  # Goal: Dataframe with coefficients and standard error of statistics from model
  # ------ INPUTS ------.
  # data : (dataframe) df to analize
  # w : (string) name of replicated weight var
  # x : (string) lvariable from which to get frequencies
  # by.var : (string vector) variables to break analysis by
  # over : (vector string) columns over which to do analysis
  # test : (bool) If TRUE will calculate the difference between over variables
  
  res.df <- grouped_sum_freqs(data = data,
                              w = w,
                              small.level = c(by.var, over, x),
                              big.level = c(by.var, over)) %>% 
    ungroup()
  
  # Put together all categorical variables into by.var
  res.df <- res.df %>% 
    unite("by.var", all_of(c(by.var, over, x)), sep = "|")
  
  # Give names with weight to number columns
  #colnames(res.df) <- c(colnames(res.df)[1], 
  #                      paste0(colnames(res.df[-1]),paste0("|",w))) #separator must be | for pretty names
  
  return(res.df)
}

row.pct.byvar.x.PAR <- function(data, x, by.var, over, test = F, flag = F, w, ...) {
  # Goal: Dataframe with coefficients and standard error of statistics from model
  # ------ INPUTS ------.
  # data : (dataframe) df to analize
  # w : (string) name of replicated weight var
  # x : (string) lvariable from which to get frequencies
  # by.var : (string vector) variables to break analysis by
  # over : (vector string) columns over which to do analysis
  # test : (bool) If TRUE will calculate the difference between over variables
  # ...
  #(functions) grouped_sum_freqs
  arg <- list(...)
  
  res.df <- arg$grouped_sum_freqs(data = data,
                              w = w,
                              small.level = c(by.var, over, x),
                              big.level = c(by.var, over)) %>% 
    ungroup()
  
  # Put together all categorical variables into by.var
  res.df <- res.df %>% 
    unite("by.var", all_of(c(by.var, over, x)), sep = "|")
  
  # Give names with weight to number columns
  #colnames(res.df) <- c(colnames(res.df)[1], 
  #                      paste0(colnames(res.df[-1]),paste0("|",w))) #separator must be | for pretty names
  
  return(res.df)
}

pv.loop.freq.on.weights <- function(data, x, by.var, over, test = F, flag = F, svy, rep_weights, pv = F, ...) {
  # Goal: List with all weighted frequencies of x by by.var
  # ------ INPUTS ------.
  # data : (dataframe) df to analize
  # w : (string) name of replicated weight var
  # x : (string) lvariable from which to get frequencies
  # by.var : (string vector) variables to break analysis by
  # over : (vector string) columns over which to do analysis
  # test : (bool) If TRUE will calculate the difference between over variables
  # rep_weights : (string vector) names of replicated weight vars
  # flag : (Bool) TRUE → Show NaN when there is not enough coverage
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # pv : (Bool) TRUE → We are in plausible values and must parallelize
  
  if (!pv) {
  # NON PARALLEL ------------------------------------------------------------.
    res.l <- lapply(rep_weights, function(w.i) {
      # would remove NAs from over variables here
      row.pct.byvar.x(data = data,
                      x =  x,
                      by.var =  by.var,
                      over =  over,
                      test = test,
                      flag = flag,
                      w =  w.i)
    })
  # PARALLEL ---------------------------------------------------------------.
  }else{
    res.l <- foreach(w.i = rep_weights,
                     .packages = c("dplyr","tidyr", "purrr"),
                     .export = c("row.pct.byvar.x.PAR","grouped_sum_freqs")) %dopar% {
                      # would remove NAs from over variables here             
                               
                       # same loop but paralelizing w.i
                       row.pct.byvar.x.PAR(data = data.par,
                                       x =  x,
                                       by.var =  by.var,
                                       over =  over,
                                       test = test,
                                       flag = flag,
                                       w =  w.i,
                                       grouped_sum_freqs = grouped_sum_freqs)
                     }
  }
  
  
  
  # ---------------- FLAGS
  if (flag) {
    # Get n for flags and separate column
    n.df <- n.obs.x(df = data, by.var = c(by.var, over, x), x = x, svy = svy)
    # Fix for frequencies
    n.df <- n_obs_sch_freq_fix(n_df = n.df, by.var = by.var, over = over, x = x)
    
    res.l <- lapply(res.l, function(res.i){
      # Merge both dfs together n and freqs
      res.df <- left_join(res.i, n.df, by = c("by.var"="by.group"))
      
      #Assign NaN when not enough coverage for estimation
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
        separate(col = "by.var", into = all_of(c(by.var, over, x)), sep = "\\|") %>% 
        over.test.freq(over = c(over, x)) %>% 
        rename("by.var"="by.group")
      
      return(res.df)
    })
  } 
  # ---------------- .
  # ---------------- FORMAT DF to have over and x on the columns
  res.l <- lapply(res.l, function(res.i){
    res.df <- res.i %>% 
      # Split by.var leaving only the variables in "by.var" united 
      separate(col = "by.var", into = all_of(c(by.var, over, x)), sep = "\\|") %>% 
      unite(col = "by.var", all_of(by.var), sep = "|") %>% 
      # Add variable name to column values
      mutate("{x}" := paste0(x,".",get(x))) %>% 
      # Pivot to columns over and x
      pivot_wider(names_from = all_of(c(x,over)),
                  # grab only numeric columns after by.var, over, and x
                  values_from = colnames(select_if(.,is.numeric)),
                  names_sep = "..")
    
    return(res.df)
  })
  # ---------------- .
  
  return(res.l)
}

# --------- FINAL FREQUENCIES FUNCTION
pv.rrepest.frequencies <- function(data, svy, x, by.var = NULL, over = NULL, test = F,
                                user_na=F, show_na=F, flag = F, fast = F, pv = F, ...) {
  # Goal: Dataframe with means and SE by Fay's BRR model
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # by.var : (string vector) column in which we'll break down results
  # x : (string) variable from where to get frequencies
  # over : (vector string) columns over which to do analysis
  # test : (bool) If TRUE will calculate the difference between over variables
  # user_na : (Bool) TRUE → show nature of user defined missing values for by.var
  # show_na : (Bool) TRUE → include na in frequencies of x
  # flag : (Bool) TRUE → Show NaN when there is not enough cases (or schools)
  # fast : (bool) TRUE → Only do 6 replicated weights
  # pv : (Bool) TRUE → We are in plausible values and must parallelize
  # ...
  # isced : (number) isced level to analyze
  # pvdigits : (string/ numeric vector) pv.digits argument: # of pvs or @ if none
  
  # Get optional arguments
  extra.args <- list(...)
  

  # Remove NA's from tgt and over -------------------------------------------
  
  # If there is an OVER variable remove NAs from OVER vars
  if (length(get.pv.arguments(extra.args$pvdigits,over)) > 0) {
    if (!any(show_na,user_na)) {
      for (i in get.pv.arguments(extra.args$pvdigits,over)) {
        data <- data %>% drop_na(i)
      }
    }
  }
  
  # X is categorical in a frequency, must be formatted accordingly
  data <- format_data_categ_vars(data,
                               get.pv.arguments(extra.args$pvdigits, x),
                               show_na)
  
  # if there are PVs, export this new database to the cluster "cl"
  # if (pv) {
  #   clusterExport(cl, "data.par",envir = environment())
  # }
  
  
  # Get weight names
  weight.names <- replicated_w_names(svy, ...)
  # If fast then less replicated weights
  if (fast) {
    weight.names <- weight.names[1:6]
  }
  
  # Loop over each variable
  brr.res <- pv.loop.freq.on.weights(data = data,
                                  svy = svy,
                                  x = x, 
                                  by.var = by.var, 
                                  over = over,
                                  test = test,
                                  flag = flag,
                                  rep_weights = weight.names,
                                  pv = pv,
                                  ... = ...)
  
  
  
  # Reorder data to have b and se intertwined
  res <- pv.get.se.reorder(brr.res, svy = svy, pv = pv, ...)
  
  
  return(res)
  
}
