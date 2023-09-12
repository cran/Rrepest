################# Weighted correlation with PVs #################
# 7 April 2023
# Rodolfo Ilizaliturri
#############################################################
# Goal: Do weighted correlation

# Bivariate correlation and covariance ------------------------------------
################# Weighted correlation #################
# 11 March 2023
# Rodolfo Ilizaliturri
#############################################################
# Goal: Do weighted correlation

# Bivariate correlation and covariance ------------------------------------

weighted.corr <- function(x, y, w, na.rm = T) {
  # Goal: Get weighted correlation
  # ------ INPUTS ------.
  # x: (vector) variable from where to get correlation
  # y: (vector) variable from where to get correlation
  # w : (vector) vector of weights
  
  # Use non NAs in both x and y
  use <- !is.na(x) & !is.na(y)
  x <- x[use]
  y <- y[use]
  w <- w[use]
  
  # Calculate variance and covariance of x and y
  w.mean.x <- weighted.mean(x, w, na.rm = na.rm)
  w.mean.y <- weighted.mean(y, w, na.rm = na.rm)
  cov.w <- sum(w*(x-w.mean.x)*(y-w.mean.y), na.rm = na.rm)
  var.w.x <- sum(w*(x-w.mean.x)^2, na.rm = na.rm)
  var.w.y <- sum(w*(y-w.mean.y)^2, na.rm = na.rm)
  
  return(cov.w / sqrt(var.w.x * var.w.y))
}
weighted.cov <- function(x, y, w, na.rm = T) {
  # Goal: Get weighted covariance
  # ------ INPUTS ------.
  # x: (vector) variable from where to get correlation
  # y: (vector) variable from where to get correlation
  # w : (vector) vector of weights
  
  # Use non NAs in both x and y
  use <- !is.na(x) & !is.na(y)
  x <- x[use]
  y <- y[use]
  w <- w[use]
  
  # Covariance
  w.mean <- weighted.mean(x, w, na.rm = na.rm)
  cov.w <- sum(w * (x - w.mean) * (y - w.mean), na.rm = na.rm)/sum(w, na.rm = na.rm)
  return(cov.w)
}

# Ex. ----
# df.tests <- df.t[c("CNTRY","T3STAKE","T3TEAM","T3STUD","IDPOP","TCHWGT")] %>% 
#   filter(IDPOP == 2,
#          CNTRY == "ABA") %>% 
#   user_na_to_na()
# 
# weighted.corr(df.tests$T3STAKE,df.tests$T3TEAM,df.tests$TCHWGT)

# ----


# Multivariate correlation and covariance ---------------------------------
# Does all 2 Choose length(vector) correlations
weighted.corr.cov.n <- function(data, x, w =  rep(1, length(data[x[1]])), corr = T, na.rm = T) {
  # Goal: Get weighted correlation or covariance of n variables
  # ------ INPUTS ------.
  # data : (dataframe) df to analyze
  # x: (vector string) variables from where to get correlation/covariance
  # w : (string) weight name
  # corr : (bool) T → get correlation, F → get covariance
  
  # Get dataframe of correlation
  vec.res <- apply(as.data.frame(combn(x,2)), 2, function(x.i) {
    if (corr) {
      res <- weighted.corr(x = data[x.i[1]],
                           y = data[x.i[2]], 
                           w = data[w],
                           na.rm = na.rm)
    }else{
      res <- weighted.cov(x = data[x.i[1]],
                          y = data[x.i[2]], 
                          w = data[w],
                          na.rm = na.rm)
    }
    
    return(res)
  })
  # Convert vector to dataframe if 1 by n
  vec.res <- data.frame(t(vec.res))
  
  # Get column names
  vec.names <- apply(as.data.frame(combn(x,2)), 2, function(x.i) {
    if (corr) {
      res <- paste0("corr.",x.i[1],"&",x.i[2])
    }else{
      res <- paste0("cov.",x.i[1],"&",x.i[2])
    }
    return(as.vector(res))
  })
  colnames(vec.res) <- vec.names
  
  return(vec.res)
}
# Ex. weighted.corr.cov.n(df.tests,c("T3STAKE","T3TEAM","T3STUD"),"TCHWGT")

# Special to be faster in the loop
weighted.corr.cov.n.gby <- function(x, w, corr = T) {
  # Goal: Create string to be later evaluated in group.by with dplyr
  # ------ INPUTS ------.
  # x: (vector string) variables from where to get correlation/covariance
  # w : (string) weight name
  # corr : (bool) T → get correlation, F → get covariance
  
  # Get strings of correlation for group_by
  str.res <- apply(as.data.frame(combn(x,2)), 2, function(x.i) {
    if (corr) {
      res <- paste0( "corr.", x.i[1], "_", x.i[2]," = weighted.corr(get('",x.i[1],"'),get('",x.i[2],"'),get('",w,"'))" )
      
    }else{
      res <- paste0( "cov.", x.i[1], "_", x.i[2]," = weighted.cov(get('",x.i[1],"'),get('",x.i[2],"'),get('",w,"'))" )
    }
    return(res)
  })
  
  
  return(paste(str.res, collapse = ","))
}
# Ex. ----
# eval(parse(text = 
#              paste0("df.t %>% group_by(CNTRY) %>% summarise(",
#                     weighted.corr.cov.n.gby(c("T3STAKE","T3TEAM","T3STUD"),"TCHWGT"),
#                     ")")
# ))
# ----
weighted.corr.cov.n.gby.PAR <- function(x, w, corr = T, ...) {
  # Goal: Create string to be later evaluated in group.by with dplyr
  # ------ INPUTS ------.
  # x: (vector string) variables from where to get correlation/covariance
  # w : (string) weight name
  # corr : (bool) T → get correlation, F → get covariance
  # ... (function) weighted.corr, weighted.cov
  
  # Get optional arguments
  arg <- list(...) 
  
  # Get strings of correlation for group_by
  str.res <- apply(as.data.frame(combn(x,2)), 2, function(x.i) {
    if (corr) {
      res <- paste0( "corr.", x.i[1], "_", x.i[2]," = weighted.corr(get('",x.i[1],"'),get('",x.i[2],"'),get('",w,"'))" )
      
    }else{
      res <- paste0( "cov.", x.i[1], "_", x.i[2]," = weighted.cov(get('",x.i[1],"'),get('",x.i[2],"'),get('",w,"'))" )
    }
    return(res)
  })
  
  
  return(paste(str.res, collapse = ","))
}
# Loop on weights for corr and cov ------------------------------------
pv.loop.corr.cov.on.weights <- function(data, svy, x, by.var, over, test = F, flag = F, rep_weights, corr = T, pv = F) {
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
  # corr : (bool) TRUE → get correlation, F → get covariance
  # pv : (Bool) TRUE → We are in plausible values and must parallelize
  
  if (!pv) {
    # NON PARALLEL ------------------------------------------------------------.
  
    ###################################################
    ################ Loop correlations ################ 
    res.l <- lapply(rep_weights, function(w.i) {
      res.str <- paste0("data %>% group_by(across(all_of(c(by.var,over)))) %>% summarise(",weighted.corr.cov.n.gby(x = x, w = w.i, corr = corr),")")
      
      res.df <- eval(parse(text = res.str)) %>% 
        ungroup()
      
      # Put together all categorical variables into by.var
      res.df <- res.df %>% 
        unite("by.var", all_of(c(by.var, over)), sep = "|")
      return(res.df)
    })
    ##################################################
  
    # PARALLEL ---------------------------------------------------------------.
  }else{ 
    res.l <- foreach(w.i = rep_weights,
                     .packages = c("dplyr","tidyr"),
                     .export = c("weighted.corr.cov.n.gby.PAR","weighted.corr",
                                 "weighted.cov")) %dopar% {
                                  # would remove NAs from over variables here
                                   res.str <- paste0("data.par %>% group_by(across(all_of(c(by.var,over)))) %>% summarise(",
                                                     weighted.corr.cov.n.gby.PAR(x = x, 
                                                                                 w = w.i, 
                                                                                 corr = corr, 
                                                                                 weighted.corr = weighted.corr,
                                                                                 weighted.cov = weighted.cov),")")
                                   
                                   res.df <- eval(parse(text = res.str)) %>% 
                                     ungroup()
                                   
                                   # Put together all categorical variables into by.var
                                   res.df <- res.df %>% 
                                     unite("by.var", all_of(c(by.var, over)), sep = "|")
                                   
                                   return(res.df) 
                                 }
  }
  # ---------------- FLAGS
  if (flag) {
    # Get n for flags and separate column
    n.df <- n.obs.x(df = data, by.var = c(by.var, over), x = x, svy = svy)
    
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
      # Split by.var into c(by.var, over) by |
      res.df <- res.i %>% 
        separate(col = "by.var", into = all_of(c(by.var, over)), sep = "\\|") %>% 
        over.test(over = all_of(c(over))) %>% 
        rename("by.var"="by.group")
      
      return(res.df)
    })
  } 
  # ---------------- .
  
  # ---------------- FORMAT DF to have over on the columns
  res.l <- lapply(res.l, function(res.i){
    res.df <- res.i %>% 
      # Split by.var leaving only the variables in "by.var" united 
      separate(col = "by.var", into = all_of(c(by.var, over)), sep = "\\|") %>% 
      unite(col = "by.var", all_of(by.var), sep = "|")
    
    if (length(over) >= 1) {
      # Pivot to columns over
      if (ncol(select_if(res.df,is.numeric)) > 1) {
        # If more than one numeric column just pivot
        res.df <- res.df %>% 
          pivot_wider(names_from = all_of(c(over)),
                      # grab only numeric columns after by.var, over, and x
                      values_from = colnames(select_if(.,is.numeric)),
                      names_sep = "..")
      } else {
        # If only one numeric column
        
        # Original colname
        cor.col.name <- colnames(res.df)[length(res.df)]
        
        res.df <- res.df %>% 
          pivot_wider(names_from = all_of(c(over)),
                      # grab only numeric columns after by.var, over, and x
                      values_from = colnames(select_if(.,is.numeric)),
                      names_sep = "..")
        
        colnames(res.df) <- c("by.var", paste0(cor.col.name,"..",colnames(res.df)[-1]))
      }
      
    }
    
    
    return(res.df)
  })
  # ---------------- .
  
  
  return(res.l)
}

# FINAL CORRELATION COVARIANCE FUNCTION --------- 
pv.rrepest.corr.cov <- function(data, svy, x, by.var = NULL, over = NULL, test = F,
                             user_na=F, show_na=F, flag = F, fast = F, corr = T, pv = F,
                             ...) {
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
  # corr : (bool) T → get correlation, F → get covariance
  # pv : (Bool) TRUE → We are in plausible values and must parallelize
  # ...
  # isced : (number) isced level to analyze
  
  # Get optional arguments
  extra.args <- list(...)
  
  # All variables in dataframe must be LOWERCASE
  over <- tolower(over)
  by.var <- tolower(by.var)
  x <- tolower(x)
  
  # # If there is an OVER variable remove NAs from OVER vars
  # if (length(over) > 0) {
  #   for (i in over) {
  #     df <- df %>% drop_na(i)
  #   }
  # }
  
  # Get weight names
  weight.names <- replicated_w_names(svy, ...)
  # If fast then less replicated weights
  if (fast) {
    weight.names <- weight.names[1:6]
  }
  
  # Loop over each variable
  brr.res <- pv.loop.corr.cov.on.weights(data = data,
                                      svy = svy,
                                      x = x, 
                                      by.var = by.var, 
                                      over = over,
                                      test = test,
                                      flag = flag,
                                      rep_weights = weight.names,
                                      corr = corr,
                                      pv = pv)
  
  # Reorder data to have b and se intertwined
  res <- pv.get.se.reorder(brr.res, svy = svy, pv = pv, ... = ...)
  
  
  
  return(res)
  
}


# rrepest.corr.cov(df.t, "TALISTCH", c("T3STAKE","T3TEAM"),"cntry",over = "tt3g01",fast = T,test=T)


