################# PV B and SE calculation from LISTS #################
# 28 Mar 2023
# Rodolfo Ilizaliturri
#############################################################
# Goal: From a list of size n pvs, get Sampling Var, Imputation Var, SE and Estimate
# Note: Each dataframe must be a 1st column with by.var and all after a sequence of b., se. results


get.common.name <- function(str.vec, pv.inputs) {
  # Goal: Get common part of the string in a vector of strings
  # ------ INPUTS ------.
  # str.vec : (String vector) vector with the almost identical strs
  # pv.inputs : (string vector) All strings containing an @ as an input
  
  for (pv.in.i in pv.inputs){
    # Get regular expression instruction to replace string names
    #Separate by @ but keep the @ as separator
    reg.inst <- unlist(strsplit(pv.in.i, "(?<=@)|(?=@)", perl=TRUE)) %>%
      # Replace the @ with digit for reg exp
      str_replace("@","\\\\d") %>%
      #paste together with +
      paste0(collapse = "+")
    
    # Use the re exp instruction
    res <- gsub(reg.inst,
    # Replace with separated parts plus the @ symbol in place
                paste0(unlist(strsplit(pv.in.i, "(?<=@)|(?=@)", perl=TRUE)),collapse = ""),
                str.vec[1])
    
  }
  
  return(res)
}
#Ex. get.common.name(get.pv.names(names(df.qqq), "pv@math"))


b.se.pv <- function(res.l, pv.inputs, statistic){
  # Goal: From a list of size n pvs, get Sampling Var, Imputation Var, SE and Estimate
  # ------ INPUTS ------.
  # res.l : (list of dfs) Each dataframe must have a 1st column named "by.var" and all after a sequence of [b. se.] results
  # pv.inputs : (string vector) All strings containing an @ as an input
  
  # Names from columns
  c.names <- names(res.l[[1]])
  #Start on the second colname
  res.l.b.se <- lapply(2:length(c.names),function(n.i){
    
    # β -----------------------------------------------------------------------.
    # If b. then create a dabase and take the average
    if (startsWith(c.names[n.i],"b.")) {
      # Grab first element and n.i element of each df
      df.b <- lapply(res.l, function(df.i) df.i[c(1,n.i)]) %>% 
        reduce(full_join, by = "by.var") %>% 
      # When there are missing values depending on the pv # replace NA with 0 
        replace(is.na(.),0)
      # Get the common name an all to use as variable name
      name.b <- get.common.name(names(df.b)[-1],pv.inputs)
      
      # If general argument "gen" the common name will be the first name
      if ("gen" %in% statistic) name.b <- c.names[n.i]
      
      # Mean of all rows for estimate of b  
      df.b[name.b] <- rowMeans(df.b[-1])
      
      res <- df.b[c(1,length(df.b))]
      # If se. then create a dabase and take the average
      
      # SE ----------------------------------------------------------------------.
    } else if (startsWith(c.names[n.i],"se.")){
      
      # SAMPLING VARIANCE------------------------------------------------------.
      # Grab first element and n.i element of each df for SE
      df.se <- lapply(res.l, function(df.i) df.i[c(1,n.i)]) %>% 
        reduce(full_join, by = "by.var") #%>% 
        # When there are missing values depending on the pv # replace NA with 0 
        # replace(is.na(.),0)
      # Get the common name an all to use as variable name
      name.se <- get.common.name(names(df.se)[-1],pv.inputs)
      
      # If general argument "gen" the common name will be the first name
      if ("gen" %in% statistic) name.se <- c.names[n.i]
      
      sampling.variance <- rowMeans(df.se[-1], na.rm = T)
      
      # IMPUTATION VARIANCE----------------------------------------------------.
      # Grab first element and n.i - 1 element of each df for B
      df.b <- lapply(res.l, function(df.i) df.i[c(1,n.i-1)]) %>% 
        reduce(full_join, by = "by.var") #%>% 
        # When there are missing values depending on the pv # replace NA with 0 
        # replace(is.na(.),0)
      # Number of PVs
      n.pvs <- length(df.b)-1
      # Mean of estimates
      mean_b <- rowMeans(df.b[-1], na.rm = T)
      imputation.variance <- (1+1/n.pvs)*rowSums((df.b[-1] - mean_b)^2, na.rm = T)/(n.pvs-1)
      
      #Calculate SE and name the variable according to most common strings
      df.se[name.se] <- sqrt(imputation.variance + sampling.variance)
      
      res <- df.se[c(1,length(df.se))]
    }
    return(res)
  })
  return(reduce(res.l.b.se, full_join, by = "by.var"))
}



