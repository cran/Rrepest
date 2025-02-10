################# General statistics on Continuous variables #################
# 12 Feb 2023
# Rodolfo Ilizaliturri
#############################################################
# Goal: Obtain several statistics using Fay's BRR model

# ------------------.
# --------- WORK FUNCTIONS
# ------------------.

# Variance factors for SE
rrvar.pv <- function(df.rr, svy, pv = F, ...) {
  # Goal: Dispersion (variance) from total mean to get SE of statistic
  # ------ INPUTS ------.
  # df.rr : (dataframe) df containing total (1st columns) and replicate weighted statistic ONLY
  # svy : (string) List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # pv : (Bool) TRUE → We are in plausible values and keep calculation as variance, not SE
  
  #Get args ...
  arguments <- list(...)
  
  # f : (number) variance factor (depends on replication method: BRR, jackknife-1, jk-2,…) (Fay = 1.5)
  if (svy %in% c("TALISTCH","TALISSCH","PISA","PISA2015","PBTS")) f = 1/(0.5^2)
  else if (svy == "SSES") f = 76/2
  else if (svy == "SSES2023") f = 1/(0.5^2) #Changed from Jk1 method to BBR method 
  else if (svy %in% c("ALL","IALS")) f = 30
  else if (svy == "IELS") f = 92/23
  ### for the case of PIAAC, f depends on countries. The correct variance factor will be applied in rrepest_base
  ### before b.se.pv is called.
  else if (svy == "PIAAC") f = 80
  else if (svy == "PISAOOS") f = 29
  else if (svy %in% c("TIMSS","PIRLS")) f = 1/2*150
  else if (svy %in% c("ICCS","ICILS")) f = 1*75
  else if (svy %in% c("ICCS_T","ICILS_T")) f = 1*75
  else if (svy %in% c("ICCS_C","ICILS_C")) f = 1*75
  else if (svy %in% c("LEADER_TALISEC","STAFF_TALISEC","TALISEC_LEADER","TALISEC_STAFF")) f = 92/23

  
  #if there is custom variance factor grab it from the arguments
  if (!is.null(arguments$var.factor)) {
    f = arguments$var.factor
  }
  
  # Calculate disperion from the mean squared time variance factor according to survey
  m <- df.rr[1]
  vec <- df.rr[-1]
  rr.var <- mean((m - vec)^2)*f
  
  if (!pv) {
    # Only one estimation (n.PV = 1), then can take sqrt of Var to get SE
    rr.se <- sqrt(rr.var)
    return(rr.se)
  } else {
    # Return variance for calculation of Sampling and Imputation variance
    return(rr.var)
  }
}

#Number of observations valid for x
n.obs.x <- function(df, by.var, x, svy) {
  # Goal: Number of schools and teachers calculation is performed on
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze
  # by.var : (string) column in which we'll break down results
  # x : (string) variable from where to get means
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  
  #All lower caps column names
  colnames(df) <- colnames(df) %>% tolower()
  by.var <- by.var %>% tolower()
  x <- x %>% tolower()
  
  tot.n <- df %>%
    group_by(across(all_of(by.var))) %>%
    {if (svy == "TALISSCH") summarise(., n.obs = sum(!is.na(get(x)))) 
      else if (svy == "TALISTCH") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$idschool)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy == "TALISEC_STAFF") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$idcentre)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy == "PISA2015") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$cntschid)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy == "PISA") {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$schoolid)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else if (svy %in% c("SSES","SSES2023")) {mutate(.,school.n = ifelse(is.na(get(x)) == T, NA, .data$schid)) %>% 
          summarise(., n.obs = sum(!is.na(get(x))),
                    n.sch = n_distinct(school.n, na.rm = T))}
      else summarise(., n.obs = sum(!is.na(get(x))))
    } %>%
    unite("by.group", all_of(by.var), sep = "|")
  
  #add x colanames
  colnames(tot.n)[-1] <- paste(colnames(tot.n)[-1], tolower(x), sep = ".")
  
  return(tot.n)
}
# n.obs.x(df = df.qqq,by.var = "cnt",x = "wb173q03ha",svy = "PISA2015")
# n.obs.x(df = df.p,by.var = "cntry",x = "tc3g06b",svy = "TALISSCH")
# n.obs.x(df = df.t,by.var = "cntry",x = "tt3g01",svy = "TALISTCH")


#' Formatting target, by, and over variables for Rrepest.
#'
#' @param df (data frame) Data to analyze.
#' @param svy (string) Possible projects to analyse: PIAAC, PISA, TALISSCH, TALISTCH, etc.
#' @param x (string vector) Target variables.
#' @param by.over (string vector) Variables to break analysis by.
#' @param user_na (bool) TRUE → show nature of user defined missing values
#' @param ... Optional arguments such as custom weights (cm.weights) 
#'
#' @return Data frame with variables in numeric format for analysis.
#' @export
#'
#' @examples
#' df1 <- format_data_repest(df_pisa18, "PISA", "pv1math", "cnt")
#' df2 <- format_data_repest(df_pisa18, "PISA", "pv1math", c("cnt","st004d01t"))
#' df3 <- format_data_repest(df_pisa18, "PISA", "pv1math", c("cnt","st004d01t","iscedl"))
#' df4 <- format_data_repest(df_talis18, "TALISTCH", "tt3g02", "cntry", isced = 2)
format_data_repest <- function(df, svy, x, by.over, user_na = F, ...) {
  # Goal: Get database into appropriate format
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # by.over : (string vector) variables to break analysis by
  # user_na : (Bool) TRUE → show nature of user defined missing values
  
  #Get isced from ...
  arguments <- list(...)
  
  # Lowercase variables
  names(df) <- names(df) %>% tolower()
  
  # If isced specified for TALIS filter
  if (svy %in% c("TALISSCH","TALISTCH") & !is.null(arguments$isced)) {
    df <- df %>% 
      filter(idpop %in% arguments$isced)
  }
  
  # Weight names depending on project (TALIS, PISA, etc.)
  if (svy == "TALISTCH") weight.variables <- c("tchwgt",paste0("trwgt",1:100))
  else if (svy == "TALISSCH") weight.variables <- c("schwgt",paste0("srwgt",1:100))
  else if (svy %in% c("PISA","PISA2015")) weight.variables <- c("w_fstuwt",paste0("w_fsturwt",1:80))
  else if (svy == "SSES") weight.variables <- c("wt2019",paste0("rwgt",1:76))
  else if (svy == "SSES2023") weight.variables <- c("wt2023",paste0("rwgt",1:80))
  else if (svy == "PISAOOS") weight.variables <- c("spfwt0",paste0("spfwt",1:30))
  else if (svy == "TALISEC_STAFF") weight.variables <- c("staffwgt",paste0("srwgt",1:92))
  else if (svy == "TALISEC_LEADER") weight.variables <- c("cntrwgt",paste0("crwgt",1:92))
  else if (svy == "PIAAC") weight.variables <- c("spfwt0",paste0("spfwt",1:80))
  else if (svy %in% c("ALL","IALS")) weight.variables <- c("popwt",paste0("replic",1:30))
  else if (svy == "IELS") weight.variables <- c("childwgt",paste0("srwgt",1:92))
  else if (svy =="PBTS") weight.variables <- c("w_stu",paste0("rwgt",1:80))
  else if (svy %in% c("TIMSS","PIRLS")) weight.variables <- c("wgr",paste0("jr",1:150))
  else if (svy %in% c("ICCS","ICILS")) weight.variables <- c("totwgts",paste0("srwgt",1:75))
  else if (svy %in% c("ICCS_T","ICILS_T")) weight.variables <- c("totwgtt",paste0("trwgt",1:75))
  else if (svy %in% c("ICCS_C","ICILS_C")) weight.variables <- c("totwgtc",paste0("crwgt",1:75))
  else if (svy == "SVY") weight.variables <- arguments$cm.weights #custom optional weights that must exist if "SVY"
  
  #if there is custom name of weights grab it from the arguments
  if (!is.null(arguments$cm.weights)) {
    weight.variables <- arguments$cm.weights #custom optional weights
  }
  
  #if PIAAC, compute variance factor at the respondent level
  if (svy=="PIAAC") {
    df <- df %>% mutate(var_fac=
                          case_when(vemethodn==1 ~ (venreps-1)/venreps,
                                    vemethodn==2 ~ 1,
                                    vemethodn==3 ~ 1/venreps,
                                    vemethodn==4 ~ 1/(venreps*(1-vefayfac)^2)
                          ))
  }
  
  
  #If x has an @ for PVs
  if (grep("@", x) %>% length() != 0) {
    x <- get.pv.names(df, x)
  }
  
  num.vars <- c(weight.variables, x)
  
  #Convert x and weights to NUMERIC variables removing user defined na and just leave NA
  df.res <- df %>% 
    mutate(across(contains(num.vars) & ( where(is.labelled)), remove_user_na, user_na_to_na = T)) %>%
    mutate(across(contains(num.vars) & ( where(is.labelled)), as.numeric))
  
  #If we want to know NA identity or not (eg. Not reached, Absent)
  if (!user_na) {
    df.res <- df.res %>% 
      mutate_at(by.over,to_character, user_na_to_na = TRUE)
  }else{
    df.res <- df.res %>% 
      mutate_at(by.over,to_character)
  }
  
  #Show variables that create over categories in column names of results
  if (!is.null(arguments$show_over_vars)) {
    for (i in tolower(arguments$show_over_vars)) {
      df.res[[i]] <- paste(i, df.res[[i]], sep = "..")
    }
  }
  
  
  return(df.res)
}

do_group <- function(data, grp.l, user_na = F, ...){
  # Goal: Create dataframe with the groups on the variables and values defined
  # ------ INPUTS ------.
  # data : (dataframe) Data to analyze
  # grp.l : (list) list of groups to redifine {group_name = {column, values_in_group},...}
  res.l <- lapply(names(grp.l), function(g) {
    # Assign name of group to all the column value
    data %>% 
      filter(get(grp.l[[g]][["column"]]) %in% grp.l[[g]][["cases"]]) %>% 
      mutate(!!grp.l[[g]][['column']] := g) %>% 
      return()
  })
  
  
  for (g.i in sapply(names(grp.l),function(g) grp.l[[g]][["column"]]) %>% unique()) {
    #If we want to know NA identity or not (eg. Not reached, Absent) to get correct format in column
    if (!user_na) {
      data <- data %>% 
        mutate_at(g.i,to_character, user_na_to_na = TRUE)
    }else{
      data <- data %>% 
        mutate_at(g.i,to_character)
    }
  }
  

  return(append(res.l,data))
  
}

#Check existance of argument and use it if so
is.there <- function(argument) {
  if(!is.null(argument)) return(argument)
  else return(NULL)
}


#Plausible Values names from unknown digits of PVs
get.pv.names <- function(df, pv.var) {
  # Goal: Do means, variance or standard deviation
  # ------ INPUTS ------.
  # df : (dataframe) df to analize
  # pv.var : variable that contains an @ symbol to interpret as PVs
  
  pv.split <- strsplit(pv.var,"@")[[1]]
  pv.gex <- paste0("^",pv.split[1],".*",pv.split[2],"$")
  pv.names <- grep(pv.gex,colnames(df) %>% tolower(), value = T)
  
  return(pv.names)
}
#EX. x <- get.pv.names(df,"pv@math")

#Weight names
replicated_w_names <- function(svy, ...) {
  # Goal: Names of weighting variables corresponding to survey
  # ------ INPUTS ------.
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  
  #Get custom arguments...
  arguments <- list(...)
  
  # Weight names depending on project (TALIS, PISA, etc.)
  if (svy == "TALISTCH") weight.variables <- c("tchwgt",paste0("trwgt",1:100))
  else if (svy == "TALISSCH") weight.variables <- c("schwgt",paste0("srwgt",1:100))
  else if (svy %in% c("PISA","PISA2015")) weight.variables <- c("w_fstuwt",paste0("w_fsturwt",1:80))
  else if (svy == "PISAOOS") weight.variables <- c("spfwt0",paste0("spfwt",1:30))
  else if (svy == "SSES") weight.variables <- c("wt2019",paste0("rwgt",1:76))
  else if (svy == "SSES2023") weight.variables <- c("wt2023",paste0("rwgt",1:80))
  else if (svy == "TALISEC_STAFF") weight.variables <- c("staffwgt",paste0("srwgt",1:92))
  else if (svy == "TALISEC_LEADER") weight.variables <- c("cntrwgt",paste0("crwgt",1:92))
  else if (svy == "PIAAC") weight.variables <- c("spfwt0",paste0("spfwt",1:80))
  else if (svy %in% c("ALL","IALS")) weight.variables <- c("popwt",paste0("replic",1:30))
  else if (svy == "IELS") weight.variables <- c("childwgt",paste0("srwgt",1:92))
  else if (svy =="PBTS") weight.variables <- c("w_stu",paste0("rwgt",1:80))
  else if (svy %in% c("TIMSS","PIRLS")) weight.variables <- c("wgr",paste0("jr",1:150))
  else if (svy %in% c("ICCS","ICILS")) weight.variables <- c("totwgts",paste0("srwgt",1:75))
  else if (svy %in% c("ICCS_T","ICILS_T")) weight.variables <- c("totwgtt",paste0("trwgt",1:75))
  else if (svy %in% c("ICCS_C","ICILS_C")) weight.variables <- c("totwgtc",paste0("crwgt",1:75))
  else if (svy == "SVY") weight.variables <- arguments$cm.weights #custom optional weights that must exist if "SVY"
  
  #if there is custom name of weights grab it from the arguments
  if (!is.null(arguments$cm.weights)) {
    weight.variables <- arguments$cm.weights #custom optional weights
  }
  
  return(c(weight.variables[1], weight.variables[-1]))
}
#EX. wts <- replicated_w_names("PISA")
#EX. wts <- replicated_w_names("TALISTCH")

add_PIAAC_variance_factors <- function(pv.l, pv, df,by.var){
  # GOAL: multiply each se in pv.l by the variance factor specific to the by.var
  # ------ INPUTS ------
  # pv.l (list of df) for each pv, a df with bs and ses
  # pv (boolean) is there any pv
  # df (df) respondent level data
  # by.var (string) value of the by.var
  
  
  #in case the value of the variance factor is not constant within the estimation sample:
  # take the average, and then print a message
  # can happens when the sample contains several countries 
  var_facs <- df %>%  group_by(all_of(across(by.var))) %>%  
    distinct(across("var_fac")) %>% 
    rename(`by.var`=by.var)
  
  check <- var_facs %>%  count %>% 
    ungroup %>% summarize(max(n)) %>%  pull
  
  if (check>1) {
    var_facs <- var_facs %>%  group_by(all_of(across(by.var))) %>% 
      summarise(var_fac=mean(.data$var_fac, na.rm=TRUE))
    message("Variance factors are not constant within by.var")
  } 
  
  # if there is no pv the 'ses' in pv.l are multiplied by the sqrt of the variance factor
  if (!pv) {
    pv.l[[1]] <-  pv.l[[1]] %>% full_join(var_facs, by= "by.var") %>% 
      mutate(across(starts_with("se"),
                    ~ .x *sqrt(.data$var_fac))) %>% 
      select(-"var_fac")
  } else {
    # if there is a pv the 'ses' in pv.l are multiplied by the variance factor 
    # (in such a case, the 'ses' are actually the square of the true ses)!!!
    pv.l <- lapply(pv.l, FUN = function(pv.i) {
      pv.i %>% full_join(var_facs, by= "by.var") %>% 
        mutate(across(starts_with("se"),
                      ~ .x *.data$var_fac)) %>% 
        select(-"var_fac")
    })
    
  }
  
  
  return(pv.l)
}





get.pv.arguments <- function(digit.pvs, argument) {
  # Goal: Replace @ with all preset PV digits from arguments
  # ------ INPUTS ------.
  # digit.pvs (Numeric vector) : 1:x List of possible digits (comes from digits.pv)
  # argument (String vector) : names of variables to analyse, at least one with a @
  
  res <- lapply(digit.pvs, function(x.i){
    gsub("@", as.character(x.i), argument)
  }) %>%
    # Merge all with only unique elements
    reduce(union)
  
  return(res)
}

#Digits in PVs
digits.pvs <- function(svy, ...) {
  # Goal: Number of plausible values corresponding to survey
  # ------ INPUTS ------.
  # svy : List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  
  # Extract number of pvs if svy = "SVY"
  arguments <- list(...)
  
  # Number of plausible values (TALIS, PISA, etc.)
  if (svy %in% c("PISA2015","PISAOOS","PIAAC","ALL","IALS")) n.d.pvs <- 1:10
  else if (svy %in% c("PISA","IELS","PBTS","ICCS",
                      "ICILS","ICCS_T","ICILS_T",
                      "ICCS_C","ICILS_C","TIMSS","PIRLS")) n.d.pvs <- 1:5
  
  #if there is custom number of pvs grab it from the arguments
  if (!is.null(arguments$n.pvs)) {
    n.d.pvs <- 1:arguments$n.pvs
  }
  
  
  return(n.d.pvs)
}

#Statistics
weighted.var <- function(x, w, na.rm = T) {
  # Goal: Obtained weighted variance of a vector
  # ------ INPUTS ------.
  # x : (vector) variable to analyze
  # w : (vector) vector of weights
  # na.rm : (bool) if TRUE (default) remove missing values.
  
  w.var <- sum(w*(x-weighted.mean(x,w,na.rm = na.rm))^2, na.rm = na.rm)/sum(w,na.rm = na.rm)
  return(w.var)
}
weighted.std <- function(x, w, na.rm = T) {
  # Goal: Obtained weighted standard deviation of a vector
  # ------ INPUTS ------.
  # x : (vector) variable to analyze
  # w : (vector) vector of weights
  # na.rm : (bool) if TRUE (default) remove missing values.
  
  w.std <- (sum(w*(x-weighted.mean(x,w,na.rm = na.rm))^2, na.rm = na.rm)/sum(w,na.rm = na.rm))^(1/2)
  return(w.std)
}

#Core loop
doing_statistic_string <- function(statistic, over.by){
  # Goal: Have string to be evaluated on loop and grouped by
  # ------ INPUTS ------.
  # over.by : (string vector) variables to break analysis by
  # statistic : (string) accepts "mean","var","std", "quant", "iqr"
  
  #After, need to fo eval(parse(text= "..."))
  
  #Create string for over.by in data.table
  over.by.str <- paste0("c('",paste0(over.by,collapse = "','"),"')")
  
  #Do string for data.table and statistic
  if (statistic %in% c("quant","iqr")) {
    fun.str <- paste0("dt[, .(w.",
                      statistic," = weighted.",
                      statistic,"(get(x.i),get(w.i),q.iqr)), by = ",
                      over.by.str,"]")

  }else{
    fun.str <- paste0("dt[, .(w.",
                      statistic," = weighted.",
                      statistic,"(get(x.i),get(w.i),na.rm = T)), by = ",
                      over.by.str,"]")
  }
  return(fun.str)
}

#Sampling error
# TALIS
brr.se <- function(v, Fay = 0.5) {
  # Goal: Dispersion (like variance) from total mean to get SE of statistic
  # ------ INPUTS ------.
  # v : (dataframe) df containing total and replicate weighted statistic
  # Fay : (number) Number corresponding to adjustment by method (Fay = 1.5)
  m <- v[1]
  vec <- v[-1]
  rr.var <- mean((m - vec)^2)*1/Fay^2
  rr.se <- sqrt(rr.var)
  
  return(rr.se)
  
}
# PISA

vectorize.stat.on.weights <- function (df, statistic = "mean", rep_weights, x, by.var, over = NULL, test = F, ...) {
  # Goal: Get list of dataframes with total and replicated weight estimates for x (either x or pvs)
  # ------ INPUTS ------.
  # df : (dataframe) df to analize
  # rep_weights : (string vector) names of replicated weight vars
  # over : (vector string) columns over which to do analysis
  # x: (string vector) variable or pvs from wich to extract statistics
  # by.var : (string vector) variables to break analysis by
  # statistic : (string) accepts "mean","var","std", "quant", "iqr"
  # ... Dots
  # q.iqr : (vector numeric) of length 1 if quantile, of length 2 if interquantile range
  
  arguments <- list(...)
  
  dt <- df %>%
    select(all_of(c(rep_weights, x, by.var, over))) %>% #Select only data to analyze
    filter_at(vars(all_of(x)), all_vars(!is.na(.))) %>%  #Filter on only non NA values move here due to performance
    as.data.table() # Convert to data.table
  
  
  # Create string to be evaluated
  # If quantile or iqr add q.iqr to string
  if (!is.null(arguments$q.iqr)) {
    stat.str <- doing_statistic_string(statistic, c(by.var, over))
    stat.str <- sub("q.iqr",
                    paste0("c(",paste0(arguments$q.iqr,collapse = ","),")"),
                    stat.str)
  } else {
    stat.str <- doing_statistic_string(statistic, c(by.var, over))
  }
  
  #----- Loop over x's
  res.x.par <- lapply(x, function(x.i) {
    #----- Loop over weights
    res.w.par <- lapply(rep_weights, function(w.i) {
      #----- Calculate statistic from string stat.str
      res.col <- eval(parse(text = stat.str))
    
      #------ If test for over add tests to res.col
      if (test) {
        res.col <- over.test(res.col, over) %>% 
          column_to_rownames(var= "by.group")
      }else{
        res.col <- res.col %>% 
          remove_rownames %>%
          unite("by.group", all_of(c(by.var,over)), sep = "|") %>% 
          column_to_rownames(var= "by.group")
      }
      
      # -- Track weight number
      names(res.col)[ncol(res.col)] <- paste0("w.",w.i)
      return(res.col)
    })
    res.w.par <- res.w.par %>% reduce(cbind) %>% as.data.frame()
  })
  
  return(res.x.par)
}
#EX. res.list <- vectorize.stat.on.weights(df,"mean",wts,"pv1math","cnt")
#EX. res.list <- vectorize.stat.on.weights(df,"mean",wts, x,"cnt")
#EX. res.list <- vectorize.stat.on.weights(df,"mean",wts,"pv1math","cnt","st004d01t")
#EX. res.list <- vectorize.stat.on.weights(df,"mean",wts,"pv1math","cnt","st004d01t",T)
#EX. res.list <- vectorize.stat.on.weights(df,"mean",wts,"pv1math","cnt",c("st004d01t","iscedl"),T)
#EX. res.list <- vectorize.stat.on.weights(df,"mean",wts,"pv1math","cnt",c("iscedl","st004d01t"),T)
#EX. res.list <- vectorize.stat.on.weights(df,"mean",wts, "tt3g02","cntry")
#EX. res.list <- vectorize.stat.on.weights(df,"quant",wts, "tt3g02","cntry", q.iqr = .5)
#EX. res.list <- vectorize.stat.on.weights(df,"iqr",wts, "tt3g02","cntry", q.iqr = c(.5,.9))


rrepest.stats <- function(df, svy, statistic = "mean", x, by.var, over, test, show.n = "n", user_na=F, ...){
  # Note: Same as rrepest.gstats without the number of schools and teachers
  # Goal: Dataframe with statistic and SE by Fay's BRR model
  # ------ INPUTS ------.
  # df : (dataframe) df to analyze
  # svy : (string) List of possible projects to analyse PIAAC, PISA, TALISSCH and TALISTCH
  # statistic : (string) accepts "mean","var","std"
  # by.var : (string) column in which we'll break down results !IOP!: several variables
  # x : (string) variable from where to get statistics
  # over : (vector string) columns over which to do analysis
  # test : (Bool) make differences across over categories
  # user_na : (Bool) TRUE → show nature of user defined missing values in by.var
  # show.n : (string options) "n" shows n values and keeps all data, "flag"" removes n values and places NaN in rows
  # ... Dot arguments
  # isced : (number) isced level to analyze
  
  #grab optional arguments
  extra <- list(...)
  
  # If there is an OVER variable the data must be flagged and remove NAs from OVER vars
  if (length(over) > 0) {
    show.n <- "flag"
    
    #remove NAs on over
    for (i in over) {
      data <- data %>% drop_na(i)
    }
  }
  # If there is an @ in x grab PVs
  if (grep("@", x) %>% length() != 0) {
    x <- get.pv.names(df.qqq, x)
  }
  # Get weight names
  wts <- replicated_w_names(svy)
  # Format data according to survey
  df <- format_data_repest(df, svy, x, c(by.var,over), user_na, isced = is.there(extra$isced))
  
  # Loop over weights
  res.list <- vectorize.stat.on.weights(df = df,
                                        statistic = statistic,
                                        rep_weights = wts,
                                        x = x,
                                        by.var = by.var,
                                        over = over,
                                        test = test)
  
  
  
}



separate_test_name <- function(test_name){
  # GOAL: Create two strings from the test column name for coverage
  # ------ INPUTS ------.
  # test_name : (string) string containing column name from test with structure nn.(x-y) 
  name_parts <- strsplit(test_name,"..",fixed = TRUE)[[1]]
  test_parts <- strsplit(name_parts[length(name_parts)],"-")[[1]]
  
  string_1 <- paste(c(name_parts[1:(length(name_parts)-1)],substr(test_parts[1],2,nchar(test_parts[1]))),collapse = "..")
  string_2 <- paste(c(name_parts[1:(length(name_parts)-1)],substr(test_parts[2],1,nchar(test_parts[2])-1)),collapse = "..")
  return(c(string_1,string_2))
}

nans_from_se2b <- function(df){
  # GOAL: Force NaNs that appear in se. columns to b. columns from coverage and flags
  # ------ INPUTS ------.
  # df : (dataframe) Resulting df from analysis with se. and b. columns
  for (col_i in names(df)){
    # Get se. columns
    if(startsWith(col_i,"se")){
      # Find the NaN in the column
      nan_s <- which(is.nan(df[col_i][[1]]))
      # If there are, replace the same values with NaN in b.
      if (length(nan_s)>0){
        common_name <- substr(col_i,4,nchar(col_i))
        df[paste0("b.",common_name)][[1]][nan_s] <- NaN
      }
    }
  }
  return(df)
}

