################# Flags #################
# 25 Feb 2023
# Rodolfo Ilizaliturri
#############################################################
# Goal: Replace numeric variables with nans where there is not enough schools and or teachers

# General for all
flags <- function(data, svy) {
  # Goal: Replace numeric variables with nans where there is not enough schools and or teachers
  # ------ INPUTS ------.
  # data : (dataframe) must have last column have "n.sch" or two last columns "n.tch" and "n.sch"
  # svy : (str) survey to analyse
  
  if (svy == "TALISSCH") {
    #If principals questionnaire the just check for schools to add NaN
    res.df <- data %>% 
      mutate_if(is.numeric, list(~ifelse(
        # If is not NA and less than 10 schools
        !is.na(grep("n.obs", names(data), value=TRUE) %>% get()) &
          grep("n.obs", names(data), value=TRUE) %>% get() < 10, NaN, .)))
    
    
    #remove last columns
    res.df <- res.df[-c(length(data))]
    res.df[ncol(res.df)] <- labelled_spss(res.df[[names(res.df)[ncol(res.df)]]], c(flagged = NaN)) %>% as.data.frame()
  }else if (svy == "TALISTCH"){
    #If teachers questionnaire check for schools and teachers to add NaN
    res.df <- data %>% 
      mutate_if(is.numeric, list(~ifelse(
        # If is not NA and less than 30 teachers and 10 schools
        !is.na(grep("n.sch", names(data), value=TRUE) %>% get()) & (
          grep("n.sch", names(data), value=TRUE) %>% get() < 10 |
            grep("n.obs", names(data), value=TRUE) %>% get() < 30    ), NaN, .)))
    
    
    #remove last 2 columns
    res.df <- res.df[-c(length(data)-1,length(data))]
    #res.df[ncol(res.df)] <- labelled_spss(res.df[[names(res.df)[ncol(res.df)]]], c(flagged = NaN))
  }else if (svy %in% c("PISA2015","PISA","SSES","SSES2023")){
    #If teachers questionnaire check for schools and teachers to add NaN
    res.df <- data %>% 
      mutate_if(is.numeric, list(~ifelse(
        # If is not NA and less than 30 teachers and 5 schools
        !is.na(grep("n.sch", names(data), value=TRUE) %>% get()) & (
          grep("n.sch", names(data), value=TRUE) %>% get() < 5 |
            grep("n.obs", names(data), value=TRUE) %>% get() < 30    ), NaN, .)))
    
    
    #remove last 2 columns
    res.df <- res.df[-c(length(data)-1,length(data))]
    #res.df[ncol(res.df)] <- labelled_spss(res.df[[names(res.df)[ncol(res.df)]]], c(flagged = NaN))
  }else {
    #If all other surveys the just check for schools to add NaN
    res.df <- data %>% 
      mutate_if(is.numeric, list(~ifelse(
        # If is not NA and less than 30 observations
        !is.na(grep("n.obs", names(data), value=TRUE) %>% get()) &
          grep("n.obs", names(data), value=TRUE) %>% get() < 30, NaN, .)))
    
    
    #remove last columns
    res.df <- res.df[-c(length(data))]
    res.df[ncol(res.df)] <- labelled_spss(res.df[[names(res.df)[ncol(res.df)]]], c(flagged = NaN)) %>% as.data.frame()
  }
  return(res.df)
}

# For rrepest.TALIS
flags.nan <- function(data, svy) {
  # Goal: Replace numeric variables with nans where there is not enough schools and or teachers
  # ------ INPUTS ------.
  # data : (dataframe) must have last column have "n.sch" or two last columns "n.tch" and "n.sch"
  # svy : (str) survey to analyse
  
  if (svy == "TALISSCH") {
    #If principals questionnaire the just check for schools to add NaN
    res.df <- data %>% 
      mutate_if(is.numeric, list(~ifelse(
        # If is not NA and less than 10 schools
        !is.na(grep("n.sch", names(data), value=TRUE) %>% get()) &
          grep("n.sch", names(data), value=TRUE) %>% get() < 10, NaN, .)))

    
    #remove last columns
    res.df <- res.df[-c(length(data))]
    res.df[ncol(res.df)] <- labelled_spss(res.df[[names(res.df)[ncol(res.df)]]], c(flagged = NaN)) %>% as.data.frame()
  }else{
    #If teachers questionnaire check for schools and teachers to add NaN
    res.df <- data %>% 
      mutate_if(is.numeric, list(~ifelse(
        # If is not NA and less than 30 teachers and 10 schools
        !is.na(grep("n.sch", names(data), value=TRUE) %>% get()) & (
          grep("n.sch", names(data), value=TRUE) %>% get() < 10 |
            grep("n.tch", names(data), value=TRUE) %>% get() < 30    ), NaN, .)))
      
    
    #remove last 2 columns
    res.df <- res.df[-c(length(data)-1,length(data))]
    #res.df[ncol(res.df)] <- labelled_spss(res.df[[names(res.df)[ncol(res.df)]]], c(flagged = NaN))
  }
  return(res.df)
}
