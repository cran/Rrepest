#' Grouped frequency counts
#' 
#' @description Computes a data frame with frequency counts.
#' @param data (data frame) Data to analyse
#' @param small.level (string vector) All variables to get grouped sum that will sum up to 100
#' @param big.level (string vector) Variables... Must be fully contained in variables from small.level.
#' @param w (string) Name of the numeric variable representing the weights (optional)
#'
#' @return Data frame containing the frequency counts
#' @export
#'
#' @examples grouped_sum_freqs(data = mtcars,small.level = c("cyl","am"),big.level = c("cyl"))
#' 

grouped_sum_freqs <- function(data, small.level, big.level, w = NULL) {
  # Goal: Get a dataframe with frequencies from the grouped sum of small.level and big.level used for getting percentages
  # Example: cnt gender wealth 
  # ------ INPUTS ------.
  # data : (dataframe) df to analize
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
