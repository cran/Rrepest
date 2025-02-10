################# Invert Test Column #################
# 15 Sept 2023
# Rodolfo Ilizaliturri
#############################################################

#' inv_test
#' @description Invert test column from Rrepest test = TRUE by name on "b." and "se." in the column name and by sign (*-1) on "b."
#' @param data (dataframe) df to analyze
#' @param name_index (string/numeric) name or index for the estimate (b.) columns containing the data for the test in Rrepest)
#'
#' @return Dataframe cointaining inverted test column names for "b." and "se." according to Rrepest structure and column multiplied by (-1) for "b."
#' @export
#'
#' @examples
#' inv_test(rrepest_pisa_age_gender, name_index = 6) 
inv_test <- function(data, name_index){
  # Goal: Invert test column from Rrepest by name on b. and se. in the test part in the column name and by sign (*-1) on b.
  # ------ INPUTS ------.
  # data : (dataframe) df to analize
  # name_index : (string/numeric) name or index for the estimate (b.) columns containing the data for the test in Rrepest
  
  rev_col_name <- function(input_string){
    # Split the string on the last ".."
    split_parts <- strsplit(input_string, "\\.{2}(?!.*\\.{2})", perl = TRUE) %>% unlist()
    # Grab the second part where the test string is
    test_parts <- substr(split_parts[2],2,nchar(split_parts[2])-1) %>% strsplit(split = "-") %>% unlist
    # Inversed result
    output_string <- paste0(split_parts[1],"..(",test_parts[2],"-",test_parts[1],")")
    return(output_string)
  }
  if (is.numeric(name_index)){ #Index -> Numeric
    # Get name 
    input_string <- data %>% select(name_index, name_index+1) %>% names()
    # Vectorize rev_col_name
    output_string <- sapply(input_string, rev_col_name, USE.NAMES = FALSE)
    # Rename original column
    data <- data %>% rename(!!!setNames(input_string,output_string))
    # Invert sign of column
    data <- data %>% mutate_at(name_index, ~ . * -1) 
  } else if (is.character(name_index)) { #Name -> Characheter
    # Get name 
    input_string <- data %>% select(which(names(data)==name_index), which(names(data)==name_index)+1) %>% names()
    # Vectorize rev_col_name
    output_string <- sapply(input_string, rev_col_name, USE.NAMES = FALSE)
    # Rename original column
    data <- data %>% rename(!!!setNames(input_string,output_string))
    # Invert sign of column
    data <- data %>% mutate_at(which(names(data)==name_index), ~ . * -1) 
  }
  return(data)
}
