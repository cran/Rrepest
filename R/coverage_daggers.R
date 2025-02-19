#' Dagger or double dagger according to coverage level
#' 
#' @description
#' Transform coverage columns in Rrepest to dagger and double dagger according
#' to the coverage (cvge) column. Default levels are 75 (dagger) and 50 (double dagger). 
#' If coverage is above both levels, no symbol is produced (empty). Used with the coverage o
#' option in Rrepest set to TRUE to produce columns with coverage percentages.
#' 
#' @param res_df (data frame) Rrepest output with columns for coverage (cvge)
#' @param one_dagger (numeric) Level at which the coverage is transformed into a dagger. 75 by default.
#' @param two_dagger (numeric) Level at which the coverage is transformed into a double dagger. 50 by default
#'
#' @return Dataframe with daggers or double daggers in coverage column
#' @export
#'
#' @examples
#' coverage_daggers(talis18_tt3g23o_freq, one_dagger = 95, two_dagger = 90)

coverage_daggers <- function(res_df, one_dagger = 75, two_dagger = 50){
  # GOAL: Assign dagger and double dagger to coverage column for determined thresholds
  # --- ARGUMENTS ---
  # res_df (data frame) dataframe qith cvge. column for coverage (as in Rrepest)
  # one_dagger (numeric) threshold for one dagger
  # two_dagger (numeric) threshold for two daggers
  
  # Get coverage variables from result
  cge_vars <- names(res_df)[startsWith(names(res_df),"cvge.")]
  
  for (cge_i in cge_vars){
    # Initiate NA column
    cge_dagger <- rep(NA, nrow(res_df))
    # Assign two daggers
    cge_dagger[res_df[[cge_i]] < two_dagger] <- "\U2021"
    # Assign one dagger
    cge_dagger[res_df[[cge_i]] >= two_dagger & res_df[[cge_i]] < one_dagger] <- "\U2020"
    # Replace coverage columns with dagger
    res_df[[cge_i]] <- cge_dagger
  }
  
  return(res_df)
}