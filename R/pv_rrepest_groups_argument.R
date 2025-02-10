#' Group list
#' 
#' @description Obtains a list with the arguments for the grp() function used within Rrepest() function's average() and group() options.
#' @param group.name (string) Name of the group to be displayed
#' @param column (string) Column/variable of interest to be grouped
#' @param cases  (string vector) Rows/values to be included in the group
#' @param full_weight  (bool) TRUE: average of the group will be weighted average
#' @export
#' @return List containing the arguments for the grp() function used within Rrepest() function's average() and group() options.
#'
#' @examples
#' append(grp("OECD Average","CNTRY",c("HUN","MEX")), grp("Europe","CNTRY",c("ITA","FRA")))
#' 
grp <- function(group.name, column, cases, full_weight = FALSE) {
  res <- list()
  res[[group.name]] <- list(column = column, cases = cases, full_weight = full_weight)
  return(res)
}
  
