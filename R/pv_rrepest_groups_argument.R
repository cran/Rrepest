
#' grp
#' 
#' @description Obtain a list as argument for groups to be evaluated in data
#' @param group.name (string) Name of the group to be displayed
#' @param column (string) Column where the data is located
#' @param cases  (string vector) List of values to be put into the group
#' @export
#' @return list of groups to redifine {group_name = {column, values_in_group},...}
#'
#' @examples
#' append(grp("OECD Average","CNTRY",c("HUN","MEX")), grp("Europe","CNTRY",c("ITA","FRA")))
grp <- function(group.name, column, cases) {
  res <- list()
  res[[group.name]] <- list(column = column, cases = cases)
  return(res)
}
  
