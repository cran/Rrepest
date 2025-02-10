#' Rrepest table of results for PISA 2018 showing age and gender
#'
#' A table of results from Rrepest of the mean age of students broken down by gender for PISA 2018.
#'
#' @format ## `rrepest_pisa_age_gender`
#' A data frame in tibble format with 3 rows and 7 columns:
#' \describe{
#'   \item{cnt}{Country ISO 3166-1 alpha-3 codes.}
#'   \item{b.mean.age..Female}{Mean age for female students.}
#'   \item{se.mean.age..Female}{Standar error of the mean age for female students.}
#'   \item{b.mean.age..Male}{Mean age for male students.}
#'   \item{se.mean.age..Male}{Standar error of the mean age for male students.}
#'   \item{b.mean.age..(Female-Male)}{Difference of the mean age from female to male students.}
#'   \item{se.mean.age..(Female-Male)}{Standard error of the difference of the mean age from female to male students.}
#' }
#' @source <https://www.oecd.org/en/data/datasets/pisa-2018-database.html#data>
"rrepest_pisa_age_gender"

#' Rrepest table of results for PISA 2018 showing the age and completed schooling level of students' mothers
#'
#' A table of results from Rrepest of the mean age of students broken down by 
#' completed schooling level of students' mothers for PISA 2018.
#'
#' @format ## `rrepest_pisa_age_isced`
#' A data frame in tibble format with 3 rows and 7 columns:
#' \describe{
#'   \item{cnt}{Country ISO 3166-1 alpha-3 codes.}
#'   \item{b.mean.age..ISCED level 1}{Mean age of students whose mothers completed ISCED 1.}
#'   \item{se.mean.age..ISCED level 1}{Standard error of the mean age of students whose mothers completed ISCED 1.}
#'   \item{b.mean.age..ISCED level 2}{Mean age of students whose mothers completed ISCED 2.}
#'   \item{se.mean.age..ISCED level 2}{Standard error of the mean age of students whose mothers completed ISCED 2.}
#'   \item{b.mean.age..ISCED level 3A}{Mean age of students whose mothers completed ISCED 3A.}
#'   \item{se.mean.age..ISCED level 3A}{Standard error of the mean age of students whose mothers completed ISCED 3A.}
#'   \item{b.mean.age..ISCED level 3B, 3C}{Mean age of students whose mothers completed ISCED 3B/3C.}
#'   \item{se.mean.age..ISCED level 3B, 3C}{Standard error of the mean age of students whose mothers completed ISCED 3B/3C.}
#'   \item{b.mean.age..She did not complete  ISCED level 1}{Mean age of students whose mothers did not complete ISCED 1.}
#'   \item{se.mean.age..She did not complete  ISCED level 1}{Standard error of the mean age of students whose mothers did not complete ISCED 1.}
#'   \item{b.mean.age..(ISCED level 1-She did not complete  ISCED level 1)}{Mean age difference between ISCED 1 completed vs. non-ISCED 1 completed mothers.}
#'   \item{se.mean.age..(ISCED level 1-She did not complete  ISCED level 1)}{Standard error of the mean age difference between ISCED 1 completed vs. non-ISCED 1 completed mothers.}
#'   ...
#' }
#' @source <https://www.oecd.org/en/data/datasets/pisa-2018-database.html#data>
"rrepest_pisa_age_isced"

#' Teaching and Learning International Survey (TALIS) 2018 noisy data subset
#'
#' A subset of noisy data from the Teaching and Learning International Survey (TALIS) 2018 cycle
#' for Mexico, Italy, and France.
#' @format ## `df_talis18`
#' A data frame in tibble format with 496 rows and 548 columns from which only some relevant variables are listed below. 
#' For a detailed description of all variables go to https://www.oecd.org/en/data/datasets/talis-2018-database.html#data
#' \describe{
#'   \item{idlang}{Country language}
#'   \item{cntry}{Country ISO 3166-1 alpha-3 codes.}
#'   \item{tt3g01}{Gender}
#'   \item{tt3g02}{Age}
#'   \item{tt3g03}{Highest level of formal education completed}
#'   ...
#' }
#' @source <https://www.oecd.org/en/data/datasets/talis-2018-database.html#data>
"df_talis18"

#' Program for International Student Assessment (PISA) 2018 noisy data subset
#'
#' A subset of noisy data from the Program for International Student Assessment (PISA) 2018 cycle
#' for Mexico, Italy, and France.
#' @format ## `df_pisa18`
#' A data frame in tibble format with 1269 rows and 1106 columns from which only some relevant variables are listed below. 
#' For a detailed description of all variables go to https://www.oecd.org/en/data/datasets/pisa-2018-database.html#data
#' \describe{
#'   \item{idlang}{Country language}
#'   \item{CNT}{Country ISO 3166-1 alpha-3 codes.}
#'   \item{st001d01t}{Student International Grade}
#'   \item{st004d01t}{Gender}
#'   ...
#' }
#' @source <https://www.oecd.org/en/data/datasets/pisa-2018-database.html#data>
"df_pisa18"