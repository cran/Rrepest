% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R, R/oecd_data.R
\docType{data}
\name{df_pisa18}
\alias{df_pisa18}
\title{Program for International Student Assessment (PISA) 2018 noisy data subset}
\format{
\subsection{\code{df_pisa18}}{

A data frame in tibble format with 1269 rows and 1106 columns from which only some relevant variables are listed below.
For a detailed description of all variables go to https://www.oecd.org/en/data/datasets/pisa-2018-database.html#data
\describe{
\item{idlang}{Country language}
\item{CNT}{Country ISO 3166-1 alpha-3 codes.}
\item{st001d01t}{Student International Grade}
\item{st004d01t}{Gender}
...
}
}

A data frame with 1269 rows/observations and 1120 columns/variables.
}
\source{
\url{https://www.oecd.org/en/data/datasets/pisa-2018-database.html#data}
}
\usage{
df_pisa18

data(df_pisa18)
}
\description{
A subset of noisy data from the Program for International Student Assessment (PISA) 2018 cycle
for Mexico, Italy, and France.

This dataset is a subset of the PISA 2018 database produced by the OECD for the countries of France, Italy, and Mexico.
}
\keyword{datasets}
