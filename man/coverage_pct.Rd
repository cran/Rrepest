% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coverage_pct.R
\name{coverage_pct}
\alias{coverage_pct}
\title{Coverage percentage  (1 - mean(is.na)) * 100}
\usage{
coverage_pct(df, by, x, w = NULL, limit = NULL)
}
\arguments{
\item{df}{(data frame) Data to analyse}

\item{by}{(string vector) Variable(s) used for tabulating the variable of interest}

\item{x}{(string) Variable of interest for which to compute the number of valid (i.e. non-missing) observations}

\item{w}{(string) Vector of weights}

\item{limit}{(numeric) Threshold at which, if lower, value will be TRUE}
}
\value{
Data frame containing the number of valid (i.e. non-missing) observations for the variable of interest
}
\description{
Cpmputes teh coverage percentage for the column/variable of interest.
}
\examples{
data(df_pisa18)
data(df_talis18) 

coverage_pct(df = df_pisa18, by = "cnt",x = "wb173q03ha")
coverage_pct(df = df_talis18, by = "cntry",x = "tt3g01", w = "TCHWGT")
}
