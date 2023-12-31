% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_obs_x.R
\name{n_obs_x}
\alias{n_obs_x}
\title{Number of observations valid for column x}
\usage{
n_obs_x(df, by, x, svy = NULL)
}
\arguments{
\item{df}{(dataframe) data to analyze}

\item{by}{(string vector) column by which we'll break down results}

\item{x}{(string) variable from where to get means}

\item{svy}{(string) Possible projects to analyse.must be equal to ALL, IALS,
IELS, PIAAC, PISA, PISA2015, PISAOOS, TALISSCH, TALISTCH}
}
\value{
Dataframe containing the number of observations valid for the target variable x
}
\description{
Number of observations valid for column x
}
\examples{
data(df_pisa18)
data(df_talis18) 

n_obs_x(df = df_pisa18, by = "cnt",x = "wb173q03ha", svy = "PISA2015")
n_obs_x(df = df_talis18, by = "cntry",x = "tt3g01", svy = "TALISTCH")
}
