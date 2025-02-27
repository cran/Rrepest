% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/average.R
\name{average_groups}
\alias{average_groups}
\title{Group Averages}
\usage{
average_groups(
  res,
  data = NULL,
  group,
  by = NULL,
  over = NULL,
  est = NULL,
  svy = NULL,
  user_na = FALSE,
  ...
)
}
\arguments{
\item{res}{(dataframe) df of results with b. and se. to average}

\item{data}{(dataframe) df from which to get replicated weights}

\item{group}{(grp function) that takes arguments group.name, column, cases to create averages at the end of dataframe}

\item{by}{(string vector) column in which we'll break down results}

\item{over}{(vector string) columns over which to do analysis}

\item{est}{(est function) that takes arguments what = estimate, tgt = target, rgr = regressor}

\item{svy}{(string) name of possible projects to analyse TALISSCH and TALISTCH}

\item{user_na}{(bool) TRUE: show nature of user defined missing values for by.var}

\item{...}{Additional arguments such as na_to_zero : (Bool) TRUE → will take NA as zero for the simple average calculation}
}
\value{
Dataframe with avergas or weighted averages (totals) in last rows for the selected cases
}
\description{
Group Averages
}
