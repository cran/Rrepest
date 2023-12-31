% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rrepest.R
\name{Rrepest}
\alias{Rrepest}
\title{Rrepest}
\usage{
Rrepest(
  data,
  svy,
  est,
  by = NULL,
  over = NULL,
  test = FALSE,
  user_na = FALSE,
  show_na = FALSE,
  flag = FALSE,
  fast = FALSE,
  tabl = FALSE,
  average = NULL,
  group = NULL,
  ...
)
}
\arguments{
\item{data}{(dataframe) df to analyze}

\item{svy}{(string) Possible projects to analyse.must be equal to ALL, IALS,
IELS, PIAAC, PISA, PISA2015, PISAOOS, TALISSCH, TALISTCH .}

\item{est}{(est function) that takes arguments stimate, target variable, regressor (optional for linear regressions)}

\item{by}{(string vector) column in which we'll break down results}

\item{over}{(vector string) columns over which to do analysis}

\item{test}{(bool) TRUE: will calculate the difference between over variables}

\item{user_na}{(bool) TRUE: show nature of user defined missing values for by.var}

\item{show_na}{(bool) TRUE: include na in frequencies of x}

\item{flag}{(bool) TRUE: Show NaN when there is not enough cases (or schools)}

\item{fast}{(bool) TRUE: Only do 6 replicated weights}

\item{tabl}{(bool) TRUE: Creates a flextable with all examples}

\item{average}{(grp function) that takes arguments group.name, column, cases to create averages at the end of df}

\item{group}{(grp function) that takes arguments group.name, column, cases to create groups at the end of df}

\item{...}{Optional filtering parameters: i.e.:
isced = 2,
n.pvs = 5,
cm.weights = c("finw",paste0("repw",1:22))
var.factor = 1/(0.5^2)
z.score = qnorm(1-0.05/2)}
}
\value{
Dataframe containing estimation "b." and standard error "se." of desired processes
}
\description{
Estimates statistics using replicate weights (Balanced Repeated Replication (BRR) weights,
Jackknife replicate weights,...), thus accounting for complex survey
designs in the estimation of sampling variances. It is specially designed
to be used with the data sets produced by the Organization for Economic Cooperation and Development (OECD),
some of which include the Programme for International Student Assessment (PISA) and
Teaching and Learning International Survey (TALIS) data sets, but works for all International Large Scale Assessments
that use replicated weights.  It also allows for
analyses with multiply imputed variables (plausible values); where
plausible values are included in a pvvarlist, the average estimator
across plausible values is reported and the imputation error is added to
the variance estimator.
}
\examples{
data(df_pisa18)

Rrepest(data = df_pisa18,
svy = "PISA2015",
est = est("mean","AGE"),
by = c("CNT"))



}
