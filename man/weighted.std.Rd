% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighted_sdev.R
\name{weighted.std}
\alias{weighted.std}
\title{Weighted standard deviation}
\usage{
weighted.std(x, w, na.rm = TRUE)
}
\arguments{
\item{x}{(numeric vector) Variable of interest for which to compute the standard deviation.}

\item{w}{(numeric vector) Vector with the weights.}

\item{na.rm}{(bool) if TRUE: Excludes missing values before computing the standard deviation}
}
\value{
Scalar containing the standard deviation
}
\description{
Computes the weighted standard deviation of a numeric vector.
}
\examples{
data(df_talis18)

weighted.std(df_talis18$TT3G02, df_talis18$TRWGT1)
}
