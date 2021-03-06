% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/outlier.R
\name{outlier}
\alias{outlier}
\title{The main CADE outlier function}
\usage{
outlier(df, prop = 1, skip = c(), cutoff = 0.5, ...)
}
\arguments{
\item{df}{The data frame in question}

\item{prop}{The proportion of true to false cases}

\item{skip}{Fields that should not be included}

\item{cutoff}{The probability above which to consider an outlier}

\item{...}{Additional arguments to be passed on to randomForest}
}
\description{
This function calls and runs CADE
}
\examples{
outlier(cars)
}
\references{
Friedland, L., Gentzel, A. & Jensen, D. (2014). Classifier-Adjusted Density Estimation for
   Anomaly Detection and One-Class Classification
}
\keyword{anonaly}
\keyword{detection}

