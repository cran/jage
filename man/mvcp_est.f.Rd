% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mvcp_est.f.R
\name{mvcp_est.f}
\alias{mvcp_est.f}
\title{A function for estimating age from 8 teeth via multivariate cumulative probit and a user-determined prior.
Call directly iff you do NOT want to correct for fuzzy posteriors. Call find_fuzzies.f otherwise.}
\usage{
mvcp_est.f(prior, dat)
}
\arguments{
\item{prior}{as string argument for desired prior}

\item{dat}{as input data.table of rows of dental development scores, may have only 1 row}
}
\value{
returns postm as a data.table of posterior probability distributions
}
\description{
A function for estimating age from 8 teeth via multivariate cumulative probit and a user-determined prior.
Call directly iff you do NOT want to correct for fuzzy posteriors. Call find_fuzzies.f otherwise.
}
