% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mfh_collapse.R
\name{mfh_collapse}
\alias{mfh_collapse}
\title{A function for collapsing Moorrees et al. (1963) dental development stages for use in find_fuzzies.f and mvcp_est.f}
\usage{
mfh_collapse(cases)
}
\arguments{
\item{cases}{as input data.table of Moorrees et al. dental development scores, or vector of scores from single individual}
}
\value{
returns data.table of collapsed scores
}
\description{
WARNING: this function is based on scoring with an additional crypt stage. Pre-collapse staging: 0-no crypt, 1-crypt, 2-Ci, 3-Cco, 4-Coc, 5-Cr1/2, 6-Cr3/4, 7-Crc, 8-Ri, 9-Cli, 10-R1/4, 11-R1/2, 12-R3/4, 13-Rc, 14-A1/2, 15-Ac
}
\examples{
mfh_collapse(c(NA,NA,15,15,14,12,11,15,11,8))
}
