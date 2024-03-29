% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adj_case_cntrl_status.R
\name{adj_case_cntrl_status}
\alias{adj_case_cntrl_status}
\title{Adjusts the case status of individuals with late diagnosed endpoints}
\usage{
adj_case_cntrl_status(study_data, endpt)
}
\arguments{
\item{study_data}{A data.frame with at least columns \code{STUDY_TIME}
and i.e. \code{J10_ASTHMA}, and \code{J10_ASTHMA_DATE}
where the columns are the study endpoint and
date, which will differ depending on the input
variable \code{endpt}.}

\item{endpt}{A character. The column name of the current endpoint of
interest.}
}
\description{
Those individuals where the endpoint diagnosis date is after the
study has ended, are considered controls.
}
\author{
Kira E. Detrois
}
