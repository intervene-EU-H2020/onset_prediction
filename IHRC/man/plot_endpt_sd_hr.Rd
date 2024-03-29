% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_HRs.R
\name{plot_endpt_sd_hr}
\alias{plot_endpt_sd_hr}
\title{Plots the HR from the Cox-PH model of a backwards study

Creates a single ggplot with the different endpoints HRs.}
\usage{
plot_endpt_sd_hr(coxph_hrs, study_setup, surv_ana)
}
\arguments{
\item{coxph_hrs}{A tibble. The Cox-PH HR results. Needs
to at least contain the columns \code{ENDPOINT},
\code{HR}, \code{CI_NEG}, \code{CI_POS}, and \code{GROUP}.}

\item{study_setup}{An S4 \code{study_setup} object. The current study setup.
See class definition \link[Istudy:study_setup]{Istudy::study_setup}.}

\item{surv_ana}{An S4 \code{surv_ana} object. The current survival analysis setup.
See class definition \link{surv_ana}.}
}
\description{
Plots the HR from the Cox-PH model of a backwards study

Creates a single ggplot with the different endpoints HRs.
}
\author{
Kira E. Detrois
}
