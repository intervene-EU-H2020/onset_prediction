% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_surv_studies.R
\name{run_models}
\alias{run_models}
\title{Runs the Cox-PH on the data}
\usage{
run_models(study_setup, endpts, surv_ana, is_first_endpt, subset_train)
}
\arguments{
\item{study_setup}{An S4 \code{study_setup} object. The current study setup.
See class definition \link[Istudy:study_setup-class]{Istudy::study_setup}.}

\item{endpts}{A string (vector). The endpoints of interest.}

\item{surv_ana}{An S4 \code{surv_ana} object. The current survival analysis setup.
See class definition \link{surv_ana}.}
}
\description{
Reads in previously created phenotype-score files. Then creates a \code{study} object for
each selected endpoint and runs the Cox-PH models and extracts the HR data and
c-index for each model. Finally, writes them to files and plots the HR data.
}
\author{
Kira E. Detrois
}
