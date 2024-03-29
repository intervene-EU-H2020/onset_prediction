% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study_class_def.R
\docType{class}
\name{study_setup-class}
\alias{study_setup-class}
\alias{study_setup}
\title{An S4 class representing the study setup}
\description{
An S4 class representing the study setup
}
\section{Slots}{

\describe{
\item{\code{study_type}}{A character. Can be either \code{forward} or \code{backward}.
\code{forward} considers individuals of a certain age.
It calculates the exposure, washout, and observation
period onwards from this age.
\code{backward} considers all individuals at a set time
point. The observation and washout period are calcualted
backwards from this time point.}

\item{\code{exp_age}}{An integer. Age at which exposure period starts
(in years).}

\item{\code{exp_len}}{An integer. Length of the exposure period (in years).}

\item{\code{wash_len}}{An integer. Length of the washout period (in years).}

\item{\code{obs_len}}{An integer. Length of the observation period (in years).}

\item{\code{obs_end_date}}{A Date. The end of the observation period.}

\item{\code{down_fctr}}{An integer. Defines how many controls there
should be for every case.
Default is NA, which means no
downsampling is performed.}

\item{\code{exp_f1998}}{A boolean. Defines whether to filter out any data from before 1998.}

\item{\code{ancs}}{A character (vector). The ancestries to consider.}

\item{\code{obs_age_range}}{A numeric. The age range of individuals in the observation
period. Inclusive interval.}
}}

\author{
Kira E. Detrois
}
