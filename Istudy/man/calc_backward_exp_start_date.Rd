% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_study_data_dates.R
\name{calc_backward_exp_start_date}
\alias{calc_backward_exp_start_date}
\title{Calcualtes the end date of the exposure period for a backward study setup}
\usage{
calc_backward_exp_start_date(obs_end_date, exp_len, wash_len, obs_len)
}
\arguments{
\item{obs_end_date}{A Date. The end of the observation period.}

\item{exp_len}{An integer. Length of the exposure period (in years).}

\item{wash_len}{An integer. Length of the washout period (in years).}

\item{obs_len}{An integer. Length of the prediction period (in years).}
}
\value{
A Date
}
\description{
Calcualtes the end date of the exposure period for a backward study setup
}
\author{
Kira E. Detrois
}
