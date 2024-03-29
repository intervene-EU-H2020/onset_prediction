% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_tibbles.R
\name{create_empty_hr_res}
\alias{create_empty_hr_res}
\title{Creates an empty tibble for endpoints hazard ratios}
\usage{
create_empty_hr_res()
}
\value{
An empty tibble with the following columns:
\itemize{
\item ENDPOINT: endpoint name
\item EXP_AGE: exposure age
\item VAR: predictor variable
\item GROUP: group
\item N_CONTROLS: number of controls
\item N_CASES: number of cases
\item BETA: beta coefficient
\item SE: standard error
\item P_VAL: p-value
\item HR: hazard ratio
\item CI_NEG: negative bound of the 95\% confidence interval
\item CI_POS: positive bound of the 95\% confidence interval
}
}
\description{
Creates an empty tibble for endpoints hazard ratios
}
\author{
Kira E. Detrois
}
