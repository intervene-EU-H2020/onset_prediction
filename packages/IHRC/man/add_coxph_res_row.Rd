% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_tibbles.R
\name{add_coxph_res_row}
\alias{add_coxph_res_row}
\title{Adds a row to the Cox-PH HR results tibble}
\usage{
add_coxph_res_row(hr_res, coxph_mdl, study, preds)
}
\arguments{
\item{hr_res}{A tibble with the results for previous endpts,
with columns: \code{ENDPOINT}, \code{EXP_AGE}, \code{VAR},
\code{GROUP}, \code{N_CONTROLS}, \code{N_CASES}, \code{BETA},
\code{SE}, \code{P_VAL}, \code{HR}, \code{CI_NEG}, \code{CI_POS}.}

\item{coxph_mdl}{A \code{coxph} object. See function \link[survival:coxph]{survival::coxph}.}

\item{study}{An S4 \code{study} object. The current study.
See class definition \link[Istudy:study-class]{Istudy::study}.}
}
\value{
A tibble. The updated \code{hr_res} with the added row for
the HRs of the current endpoint.
}
\description{
Adds a row to the Cox-PH HR results tibble
}
\author{
Kira E. Detrois
}
