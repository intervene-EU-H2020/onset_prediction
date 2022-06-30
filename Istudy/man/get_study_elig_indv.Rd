% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_study_elig_indv.R
\name{get_study_elig_indv}
\alias{get_study_elig_indv}
\title{Gets the individuals which are eligble given the study setup}
\usage{
get_study_elig_indv(
  pheno_data,
  study,
  write_res = FALSE,
  res_dir = NA_character_
)
}
\arguments{
\item{pheno_data}{A data.frame with at least the columns:
\code{ID}, \code{SEX}, \code{DATE_OF_BIRTH}, \code{ANCESTRY},
\code{START_OF_FOLLOWUP}, \code{END_OF_FOLLOWUP},
\code{DATE_OF_BIRTH}, and i.e. \code{J10_ASTHMA}, and
\code{J10_ASTHMA_DATE} where the columns are the study
endpoint and date, which will differ depending on
the input variable \code{endpt}.}

\item{study}{An S4 class representing the study setup.}

\item{write_res}{A boolean. Defines whether to write the results to
a file or not. Default is FALSE.}

\item{res_dir}{A character. The directory to write the results to.}
}
\value{
A tibble with the information for the eligible individuals
with columns: \code{ID}, \code{SEX}, \code{DATE_OF_BIRTH},
\code{START_OF_FOLLOWUP}, \code{END_OF_FOLLOWUP}, \code{ANCESTRY},
and i.e. \code{J10_ASTHMA}, and \verb{J10_ASTHMA_DATE. where the last two columns are the study endpoint and date,  which will differ depending on the input variable }endpt`.
}
\description{
\itemize{
\item The study setup consist of an exposure window,
a washout period, and a prediction period. See function
\code{\link{calc_study_time}}.
\item Eligible individuals cannot have missing date in the
column for the endpoint of interest. See function
\code{\link{filter_missing_endpt_data}}.
\item Eligible individuals cannot have the
selected endpoint of interest inside the endpoint free
period. The endpoint free interval is the period from birth
until the prediction period begins. See function
\code{\link{filter_early_endpt}}.

}
}
\author{
Kira E. Detrois
}