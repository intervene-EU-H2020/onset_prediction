% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_file_names.R
\name{get_full_file_name_path}
\alias{get_full_file_name_path}
\title{Creats the file directory and name for the different result types}
\usage{
get_full_file_name_path(res_type, study_setup, endpt = NULL, surv_ana)
}
\arguments{
\item{res_type}{A string. The results type.}

\item{study_setup}{An S4 \code{study_setup} object. The current study setup.
See class definition \link[Istudy:study_setup-class]{Istudy::study_setup}.}

\item{endpt}{A string. The current endpoint.}

\item{surv_ana}{An S4 \code{surv_ana} object. The current survival analysis setup.
See class definition \link{surv_ana}.}
}
\value{
A string. The file name.
}
\description{
Creats the file directory and name for the different result types
}
\author{
Kira E. Detrois
}
