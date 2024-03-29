% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_icd_file.R
\name{read_icd_file}
\alias{read_icd_file}
\title{Reads in the ICD file according to INTERVENE standard}
\usage{
read_icd_file(file_path)
}
\arguments{
\item{file_path}{A string (string). The path to the file.}
}
\value{
A tibble with at least columns \code{ID}, \code{EVENT_AGE}, \code{ICD_VERSION}, and
\code{PRIMARY_ICD}. Additionally, can have column \code{SECONDARY_ICD} if available in
the data.
}
\description{
See \href{https://docs.google.com/document/d/1E2Jc72CmMItEchgQaCvfA4MhZUkQYjALwTu3dCl7qd8/edit}{INTERVENE ICD longitudinal file definition v1}.
}
\details{
The needed columns are at least \code{ID}, \code{EVENT_AGE}, \code{ICD_VERSION},
\code{PRIMARY_ICD},
}
\author{
Kira E. Detrois
}
