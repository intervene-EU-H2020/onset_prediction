#' Calculates the date of onset and the age in days
#' 
#' Calculates the age at onset of the endpoint of interest.
#' The date of onset is either the date of diagnosis for cases or the
#' end of the study period for controls.
#'  
#' 
#' @inheritParams get_study_elig_indv
#' 
#' @return A list(`age_days`, `age_date`).
#'         \itemize{
#'          \item `age_days`: The age at onset in days
#'          \item `age_date`: The date of onset
#'         } 
#' 
#' @importFrom lubridate %m+%
#' @export
#' 
#' @examples 
#' bds <- c(as.Date("1923/07/01"), as.Date("1823/07/02"), as.Date("2002/04/01"))
#' study <- new("study", exp_age=10, exp_len=10, wash_len=2, obs_len=8)
#' calc_study_time(bds, study)
#' 
#' @author Kira E. Detrois
calc_onset_time <- function(pheno_data, 
                            study) {

    endpt_date_str <- paste0(study@endpt, "_DATE")
    endpt_diag <- dplyr::pull(pheno_data, get(endpt_date_str))

    study_end <- calc_end_of_study(pheno_data$DATE_OF_BIRTH,
                                   study)

    endpt_diag[is.na(endpt_diag)] <- study_end[is.na(endpt_diag)]

    endpt_diag <- as.Date(endpt_diag, origin="1970/01/01")

    age_at_onset <- lubridate::time_length(difftime(endpt_diag, 
                                                    pheno_data$DATE_OF_BIRTH,
                                                    'years'))

    return(list(age_days=age_at_onset, onset_date=endpt_diag))
}
