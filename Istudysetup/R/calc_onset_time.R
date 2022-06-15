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
#' calc_study_time(bds,
#'                 exp_age=30,
#'                 exp_len=10,
#'                 wash_len=2,
#'                 obs_len=8)
#' 
#' @author Kira E. Detrois
calc_onset_time <- function(pheno_data, 
                            endpt,
                            exp_age, 
                            exp_len, 
                            wash_len, 
                            obs_len) {

    test_length_vars_are_integers(as.list(environment()))             
    test_endpt_input_correct(as.list(environment()))

    endpt_date_str <- paste0(endpt, "_DATE")
    endpt_diag <- dplyr::pull(pheno_data, get(endpt_date_str))

    study_end <- calc_end_of_study(pheno_data$DATE_OF_BIRTH,
                                   exp_age, 
                                   exp_len, 
                                   wash_len, 
                                   obs_len)

    endpt_diag[is.na(endpt_diag)] <- study_end[is.na(endpt_diag)]

    endpt_diag <- as.Date(endpt_diag, origin="1970/01/01")

    age_at_onset <- lubridate::time_length(difftime(endpt_diag, 
                                                    pheno_data$DATE_OF_BIRTH,
                                                    'years'))

    return(list(age_days=age_at_onset, onset_date=endpt_diag))
}
