#' Calculates the age at onset 
#' 
#' Calculates the age at onset of the endpoint of interest in days
#' from birth.
#' The age of onset is either the date of diagnosis for cases or the
#' end of the study period for controls.
#'  
#' 
#' @inheritParams get_study_elig_indv
#' 
#' @return A numeric. The age at onset of the endpoint in days from birth
#'                    for each individual.
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
calc_age_at_onset <- function(pheno_data, 
                              exp_age, 
                              exp_len, 
                              wash_len, 
                              obs_len,
                              endpt) {
    endpt_diag <- dplyr::pull(pheno_data, get(paste0(endpt, "_DATE")))

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

    return(age_at_onset)
}