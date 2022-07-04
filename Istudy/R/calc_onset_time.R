#' Calculates the date of onset and the age in days
#' 
#' Calculates the age at onset of the endpoint of interest.
#' The date of onset is either the date of diagnosis for cases or the
#' end of the study period for controls.
#'  
#' @param pheno_data A data.frame with at least columns `DATE_OF_BIRTH`
#'                   and i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE` 
#'                   where the columns are the study endpoint and 
#'                   date, which will differ depending on the input 
#'                   variable `endpt`.
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
#' study <- methods::new("study", endpt="J10_ASTHMA", exp_age=30, 
#'                       exp_len=10, wash_len=2, obs_len=8, ancs="EUR")
#' calc_study_time(bds, study)
#' 
#' @author Kira E. Detrois
calc_onset_time <- function(pheno_data, 
                            study) {
    check_cols_exist(pheno_data, 
                     c(study@endpt, paste0(study@endpt, "_DATE"), "DATE_OF_BIRTH"),
                     "calc_onset_time")  

    endpt_date <- dplyr::pull(pheno_data, get(paste0(study@endpt, "_DATE")))
    study_end <- calc_end_of_study(pheno_data$DATE_OF_BIRTH,
                                   study)
    endpt_date[is.na(endpt_date)] <- study_end[is.na(endpt_date)]
    endpt_date <- as.Date(endpt_date, origin="1970/01/01")
    age_at_onset <- lubridate::time_length(difftime(endpt_date, 
                                                    pheno_data$DATE_OF_BIRTH,
                                                    'years'))

    return(list(age_days=age_at_onset, onset_date=endpt_date))
}
