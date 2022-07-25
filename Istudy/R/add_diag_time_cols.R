#' Adds columns for the date and exact age in years of onset 
#' 
#' Calculates the age at onset of the endpoint of interest.
#' For cases the date of onset is the date of diagnosis and for
#' controls it is the end of the study period.
#'  
#' @param pheno_data A data.frame with at least columns `DATE_OF_BIRTH`
#'                   and i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE` 
#'                   where the columns are the study endpoint and 
#'                   date, which will differ depending on the input 
#'                   variable `endpt`.
#' @inheritParams get_study_elig_indv
#' 
#' @return The data.frame. For example for endpoint `J10_ASTHMA`, 
#'             the data.frame will have and added column `J10_ASTHMA_AGE`, 
#'             and for controls the column `J10_ASTHMA_DATE` now contains
#'             the study end date.
#' 
#' @export
#' 
#' @author Kira E. Detrois
add_diag_time_cols <- function(pheno_data, 
                               study) {
    onset_time <- calc_diag_time(pheno_data, study)
    pheno_data[,paste0(study@endpt, "_AGE")] <- onset_time$exact_age_yrs
    pheno_data[,paste0(study@endpt, "_DATE")] <- onset_time$onset_date

    return(pheno_data)
}

#' Calculates the date and exact age in years of onset 
#' 
#' Calculates the age at onset of the endpoint of interest.
#' For cases the date of onset is the date of diagnosis and for
#' controls it is the end of the study period.
#'  
#' @inheritParams add_diag_time_cols
#' 
#' @return A list(`exact_age_yrs`, `age_date`).
#'         \itemize{
#'          \item `exact_age_yrs`: The age at onset in days
#'          \item `age_date`: The date of onset
#'         } 
#' 
#' @export
#' 
#' @author Kira E. Detrois
calc_diag_time <- function(pheno_data, 
                           study) {
    check_cols_exist(pheno_data, 
                     c(study@endpt, paste0(study@endpt, "_DATE"), "DATE_OF_BIRTH", "EXP_LEN"),
                     "calc_diag_time")  

    endpt_date <- dplyr::pull(pheno_data, get(paste0(study@endpt, "_DATE")))
    study_end <- calc_end_of_study(pheno_data, study)
    cntrls_idxs <- (pheno_data[,study@endpt] == 0)
    # Setting Date / AGE at onset of controls to end of study
    endpt_date[cntrls_idxs] <- study_end[cntrls_idxs]
    # Gets exact age in years
    endpt_date <- as.Date(endpt_date, origin="1970/01/01")
    age_at_onset <- lubridate::time_length(pheno_data$DATE_OF_BIRTH %--% endpt_date, "years")
    return(list(exact_age_yrs=age_at_onset, onset_date=endpt_date))
}
