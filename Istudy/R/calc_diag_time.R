#' Adds columns for the date and exact age in years of onset 
#' 
#' Calculates the age at onset of the endpoint of interest.
#' For cases the date of onset is the date of diagnosis and for
#' controls it is the end of the study period.
#'  
#' @param study_data A data.frame with at least columns `DATE_OF_BIRTH`
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
add_diag_time_cols <- function(study_data, 
                               endpt) {
    onset_time <- calc_diag_time(study_data, endpt)
    study_data[,paste0(endpt, "_AGE")] <- onset_time$exact_age_yrs
    study_data[,paste0(endpt, "_DATE")] <- onset_time$onset_date

    return(study_data)
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
calc_diag_time <- function(study_data, 
                           endpt) {
    check_cols_exist(study_data, 
                     c(endpt, paste0(endpt, "_DATE"), "OBS_END_DATE", "EXP_END_DATE"),
                     "calc_diag_time")  

    endpt_date <- dplyr::pull(study_data, get(paste0(endpt, "_DATE")))
    folend_date <- dplyr::pull(study_data, END_OF_FOLLOWUP)
    cntrls_idxs <- (study_data[,endpt] == 0)
    # Setting Date / AGE at onset of controls to end of study
    endpt_date[cntrls_idxs] <- study_data$OBS_END_DATE[cntrls_idxs]
    # Setting censored individuals dates
    cens_idxs <- (study_data$END_OF_FOLLOWUP < study_data$OBS_END_DATE & 
                    !is.na(study_data$END_OF_FOLLOWUP))
    endpt_date[cens_idxs] <- study_data$END_OF_FOLLOWUP[cens_idxs]

    # Gets exact age in years
    endpt_date <- as.Date(endpt_date, origin="1970/01/01")
    age_at_onset <- lubridate::time_length(study_data$EXP_END_DATE %--% endpt_date, "years")
    return(list(exact_age_yrs=age_at_onset, onset_date=endpt_date))
}
