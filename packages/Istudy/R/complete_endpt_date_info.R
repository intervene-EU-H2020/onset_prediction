#' Adds endpoint date information for controls and censored individuals
#' 
#' For cases endpoint date is the date of diagnois. 
#' For controls, it is the end of the observation period, and for 
#' censored individuals it is the end of followup date.
#'  
#' @param study_data A data.frame with at least columns `DATE_OF_BIRTH`
#'                   and i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE` 
#'                   where the columns are the study endpoint and 
#'                   date, which will differ depending on the input 
#'                   variable `endpt`.
#' @inheritParams adj_case_cntrl_status
#' 
#' @return The data.frame. For example for endpoint `J10_ASTHMA`, 
#'             the data.frame will have updated column`J10_ASTHMA_DATE`.
#' 
#' @export
#' 
#' @author Kira E. Detrois
complete_endpt_date_info <- function(study_data, 
                                     endpt) {
    check_cols_exist(study_data, 
                     c(endpt, paste0(endpt, "_DATE"), "OBS_END_DATE", "EXP_END_DATE"),
                     "calc_diag_time")  

    endpt_date <- dplyr::pull(study_data, 
                              get(paste0(endpt, "_DATE")))
    cntrls_idxs <- (study_data[,endpt] == 0)
    # Setting Date / AGE at onset of controls to end of study
    endpt_date[cntrls_idxs] <- study_data$OBS_END_DATE[cntrls_idxs]
    # Setting censored individuals dates
    cens_idxs <- (study_data$END_OF_FOLLOWUP < study_data$OBS_END_DATE & 
                    !is.na(study_data$END_OF_FOLLOWUP))
    endpt_date[cens_idxs] <- study_data$END_OF_FOLLOWUP[cens_idxs]

    study_data[,paste0(endpt, "_DATE")] <- endpt_date

    return(study_data)
}