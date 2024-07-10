#' Adds column with age at the endpoint diagnosis
#' 
#' The age is calculated from birth and from baseline.
#'  
#' @param study_data A data.frame with at least columns `DATE_OF_BIRTH`,
#'                   and i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE` where 
#'                   the columns are the study endpoint and date, which will 
#'                   differ depending on the input variable `endpt`.
#' @param endpt A string. The current endpoint. 
#' 
#' @return The data.frame with tow added columns `AGE_AT_BASE` and `AGE_AT_END`
#' 
#' @export
#' 
#' @author Kira E. Detrois
add_age_event_cols <- function(study_data, 
                               endpt) {
    check_cols_exist(study_data, 
                     c(endpt, paste0(endpt, "_DATE"), "WASH_END_DATE", "DATE_OF_BIRTH"),
                     "add_age_columns")  

    surv_interval <- study_data$WASH_END_DATE %--% dplyr::pull(study_data, get(paste0(endpt, "_DATE")))
    surv_time <- lubridate::time_length(surv_interval, "years")
    study_data[,paste0(endpt, "_AGE_FROM_BASE")] <- surv_time

    surv_age_interval <- study_data$DATE_OF_BIRTH %--% dplyr::pull(study_data, get(paste0(endpt, "_DATE")))
    surv_age <- lubridate::time_length(surv_age_interval, "years")
    study_data[,paste0(endpt, "_AGE_AT_EVENT")] <- surv_age

    return(study_data)
} 