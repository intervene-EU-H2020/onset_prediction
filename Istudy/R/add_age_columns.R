#' Adds column with survival time from baseline 
#' 
#' Survival time is counted from the start of the observation
#' period until end of followup.
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
add_age_obs_cols <- function(study_data) {
    check_cols_exist(study_data, 
                     c("OBS_START_DATE", "OBS_END_DATE", "DATE_OF_BIRTH"),
                     "add_age_obs_cols")  

    age_base_interval <- study_data$DATE_OF_BIRTH %--% study_data$OBS_START_DATE
    age_base <- lubridate::time_length(age_base_interval, "years")
    study_data[,"AGE_AT_BASE"] <- age_base

    age_end_interval <- study_data$DATE_OF_BIRTH %--% study_data$OBS_END_DATE
    age_end <- lubridate::time_length(age_end_interval, "years")
    study_data[,"AGE_AT_END"] <- age_end

    return(study_data)
}

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