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
add_baseline_surv_age <- function(study_data, 
                                  endpt) {
    check_cols_exist(study_data, 
                     c(endpt, paste0(endpt, "_DATE"), "WASH_END_DATE"),
                     "add_baseline_surv_age")  

    surv_interval <- study_data$WASH_END_DATE %--% dplyr::pull(study_data, get(paste0(endpt, "_DATE")))
    surv_age <- lubridate::time_length(surv_interval, "years")
    study_data[,paste0(endpt, "_AGE")] <- surv_age

    return(study_data)
}