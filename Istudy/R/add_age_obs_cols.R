#' Adds column with age at start and end of the observation period
#' 
#' The age at the start of the observation period is the age at
#' baseline.
#' 
#'  
#' @param study_data A data.frame with at least columns `DATE_OF_BIRTH`,
#'                   `OBS_START_DATE` and `OBS_END_DATE`.
#' 
#' @return The data.frame with tow added columns `AGE_AT_BASE` and `AGE_AT_END`
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

