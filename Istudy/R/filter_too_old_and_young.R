#' Filters out individuals that are too old or young for the study
#' 
#' This is mainly needed for studies that consider variable exposure lengths for 
#' individuals selected at a given time point. 
#' Here the exposure length will be the time from birth until the time point
#' considered and should be provided in the S4 study object in slot `EXP_END`
#' in the same order as the individuals in the data.frame. A study setup like
#' this can be created using the function \code{\link{get_backward_study}}.
#' 
#' It might be that the birth of an individual is after the selected time point
#' so their exposure lengths are negative. These individuals are always removed.
#' Additionally, individuals are filtered out based on some maximum exposure 
#' length which can be set using the input variable `max_age`.
#' 
#' @param study_data A data.frame with at least column `EXP_END`.
#' @param max_age A numeric. The maximum length of the exposure period.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
filter_too_old_and_young <- function(study_data,
                                     study_type,
                                     max_age=200,
                                     filter_1998=FALSE) {
    check_cols_exist(study_data, 
                     c("DATE_OF_BIRTH", "EXP_START_DATE", 
                     "OBS_END_DATE", "END_OF_FOLLOWUP"),
                     "filter_too_old_and_young")
    # Too young
    if(study_type == "backward") {
        study_data <- dplyr::filter(study_data, 
                                    EXP_START_DATE <= OBS_END_DATE & 
                                    DATE_OF_BIRTH <= EXP_START_DATE)
    }
    # Too old

    study_data <- dplyr::filter(study_data, 
                    lubridate::time_length(DATE_OF_BIRTH %--% OBS_END_DATE, "years") < max_age)
    if(filter_1998) {
        study_data <- dplyr::filter(study_data, 
                        lubridate::year(EXP_START_DATE) >= 1998)
    }

    # End of followup is either last day indv known to be alive or death date
    # Has to be non-censored at least at the end of the wash-out period
    study_data <- dplyr::filter(study_data, END_OF_FOLLOWUP > WASH_END_DATE | 
                                    is.na(END_OF_FOLLOWUP))

    return(study_data)
}

#' Censors individuals once they reach the maximum age
censor_old_age <- function(study_data,
                           max_age=200) {
    reach_max_age <- (lubridate::time_length(study_data$DATE_OF_BIRTH %--% study_data$OBS_END_DATE, "years") > max_age) & (study_data$OBS_END_DATE < study_data$END_OF_FOLLOWUP | is.na(study_data$END_OF_FOLLOWUP))
    study_data[reach_max_age,]$END_OF_FOLLOWUP <- study_data[reach_max_age,]$OBS_END_DATE
    return(study_data)
}