#' Filters out individuals that are too old or young for the study
#' 
#' This is mainly needed for studies that consider variable exposure lengths for 
#' individuals selected at a given time point. 
#' Here the exposure length will be the time from birth until the time point
#' considered and should be provided in the S4 study object in slot `EXP_END`
#' in the same order as the individuals in the data.frame.
#' 
#' It might be that the birth of an individual is after the selected time point
#' so their exposure lengths are negative. These individuals are always removed.
#' 
#' @param study_data A data.frame with at least column `EXP_END`.
#' @param study_type A character. Can be either `forward` or `backward`. 
#' @param max_age A numeric. The maximum age for individuals in the study. 
#'                           Individuals are censored, once they reach the maximum age. 
#' 
#' @export 
#' 
#' @author Kira E. Detrois
filter_too_old_and_young <- function(study_data,
                                     study_type,
                                     max_age=200) {
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
    study_data <- filter_old_age(study_data, max_age)
    study_data <- dplyr::filter(study_data, 
                                   lubridate::year(EXP_START_DATE) >= 1998)

    # End of followup is either last day indv known to be alive or death date
    # Has to be non-censored at least at the end of the wash-out period
    study_data <- dplyr::filter(study_data, END_OF_FOLLOWUP > WASH_END_DATE | 
                                    is.na(END_OF_FOLLOWUP))

    return(study_data)
}

#' Censors individuals once they reach the maximum age
#' 
#' @inheritParams filter_too_old_and_young
#' 
#' @export 
filter_old_age <- function(study_data,
                           max_age=200) {
    study_data <- dplyr::mutate(study_data, 
                                OBS_END_AGE=lubridate::time_length(study_data$DATE_OF_BIRTH %--% study_data$OBS_END_DATE, "years"))

    study_data <- dplyr::filter(study_data, OBS_END_AGE < max_age)
    return(study_data)
}