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
#' @param obs_age_range A numeric. The age range of individuals in the observation
#'                                 period. Inclusive interval. 
#' 
#' @export 
#' 
#' @author Kira E. Detrois
filter_too_old_and_young <- function(study_data,
                                     study_type,
                                     obs_age_range=c(0,200),
                                     exp_f1998=TRUE) {
    check_cols_exist(study_data, 
                     c("DATE_OF_BIRTH", "EXP_START_DATE", 
                     "OBS_END_DATE", "END_OF_FOLLOWUP"),
                     "filter_too_old_and_young")
    study_data <- filter_young_age(study_data, study_type, obs_age_range[1])
    study_data <- filter_old_age(study_data, obs_age_range[2])

    if(exp_f1998)
        study_data <- dplyr::filter(study_data, 
                                    lubridate::year(EXP_START_DATE) >= 1998)

    # End of followup is either last day indv known to be alive or death date
    # Has to be non-censored at least at the end of the wash-out period
    study_data <- dplyr::filter(study_data, 
                                END_OF_FOLLOWUP > WASH_END_DATE | 
                                is.na(END_OF_FOLLOWUP))

    return(study_data)
}

filter_young_age <- function(study_data,
                             study_type,
                             min_obs_age) {
   
    if(study_type == "backward") {
        study_data <- dplyr::filter(study_data, 
                                    EXP_START_DATE <= OBS_END_DATE & 
                                    DATE_OF_BIRTH <= EXP_START_DATE)
    }
    if(min_obs_age != 0) {
        study_data <- dplyr::filter(study_data, 
                                    AGE_AT_BASE >= min_obs_age)
    }
    return(study_data)
}

#' Censors individuals once they reach the maximum age
#' 
#' @inheritParams filter_too_old_and_young
#' 
#' @export 
filter_old_age <- function(study_data,
                           max_obs_age) {
    study_data <- dplyr::filter(study_data, 
                                AGE_AT_BASE <= max_obs_age)
    return(study_data)
}