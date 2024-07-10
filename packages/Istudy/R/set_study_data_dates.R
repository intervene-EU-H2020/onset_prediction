#' Sets the important dates for the study individuals
#' 
#' Sets columns for `YEAR_OF_BIRTH`, `EXP_START_DATE`, `EXP_END_DATE`,
#' `WASH_END_DATE`, `OBS_END_DATE`, `ENDPT_FREE_PERIOD`, `STUDY_TIME`.
#' For each individuals depending on the selected study setup.
#' 
#' @param study_data A tibble. The data on all study individuals.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' 
#' @export 
#' @importFrom lubridate %m-%
#' 
#' @author Kira E. Detrois
set_study_dates <- function(study_data,
                            study_setup) {
    cols <- c("EXP_START_DATE", "WASH_START_DATE", "EXP_END_DATE", "OBS_START_DATE", 
              "WASH_END_DATE", "OBS_END_DATE", "ENDPT_FREE_PERIOD", "STUDY_TIME",
              "YEAR_OF_BIRTH")
    study_data$YEAR_OF_BIRTH <- lubridate::year(study_data$DATE_OF_BIRTH)
    study_data$EXP_START_DATE <- calc_exp_start_date(
                                                    study_data=study_data,
                                                    study_setup=study_setup)
        study_data$WASH_START_DATE <- calc_wash_start_date(
                                                    study_data=study_data,
                                                    study_setup=study_setup)
        study_data$EXP_END_DATE <- calc_exp_end_date(study_data, study_setup@exp_len)
        study_data$OBS_START_DATE <- calc_obs_start_date(
                                                    study_data=study_data,
                                                    study_setup=study_setup)
        study_data$WASH_END_DATE <- calc_wash_end_date(study_data, study_setup@wash_len)
        study_data$OBS_END_DATE <- calc_obs_end_date(study_data=study_data,
                                                    study_setup=study_setup)
        study_data$ENDPT_FREE_PERIOD <- study_data$DATE_OF_BIRTH %--% study_data$WASH_END_DATE
        study_data$STUDY_TIME <- study_data$EXP_START_DATE %--% study_data$OBS_END_DATE
    

    return(study_data)
}

#' Calcualtes the end date of the washout period for each 
#' individual
#' 
#' @inheritParams set_study_dates
#' @param wash_len An integer. Length of the washout period (in years).
#' 
#' @return A vector of Dates.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_wash_end_date <- function(study_data,
                               wash_len) {
    if(wash_len != 0)
        wash_end_date <- study_data$OBS_START_DATE %m-% lubridate::days(1)
    else 
        wash_end_date <- study_data$WASH_START_DATE
    return(wash_end_date)
}

#' Calcualtes the end date of the exposure period
#' 
#' @inheritParams set_study_dates
#' @param exp_len An integer. Length of the exposure period (in years).
#' 
#' @return A vector of Dates.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_exp_end_date <- function(study_data, 
                              exp_len) {
    if(exp_len != 0 | is.na(exp_len))
        exp_end_date <- study_data$WASH_START_DATE %m-% lubridate::days(1)
    else 
        exp_end_date <- study_data$EXP_START_DATE
    return(exp_end_date)
}

#' Calcualtes the start date of the observation period for each 
#' individual
#' 
#' Calcualtes the start date of the observation period for each 
#' individual, depending on the selected study setup.
#' 
#' @inheritParams set_study_dates
#' 
#' @return A vector of Dates. The start date of the observation period 
#' for each individual.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_obs_end_date <- function(study_data,
                              study_setup) {
    if(study_setup@study_type == "forward") {
        obs_end_date <- study_data$OBS_START_DATE %m+% lubridate::years(study_setup@obs_len)
    } else if(study_setup@study_type == "backward") {
        obs_end_date <- study_setup@obs_end_date
    }

    return(obs_end_date)
}

#' Calcualtes the end date of the washout period for each individual
#' 
#' Calcualtes the end date of the washout period for each individual,
#' depending on the selected study setup.
#' 
#' @inheritParams set_study_dates
#' 
#' @return A vector of Dates. The end of the washout period for
#' each individual.
#' 
#' @importFrom lubridate %m-%
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_obs_start_date <- function(study_data,
                                study_setup) {
    if(study_setup@study_type == "forward") {
        obs_start_date <- study_data$WASH_START_DATE %m+% lubridate::years(study_setup@wash_len)
    } else if(study_setup@study_type == "backward") {
        obs_start_date <- calc_backward_obs_start_date(study_setup@obs_end_date,
                                                       study_setup@obs_len)
    }
    return(obs_start_date)
}

#' Calcualtes the start date of the observation period for a backward study setup
#' 
#' @param obs_end_date A Date. The end of the observation period.
#' @param obs_len An integer. Length of the prediction period (in years).
#' 
#' @return A Date
#' 
#' @importFrom lubridate %m-%

#' @export 
#' 
#' @author Kira E. Detrois
calc_backward_obs_start_date <- function(obs_end_date, 
                                         obs_len) {
    obs_end_date %m-% lubridate::years(obs_len)
}

#' Calcualtes the end date of the exposure period for each individual
#' 
#' Calcualtes the end date of the exposure period for each individual,
#' depending on the selected study setup.
#' 
#' @inheritParams set_study_dates
#' 
#' @return A vector of Dates. The end of the exposure period for
#' each individual.
#' 
#' @importFrom lubridate %m-%
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_wash_start_date <- function(study_data,
                                 study_setup) {
    if(study_setup@study_type == "forward") {
        wash_start_date <- study_data$EXP_START_DATE %m+% lubridate::years(study_setup@exp_len)
    } else if(study_setup@study_type == "backward") {
        wash_start_date <- calc_backward_wash_start_date(study_setup@obs_end_date,
                                                         study_setup@wash_len,
                                                         study_setup@obs_len)
    }

    return(wash_start_date)
}


#' Calcualtes the start date of the washout period for a backward study setup
#' 
#' @param obs_end_date A Date. The end of the observation period.
#' @param exp_len An integer. Length of the exposure period (in years).
#' @param obs_len An integer. Length of the prediction period (in years).
#' 
#' @return A Date
#' 
#' @importFrom lubridate %m-%

#' @export 
#' 
#' @author Kira E. Detrois
calc_backward_wash_start_date <- function(obs_end_date, 
                                          wash_len,
                                          obs_len) {
    obs_end_date %m-% lubridate::years(obs_len + wash_len)
}


#' Calcualtes the end date of the exposure period for each individual
#' 
#' Calcualtes the end date of the exposure period for each individual,
#' depending on the selected study setup.
#' 
#' @inheritParams set_study_dates
#' 
#' @return A vector of Dates. The end of the observation period for
#' each individual.
#' 
#' @importFrom lubridate %m+%
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_exp_start_date <- function(study_data,
                                study_setup) {
    if(study_setup@study_type == "forward") {
        exp_start_date <- study_data$DATE_OF_BIRTH %m+% lubridate::years(study_setup@exp_age)
    } else if(study_setup@study_type == "backward") {
        if(!all(is.na(study_setup@exp_age))) {
            exp_start_date <- study_data$DATE_OF_BIRTH
        } else {
            exp_start_date <- calc_backward_exp_start_date(study_setup@obs_end_date,
                                                           study_setup@exp_len,
                                                           study_setup@wash_len,
                                                           study_setup@obs_len)
        }
    }
    return(exp_start_date)
}

#' Calcualtes the end date of the exposure period for a backward study setup
#' 
#' @param obs_end_date A Date. The end of the observation period.
#' @param exp_len An integer. Length of the exposure period (in years).
#' @param wash_len An integer. Length of the washout period (in years).
#' @param obs_len An integer. Length of the prediction period (in years).
#' 
#' @return A Date
#' 
#' @importFrom lubridate %m-%

#' @export 
#' 
#' @author Kira E. Detrois
calc_backward_exp_start_date <- function(obs_end_date,
                                         exp_len,
                                         wash_len,
                                         obs_len) {
    obs_end_date %m-% lubridate::years(obs_len + wash_len + exp_len) 
}
