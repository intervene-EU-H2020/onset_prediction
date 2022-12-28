#' Sets the important dates for the study individuals
#' 
#' Sets columns for `YEAR_OF_BIRTH`, `EXP_START_DATE`, `EXP_END_DATE`,
#' `WASH_END_DATE`, `OBS_END_DATE`, `ENDPT_FREE_PERIOD`, `STUDY_TIME`.
#' For each individuals depending on the selected study setup.
#' 
#' @param study_data A tibble. The data on all study individuals.
#' @inheritParams filter_too_old_and_young
#' @param exp_age An integer. Age at which exposure period starts 
#'                            (in years).
#' @param exp_len An integer. Length of the exposure period (in years).
#' @param wash_len An integer. Length of the washout period (in years).
#' @param obs_len An integer. Length of the observation period 
#'                            (in years).
#' @param obs_end_date A Date. The end of the observation period.
#' @return The S4 study object with the `study_data` tibble updated
#' with the date columns.
#' 
#' @export 
#' @importFrom lubridate %m-%
#' 
#' @author Kira E. Detrois
set_study_dates <- function(study_data,
                            study_type="forward",
                            exp_age=NA_integer_,
                            exp_len=NA_integer_,
                            wash_len=2,
                            obs_len=8,
                            obs_end_date=as.Date("2021/01/01")) {
    study_data$YEAR_OF_BIRTH <- lubridate::year(study_data$DATE_OF_BIRTH)
    study_data$EXP_START_DATE <- calc_exp_start_date(
                                                study_data=study_data,
                                                study_type=study_type,
                                                exp_age=exp_age,
                                                exp_len=exp_len,
                                                wash_len=wash_len,
                                                obs_len=obs_len,
                                                obs_end_date=obs_end_date)
    study_data$WASH_START_DATE <- calc_wash_start_date(
                                                study_data=study_data,
                                                study_type=study_type,
                                                exp_len=exp_len,
                                                wash_len=wash_len,
                                                obs_len=obs_len,
                                                obs_end_date=obs_end_date)
    study_data$EXP_END_DATE <- calc_exp_end_date(study_data, exp_len)
    study_data$OBS_START_DATE <- calc_obs_start_date(
                                                study_data=study_data,
                                                study_type=study_type,
                                                wash_len=wash_len,
                                                obs_len=obs_len,
                                                obs_end_date=obs_end_date)
    study_data$WASH_END_DATE <- calc_wash_end_date(study_data, wash_len)
    study_data$OBS_END_DATE <- calc_obs_end_date(study_data=study_data,
                                                 study_type=study_type,
                                                 obs_len=obs_len,
                                                 obs_end_date=obs_end_date)
    study_data$ENDPT_FREE_PERIOD <- study_data$DATE_OF_BIRTH %--% study_data$WASH_END_DATE
    study_data$STUDY_TIME <- study_data$EXP_START_DATE %--% study_data$OBS_END_DATE

    return(study_data)
}

calc_wash_end_date <- function(study_data,
                               wash_len) {
    if(wash_len != 0)
        wash_end_date <- study_data$OBS_START_DATE %m-% lubridate::days(1)
    else 
        wash_end_date <- study_data$WASH_START_DATE
    return(wash_end_date)
}

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
                              study_type,
                              obs_len,
                              obs_end_date) {
    if(study_type == "forward") {
        obs_end_date <- study_data$OBS_START_DATE %m+% lubridate::years(obs_len)
    } else if(study_type == "backward") {
        obs_end_date <- obs_end_date
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
                               study_type,
                               wash_len,
                               obs_len,
                               obs_end_date) {
    if(study_type == "forward") {
        wash_end_date <- study_data$WASH_START_DATE %m+% lubridate::years(wash_len)
    } else if(study_type == "backward") {
        wash_end_date <- obs_end_date %m-% lubridate::years(obs_len)
    }
    return(wash_end_date)
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
                              study_type,
                              exp_len,
                              wash_len,
                              obs_len,
                              obs_end_date) {
    if(study_type == "forward") {
        exp_end_date <- study_data$EXP_START_DATE %m+% lubridate::years(exp_len)
    } else if(study_type == "backward") {
        exp_end_date <- obs_end_date %m-% lubridate::years(obs_len + wash_len)
    }

    return(exp_end_date)
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
#' @importFrom lubridate %m+%
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_exp_start_date <- function(study_data,
                                study_type,
                                exp_age,
                                exp_len,
                                wash_len,
                                obs_len,
                                obs_end_date) {
    if(study_type == "forward") {
        exp_start_date <- study_data$DATE_OF_BIRTH %m+% lubridate::years(exp_age)
    } else if(study_type == "backward") {
        if(!is.na(exp_age)) {
            exp_start_date <- study_data$DATE_OF_BIRTH
        } else {
            exp_start_date <- obs_end_date %m-% lubridate::years(obs_len + wash_len + exp_len) 
        }
    }
    return(exp_start_date)
}
