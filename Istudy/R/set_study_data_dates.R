#' Sets the important dates for the study individuals
#' 
#' Sets columns for `YEAR_OF_BIRTH`, `EXP_START_DATE`, `EXP_END_DATE`,
#' `WASH_END_DATE`, `OBS_END_DATE`, `ENDPT_FREE_PERIOD`, `STUDY_TIME`.
#' For each individuals depending on the selected study setup.
#' 
#' @param .Object The S4 study object.
#' 
#' @return The S4 study object with the `study_data` tibble updated
#' with the date columns.
#' 
#' @author Kira E. Detrois
set_study_data_dates <- function(.Object) {
    .Object@study_data$YEAR_OF_BIRTH <- lubridate::year(.Object@study_data$DATE_OF_BIRTH)
    .Object@study_data$EXP_START_DATE <- calc_exp_start_date(.Object)
    .Object@study_data$EXP_END_DATE <- calc_exp_end_date(.Object)
    .Object@study_data$WASH_END_DATE <- calc_wash_end_date(.Object)
    .Object@study_data$OBS_END_DATE <- calc_obs_end_date(.Object)
    .Object@study_data$ENDPT_FREE_PERIOD <- .Object@study_data$DATE_OF_BIRTH %--% .Object@study_data$WASH_END_DATE
    .Object@study_data$STUDY_TIME <- .Object@study_data$EXP_START_DATE %--% .Object@study_data$OBS_END_DATE

    return(.Object)
}

#' Calcualtes the start date of the observation period for each 
#' individual
#' 
#' Calcualtes the start date of the observation period for each 
#' individual, depending on the selected study setup.
#' 
#' @inheritParams set_study_data_dates
#' 
#' @return A vector of Dates. The start date of the observation period 
#' for each individual.
#' 
#' @author Kira E. Detrois
calc_obs_end_date <- function(.Object) {
    if(.Object@study_type == "forward") {
        obs_end_date <- .Object@study_data$WASH_END_DATE %m+% lubridate::years(.Object@obs_len)
    } else if(.Object@study_type == "backward") {
        obs_end_date <- .Object@obs_end_date
    }
    return(obs_end_date)
}

#' Calcualtes the end date of the washout period for each individual
#' 
#' Calcualtes the end date of the washout period for each individual,
#' depending on the selected study setup.
#' 
#' @inheritParams set_study_data_dates
#' 
#' @return A vector of Dates. The end of the washout period for
#' each individual.
#' 
#' @importFrom lubridate %m-%
#' 
#' @author Kira E. Detrois
calc_wash_end_date <- function(.Object) {
    if(.Object@study_type == "forward") {
        wash_end_date <- .Object@study_data$EXP_END_DATE %m+% lubridate::years(.Object@wash_len)
    } else if(.Object@study_type == "backward") {
        wash_end_date <- .Object@obs_end_date %m-% lubridate::years(.Object@obs_len)
    }
    return(wash_end_date)
}

#' Calcualtes the end date of the exposure period for each individual
#' 
#' Calcualtes the end date of the exposure period for each individual,
#' depending on the selected study setup.
#' 
#' @inheritParams set_study_data_dates
#' 
#' @return A vector of Dates. The end of the exposure period for
#' each individual.
#' 
#' @importFrom lubridate %m-%
#' 
#' @author Kira E. Detrois
calc_exp_end_date <- function(.Object) {
    if(.Object@study_type == "forward") {
        exp_end_date <- .Object@study_data$EXP_START_DATE %m+% lubridate::years(.Object@exp_len)
    } else if(.Object@study_type == "backward") {
        exp_end_date <- .Object@obs_end_date %m-% lubridate::years(.Object@obs_len + .Object@wash_len)
    }
    return(exp_end_date)
}

#' Calcualtes the end date of the exposure period for each individual
#' 
#' Calcualtes the end date of the exposure period for each individual,
#' depending on the selected study setup.
#' 
#' @inheritParams set_study_data_dates
#' 
#' @return A vector of Dates. The end of the observation period for
#' each individual.
#' 
#' @importFrom lubridate %m+%
#' @importFrom lubridate %m+%
#' 
#' @author Kira E. Detrois
calc_exp_start_date <- function(.Object) {
    if(.Object@study_type == "forward") {
        exp_start_date <- .Object@study_data$DATE_OF_BIRTH %m+% lubridate::years(.Object@exp_age)
    } else if(.Object@study_type == "backward") {
        if(!is.na(.Object@exp_age)) {
            exp_start_date <- .Object@study_data$DATE_OF_BIRTH
        } else {
            exp_start_date <- .Object@obs_end_date %m-% lubridate::years(.Object@obs_len + .Object@wash_len + .Object@exp_len) 
        }
    }
    return(exp_start_date)
}
