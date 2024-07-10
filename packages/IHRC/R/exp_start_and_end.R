
#' Calcualtes the age of individuals at exposure start
#' 
#' Calculates the length of the exposure period in (exact) years
#' from birth until the start date of the exposure period.
#'
#' @param elig_indv A data.frame of individuals with columns 
#'                   `ID`, `DATE_OF_BIRTH`, and `EXP_START_DATE`.
#' 
#' @return tibble with columns `ID` and `EXP_START`,
#'           which is the ID of the individual and the corresponding 
#'           calculated exposure start date.
#' 
#' @import dplyr
#' @importFrom lubridate %--%
#' 
#' @author Kira E. Detrois
#' 
#' @export
calc_exp_start_age <- function(elig_indv) {
    elig_indv <- dplyr::mutate(elig_indv, 
                               EXP_START = lubridate::time_length(DATE_OF_BIRTH %--% EXP_START_DATE, "years"))
    return(dplyr::select(elig_indv, ID, EXP_START))
}

#' Calcualtes the age of individuals at exposure end
#' 
#' Calculates the length of the exposure period in years
#' from birth until the end date of the exposure period.
#'
#' @param elig_indv A data.frame of individuals with columns 
#'                   `ID`, `DATE_OF_BIRTH`, and `EXP_END_DATE`.
#' 
#' @return tibble with columns `ID` and `EXP_END`,
#'           which is the ID of the individual and the corresponding 
#'           calculated exposure end date.
#' 
#' @export
#' 
#' @import dplyr
#' @importFrom lubridate %--%
#' 
#' @author Kira E. Detrois
calc_exp_end_age <- function(elig_indv) {
    elig_indv <- dplyr::mutate(elig_indv, 
                               EXP_END = lubridate::time_length(DATE_OF_BIRTH %--% EXP_END_DATE, "years"))
    return(dplyr::select(elig_indv, ID, EXP_END))
}

