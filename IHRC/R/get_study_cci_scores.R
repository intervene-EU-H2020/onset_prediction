#' Calcualtes the CCI score on the exposure window
#' 
#' @param elig_indv A data.frame. The individuals which were eligble under
#'                        the current study setup. Needs to at least 
#'                        contain the column defined in `ID`, `DATE_OF_BIRTH`,
#'                        and `EXP_END_PERIOD`.
#' @param icd_data A data.frame with at least columns `ID`, `Event_age`, 
#'                  and `PRIMARY_ICD`.
#' 
#' @return A data.frame with columns `ID`, and `SCORE`.
#' 
#' @author Kira E. Detrois
get_study_CCI_data <- function(elig_indv,
                               icd_data) {
    cci_data <- ICCI::calc_cci(icd_data,
                               exp_start=calc_exp_start_age(elig_indv),
                               exp_end=calc_exp_end_age(elig_indv)) 
    return(cci_data)
}

#' @importFrom lubridate %--%
calc_exp_end_age <- function(elig_indv) {
    elig_indv <- dplyr::mutate(elig_indv, 
                               EXP_END = lubridate::time_length(DATE_OF_BIRTH %--% EXP_END_DATE, "years"))
    return(dplyr::select(elig_indv, ID, EXP_END))
}

#' @importFrom lubridate %--%
calc_exp_start_age <- function(elig_indv) {
    elig_indv <- dplyr::mutate(elig_indv, 
                               EXP_START = lubridate::time_length(DATE_OF_BIRTH %--% EXP_START_DATE, "years"))
    return(dplyr::select(elig_indv, ID, EXP_START))
}