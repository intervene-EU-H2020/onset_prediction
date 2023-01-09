#' Calcualtes the MI score on the exposure window
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
get_study_med_data <- function(elig_indv,
                               atc_data) {
    med_data <- ICCI::calc_med(atc_data,
                               exp_start=calc_exp_start_age(elig_indv),
                               exp_end=calc_exp_end_age(elig_indv)) 
    return(med_data)
}