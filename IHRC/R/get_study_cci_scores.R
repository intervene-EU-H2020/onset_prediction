#' Calcualtes the CCI score on the exposure window
#' 
#' @param elig_indv A data.frame. The individuals which were eligble under
#'                        the current study setup. Needs to at least 
#'                        contain the column defined in `ID`, and 
#'                        `EXP_LEN` or `EXP_END`.
#' @param icd_data A data.frame with at least columns `ID`, `Event_age`, 
#'                  and `PRIMARY_ICD`.
#' 
#' @return A data.frame with columns `ID`, and `SCORE`.
#' 
#' @author Kira E. Detrois
get_study_cci_scores <- function(elig_indv,
                                 icd_data) {
    if("EXP_LEN" %in% colnames(elig_indv)) {
        elig_indv <- dplyr::rename(elig_indv, 
                                   EXP_END=EXP_LEN)
    } 
    cci_data <- ICCI::calc_cci(icd_data,
                               exp_start=0,
                               exp_end=dplyr::select(elig_indv, ID, EXP_END)) %>%
                    dplyr::rename(SCORE=CCI_score)
    return(cci_data)
}