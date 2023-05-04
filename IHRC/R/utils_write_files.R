#' Writes model results to a tab-delim file
#'  
#' @param hr_res A tibble containing the hazard ratios for each exposure age.
#' @param c_idxs_res A data.frame. The c-index results.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res_files <- function(hr_res,
                            c_idxs_res,
                            study_setup,
                            surv_ana,
                            score_types,
                            is_first_endpt) {

}