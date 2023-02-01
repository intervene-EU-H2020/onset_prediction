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
                            surv_ana) {
    file_path_coxph <- get_full_file_name_path(res_type="coxph",
                                               study_setup=study_setup,
                                               surv_ana=surv_ana)
    file_path_cidx <- get_full_file_name_path(res_type="cidx",
                                              study_setup=study_setup,
                                              surv_ana=surv_ana)

    if(!is.null(file_path_coxph)) {
        hr_res <- dplyr::filter(hr_res, !stringr::str_detect(VAR, "PC"))
        hr_res <- dplyr::filter(hr_res, !stringr::str_detect(VAR, "BATCH"))
        readr::write_delim(x=hr_res, 
                           file=file_path_coxph, 
                           delim="\t")
        readr::write_delim(x=c_idxs_res,
                           file=file_path_cidx,
                           delim="\t")
    }
}