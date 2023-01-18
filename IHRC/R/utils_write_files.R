#' Writes model results to a tab-delim file
#'  
#' @inheritParams add_coxph_res_row
#' @inheritParams add_cidx_res_row
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res_files <- function(endpt_hr_res,
                            endpt_c_idxs_tib,
                            study,
                            surv_ana) {
    file_path_coxph <- check_and_get_file_path(res_type="coxph",
                                               study_setup=study@study_setup,
                                               endpt=study@endpt,
                                               surv_ana=surv_ana)
    file_path_cidx <- check_and_get_file_path(res_type="cidx",
                                              study_setup=study@study_setup,
                                              endpt=study@endpt,
                                              surv_ana=surv_ana)

    if(!is.null(file_path_coxph)) {
        endpt_hr_res <- dplyr::filter(endpt_hr_res, !stringr::str_detect(VAR, "PC"))
        endpt_hr_res <- dplyr::filter(endpt_hr_res, !stringr::str_detect(VAR, "BATCH"))
        readr::write_delim(x=endpt_hr_res, 
                           file=file_path_coxph, 
                           delim="\t")
        readr::write_delim(x=endpt_c_idxs_tib,
                           file=file_path_cidx,
                           delim="\t")
    }
}