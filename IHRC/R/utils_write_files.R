#' Writes model results to a tab-delim file
#'  
#' @inheritParams add_coxph_res_row
#' @inheritParams add_cidx_res_row
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res_files <- function(endpt_hrs_tib,
                            endpt_c_idxs_tib,
                            ana_details) {
    file_path_coxph <- check_and_get_file_path(ana_details=ana_details,
                                               res_type="coxph")
    file_path_cidx <- check_and_get_file_path(ana_details=ana_details,
                                              res_type="cidx")

    if(!is.null(file_path_coxph)) {
        endpt_hrs_tib <- dplyr::filter(endpt_hrs_tib, 
                                      !stringr::str_detect("[BATCH|PC]", endpt_hrs_tib$VAR))
        readr::write_delim(x=endpt_hrs_tib, 
                           file=file_path_coxph, 
                           delim="\t")
        readr::write_delim(x=endpt_c_idxs_tib,
                           file=file_path_cidx,
                           delim="\t")
    }
}

#' Creates a list if the details from the survival analysis
#' 
#' For plotting and file saving purposes, so that one can easily use
#' the same functions also without creating a surv_ana object.
get_ana_details_from_surv_ana <- function(surv_ana) {
    ana_details <- list()
    ana_details$study_type <- surv_ana@study@study_type
    ana_details$endpt <- surv_ana@study@endpt
    ana_details$exp_len <- surv_ana@study@exp_len
    ana_details$wash_len <- surv_ana@study@wash_len
    ana_details$obs_len <- surv_ana@study@obs_len
    ana_details$obs_end_date <- surv_ana@study@obs_end_date
    ana_details$preds <- surv_ana@preds
    ana_details$plot_preds <- surv_ana@plot_preds
    ana_details$write_res <- surv_ana@write_res
    ana_details$res_dir <- surv_ana@res_dir
    return(ana_details)
}