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
                            surv_ana) {
    file_path_coxph <- check_and_get_file_path(surv_ana=surv_ana,
                                               res_type="coxph")
    file_path_cidx <- check_and_get_file_path(surv_ana=surv_ana,
                                              res_type="cidx")

    if(!is.null(file_path_coxph)) {
        readr::write_delim(x=endpt_hrs_tib, 
                           file=file_path_coxph, 
                           delim="\t")
        readr::write_delim(x=endpt_c_idxs_tib,
                           file=file_path_cidx,
                           delim="\t")
    }
}