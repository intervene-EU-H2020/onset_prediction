#' Writes model results to a tab-delim file
#'  
#' @inheritParams add_coxph_res_row
#' @inheritParams add_cidx_res_row
#' @inheritParams add_risk_group_col
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res_files <- function(endpt_hrs_tib,
                            endpt_c_idxs_tib,
                            surv_ana) {
    file_path_coxph <- check_and_get_file_path(surv_ana,
                                               res_type="coxph")
    file_path_cidx <- stringr::str_replace(string=file_path_coxph,
                                           pattern="coxph",
                                           replacement="cidx")
    if(!is.null(file_path_coxph)) {
        readr::write_delim(x=endpt_hrs_tib, 
                           file=file_path_coxph, 
                           delim="\t")
        readr::write_delim(x=endpt_c_idxs_tib,
                           file=file_path_cidx,
                           delim="\t")
    }
}

#' Write score cut-off values and groups to log
#' 
#' @inheritParams get_risk_group_labs
#' @inheritParams add_risk_group_col
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_score_groups_to_log <- function(score_group_tbl,
                                      surv_ana) {
    file_path <- check_and_get_file_path(surv_ana,
                                         res_type="log")
    if(!is.null(file_path)) {
        readr::write_delim(log_msg_table(score_group_tbl), 
                           file=file_path, 
                           delim=" ",
                           col_names=TRUE)
    }
}

#' Creates a string of the current study setup
#' 
#' @inheritParams get_risk_group_labs
#' 
#' @return A tibble with columns `GROUP`, `SCORE_CUT`
#' 
#' @export 
#' 
#' @author Kira E. Detrois
log_msg_table <- function(score_group_tbl) {
    score_tib <- tibble::tibble(GROUP=names(score_group_tbl), 
                                SCORE_CUT=score_group_tbl)

    return(score_tib)
}