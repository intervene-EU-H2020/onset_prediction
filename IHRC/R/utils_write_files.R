#' Writes model results to a tab-delim file
#'  
#' @inheritParams add_coxph_res_row
#' @inheritParams calc_endpt_studies_hrs
#' @inheritParams add_risk_group_col
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res_files <- function(endpt_hrs_tib,
                            endpt_cidx_tib,
                            score_type,
                            endpt_studies,
                            bin_cut,
                            write_res,
                            res_dir) {
    file_path_coxph <- check_and_get_file_path(score_type,
                                               endpt_studies[[1]],
                                               write_res,
                                               res_dir,
                                               res_type="coxph",
                                               bin_cut=bin_cut)
    file_path_cidx <- stringr::str_replace(string=file_path_coxph,
                                           pattern="coxph",
                                           replacement="cidx")
    if(!is.null(file_path_coxph)) {
        readr::write_delim(x=endpt_hrs_tib, 
                           file=file_path_coxph, 
                           delim="\t")
        readr::write_delim(x=endpt_cidx_tib,
                           file=file_path_cidx,
                           delim="\t")
    }
}

#' Write score cut-off values and groups to log
#' 
#'@inheritParams get_risk_group_labs
#' @inheritParams add_risk_group_col
#' @inheritParams calc_endpt_studies_hrs
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_score_groups_to_log <- function(score_group_tbl,
                                      score_type,
                                      study,
                                      write_res,
                                      res_dir) {
    file_path <- check_and_get_file_path(score_type,
                                         study,
                                         write_res,
                                         res_dir,
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