#' Writes model results to a tab-delim file
#'  
#' @inheritParams add_coxph_row
#' @inheritParams calc_studies_hrs
#' @inheritParams add_risk_group_col
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res_file <- function(coxph_res_tib,
                           study,
                           write_res,
                           res_dir) {
    if(Istudy::check_res_dir(write_res, res_dir)) {
        coxph_res_dir <- paste0(res_dir, "coxph_res/")

        if(!dir.exists(coxph_res_dir)) {
                dir.create(coxph_res_dir)
        }
        study <- Istudy::setEndpt(study, "study") # Cheating the system for the file name
        res_file_name <- paste0(Istudy::get_study_file_name(study), "_coxph_res.tsv")
        readr::write_delim(coxph_res_tib, 
                           file=paste0(coxph_res_dir, res_file_name), delim="\t")
    }}

#' Write score cut-off values and groups to log
#' 
#' @inheritParams check_has_mid_group
#' @inheritParams add_risk_group_col
#' @inheritParams calc_studies_hrs
#' 
#' @author Kira E. Detrois
write_score_groups_to_log <- function(score_group_tbl,
                                      study,
                                      write_res,
                                      res_dir) {
    if(Istudy::check_res_dir(write_res, res_dir)) {
        log_res_dir <- paste0(res_dir, "log/")
        if(!dir.exists(log_res_dir)) {
            dir.create(log_res_dir)
        } 
        file_path <- paste0(log_res_dir, Istudy::get_study_file_name(study), "_score_log.txt")
        readr::write_delim(log_msg_table(score_group_tbl), 
                           append=check_file_exists(file_path),
                           col_names=TRUE,
                           delim=" ",
                           file_path)
    }
}

#' Creates a string of the current study setup
#' 
#' @inheritParams check_has_mid_group
#' 
#' @return A tibble with columns `GROUP`, `SCORE_CUT`
#' 
#' @author Kira E. Detrois
log_msg_table <- function(score_group_tbl) {
    score_tib <- tibble::tibble(GROUP=names(score_group_tbl), 
                                SCORE_CUT=score_group_tbl)

    return(score_tib)
}


#' Checks whether the log file already exists
#' 
#' @param file_path A character. The path to the file.
#' 
#' @return A boolean. Whether the file exists.
#' 
#' @author Kira E. Detrois
check_file_exists <- function(file_path) {
    if(file.exists(file_path)) {
        append_mode <- TRUE
    } else {
        append_mode <- FALSE
    }
}