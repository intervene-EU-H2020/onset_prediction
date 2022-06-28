#' Writes model results to a tab-delim file
#'  
#' @export 
#' 
#' @author Kira E. Detrois
write_res_file <- function(coxph_res,
                           study,
                           write_res,
                           res_dir) {
    if(Istudy::check_res_dir(write_res, res_dir)) {
        Istudy::setEndpt(study, "study") # Cheating the system for the file name
        res_file_name <- paste0(Istudy::get_study_file_name(study), 
                                "_coxph_res.tsv")
        readr::write_delim(coxph_res, 
                           paste0(res_dir, res_file_name), delim="\t")
    }
}

#' Write score cut-off values and groups to log
#' 
#'  
#' @author Kira E. Detrois
write_score_groups_to_log <- function(score_group_tbl,
                                      study,
                                      write_res,
                                      res_dir) {
    if(Istudy::check_res_dir(write_res, res_dir)) {
        file_path <- paste0(res_dir, Istudy::get_study_file_name(study), "_log.txt")
        readr::write_delim(log_msg_table(score_group_tbl), 
                           append=check_file_exists(file_path),
                           col_names=TRUE,
                           delim=" ",
                           file_path)
    }
}

#' Creates a string of the current study setup
#' 
#' Only used inside `write_log` function.
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
