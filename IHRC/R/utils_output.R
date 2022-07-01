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
                           score_type,
                           study,
                           bin_cut,
                           write_res,
                           res_dir) {
    if(Istudy::check_res_dir(write_res, res_dir)) {
        coxph_res_dir <- paste0(res_dir, "coxph_res/")

        if(!dir.exists(coxph_res_dir)) {
                dir.create(coxph_res_dir)
        }
        study <- Istudy::setEndpt(study, "study") # Cheating the system for the file name
        file_name <- paste0(Istudy::get_study_file_name(study), "_", score_type)
        if(score_type != "CCI") {
            file_name <- paste0(file_name, "_coxph.tsv")
        } else {
            file_name <- paste0(file_name, "g", bin_cut, "_coxph.tsv")
        }
        file_path <- paste0(coxph_res_dir, file_name)
        readr::write_delim(coxph_res_tib, 
                           file=file_path, 
                           delim="\t")
    }}

#' Write score cut-off values and groups to log
#' 
#'@inheritParams get_group_labs
#' @inheritParams add_risk_group_col
#' @inheritParams calc_studies_hrs
#' 
#' @author Kira E. Detrois
write_score_groups_to_log <- function(score_group_tbl,
                                      score_type,
                                      study,
                                      write_res,
                                      res_dir) {
    if(Istudy::check_res_dir(write_res, res_dir)) {
        log_res_dir <- paste0(res_dir, "log/")
        if(!dir.exists(log_res_dir)) {
            dir.create(log_res_dir)
        } 
        file_name <- paste0(Istudy::get_study_file_name(study), "_", score_type, "_cut_log.txt")
        
        file_path <- paste0(log_res_dir, file_name)
        readr::write_delim(log_msg_table(score_group_tbl), 
                           col_names=TRUE,
                           delim=" ",
                           file_path)
    }
}

#' Creates a string of the current study setup
#' 
#' @inheritParams get_group_labs
#' 
#' @return A tibble with columns `GROUP`, `SCORE_CUT`
#' 
#' @author Kira E. Detrois
log_msg_table <- function(score_group_tbl) {
    score_tib <- tibble::tibble(GROUP=names(score_group_tbl), 
                                SCORE_CUT=score_group_tbl)

    return(score_tib)
}