#' Writes model results to a tab-delim file
#'  
#' @inheritParams add_coxph_res_row
#' @inheritParams calc_studies_hrs
#' @inheritParams add_risk_group_col
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res_file <- function(coxph_res_tib,
                           score_type,
                           bin_cut,
                           study,
                           write_res,
                           res_dir) {
    file_path <- check_and_get_file_path(score_type,
                                         study,
                                         write_res,
                                         res_dir,
                                         res_type="coxph",
                                         bin_cut=bin_cut)
        if(!is.na(file_path)) {
            readr::write_delim(coxph_res_tib, 
                               file=file_path, 
                               delim="\t")
        }
}

#' Creats the file directory and name for the different results
#' 
#' @inheritParams add_risk_group_col
#' @param res_type A character. The results type.
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
check_and_get_file_path <- function(score_type,
                                    study,
                                    write_res,
                                    res_dir,
                                    res_type,
                                    bin_cut=1) {

    if(write_res) {
        # Score results specific folder
        res_dir <- paste0(res_dir, score_type, "/")
        # Results type specific folder
        type_dir <- dplyr::case_when(
            res_type == "endpt" ~ "plots/endpts/",
            res_type == "distr" ~ "plots/",
            res_type == "coxph" ~ "coxph_res/",
            res_type == "log" ~ "log/",
            res_type == "HRs" ~ "plots/HRs/", 
        )
        res_dir <- paste0(res_dir, type_dir)
        # Make the folder if it doesn't exist yet
        if(Istudy::check_res_dir(write_res, res_dir)) {
            file_name <- dplyr::case_when(
             res_type == "endpt" ~ get_endpt_score_file_name(study,
                                                             score_type),
             res_type == "distr" ~ get_score_distr_file_name(study,
                                                             score_type),
             res_type == "coxph" ~ get_coxph_res_file_name(study, 
                                                           score_type,
                                                           bin_cut),
             res_type == "log" ~  get_score_cut_file_name(study,
                                                          score_type),
             res_type == "HRs" ~ get_hr_file_name(study,
                                                  score_type,
                                                  bin_cut)
            )
            file_path <- paste0(res_dir, file_name)
            return(file_path)
        } 
    }
    return(NA_character_)
}

#' Creats the file name for the score distribution plots
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
get_score_distr_file_name <- function(study,
                                      score_type) {
    plot_descr <- paste0(study@exp_age, "_to_", study@exp_age+study@exp_len)
    paste0(score_type, "_score_distr_", plot_descr, ".png")
}

#' Creats the file name for the HR plots
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
get_hr_file_name <- function(study,
                             score_type,
                             bin_cut=1) {
    file_name <- paste0(study@endpt, "_", study@exp_len, "_", study@wash_len, "_", study@obs_len, "_", score_type)
    if(score_type == "CCI") {
        file_name <- paste0(file_name, "_cut", bin_cut)
    } 
    paste0(file_name, "_HRs.png")
}

#' Creats the file name for the score cut log file
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
get_score_cut_file_name <- function(study,
                                    score_type) {
    paste0(Istudy::get_study_file_name(study), "_", score_type, "_cut_log.txt")
}

#' Creats the file name for endpoint specific score distribution plot
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
get_endpt_score_file_name <- function(study,
                                      score_type) {
    paste0(Istudy::get_study_file_name(study), "_", score_type, "_score.png")
}

#' Creats the file name for Cox-PH model results file
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
get_coxph_res_file_name <- function(study,
                              score_type,
                              bin_cut) {
    study <- Istudy::setEndpt(study, "study") # Cheating the system for the file name
    file_name <- paste0(Istudy::get_study_file_name(study), "_", score_type)
    if(score_type != "CCI") {
        file_name <- paste0(file_name, "_coxph.tsv")
    } else {
        file_name <- paste0(file_name, "g", bin_cut, "_coxph.tsv")
    }
}

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
    file_path <- check_and_get_file_path(score_type,
                                         study,
                                         write_res,
                                         res_dir,
                                         res_type="log")
    if(!is.na(file_path)) {
        readr::write_delim(log_msg_table(score_group_tbl), 
                           file=file_path, 
                           delim=" ",
                           col_names=TRUE)
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