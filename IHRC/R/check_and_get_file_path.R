#' Creats the file directory and name for the different result types
#' 
#' @inheritParams add_risk_group_col
#' @param res_type A character. The results type.
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
check_and_get_file_path <- function(surv_ana,
                                    res_type) {
    if(surv_ana@write_res) {
        dir_name <- ""
        if(!is.null(surv_ana@covs)) {
            dir_name <- paste0(get_pretty_covs_string(surv_ana@covs, file_name=TRUE), "/")
        } else if(res_type %in% c("HRs", "coxph", "surv")) {
            message("Could not make correct directory because covariates not given")
        }

        # Results type specific folder
        type_dir <- dplyr::case_when(
            res_type == "endpt" ~ "score_distr/endpts/",
            res_type == "distr" ~ "score_distr/",
            res_type == "coxph" ~ dir_name,
            res_type == "log" ~ "log/",
            res_type == "HRs" ~ paste0(dir_name, "HRs/"), 
            res_type == "surv" ~ paste0(dir_name, "surv/"),
        )
        curnt_res_dir <- paste0(surv_ana@res_dir, type_dir)
        # Make the folder if it doesn't exist yet
        if(Istudy::check_res_dir(surv_ana@write_res, curnt_res_dir)) {
            file_name <- dplyr::case_when(
             res_type == "endpt" ~ get_endpt_score_file_name(surv_ana),
             res_type == "distr" ~ get_score_distr_file_name(surv_ana),
             res_type == "coxph" ~ get_coxph_res_file_name(surv_ana),
             res_type == "log" ~  get_score_cut_file_name(surv_ana),
             res_type == "HRs" ~ get_hr_file_name(surv_ana),
             res_type == "surv" ~ get_surv_file_name(surv_ana)
            )
            file_path <- paste0(curnt_res_dir, file_name)
            return(file_path)
        } 
    }
    return(NA_character_)
}