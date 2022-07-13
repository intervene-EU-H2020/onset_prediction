#' Creats the file directory and name for the different result types
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
                                    covs=NULL,
                                    bin_cut=1) {
    if(write_res) {
        dir_name <- ""
        if(!is.null(covs)) {
            dir_name <- paste0(get_pretty_covs_string(covs, file_name=TRUE), "/")
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
                                                  bin_cut),
             res_type == "surv" ~ get_surv_file_name(study, 
                                                     score_type)
            )
            file_path <- paste0(res_dir, file_name)
            return(file_path)
        } 
    }
    return(NA_character_)
}