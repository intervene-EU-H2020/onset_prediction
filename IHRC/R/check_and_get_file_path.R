#' Creats the file directory and name for the different result types
#' 
#' @param res_type A character. The results type.
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
check_and_get_file_path <- function(surv_ana,
                                    res_type) {
    if(surv_ana@write_res) {
        # Results type specific folder
        type_dir <- dplyr::case_when(
            res_type == "endpt" ~ paste0("score_distr/endpts/"),
            res_type == "distr" ~ paste0( "score_distr/"),
            res_type == "log" ~ paste0("PRS_logs/score_cut/"),
            res_type == "HR RG" ~ "HRs/", 
            res_type == "HR SD" ~ "HRs/", 
            res_type == "surv" ~ "surv/",
            TRUE ~ ""
        )
        curnt_res_dir <- paste0(surv_ana@res_dir, type_dir)
        # Make the folder if it doesn't exist yet
        if(Istudy::check_res_dir(surv_ana@write_res, curnt_res_dir)) {
            file_name <- dplyr::case_when(
              res_type == "endpt" ~ get_endpt_score_file_name(surv_ana),
              res_type == "distr" ~ get_score_distr_file_name(surv_ana),
              res_type == "coxph" ~ get_coxph_res_file_name(surv_ana),
              res_type == "cidx" ~ get_cidx_res_file_name(surv_ana),
              res_type == "HR RG" ~ get_hr_rg_file_name(surv_ana),
              res_type == "HR SD" ~ get_hr_sd_file_name(surv_ana), 
              res_type == "surv" ~ get_surv_file_name(surv_ana),
              res_type == "log" ~ get_score_cut_file_name(surv_ana))
            
            file_path <- paste0(curnt_res_dir, file_name)

            return(file_path)
        } 
    }
    return(NA_character_)
}