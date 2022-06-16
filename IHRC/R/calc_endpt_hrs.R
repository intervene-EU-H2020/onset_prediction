#'@export 
calc_endpt_hr <- function(pheno_data, 
                          score_data,
                          score_col_name,
                          score_type,
                          exp_age=30,
                          exp_len=10,
                          wash_len=2,
                          obs_len=8,
                          endpts,
                          downsample_fctr=NA,
                          write_coxph_res=FALSE,
                          write_study_res=FALSE,
                          write_study_log=NA,
                          res_dir=NA) {

    score_data <- preprocess_score_data(score_data, 
                                        score_col_name)
    
    all_coxph_res <- create_empty_coxph_res_tib()       

    for(endpt in endpts) {
        elig_endpt_indv <- Istudysetup::get_study_elig_indv(
                                    pheno_data,
                                    endpt,
                                    exp_age,
                                    exp_len,
                                    wash_len,
                                    obs_len,
                                    downsample_fctr,
                                    write_res=write_study_res,
                                    res_dir=res_dir,
                                    write_log=ifelse(write_study_log, "file", NA),
                                    log_dir=res_dir)$data

        if(Istudysetup::get_n_cases(elig_endpt_indv, endpt) > 2) {
            elig_endpt_indv <- dplyr::left_join(elig_endpt_indv,
                                                score_data,
                                                by="ID")
            coxph_res <- run_coxph_ana(elig_endpt_indv, 
                                      endpt)
            all_coxph_res <- add_coxph_row(all_coxph_res,
                                         coxph_res,
                                         score_type,
                                         endpt,
                                         elig_endpt_indv)
        } else {
            message(paste0("Not enough cases for endpoint: ", endpt, " No of cases: ", Istudysetup::get_n_cases(elig_endpt_indv, endpt)))
        }
    }
    write_coxph_res(as.list(environment()))
    return(all_coxph_res)
}