calc_endpt_hr <- function(pheno_data, 
                          score_data,
                          exp_age=30,
                          exp_len=10,
                          wash_len=2,
                          obs_len=8,
                          endpts,
                          downsample_fctr=NA) {
    
    all_coxph_res <- tibble::tibble(ENDPOINT=character(),
                                    BETAS=numeric(),
                                    STD_ERRS=numeric(),
                                    PVALS=numeric(),
                                    HR=numeric(),
                                    CI_NEG=numeric(),
                                    CI_POS=numeric())
                                    
    for(endpt in endpts) {
        endpt_study <- Istudysetup::get_study_elig_indv(pheno_data,
                                                        endpt,
                                                        exp_age,
                                                        exp_len,
                                                        wash_len,
                                                        obs_len,
                                                        downsample_fctr)
        elig_endpt_indv <- endpt_study$data
        elig_endpt_indv <- dplyr::left_join(elig_endpt_indv,
                                            score_data,
                                            by="ID")
        coxph_res <- run_coxph_ana(endpt,
                                   data=elig_endpt_indv)
        all_coxph_res <- add_coxph_row(all_coxph_res,
                                       coxph_res,
                                       endpt)
    }

    return(all_coxph_res)
}

add_coxph_row <- function(all_coxph_res,
                          coxph_res,
                          endpt) {

    tibble::add_row(all_coxph_res, 
                    ENDPOINT=endpt,
                    BETAS=coxph_res$BETAS,
                    STD_ERRS=coxph_res$STD_ERRS,
                    PVALS=coxph_res$PVALS,
                    HR=coxph_res$HR,
                    CI_NEG=coxph_res$CI["neg"],
                    CI_POS=coxph_res$CI["pos"])
}

run_coxph_ana <- function(data,
                          endpt) {
    cox_formula <- as.formula(paste0("survival::Surv(", endpt, "_AGE_DAYS,", endpt, ") ~ score"))
    coxph_res <- survival::coxph(cox_formula,
                                data=data)
    coxph_res <- extract_coxph_res(coxph_res)
    return(coxph_res)
}