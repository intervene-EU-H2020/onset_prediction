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
                          write_surv_res=FALSE,
                          write_study_res=FALSE,
                          write_study_log=NA,
                          res_dir=NA) {

    score_data <- preprocess_score_data(score_data, 
                                        score_col_name)
    
    all_surv_res <- create_empty_surv_res_tib()       

    for(endpt in endpts) {
        log_file_path <- paste0(res_dir, 
                                get_study_file_name(as.list(environment())))
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
            coxph_res <- run_surv_ana(elig_endpt_indv, endpt)
            all_surv_res <- add_surv_row(all_surv_res,
                                         coxph_res,
                                         score_type,
                                         endpt,
                                         elig_endpt_indv)
        } else {
            message(paste0("Not enough cases for endpoint: ", endpt, " No of cases: ", Istudysetup::get_n_cases(elig_endpt_indv, endpt)))
        }
    }
    write_res(as.list(environment()))
    print(all_surv_res)
    return(all_surv_res)
}

#' Writes results to a tab-delim file
#' 
#' This should be called after `create_return_df`. 
#' 
#' @param envir A list with at least entries `write_res`, `res_dir` ,
#'              `elig_data`, `endpt`, `exp_age`, `exp_len`, `wash_len`, 
#'              and `obs_len`.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res <- function(envir) {
    if(envir$write_surv_res) {
        if(is.na(envir$res_dir)) {
            message("Variable write_res was set to TRUE but res_dir was not provided. Cannot write results to file.")
        } else {
            if(!dir.exists(envir$res_dir)) {
                message(paste0("The file directory ", envir$res_dir, " does not exist. Trying to create it."))
                dir.create(envir$res_dir, recursive=TRUE)
            } 
            envir$endpt <- "study" # Cheating the system for the file name
            res_file_name <- paste0(get_study_file_name(envir), 
                                    "_surv_res.tsv")
            readr::write_delim(envir$all_surv_res, 
                               paste0(envir$res_dir, res_file_name),
                               delim="\t")
            }

        }
}


preprocess_score_data <- function(score_data, 
                                  score_col_name) {
    assertthat::assert_that(score_col_name %in% colnames(score_data),
                            msg=paste0("The score_col_name ", score_col_name, " you gave is not a known column in the score_data data.frame. Have column names: ", paste0(colnames(score_data), collapse=", ")))
    dplyr::rename(score_data, "SCORE"={{ score_col_name }})
}

add_surv_row <- function(all_surv_res,
                          coxph_res,
                          score_type,
                          endpt,
                          elig_endpt_indv) {

    n_cases <- Istudysetup::get_n_cases(elig_endpt_indv, endpt)
    n_ctrls <- Istudysetup::get_n_ctrls(elig_endpt_indv, endpt)

    tibble::add_row(all_surv_res, 
                    Endpoint=endpt,
                    Score=score_type,
                    Group="all",
                    N_controls=n_ctrls,
                    N_cases=n_cases,
                    beta=coxph_res$BETAS,
                    std_errs=coxph_res$STD_ERRS,
                    p_val=coxph_res$PVALS,
                    HR=coxph_res$HR,
                    CI_pos=coxph_res$CI["pos"],
                    CI_neg=coxph_res$CI["neg"]
    )
}

run_surv_ana <- function(data,
                          endpt) {
    cox_formula <- as.formula(paste0("survival::Surv(", endpt, "_AGE_DAYS,", endpt, ") ~ SCORE"))
    coxph_res <- survival::coxph(cox_formula,
                                 data=data)
    coxph_res <- extract_surv_res(coxph_res)
    return(coxph_res)
}

create_empty_surv_res_tib <- function() {
    tibble::tibble(Endpoint=character(),
                   Score=character(),
                   Group=character(),
                   N_controls=numeric(),
                   N_cases=numeric(),
                   beta=numeric(),
                   std_errs=numeric(),
                   p_val=numeric(),
                   HR=numeric(),
                   CI_pos=numeric(),
                   CI_neg=numeric())
}