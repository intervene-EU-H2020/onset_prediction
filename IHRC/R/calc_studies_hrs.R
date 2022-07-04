#' Calcualtes HR from a Cox-PH model for each endpoint
#' 
#' @param pheno_data A data.frame with at least the columns: 
#'                   `ID`, the columns specified in `covs` and 
#'                   i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE` where the 
#'                   columns are the study endpoint and date, which will 
#'                   differ depending on the input variable `endpts`.
#' @param score_data A data.frame. Needs at least column `SCORE` if 
#'                      `score_type = CCI`, or i.e. `J10_ASTHMA_PRS` for 
#'                      `score_type = PRS` depending on the chosen `endpts`.
#' @param score_type A character. The name of the score used for the model,
#'                                i.e. CCI, or PheRS.
#' @param studies A vector of S4 classes representing the study setups.
#' @param covs A character. The column names of the covariates to add to
#'              the predictor of the Cox-PH model.
#' @param bin_cut A numeric. The binary cutoff value for classifying high
#'                  and low score individuals. Currently only in use if
#'                  the `score_type` is `CCI`.
#' @param write_res A boolean. Defines whether to save the results to 
#'                             files.
#' @param res_dir A character. The directory to write the results and
#'                             log to.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_studies_hrs <- function(pheno_data, 
                             score_data,
                             score_type,
                             studies,
                             covs=c("SEX", "YEAR_OF_BIRTH"),
                             bin_cut=1,
                             write_res=FALSE,
                             res_dir=NA) {

    coxph_res_tib <- create_empty_coxph_res_tib()       
    for(study in studies) {
        plt <- plot_score_distr(score_data, 
                                score_type, 
                                study, 
                                write_res, 
                                res_dir)

        elig_indv <- Istudy::get_study_elig_indv(pheno_data,
                                                 study,
                                                 write_res,
                                                 res_dir)

        if(Istudy::get_n_cases(elig_indv, study@endpt) > 100) {
            pheno_score_data <- join_dfs(elig_indv, 
                                         score_data,
                                         score_type,
                                         study@endpt)
            plt <- plot_endpt_score_distr(pheno_score_data,
                                          score_type,
                                          study,
                                          write_res,
                                          res_dir)
            pheno_score_data <- add_risk_group_col(pheno_score_data,
                                                   score_type,
                                                   study,
                                                   bin_cut,
                                                   write_res,
                                                   res_dir)
            coxph_mdl <- run_coxph_ana(pheno_score_data,
                                       study@endpt,
                                       covs)
                            
            coxph_res_tib <- add_coxph_row(coxph_res_tib,
                                           coxph_mdl,
                                           score_type,
                                           study@endpt,
                                           pheno_score_data)
        } else {
            message(paste0("Not enough cases for endpoint: ", study@endpt, " No of cases: ", Istudy::get_n_cases(elig_indv, study@endpt)))
        }
    }
    write_res_file(coxph_res_tib,
                   score_type,
                   study,
                   bin_cut,
                   write_res,
                   res_dir)

    return(coxph_res_tib)
}
