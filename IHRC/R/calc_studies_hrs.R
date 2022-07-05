#' Calcualtes HR from a Cox-PH model for each endpoint
#' 
#' Fits a Cox-proportional hazards model for the different studies. 
#' 
#' Selects individuals from the data that are eligible given each of the 
#' studies. For more details see package `Istudy` and function
#' \link[Istudy]{get_study_elig_indv}. Runs the analyis based on risk 
#' groups. For `PRS` these are based on the quantiles, where the 40-60% 
#' group is the reference group. For `CCI` compares two groups based on a 
#' cutoff defined in `bin_cut`.
#' 
#' For i.e. endpoint `J10_ASTHMA` and `covs = c("SEX", "YEAR_OF_BIRTH")` 
#' the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE_GROUP + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @param pheno_data A data.frame with at least the columns: 
#'                   `ID`, the columns specified in `covs` and 
#'                   i.e. `J10_ASTHMA`, and `J10_ASTHMA_AGE_DAYS` where 
#'                   the columns are the study endpoint and date, which 
#'                   will differ depending on the chosen `endpts`.
#' @param score_data A data.frame. If `score_type = CCI` needs at least 
#'                   column `SCORE`. If `score_type = PRS` columns of 
#'                   form`J10_ASTHMA_PRS` depending on the chosen `endpts`.
#' @param score_type A character. The name of the score used for the model,
#'                                i.e. CCI, or PheRS.
#' @param studies A vector of S4 classes representing the study setups.
#' @param covs A vector of characters. The column names of the covariates 
#'              to add to the predictor of the Cox-PH model.
#' @param bin_cut A numeric. The binary cutoff value for classifying high
#'                  and low score individuals. Currently only in use if
#'                  the `score_type == CCI`.
#' @param write_res A boolean. Defines whether to save the results to 
#'                             files.
#' @param res_dir A character. The directory to write the results and
#'                             log to.
#' 
#' @return A tibble with columns `Endpoint`, `Score`, `Group`, 
#'          `N_controls`, `N_cases`, `beta`, `std_errs`, `p_val`, `HR`, 
#'          `CI_pos`, `CI_neg`. The Cox-PH analysis results for all 
#'          selected endpoints. 
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_studies_hrs <- function(pheno_data, 
                             score_data,
                             score_type,
                             bin_cut=1,
                             studies,
                             covs=c("SEX", "YEAR_OF_BIRTH"),
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
                                                   bin_cut,
                                                   study,
                                                   write_res,
                                                   res_dir)
            coxph_mdl <- run_coxph_ana(pheno_score_data,
                                       study@endpt,
                                       covs)
            coxph_res_tib <- add_coxph_res_row(coxph_res_tib,
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
                   bin_cut,
                   study,
                   write_res,
                   res_dir)

    return(coxph_res_tib)
}
