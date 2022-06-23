#' Calcualtes HR from a cox-ph model for each endpoint
#' 
#' @param pheno_data A data.frame with at least the columns: 
#'                   `ID`, `SEX`, `DATE_OF_BIRTH`, `ANCESTRY`, 
#'                   `START_OF_FOLLOWUP`, `END_OF_FOLLOWUP`, 
#'                   `DATE_OF_BIRTH`, and i.e. `J10_ASTHMA`, and
#'                   `J10_ASTHMA_DATE` where the columns are the study 
#'                   endpoint and date, which will differ depending on 
#'                   the input variable `endpts`.
#' @param score_data A data.frame with the score results for each individuals.
#'                   Should have at least column defined in `score_col_name`.
#' @param score_col_name A character. The name of the column with the scores.
#' @param score_type A character. The name of the score used for the model,
#'                      i.e. CCI, or PheRS.
#' @param endpts A string. The column names of the endpoints of 
#'                        interest.
#' @param exp_age An integer. Age at which exposure period starts 
#'                            (in years).
#' @param exp_len An integer. Length of the exposure period
#'                               (in years).
#' @param wash_len An integer. Length of the washout period
#'                                (in years).
#' @param obs_len An integer. Length of the prediction period
#'                               (in years).
#' @param downsample_fctr A numeric. Defines how many controls there
#'                                   should be for every case.
#'                                   Default is NA, which means no
#'                                   downsampling is performed.
#' @param write_res A boolean. Defines whether to save the results to 
#'                             files.
#' @param res_dir A character. The directory to write the results and
#'                             log to.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_endpt_hrs <- function(pheno_data, 
                           score_data,
                           score_type,
                           endpts,
                           exp_age,
                           exp_len=10,
                           wash_len=2,
                           obs_len=8,
                           downsample_fctr=NA,
                           write_res=FALSE,
                           res_dir=NA) {
    
    coxph_res <- create_empty_coxph_res_tib()       
    for(endpt in endpts) {
        elig_endpt_indv <- Istudysetup::get_study_elig_indv(
                                    pheno_data,
                                    endpt,
                                    exp_age,
                                    exp_len,
                                    wash_len,
                                    obs_len,
                                    downsample_fctr,
                                    write_res,
                                    res_dir)$data

        if(Istudysetup::get_n_cases(elig_endpt_indv, endpt) > 100) {
            pheno_score_data <- join_dfs(elig_endpt_indv, 
                                         score_data)
            plt <- plot_endpt_score_distr(as.list(environment()))
            pheno_score_data <- add_risk_group_col(pheno_score_data)
            coxph_res <- run_and_add_coxph_ana(as.list(environment()),
                                               "SCORE_GROUP")
            coxph_res <- run_and_add_coxph_ana(as.list(environment()),
                                               "SCORE")
            write_score_groups_to_log(as.list(environment()))
        } else {
            message(paste0("Not enough cases for endpoint: ", endpt, " No of cases: ", Istudysetup::get_n_cases(elig_endpt_indv, endpt)))
        }
    }
    write_res(as.list(environment()))

    return(coxph_res)
}

#' Runs the cox-PH analysis and adds the results to the data.frame
#' `coxph_res`
#'  
#' 
#' @param envir A list with at least entries `coxph_res`, 
#'              `score_type`, `endpt`, `pheno_score_data`.
#' @param predictor A character. The predictor for the survival 
#'                               analyis string.
#' 
#' @return A tibble the results data.fram `coxph_res` with added 
#'         columns from the analysis run. 
run_and_add_coxph_ana <- function(envir,
                                  predictor) {
    curnt_coxph_res <- run_coxph_ana(envir$pheno_score_data, 
                                     envir$endpt,
                                     predictor)
    envir$coxph_res <- add_coxph_row(envir$coxph_res,
                                     curnt_coxph_res,
                                     envir$score_type,
                                     envir$endpt,
                                     envir$pheno_score_data)
    return(envir$coxph_res)
}