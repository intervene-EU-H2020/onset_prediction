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
#' @param write_hr_res A boolean that defines whether to write the results
#'                     from the cox-ph model to a file or not. Default is FALSE.
#' @param write_study_res A boolean that defines whether to write the results from
#'                        the study selection of eligible individuals for each 
#'                        endpoint, to files or not. Default is FALSE.
#' @param write_study_log A character or NA. How to write the log for the study setup.
#'                        Can be either `NA`: No log. `file`: Write to file, or
#'                       `print`: Print to console. `file`, and `print` can be used 
#'                        at the same time.
#' @param res_dir A character. The directory to write the results to.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_endpt_hr <- function(pheno_data, 
                          score_data,
                          score_col_name,
                          score_type,
                          endpts,
                          exp_age=30,
                          exp_len=10,
                          wash_len=2,
                          obs_len=8,
                          downsample_fctr=NA,
                          write_hr_res=FALSE,
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
    write_hr_res(as.list(environment()))
    return(all_coxph_res)
}