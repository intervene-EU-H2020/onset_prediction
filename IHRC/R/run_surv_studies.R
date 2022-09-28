#' Calcualtes HR from a Cox-PH model for each exposure age and endpoint
#' 
#' @param pheno_data A data.frame with at least the columns: 
#'                   `ID`, the columns specified in `covs` and 
#'                   i.e. `J10_ASTHMA`, and `J10_ASTHMA_AGE_DAYS` where 
#'                   the columns are the study endpoint and date, which 
#'                   will differ depending on the chosen `endpts`.
#'                   The phenotypic data on the individuals.
#' @param score_data A data.frame with at least columns `SCORE`, or
#'                      i.e. `J10_ASTHMA_PRS` for endpoint `J10_ASTHMA`
#'                      and score type `PRS`.
#' @param score_type A character. The score type. At the moment the two
#'                      options are `CCI`, and `PRS`.
#' @param study_type A character. Can be either `forward` or `backward`. 
#'              `forward` considers individuals of a certain age 
#'              and a set exposure, washout, and observation periods 
#'              calcualted onwards from this age. It can simply be created
#'              by creating a S4 study object.
#'              `backward` considers all individuals at a set time
#'              point. The observation and washout period are calcualted 
#'              backwards from this time point. The exposure period will 
#'              be different for each individual depending on their birth 
#'              date. This setup can be created, using the function 
#'              \code{\link{get_backward_study}}.
#' @param endpts A character (vector). The column names of the endpoints of 
#'                         interest.
#' @param exp_ages An integer (vector). Ages at which exposure period 
#'                  starts (in years).
#' @param exp_len An integer. Length of the exposure period ^
#'                            (in years).
#' @param wash_len An integer. Length of the washout period
#'                             (in years).
#' @param obs_len An integer. Length of the prediction period
#'                            (in years).
#' @param obs_end_date A Date. The end of the observation period. Needed 
#'                      for `backward` studies. If not set the end will 
#'                      be the most recent date across the diagnosis columns.
#' @param down_fctr A numeric. Defines how many controls there
#'                                   should be for every case.
#'                                   Default is NA, which means no
#'                                   downsampling is performed.
#' @param ancs A character (vector). The ancestries to consider.
#' @param max_age A numeric. The maximum age at the end of the exposure
#'                  window of individuals.
#' @param filter_1998 A boolean. Whether to filter out individuals
#'                      where the exposure period is before 1998.
#' @param covs A vector of characters. The column names of the covariates 
#'              to add to the predictor of the Cox-PH model.
#' @param bin_cut A numeric. The binary cutoff value for classifying high
#'                  and low score individuals. Currently only in use if
#'                  the `score_type == CCI`.
#' @param min_indvs An integer. The minimum number of individuals each 
#'                  group in the analyses needs to have. This is important 
#'                  for being able to export data from the FinnGen Sanbdox.
#' @param write_res A boolean. Defines whether to save the results to 
#'                             files.
#' @param res_dir A character. The directory to write the results and
#'                             log to.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
run_surv_studies <- function(pheno_data, 
                             icd_data=NULL,
                             atc_data=NULL,
                             prs_data=NULL,
                             score_type,
                             plot_preds=NULL,
                             study_type="forward",
                             endpts=c("J10_ASTHMA"),
                             exp_ages=0,
                             exp_len=NULL,
                             wash_len=2,
                             obs_len=8,
                             obs_end_date=as.Date("2021/01/01"),
                             down_fctr=NA_integer_,
                             ancs=NA_character_,
                             max_age=90,
                             filter_1998=FALSE,
                             covs=c("SEX", "YEAR_OF_BIRTH"),
                             bin_cut=1,
                             min_indvs=5,
                             write_res=FALSE,
                             res_dir=NULL) {

    if(write_res) {
        res_dir <- get_full_res_path(res_dir, down_fctr, filter_1998)
    }
    all_age_hrs_tib <- create_empty_endpt_hrs_tib() 
    all_age_cidxs_tib <- create_empty_cidx_tib()
    for(endpt in endpts) {
        for(exp_age in exp_ages) {
            study <- create_endpt_study_obj(study_data=pheno_data,
                                            study_type=study_type,
                                            preds=c(score_type, covs),
                                            endpt=endpt, 
                                            exp_age=exp_age,
                                            exp_len=exp_len,
                                            wash_len=wash_len,
                                            obs_len=obs_len,
                                            obs_end_date=obs_end_date,
                                            down_fctr=down_fctr,
                                            ancs=ancs,
                                            max_age=max_age,
                                            filter_1998=filter_1998,
                                            write_res=write_res,
                                            res_dir=res_dir)
            elig_score_data <- get_elig_score_data(score_type=score_type, 
                                                   study_data=study@study_data,
                                                   icd_data=icd_data, 
                                                   atc_data=atc_data,
                                                   prs_data=prs_data,
                                                   endpt=endpt,
                                                   min_indvs=min_indvs)
            if(is.null(plot_preds))
                plot_preds <- score_type
            surv_ana <- methods::new("surv_ana",
                                     study=study,
                                     elig_score_data=elig_score_data,
                                     min_indvs=min_indvs,
                                     preds=c(score_type, covs),
                                     plot_preds=plot_preds,
                                     bin_cut=bin_cut,
                                     write_res=write_res,
                                     res_dir=res_dir)
            coxph_mdl <- get_coxph_mdl(surv_ana=surv_ana)
            all_age_hrs_tib <- add_coxph_res_row(
                                    endpt_hrs_tib=all_age_hrs_tib,
                                    coxph_mdl=coxph_mdl,
                                    surv_ana=surv_ana)
            c_idx_res <- calc_endpt_study_cidx(surv_ana)
            all_age_cidxs_tib <- add_cidx_res_row(
                                    endpt_c_idxs_tib=all_age_cidxs_tib, 
                                    c_idx_res=c_idx_res, 
                                    surv_ana=surv_ana)
        }
    }
    if(nrow(all_age_hrs_tib) > 0 | nrow(all_age_hrs_tib) > 0) {
        ana_details <- get_ana_details_from_surv_ana(surv_ana)
        write_res_files(endpt_hrs_tib=all_age_hrs_tib,
                        endpt_c_idxs_tib=all_age_cidxs_tib,
                        ana_details=ana_details)
        plot_hrs(all_age_hrs_tib, surv_ana) 
    }
}



