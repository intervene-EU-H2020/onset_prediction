#' Fits a Cox-PH model for each endpoint
#' 
#' This function runs survival studies for a given endpoint.It fits a
#' Cox proportional hazards model to the data and 
#' calculates the C-index. The resulting HRs and c-indices are written 
#' to files if wanted. 
#' 
#' @param pheno_data A data.frame with the phenotypic data on the individuals.
#'                   Should at least contain the columns: 
#'                   `ID`, the columns specified in `covs` and 
#'                   i.e. `J10_ASTHMA`, and `J10_ASTHMA_AGE_FROM_BASE` where 
#'                   the columns are the study endpoint and the age
#'                   from baseline, which will differ depending on the chosen 
#'                   `endpts`. 
#' @param score_type A character (vector). 
#'                      The score types used in the Cox-PH model.
#' @param plot_preds A character (vector). 
#'                      The predictors to use when plotting HR.
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
#' @param obs_age_range A numeric. The age range of individuals in the observation
#'                                 period. Inclusive interval. 
#' @param covs A vector of characters. The column names of the covariates 
#'              to add to the predictor of the Cox-PH model.
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
run_surv_studies <- function(pheno_data, 
                             endpt_indvs_mat=NULL,
                             icd_data=NULL,
                             atc_data=NULL,
                             prs_data=NULL,
                             phers_data=NULL,
                             score_type,
                             plot_preds=NULL,
                             study_type="backward",
                             endpts=c("J10_ASTHMA"),
                             exp_ages=0,
                             exp_len=NULL,
                             wash_len=2,
                             obs_len=8,
                             obs_end_date=as.Date("2021/01/01"),
                             down_fctr=NA_integer_,
                             ancs=NA_character_,
                             obs_age_range=c(32,80),
                             covs=c("SEX", "YEAR_OF_BIRTH"),
                             min_indvs=5,
                             res_descr="",
                             write_res=FALSE,
                             res_dir=NULL) {
    # Get the full path for the results directory
    res_dir <- get_full_res_path(write_res, res_dir, down_fctr, study_type)

    # Initialize empty tibbles for storing results 
    all_age_hrs_tib <- create_empty_endpt_hrs_tib() 
    all_age_cidxs_tib <- create_empty_cidx_tib()

    # Loop over endpoints 
    for(endpt in endpts) {
        # Get data of individuals who can be used for the current endpoint
        study_data <- get_crnt_endpt_data(endpt_indvs_mat, endpt, pheno_data)
        for(exp_age in exp_ages) {
            study <- create_endpt_study_obj(study_data=study_data,
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
                                            obs_age_range=obs_age_range,
                                            write_res=write_res,
                                            res_dir=res_dir)
            elig_score_data <- get_elig_score_data(score_type=score_type, 
                                                   study_data=study@study_data,
                                                   icd_data=icd_data, 
                                                   atc_data=atc_data,
                                                   prs_data=prs_data,
                                                   phers_data=phers_data,
                                                   endpt=endpt,
                                                   min_indvs=min_indvs)
            if(!is.null(elig_score_data)) {
                surv_ana <- methods::new("surv_ana",
                                        study=study,
                                        elig_score_data=elig_score_data,
                                        min_indvs=min_indvs,
                                        preds=get_all_preds_sorted(score_type, covs),
                                        plot_preds=get_plot_preds(plot_preds, score_type),
                                        res_descr=res_descr,
                                        write_res=write_res,
                                        res_dir=res_dir)
                coxph_mdl <- get_coxph_mdl(surv_ana=surv_ana)
                all_age_hrs_tib <- add_coxph_res_row(
                                        endpt_hrs_tib=all_age_hrs_tib,
                                        coxph_mdl=coxph_mdl,
                                        surv_ana=surv_ana)
                c_idx <- calc_endpt_study_cidx(surv_ana,
                                               coxph_mdl)
                all_age_cidxs_tib <- add_cidx_res_row(
                                        endpt_c_idxs_tib=all_age_cidxs_tib, 
                                        c_idx_res=c_idx, 
                                        surv_ana=surv_ana)
            }
        }
    }
    save_results(all_age_hrs_tib, all_age_cidxs_tib, surv_ana)
}

#' Saves the results
#'
#' This function saves the results of a survival analysis and plots them 
#' Saves everything to files if this is wanted.
#'
#' @param all_age_hrs_tib A tibble containing the hazard ratios for each exposure age.
#' @param all_age_cidxs_tib A tibble containing the c-index for each exposure age.
#' @param surv_ana A survival analysis object
#'
#' @export
save_results <- function(all_age_hrs_tib,
                         all_age_cidxs_tib,
                         surv_ana) {
    if(nrow(all_age_hrs_tib) > 0 | nrow(all_age_cidxs_tib) > 0) {
        ana_details <- get_ana_details_from_surv_ana(surv_ana)
        write_res_files(endpt_hrs_tib=all_age_hrs_tib,
                        endpt_c_idxs_tib=all_age_cidxs_tib,
                        ana_details=ana_details)
        plot_hrs(all_age_hrs_tib, surv_ana) 
    }
}

#' Get data of individuals who can be used for the current endpoint
#' 
#' @inheritParams run_surv_studies
#' 
#' @return A data.frame that contains the data for individuals with the
#'          given endpoint.
#' 
#' @export
#' 
#' @author Kira E. Detrois
get_crnt_endpt_data <- function(endpt_indvs_mat,
                                endpt,
                                pheno_data) {
    endpt_ids <- get_endpt_ids(endpt_indvs_mat, endpt, pheno_data$ID)
    study_data <- dplyr::filter(pheno_data, ID %in% endpt_ids)
    return(study_data)
}