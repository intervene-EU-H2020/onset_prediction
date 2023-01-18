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
#' @param endpts_indvs_mat A data.frame. A matrix with the individuals that can 
#'                         be used for each endpoint.
#' @param icd_data A data.frame. A data.frame with the ICD codes of the 
#'                 individuals.
#' @param atc_data A data.frame. A data.frame with the ATC codes of the 
#'                 individuals.
#' @param prs_data A data.frame. A data.frame with the PRS scores of the 
#'                 individuals.
#' @param phers_data A data.frame. A data.frame with the PheRS scores of the
#'                   individuals.
#' @param score_type A character (vector). 
#'                      The score types used in the Cox-PH model.
#' @param score_type_combos A boolean, whether or not to create all possible
#'                          score type combinations from the score_type vector.
#'                          see function [IUtils::get_all_possible_score_type_combs].
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
                             endpts_indvs_mat=NULL,
                             icd_data=NULL,
                             atc_data=NULL,
                             prs_data=NULL,
                             phers_data=NULL,
                             score_type,
                             create_score_combos=FALSE,
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
    res_dir <- get_full_res_path(write_res=write_res,  
                                 res_dir=res_dir,
                                 down_fctr=down_fctr, 
                                 study_type=study_type, 
                                 score_type=score_type)
    surv_ana <- methods::new("surv_ana",
                             min_indvs=min_indvs,
                             score_types=score_type,
                             create_score_combos=create_score_combos,
                             covs=covs,
                             res_descr=res_descr,
                             write_res=write_res,
                             res_dir=res_dir)
    study_setups <- get_study_setups(endpts=endpts,
                                     study_type=study_type,
                                     exp_len=exp_len,
                                     wash_len=wash_len,
                                     obs_len=obs_len,
                                     obs_end_date=obs_end_date,
                                     down_fctr=down_fctr,
                                     ancs=ancs,
                                     obs_age_range=obs_age_range)
    create_pheno_score_data_files(study_setups=study_setups,
                                  surv_ana=surv_ana,
                                  score_type=score_type,
                                  pheno_data=pheno_data,
                                  endpts_indvs_mat=endpts_indvs_mat,
                                  icd_data=icd_data,
                                  atc_data=atc_data,
                                  prs_data=prs_data,
                                  phers_data=phers_data)
    run_models(study_setups=study_setups,
               surv_ana=surv_ana)
}

run_models <- function(study_setups,
                       surv_ana) {
    writeLines("Running models")
    # Running analaysis for all score type combinations
    for(crnt_score_type in surv_ana@score_combos) {
        writeLines(paste0("Score: ", paste0(crnt_score_type, collapse=", ")))

        # Initialize empty tibbles for storing results 
        hr_res <- create_empty_endpt_hr_res() 
        cidx_res <- create_empty_cidx_res()
        for(endpt in names(study_setups)) {
            writeLines(paste0("Endpoint: ", endpt))

            # Need this for the correct file name to read for study
            surv_ana <- setPreds(surv_ana, surv_ana@score_types)
            study <- get_crnt_study(surv_ana=surv_ana,
                                    study_setup=study_setups[[endpt]],
                                    endpt=endpt)
            # Setting up current survival analysis
            surv_ana <- setPreds(surv_ana, crnt_score_type)
                    
            # Running actual model
            if(nrow(study@study_data) > 0) {
                coxph_mdl <- get_coxph_mdl(surv_ana=surv_ana, study=study)
                append_log_file(coxph_mdl=coxph_mdl, 
                                study=study,
                                surv_ana=surv_ana)
                hr_res <- add_coxph_res_row(endpt_hr_res=hr_res,
                                            coxph_mdl=coxph_mdl,
                                            study=study)
                c_idx <- calc_endpt_study_cidx(surv_ana=surv_ana, 
                                               study=study,
                                               coxph_mdl=coxph_mdl)
                cidx_res <- add_cidx_res_row(endpt_c_idxs_tib=cidx_res, 
                                             c_idx_res=c_idx, 
                                             surv_ana=surv_ana,
                                             study=study)
                }
            }   
        save_results(hr_res=hr_res,
                     cidx_res=cidx_res, 
                     study=study,
                     surv_ana=surv_ana)
    }
}

get_study_setups <- function(endpts,
                             study_type,
                             exp_len,
                             wash_len,
                             obs_len,
                             obs_end_date,
                             down_fctr,
                             ancs,
                             obs_age_range) {
    study_setups <- list()
    for(endpt in endpts) {
        study_setups[[endpt]] <- methods::new("study_setup",
                                              study_type="backward",
                                              exp_age=0,
                                              exp_len=exp_len,
                                              wash_len=wash_len,
                                              obs_len=obs_len,
                                              obs_end_date=obs_end_date,
                                              down_fctr=down_fctr,
                                              exp_f1998=FALSE,
                                              ancs=ancs,
                                              obs_age_range=obs_age_range)
    }
    return(study_setups)
}
create_pheno_score_data_files <- function(study_setups,
                                          surv_ana,
                                          score_type,
                                          pheno_data,
                                          endpts_indvs_mat,
                                          icd_data,
                                          atc_data,
                                          prs_data,
                                          phers_data) {
    writeLines("Creating pheno score files")
    for(endpt in names(study_setups)) {
        writeLines(paste0("Endpoints: ", endpt))
        # Getting all data for current endpoint
        pheno_score_data <- get_pheno_score_data(
                                     score_type=score_type,
                                     pheno_data=pheno_data,
                                     study_setup=study_setups[[endpt]],
                                     endpts_indvs_mat=endpts_indvs_mat,
                                     icd_data=icd_data, 
                                     atc_data=atc_data,
                                     prs_data=prs_data,
                                     phers_data=phers_data,
                                     endpt=endpt,
                                     min_indvs=surv_ana@min_indvs)
        # Writing log files
        write_pheno_score_files(pheno_score_data=pheno_score_data,
                                study_setup=study_setups[[endpt]],
                                endpt=endpt,
                                surv_ana=surv_ana)             
    }
}

get_crnt_study <- function(surv_ana,
                           study_setup,
                           endpt) {
    file_path <- check_and_get_file_path(res_type="pheno_score",
                                         study_setup=study_setup,
                                         endpt=endpt,
                                         surv_ana=surv_ana)
    pheno_score_data <- readr::read_delim(file_path, delim="\t", show_col_types=FALSE)
    study <- methods::new("study",
                          study_setup=study_setup,
                          study_data=as.data.frame(pheno_score_data),
                          endpt=endpt)
    #write_log_file(study,
    #               surv_ana)
    return(study)
}


#' Saves the results
#'
#' This function saves the results of a survival analysis and plots them 
#' Saves everything to files if this is wanted.
#'
#' @param hr_res A tibble containing the hazard ratios for each exposure age.
#' @param cidx_res A tibble containing the c-index for each exposure age.
#' @param surv_ana A survival analysis object
#'
#' @export
save_results <- function(hr_res,
                         cidx_res,
                         study,
                         surv_ana) {
    if(nrow(hr_res) > 0 | nrow(cidx_res) > 0) {
        write_res_files(endpt_hr_res=hr_res,
                        endpt_c_idxs_tib=cidx_res,
                        study=study,
                        surv_ana=surv_ana)
        plot_hrs(coxph_hrs=hr_res, 
                 study=study,
                 surv_ana=surv_ana) 
    }
}

get_score_type_dir_name <- function(score_type) {
    dir_name <- paste0(score_type[order(score_type)], collapse="_")
    return(dir_name)
}
