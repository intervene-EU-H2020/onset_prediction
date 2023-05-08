#' Fits a Cox-PH model for each endpoint
#' 
#' This function runs survival studies for a given endpoint.It fits a
#' Cox proportional hazards model to the data and 
#' calculates the C-index. The resulting HRs and c-indices are written 
#' to files if wanted. 
#' 
#' Creates a `surv_ana` and `study_setup` object then reads in the phenotype data and
#' relevant score data, defined by the selected score types and writes the
#' phenotype-score data to files, or simply reads in previously created phenotype-score 
#' files. Then creates a `study` object for each selected endpoint and runs
#' the Cox-PH models and extracts the HR data and c-index for each model. Finally, writes
#' them to files and plots the HR data.
#' 
#' The `endpts_indvs_mat` is mostly relevant for FinnGen where we have some overalp for 
#' the PRS studies and training and testing sets. However, it could
#' also be relevant for the training and testing of the PheRS. See function
#' [IHRC::get_crnt_endpt_data].
#' 
#' @param pheno_data A data.frame. The phenotypic data on the individuals.
#'                   Should at least contain the columns as described  in 
#'                   [IHRC::get_relevant_pheno_data_cols].
#' @param endpts_indvs_mat A data.frame. A matrix with the individuals that can 
#'                         be used for each endpoint. Contains a column of individual 
#'                         IDs and a binary column for each endpoint.
#' @param icd_data A data.frame. The ICD codes of the individuals. Needs at least the 
#'                  columns as described in [IHRC::get_study_cci_data].
#' @param atc_data A data.frame. The ATC codes of the individuals. Needs at least the 
#'                  columns as described in [IHRC::get_study_med_data].
#' @param prs_data A data.frame. The PRS scores of the individuals. Needs at least the 
#'                  columns as described in [IHRC::get_prs_endpt_data].
#' @param phers_data A data.frame. The PheRS scores of the individuals. Needs at least 
#'                    the columns as described in [IHRC::get_phers_endpt_data].
#' @param zip_data A data.frame. The ZIP scores of the individuals. Needs at least the
#'                  columns as described in [IHRC::get_zip_data].
#' @param score_type A string (vector). The score types in the analysis.
#'                   Available options include:
#'                   \itemize{
#'                      \item `CCI` for Charlson comorbidity index, 
#'                      \item `EI` for Elixhauser index, 
#'                      \item `PRS` for polygenic risk score, 
#'                      \item `PheRS` for PheRS results score, 
#'                      \item `MED` for medication data, and 
#'                      \item `EDU` for educational level. The column for this needs to 
#'                                  be called `EDU` in the `pheno_data`.
#'                      \item `ZIP` for zip scores,
#'                  }
#' @param create_score_combos A boolean, whether or not to create all possible
#'                          score type combinations from the score_type vector.
#'                          see function [IUtils::get_all_possible_score_type_combs].
#' @param plot_preds A string (vector). 
#'                     The predictors to use when plotting HR.
#' @param covs A vector of characters. The column names of the covariates 
#'              to add to the predictor of the Cox-PH model.
#' @param study_type A string. Can be either `forward` or `backward`. 
#'              `forward` considers individuals of a certain age 
#'              and a set exposure, washout, and observation periods 
#'              calcualted onwards from this age. It can simply be created
#'              by creating a S4 study object.
#'              `backward` considers all individuals at a set time
#'              point. The observation and washout period are calcualted 
#'              backwards from this time point. The exposure period will 
#'              be different for each individual depending on their birth 
#'              date. 
#' @param endpts A string (vector). The endpoints of interest.
#' @param exp_ages An integer (vector). Ages at which exposure period 
#'                  starts (in years).
#' @param exp_len An integer. Length of the exposure period (in years).
#' @param wash_len An integer. Length of the washout period (in years).
#' @param obs_len An integer. Length of the prediction period (in years).
#' @param obs_end_date A Date. The end of the observation period. Needed 
#'                      for `backward` studies. If not set the end will 
#'                      be the most recent date across the diagnosis columns.
#' @param obs_age_range A numeric. The age range of individuals in the observation
#'                                 period. Inclusive interval. 
#' @param down_fctr A numeric. Defines how many controls there should be for every case.
#'                             Default is NA, which means no downsampling is performed.
#' @param ancs A string (vector). The ancestries to consider.
#' @param min_indvs An integer. The minimum number of individuals each 
#'                  group in the analyses needs to have. This is important 
#'                  for being able to export data from the FinnGen Sanbdox.
#' @param filter_1998 A boolean. Defines whether to filter out any data from before 1998.
#' @param res_descr A string. An addition to add to the results folder.
#' @param write_res A boolean. Defines whether to save the results to files.
#' @param res_dir A string. The directory to write the results and log to.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
run_surv_studies <- function(pheno_data, 
                             score_type,
                             create_score_combos=FALSE,
                             plot_preds=NULL,
                             covs=c("SEX", "YEAR_OF_BIRTH"),
                             endpts_indvs_mat=NULL,
                             icd_data=NULL,
                             atc_data=NULL,
                             prs_data=NULL,
                             phers_data=NULL,
                             zip_data=NULL,
                             study_type="backward",
                             endpts=c("J10_ASTHMA"),
                             exp_ages=0,
                             exp_len=NULL,
                             wash_len=2,
                             obs_len=8,
                             obs_end_date=as.Date("2021/01/01"),
                             obs_age_range=c(32,80),
                             down_fctr=NA_integer_,
                             ancs=NA_character_,
                             min_indvs=5,
                             filter_1998=FALSE,
                             res_descr=NULL,
                             write_res=FALSE,
                             res_dir=NULL) {
    res_dir <- get_full_res_path(write_res=write_res,  
                                 res_dir=res_dir,
                                 down_fctr=down_fctr, 
                                 study_type=study_type, 
                                 score_type=score_type,
                                 res_descr=res_descr)
    surv_ana <- methods::new("surv_ana",
                             min_indvs=min_indvs,
                             score_types=score_type,
                             create_score_combos=create_score_combos,
                             covs=covs,
                             write_res=write_res,
                             res_dir=res_dir)
    study_setup <- methods::new("study_setup",
                                study_type="backward",
                                exp_age=0,
                                exp_len=exp_len,
                                wash_len=wash_len,
                                obs_len=obs_len,
                                obs_end_date=obs_end_date,
                                down_fctr=down_fctr,
                                exp_f1998=filter_1998,
                                ancs=ancs,
                                obs_age_range=obs_age_range)
    create_pheno_score_files(study_setup=study_setup,
                             endpts=endpts,
                             surv_ana = surv_ana,
                             score_type = score_type,
                             pheno_data = pheno_data,
                             endpts_indvs_mat = endpts_indvs_mat,
                             icd_data = icd_data,
                             atc_data = atc_data,
                             prs_data = prs_data,
                             phers_data = phers_data,
                             zip_data = zip_data)
    run_models(study_setup=study_setup,
               endpts=endpts,
               surv_ana=surv_ana)
}

#' Create phenotype score files
#' 
#' Reads in the phenotype data and relevant score data, defined by the selected score
#' types and writes the phenotype-score data to files
#' 
#' The `endpts_indvs_mat` is mostly relevant for FinnGen where we have some overalp for 
#' the PRS studies and training and testing sets. However, it could
#' also be relevant for the training and testing of the PheRS. See function
#' [IHRC::get_crnt_endpt_data].
#' 
#' @inheritParams run_surv_studies
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param endpts A string (vector). The endpoints of interest.
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' 
#' @author Kira E. Detrois
#' 
#' @export
create_pheno_score_files <- function(study_setup,
                                    endpts,
                                    surv_ana,
                                    score_type,
                                    pheno_data,
                                    endpts_indvs_mat,
                                    icd_data,
                                    atc_data,
                                    prs_data,
                                    phers_data,
                                    zip_data) {
    writeLines("Creating pheno score files")
    for(endpt in endpts) {
        writeLines(paste0("Endpoint: ", endpt))
        # Getting all data for current endpoint
        pheno_score_data <- get_pheno_score_data(
                                         score_type=score_type,
                                         pheno_data=pheno_data,
                                         study_setup=study_setup,
                                         endpts_indvs_mat=endpts_indvs_mat,
                                         icd_data=icd_data, 
                                         atc_data=atc_data,
                                         prs_data=prs_data,
                                         phers_data=phers_data,
                                         zip_data=zip_data,
                                         endpt=endpt,
                                         min_indvs=surv_ana@min_indvs,
                                         error_file=surv_ana@error_file)
        # Writing log files
        write_pheno_score_files(pheno_score_data=pheno_score_data,
                                study_setup=study_setup,
                                endpt=endpt,
                                surv_ana=surv_ana)       
    }
    return(pheno_score_data)
}

#' Runs the Cox-PH on the data
#' 
#' Reads in previously created phenotype-score files. Then creates a `study` object for 
#' each selected endpoint and runs the Cox-PH models and extracts the HR data and 
#' c-index for each model. Finally, writes them to files and plots the HR data.
#' 
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param endpts A string (vector). The endpoints of interest.
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' 
#' @author Kira E. Detrois
#' 
#' @export
run_models <- function(study_setup,
                       endpts,
                       surv_ana) {
    writeLines(paste0("\nRunning models"))
    # Running analaysis for all score type combinations
    is_first_endpt <- TRUE
    for(endpt in endpts) {
        writeLines(paste0("Endpoint: ", endpt))
        # Initialize empty tibbles for storing results 
        hr_res <- create_empty_hr_res() 
        cidx_res <- create_empty_cidx_res()
        pheno_score_data <- read_pheno_score_file(study_setup=study_setup,
                                                  endpt=endpt,
                                                  surv_ana=surv_ana)
        for(crnt_score_type in surv_ana@score_combos) {
            study <- methods::new("study",
                                  study_setup=study_setup,
                                  study_data=pheno_score_data,
                                  endpt=endpt)
            # Setting up current survival analysis
            surv_ana <- setPreds(surv_ana, crnt_score_type)
            writeLines(get_surv_descr(surv_ana@preds))
            # Running actual model
            if(nrow(study@study_data) > 0) {
                coxph_mdl <- get_coxph_mdl(surv_ana=surv_ana, study=study)
                if(!is.null(coxph_mdl)) {
                    hr_res <- add_coxph_res_row(hr_res=hr_res,
                                                coxph_mdl=coxph_mdl,
                                                study=study,
                                                preds=surv_ana@preds)
                    c_idx <- calc_endpt_study_cidx(surv_ana=surv_ana, 
                                                 study=study,
                                                 coxph_mdl=coxph_mdl)
                    cidx_res <- add_cidx_res_row(c_idxs_res=cidx_res, 
                                                 c_idx_res=c_idx, 
                                                 surv_ana=surv_ana,
                                                 study=study)
                    indv_model_probs <- get_indv_model_probs(coxph_mdl=coxph_mdl, 
                                                             surv_ana=surv_ana,
                                                             study_data=study@study_data,
                                                             preds=surv_ana@preds)
                    pheno_score_data <- dplyr::left_join(pheno_score_data, 
                                                         indv_model_probs,
                                                         by="ID")
                }
            } else {
                write_to_error_file(surv_ana@error_file, paste0("Not enough data for endpoint ", endpt))
            }
        }   
        save_results(hr_res=hr_res, 
                     cidx_res=cidx_res, 
                     study_setup=study_setup, 
                     surv_ana=surv_ana,
                     score_type=surv_ana@score_types,
                     endpt=endpt,
                     pheno_score_data=pheno_score_data,
                     is_first_endpt=is_first_endpt)
        is_first_endpt <- FALSE
    }
}


#' Saves the results
#'
#' This function saves the results of a survival analysis and plots them 
#' Saves everything to files if this is wanted.
#'
#' @param hr_res A tibble containing the hazard ratios for each exposure age.
#' @param cidx_res A tibble containing the c-index for each exposure age.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#'
#' @export
save_results <- function(hr_res,
                         cidx_res,
                         study_setup,
                         surv_ana,
                         score_types,
                         endpt,
                         pheno_score_data,
                         is_first_endpt) {
    write_pheno_score_files(pheno_score_data=pheno_score_data,
                            study_setup=study_setup,
                            endpt=endpt,
                            surv_ana=surv_ana)
    if(nrow(hr_res) > 0 | nrow(cidx_res) > 0) {
        file_path_coxph <- get_full_file_name_path(res_type="coxph",
                                                study_setup=study_setup,
                                                surv_ana=surv_ana)
        file_path_cidx <- get_full_file_name_path(res_type="cidx",
                                                study_setup=study_setup,
                                                surv_ana=surv_ana)

        if(!is.null(file_path_coxph)) {
            hr_res <- dplyr::filter(hr_res, !stringr::str_detect(VAR, "PC"))
            hr_res <- dplyr::filter(hr_res, !stringr::str_detect(VAR, "BATCH"))
            hr_res <- dplyr::filter(hr_res, N_CASES > surv_ana@min_indvs)
            if(is_first_endpt) { # Creating / overwriting files for first endpoint
                readr::write_delim(x=hr_res, 
                                file=file_path_coxph, 
                                delim="\t")
                readr::write_delim(x=cidx_res,
                                file=file_path_cidx,
                                delim="\t")
            } else { # Appending files for other endpoints
                readr::write_delim(x=hr_res, 
                                file=file_path_coxph, 
                                delim="\t",
                                append=TRUE)
                readr::write_delim(x=cidx_res,
                                file=file_path_cidx,
                                delim="\t",
                                append=TRUE)
            }
        }
        plot_hrs(coxph_hrs=hr_res, 
                 study_setup=study_setup,
                 surv_ana=surv_ana) 
    }}

#' Creates a study object 
#' 
#' Reads in previously created phenotype-score files. If the file doesn't exist
#' for the endpoint creates an empty tibble. Then creates a `study` object for 
#' each selected endpoint and given `study_setup`.
#' 
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param endpt A string. The current endpoint.
#' 
#' @return An S4 study object with the current endpoint.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
create_crnt_study_from_file <- function(surv_ana,
                                        study_setup,
                                        endp, pheno_score_d) {

    study <- methods::new("study",
                          study_setup=study_setup,
                          study_data=pheno_score_data,
                          endpt=endpt)
    return(study)
}


read_pheno_score_file <- function(study_setup,
                                  endpt,
                                  surv_ana) {
    file_path <- get_full_file_name_path(res_type="pheno_score",
                                         study_setup=study_setup,
                                         endpt=endpt,
                                         surv_ana=surv_ana)
    pheno_score_data <- readr::read_delim(file_path, delim="\t", show_col_types=FALSE)
    pheno_score_data <- dplyr::filter(pheno_score_data, TRAIN_STATUS==0)
}