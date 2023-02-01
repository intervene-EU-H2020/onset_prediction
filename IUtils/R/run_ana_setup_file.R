#' Runs analysis defined in a setup file
#' 
#' Reads in all the data, using function [IUtils::get_all_data] and then runs function 
#' [IHRC::run_surv_studies].
#' 
#' @param setup_file_path A string. The path to the setup file.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
run_ana_setup_file <- function(setup_file_path) {
    setup <- get_setup(setup_file_path)
    if(!setup$read_pheno_score_files) {
        data <- IUtils::get_all_data(score_type=setup$score_type,
                                     endpts=setup$endpts,
                                     pheno_file_path=setup$pheno_file_path,
                                     icd_file_path=setup$icd_file_path,
                                     atc_file_path=setup$atc_file_path,
                                     prs_dir_path=setup$prs_dir_path,
                                     phers_dir_path=setup$phers_dir_path,
                                     phers_study_descr=get_phers_file_descr(
                                                                    study_type=setup$study_type,
                                                                    obs_end_date=setup$obs_end_date,
                                                                    exp_len=setup$exp_len,
                                                                    wash_len=setup$wash_len,
                                                                    obs_len=setup$obs_len),
                                     zip_dir_path=setup$zip_dir_path)
    }
    res <- IHRC::run_surv_studies(pheno_data=data$pheno, 
                                  endpts_indvs_mat=get_endpts_indvs_mat(setup, data$pheno),
                                  icd_data=data$icd,
                                  atc_data=data$atc,
                                  prs_data=data$prs,
                                  phers_data=data$phers,
                                  zip_data=data$zip,
                                  score_type=setup$score_type,
                                  create_score_combos=setup$create_score_combos,
                                  study_type=setup$study_type,
                                  endpts=setup$endpts,
                                  exp_len=setup$exp_len,
                                  wash_len=setup$wash_len,
                                  obs_len=setup$obs_len,
                                  obs_end_date=setup$obs_end_date,
                                  down_fctr=setup$down_fctr,
                                  ancs=setup$ancs,
                                  obs_age_range=setup$obs_age_range,
                                  covs=setup$covs,
                                  filter_1998=setup$filter_1998,
                                  min_indvs=setup$min_indvs,
                                  write_res=TRUE,
                                  res_dir=setup$res_dir,
                                  res_descr=setup$res_descr,
                                  read_pheno_score_files=setup$read_pheno_score_files)
} 

#' Reads in the endpoint individuals selection files
#' 
#' @param setup A list. The setup information from the setup file.
#' @param pheno_data A data.frame. The phenotype data. Needs at least column `ID`.
#' 
#' @return A data.frame with the individuals that can be used for each endpoint. 
#'          Contains a column of individual IDs and a binary column for each endpoint.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_endpts_indvs_mat <- function(setup,
                                 pheno_data) {
    endpts_indvs_mat <- NULL
    if("FinnGen" %in% names(setup) & any(stringr::str_detect(setup$score_type, "PRS"))) {
        endpts_indvs_mat <- read_finngen_endpts_indvs_mat(pheno_data, setup$endpts, setup$FinnGen)
    }
    if(any(stringr::str_detect(setup$score_type, "PheRS"))) {
       endpts_indvs_mat <-  IUtils::read_phers_endpts_indvs_mat(
                                               setup$phers_dir_path,
                                               indvs_ids=pheno_data$ID,
                                               set_nas_true = TRUE,
                                               endpts=setup$endpts,
                                               study_descr=setup$phers_study_descr,
                                               prev_endpts_indvs_mat=endpts_indvs_mat)
    }
    return(endpts_indvs_mat)
}