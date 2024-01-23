
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
    if(("FinnGen" %in% names(setup)) & any(stringr::str_detect(setup$score_type, "PRS"))) {
        endpts_indvs_mat <- read_finngen_endpts_indvs_mat(pheno_data, setup$endpts, setup$FinnGen)
    }
    if(any(stringr::str_detect(setup$score_type, "(PheRS)|(ZIP_prob)")) & !setup$trained_external) {
       endpts_indvs_mat <-  IUtils::read_phers_endpts_indvs_mat(
                                               setup$phers_dir_path,
                                               indvs_ids=pheno_data$ID,
                                               set_nas_true = TRUE,
                                               endpts=setup$endpts,
                                                       study_descr=IUtils::get_phers_file_descr(
                                                        study_type = setup$study_type,
                                                        obs_end_date = setup$obs_end_date,
                                                        exp_len = setup$exp_len,
                                                        wash_len = setup$wash_len,
                                                        obs_len = setup$obs_len),
                                               prev_endpts_indvs_mat=endpts_indvs_mat)
    }
    return(endpts_indvs_mat)
}