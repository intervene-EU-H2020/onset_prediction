#' Extract Eligibility Score Data 
#' 
#' Extract the score and phenotype data of eligible cases and controls for 
#' the Cox-PH analysis of a given endpoint.
#' 
#' If score type includes `CCI`, `PRS`, `EI`, `PheRS`, or `MED`, it also 
#' calls [IHRC::preprocess_score_data] to generate a data frame with the required 
#' score data, and then joins the resulting score data with the phenotypic data
#' on the eligible individuals, using the function [IHRC::join_dfs].
#'  
#' It filters the study data to include only endpoints with both
#' cases and controls that have at least a minimum number of individuals. 
#' 
#' 
#' @return The data.frame with the score data for all eligible individuals
#'          under the study setup of the current survival analysis setup.
#'          Otherwise returns NULL. Also returns NULL if there is something 
#'          wrong with the `score_data`.
#' 
#' @examples
#' get_pheno_score_data("CCI", pheno_data, icd_data, endpt="J10_ASTHMA")
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_pheno_score_data  <- function(score_type,
                            pheno_data,
                            study_setup,
                            endpts_indvs_mat,
                            icd_data=NULL,
                            atc_data=NULL,
                            prs_data=NULL,
                            phers_data=NULL,
                            endpt=NULL,
                            min_indvs=5) {
    # Get data of individuals who can be used for the current endpoint
    pheno_data <- get_crnt_endpt_data(endpts_indvs_mat=endpts_indvs_mat, 
                                      endpt=endpt, 
                                      pheno_data=pheno_data)
    pheno_data <- get_relevant_study_data_cols(pheno_data, endpt)
    pheno_data <- pheno_data[complete.cases(dplyr::select(pheno_data, -paste0(endpt, "_DATE"))),]

    # Need to preprocess score data for these score types
    if(any(stringr::str_detect(score_type, "(CCI)|(PRS)|(EI)|(PheRS)|(MED)|(EDU)"))) {
        score_data <- preprocess_score_data(score_type=score_type, 
                                            pheno_data=pheno_data,
                                            icd_data=icd_data, 
                                            atc_data=atc_data,
                                            prs_data=prs_data,
                                            phers_data=phers_data,
                                            endpt=endpt,
                                            study_setup=study_setup)
        if(!is.null(score_data)) {
            # Joining score data with phenotypic data
            study_data <- join_dfs(pheno_data=pheno_data,
                                   score_data=score_data,
                                   score_type=score_type)
            writeLines("Joined df")
        } 
    } else {
        study_data <- pheno_data
    }
    return(study_data)
}

get_relevant_study_data_cols <- function(study_data,
                                         endpt) {
    study_data <- dplyr::select(study_data,
                                ID, 
                                SEX, 
                                DATE_OF_BIRTH, 
                                ANCESTRY, 
                                # Otherwise dplyr will throw error. 
                                # test_endpt_input_correct already 
                                # checks that this is only a single 
                                # string and not a vector.
                                all_of(endpt),  
                                paste0(endpt, "_DATE"),
                                END_OF_FOLLOWUP,
                                dplyr::starts_with("PC"),
                                BATCH,
                                ISCED_2011)
    return(study_data)
}