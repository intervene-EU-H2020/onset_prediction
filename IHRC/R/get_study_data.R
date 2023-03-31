#' Get Phenotype and Score Data 
#' 
#' Extract the phenotype and score data of eligible cases and controls for 
#' the Cox-PH analysis of a given study.
#' 
#' If score type includes `CCI`, `PRS`, `EI`, `PheRS`, or `MED`, uses
#' function [IHRC::preprocess_score_data] to generate a data.frame with the required 
#' score data, and then joins the resulting score data with the phenotypic data
#' on the eligible individuals, using the function [IHRC::join_dfs].
#'  
#' It filters the study data to include only endpoints with both
#' cases and controls that have at least a minimum number of individuals. 
#' Defined in the `surv_ana` object.
#' 
#' @inheritParams run_surv_studies
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param error_file A string. The path to the error file. 
#' @param endpt A charcter. The current endpoint.
#' 
#' @return A tibble with the phenotype and score data for all eligible individuals
#'          under the current study and  survival analysis setup.
#'          Returns `NULL` if there is not enough eligible individuals defined 
#'          by `min_indvs` and if there is something wrong with the `score_data`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_pheno_score_data  <- function(score_type,
                                  pheno_data,
                                  study_setup,
                                  endpts_indvs_mat=NULL,
                                  icd_data=NULL,
                                  atc_data=NULL,
                                  prs_data=NULL,
                                  phers_data=NULL,
                                  zip_data=NULL,
                                  endpt=NULL,
                                  min_indvs=5,
                                  error_file=NULL) {
    # Get data of individuals who can be used for the current endpoint
    pheno_data <- get_crnt_endpt_data(endpts_indvs_mat=endpts_indvs_mat, 
                                      endpt=endpt, 
                                      pheno_data=pheno_data,
                                      error_file=error_file)
    pheno_data <- get_relevant_pheno_data_cols(pheno_data, endpt)

    # Need to preprocess score data for these score types
    if(any(stringr::str_detect(score_type, "(CCI)|(PRS)|(EI)|(PheRS)|(MED)|(EDU)|(ZIP)"))) {
        score_data <- preprocess_score_data(score_type=score_type, 
                                            pheno_data=pheno_data,
                                            icd_data=icd_data, 
                                            atc_data=atc_data,
                                            prs_data=prs_data,
                                            phers_data=phers_data,
                                            zip_data=zip_data,
                                            endpt=endpt,
                                            study_setup=study_setup,
                                            error_file=error_file)
        if(!is.null(score_data)) {
            # Joining score data with phenotypic data
            study_data <- join_dfs(pheno_data=pheno_data,
                                   score_data=score_data,
                                   score_type=score_type)
        } else {
            study_data <- NULL
        }
    # Otherwise only need the phenotype data
    } else {
        study_data <- pheno_data
    }
    #if(!is.null(study_data)) {
    #    # Only complete cases needed for analyses anyways
    #    study_data <- study_data[stats::complete.cases(dplyr::select(study_data, -paste0(endpt, "_DATE"))),]
    #}

    return(study_data)
}

#' Selects relevant columns from the phenotype data
#' 
#' To decrease the size of downstream joins we do not
#' need all the original columns.
#' 
#' @param pheno_data A data.frame. The phenotype data. 
#' @param endpt A string. The current endpoint.
#' 
#' @return A tibble the relevant columns from the phenotype data.
#'          \itemize{
#'              \item `ID` 
#'              \item `SEX`
#'              \item `DATE_OF_BIRTH`
#'              \item `ANCESTRY`
#'              \item The current endpoint i.e. `J10_ASTHMA`
#'              \item The date for the current endpoint i.e. `J10_ASTHMA_DATE`
#'              \item `END_OF_FOLLOWUP`
#'              \item All principal components 
#'              \item `BATCH`
#'              \item `EDU`
#'          }
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_relevant_pheno_data_cols <- function(pheno_data,
                                         endpt) {
    print(colnames(pheno_data))
    if(("ZIP" %in% colnames(pheno_data)) & ("EDU" %in% colnames(pheno_data))) {
        pheno_data <- dplyr::select(pheno_data,
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
                                    ZIP,
                                    EDU)
    } else if("EDU" %in% colnames(pheno_data)) {
                pheno_data <- dplyr::select(pheno_data,
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
                                    EDU)
    } else if("ZIP" %in% colnames(pheno_data)) {
                pheno_data <- dplyr::select(pheno_data,
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
                                    ZIP)
    }

    return(pheno_data)
}