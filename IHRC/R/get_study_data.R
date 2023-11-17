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
                                  min_indvs=min_indvs,
                                  error_file=NULL,
                                  write_progress=FALSE) {
                                  # Get data of individuals who can be used for the current endpoint
    pheno_data <- get_relevant_pheno_data_cols(pheno_data, endpt)
    print(colnames(pheno_data))
    # Need to preprocess score data for these score types
    if(any(stringr::str_detect(score_type, "(PRS)|(EI)|(PheRS)|(PheRS_transfer)|(MED)|(EDU_cont)|(Prob)"))) {
        score_data <- preprocess_score_data(score_type=score_type, 
                                            pheno_data=pheno_data,
                                            icd_data=icd_data, 
                                            atc_data=atc_data,
                                            prs_data=prs_data,
                                            phers_data=phers_data,
                                            zip_data=zip_data,
                                            endpt=endpt,
                                            study_setup=study_setup,
                                            error_file=error_file,
                                            write_progress=write_progress)
        if(!is.null(score_data)) {
            # Joining score data with phenotypic data
            study_data <- join_dfs(pheno_data=pheno_data,
                                   score_data=score_data,
                                   score_type=score_type)
            if(write_progress) writeLines("Joined score and pheno data.")
        } else {
            warning("Something went wrong with getting the score data for endpoint", endpt, ".")
            study_data <- NULL
        }
    # Otherwise only need the phenotype data
    } else {
        study_data <- pheno_data
    }
    if(!is.null(study_data)) {
        if(endpt %in% colnames(endpts_indvs_mat)) {
            train_status <- dplyr::select(endpts_indvs_mat, ID, endpt)
            colnames(train_status) <- c("ID", "TRAIN_STATUS")
            train_status <- dplyr::mutate(train_status, TRAIN_STATUS=!TRAIN_STATUS)
            study_data <- dplyr::left_join(study_data, train_status, by="ID")
            study_data$TRAIN_STATUS[is.na(study_data$TRAIN_STATUS)] <- 0
        } else {
            study_data <- dplyr::mutate(study_data, TRAIN_STATUS=0)
        }
    }
    if(write_progress) writeLines("Added training status.")
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
    select_cols <- c("ID", 
                     "SEX", 
                     "DATE_OF_BIRTH", 
                     "ANCESTRY", 
                     endpt,  
                     paste0(endpt, "_DATE"),
                     "END_OF_FOLLOWUP")
    if(("BATCH" %in% colnames(pheno_data))) {
        select_cols <- c(select_cols, "BATCH")
    }
    if(("CCI" %in% colnames(pheno_data))) {
        select_cols <- c(select_cols, "CCI")
    }
    if(("CHIP" %in% colnames(pheno_data))) {
        select_cols <- c(select_cols, "CHIP")
    }
    if(("ZIP" %in% colnames(pheno_data))) {
        select_cols <- c(select_cols, "ZIP")
    }
    if(("EDU" %in% colnames(pheno_data))) {
        select_cols <- c(select_cols, "EDU")
    }
    if(("BMI" %in% colnames(pheno_data))) {
        select_cols <- c(select_cols, "BMI")
    }
    if(("SMOKING" %in% colnames(pheno_data))) {
        select_cols <- c(select_cols, "SMOKING")
    }
    if(("EDU_cont" %in% colnames(pheno_data))) {
        select_cols <- c(select_cols, "EDU_cont")
    }
    if(("Prob" %in% colnames(pheno_data))) {
        select_cols <- c(select_cols, "Prob")
    }
    pheno_data <- dplyr::select(pheno_data, all_of(select_cols), dplyr::starts_with("PC"))
    return(pheno_data)
}