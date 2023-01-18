#' Preprocess score data
#' 
#' Preprocesses different score data, based 
#' on the selected scores and the current endpoint.
#' 
#' @param score_type a character (vector). The types of scores to be preprocessed. 
#'                      Available options include:
#'                      - "CCI" for Charlson comorbidity index, 
#'                      - "EI" for Elixhauser index, 
#'                      - "PRS" for polygenic risk score, 
#'                      - "PheRS" for PheWAS results score, 
#'                      - "MED" for medication data, and 
#'                      - "EDU" for educational level. The column for this needs to be
#'                               called ISCED_2011 in the `pheno_data`.
#' @param pheno_data A dataframe containing phenotypic data.
#' @param icd_data A dataframe containing ICD data. 
#'                  Required if "CCI" or "EI" is present in `score_type`
#' @param atc_data A dataframe containing ATC data. 
#'                  Required if "MED" is present in `score_type`
#' @param prs_data A dataframe containing PRS data. 
#'                  Required if "PRS" is present in `score_type`
#' @param phers_data A dataframe containing PheRS data. 
#'                      Required if "PheRS" is present in `score_type`
#' @param endpt A character. The current endpoint. 
#'                  Required if "PRS" or "PheRS" is present in `score_type`
#' 
#' @return A dataframe containing all the preprocessed scores specified in 
#'            `score_type`.
#' 
#' @export
#'
#' @author Kira E. Detrois
preprocess_score_data <- function(score_type,
                                  pheno_data,
                                  icd_data=NULL,
                                  atc_data=NULL,
                                  prs_data=NULL,
                                  phers_data=NULL,
                                  endpt=NULL,
                                  study_setup=NULL) {
    writeLines("Starting prep")
    score_data = NULL
    if("CCI" %in% score_type) {
        writeLines("Getting CCI")
        score_data <- get_study_cci_data(pheno_data,
                                         icd_data,
                                         score_type="CCI",
                                         study_setup)  
    } 
    if("EI" %in% score_type) {
        score_data <- get_study_cci_data(pheno_data,
                                         icd_data,
                                         score_type="CCI",
                                         study_setup)  
        if(!is.null(score_data)) {
            score_data <- dplyr::inner_join(score_data, 
                                           ei_data, 
                                           by="ID")
        } else {
            score_data <- ei_data
        }
    }
    # Adding PRS column
    if("PRS" %in% score_type) {
        writeLines("Getting PRS")
        PRS_data <- get_prs_endpt_data(score_data=prs_data,
                                       endpt=endpt)
        PRS_data <- na.omit(PRS_data)
        if(!is.null(score_data)) {
            score_data <- dplyr::inner_join(PRS_data, 
                                            score_data, 
                                            by="ID")
        } else {
            score_data <- PRS_data
        }
    }
    # Adding PheRS column
    if("PheRS" %in% score_type) {
        writeLines("Getting PheRS")
        PheRS_data <- get_phers_endpt_data(score_data=phers_data,
                                             endpt=endpt)
        PheRS_data <- na.omit(PheRS_data)
        if(!is.null(score_data)) {
            score_data <- dplyr::inner_join(PheRS_data, 
                                            score_data, 
                                            by="ID")
        } else {
            score_data <- PheRS_data
        }
    }
    if("MED" %in% score_type) {
        med_data <- get_study_med_data(pheno_data, atc_data)
        if(!is.null(score_data)) {
            score_data <- dplyr::inner_join(score_data,
                                            med_data,
                                            by="ID")
        } else {
            score_data <- med_data
        }
    }
    #' The education data comes from the phenotypic file
    if("EDU" %in% score_type) {
        writeLines("Getting Edu")
        edu_data <- get_study_edu_data(pheno_data)
        edu_data <- na.omit(edu_data)
        if(!is.null(score_data)) {
            score_data <- dplyr::inner_join(edu_data,
                                            score_data,
                                            by="ID")
        } else {
            score_data <- edu_data
        }
    }

    return(score_data)
}