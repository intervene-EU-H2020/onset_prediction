#' Preprocess score data
#' 
#' Preprocesses different score data, based 
#' on the selected score types and the current endpoint.
#' 
#' @inheritParams run_surv_studies
#' @param endpt A string. The current endpoint. 
#'                  Required if `PRS` or `PheRS` is present in `score_type`.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param error_file A string. The path to the error file. 
#' 
#' @return A data.frame containing all the preprocessed scores specified in 
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
                                  zip_data=NULL,
                                  endpt=NULL,
                                  study_setup=NULL,
                                  error_file=NULL) {
    
    score_data = NULL
    score_data <- add_cci_data(score_data=score_data,
                               score_type=score_type,
                               pheno_data=pheno_data,
                               icd_data=icd_data,
                               study_setup=study_setup)
    score_data <- add_prs_endpt_data(score_data=score_data,
                                     score_type=score_type,
                                     prs_data=prs_data,
                                     endpt=endpt)
    score_data <- add_phers_endpt_data(score_data=score_data,
                                       score_type=score_type,
                                       phers_data=phers_data,
                                       endpt=endpt)
    score_data <- add_med_endpt_data(score_data=score_data,
                                     score_type=score_type,
                                     pheno_data=pheno_data,
                                     study_setup=study_setup,
                                     atc_data=atc_data)
    score_data <- add_edu_cont_data(score_data=score_data,
                                    score_type=score_type,
                                    pheno_data=pheno_data)
    score_data <- add_prob_data(score_data=score_data,
                                    score_type=score_type,
                                    pheno_data=pheno_data)
    if(!all(score_type[!(score_type %in% c("EDU", "ZIP"))] %in% colnames(score_data))) {
        missing_score <- score_type[!(score_type %in% c("EDU", "ZIP")) & !(score_type %in% colnames(score_data))]
        write_to_error_file(error_file, paste0("Something went wrong when getting the score data for endpoint ", endpt, ". Missing ", paste0(missing_score, collapse=", "), " data.\n"))
        score_data <- NULL
    }

    return(score_data)
}

#' Adds the CCI score endpoint data to the score data
#' 
#' @param score_data A data.frame. The current score data for the study setup.
#' @inheritParams preprocess_score_data
#' 
#' @return A tibble. The score data with added columns `CCI`, or `EI` depending 
#'          on the `score_type` used. 
#' 
#' @author Kira E. Detrois
#' 
#' @export 
add_cci_data <- function(score_data,
                         score_type,
                         pheno_data,
                         icd_data,
                         study_setup) {
    if("CCI" %in% score_type) {
        cci_data <- get_study_cci_data(pheno_data,
                                       icd_data,
                                       score_type="CCI",
                                       study_setup) 
        if(!is.null(score_data) & !is.null(cci_data)) {
            new_score_data <- dplyr::full_join(score_data, cci_data, by="ID")
        } else {
            new_score_data <- cci_data
        }
        if(!is.null(new_score_data)) {
            score_data <- new_score_data
        } else {
            stop("Something went wrong when getting the CCI data.")
        }
    }     
    if("EI" %in% score_type) {
        ei_data <- get_study_cci_data(pheno_data,
                                      icd_data,
                                      score_type="CCI",
                                      study_setup)  
        if(!is.null(score_data) & !is.null(ei_data)) {
            new_score_data <- dplyr::full_join(score_data, ei_data, by="ID")
        } else {
            new_score_data <- ei_data
        }
        if(!is.null(new_score_data)) {
            score_data <- new_score_data
        }
    }
    return(score_data)
}

#' Adds the PRS endpoint data to the score data
#' 
#' @param score_data A data.frame. The current score data for the study setup.
#' @inheritParams preprocess_score_data
#' 
#' @return A tibble. The score data with added column `PRS`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
add_prs_endpt_data <- function(score_data,
                               score_type,
                               prs_data,
                               endpt) {
    if("PRS" %in% score_type) {
        
        PRS_data <- get_prs_endpt_data(score_data=prs_data,
                                       endpt=endpt)
        if(!is.null(score_data) & !is.null(PRS_data)) {
            new_score_data <- dplyr::full_join(PRS_data, score_data, by="ID")
        } else {
            new_score_data <- PRS_data
        }
        if(!is.null(new_score_data)) {
            score_data <- new_score_data
        } else {
            warning("Something went wrong when getting the PRS data for endpoint ", endpt, ".")
        }
    }
    return(score_data)
}

#' Adds the PheRS endpoint data to the score data
#' 
#' @param score_data A data.frame. The current score data for the study setup.
#' @inheritParams preprocess_score_data
#' 
#' @return A tibble. The score data with added column `PheRS`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
add_phers_endpt_data  <- function(score_data,
                                  score_type,
                                  phers_data,
                                  endpt) {
    if("PheRS" %in% score_type) {
        PheRS_data <- get_phers_endpt_data(score_data=phers_data,
                                           endpt=endpt)
        if(!is.null(score_data) & !is.null(PheRS_data)) {
            new_score_data <- dplyr::full_join(PheRS_data, 
                                               score_data, 
                                               by="ID")
        } else {
            new_score_data <- PheRS_data
        }
        if(!is.null(new_score_data)) {
            score_data <- new_score_data
        } else {
            warning("Something went wrong when getting the PheRS data for endpoint ", endpt, ".")
        }
    }
    return(score_data)
}

#' Adds the medication score endpoint data to the score data
#' 
#' @param score_data A data.frame. The current score data for the study setup.
#' @inheritParams preprocess_score_data
#' 
#' @return A tibble. The score data with added column `MED`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
add_med_endpt_data <- function(score_data,
                               score_type,
                               pheno_data,
                               study_setup,
                               atc_data) {
    if("MED" %in% score_type) {
        med_data <- get_study_med_data(pheno_data, study_setup, atc_data)
        if(!is.null(score_data) & !is.null(med_data)) {
            new_score_data <- dplyr::full_join(score_data, med_data, by="ID")
        } else {
            new_score_data <- med_data
        }
        if(!is.null(new_score_data)) {
            score_data <- new_score_data
        } else {
            stop("Something went wrong when getting the medication data.")
        }
    }
    return(score_data)
}

#' Adds the education data to the score data
#' 
#' The education data comes from the phenotype data and gets
#' mapped from ISCED 2011 to the FinnGen R10 age modes, see
#' function [IHRC::get_edu_cont_data].
#' 
#' @param score_data A data.frame. The current score data for the study setup.
#' @inheritParams preprocess_score_data
#' 
#' @return A tibble. The score data with added column `EDU`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
add_edu_cont_data <- function(score_data,
                         score_type,
                         pheno_data) {
    # The education data comes from the phenotypic file
    if("EDU_cont" %in% score_type) {
        edu_data <- get_edu_cont_data(pheno_data)
        if(!is.null(score_data) & !is.null(edu_data)) {
            new_score_data <- dplyr::full_join(edu_data, score_data, by="ID")
        } else {
            new_score_data <- edu_data
        }
        if(!is.null(new_score_data)) {
            score_data <- new_score_data
        } else {
            stop("Something went wrong when getting the education data.")
        }
    }
    return(score_data)
}

#' Adds the education data to the score data
#' 
#' The education data comes from the phenotype data and gets
#' mapped from ISCED 2011 to the FinnGen R10 age modes, see
#' function [IHRC::get_edu_cont_data].
#' 
#' @param score_data A data.frame. The current score data for the study setup.
#' @inheritParams preprocess_score_data
#' 
#' @return A tibble. The score data with added column `EDU`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
add_prob_data <- function(score_data,
                          score_type,
                          pheno_data) {
    if("Prob" %in% score_type) {
        prob_data <- get_prob_data(score_data=score_data, endpt=endpt)
        if(!is.null(score_data) & !is.null(prob_data)) {
            new_score_data <- dplyr::full_join(prob_data, score_data, by="ID")
        } else {
            new_score_data <- prob_data
        }
        if(!is.null(new_score_data)) {
            score_data <- new_score_data
        }else {
            stop("Something went wrong when getting the probability data.")
        }
    }
    return(score_data)
}