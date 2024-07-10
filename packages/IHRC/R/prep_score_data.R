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
                                  error_file=NULL,
                                  write_progress=FALSE) {
    
    score_data = NULL

    age_temp <- dplyr::select(pheno_data, ID, SEX, DATE_OF_BIRTH, matches("PC"))
    age_temp$YEAR_OF_BIRTH <- lubridate::year(age_temp$DATE_OF_BIRTH)
    age_temp$SEX <- as.factor(age_temp$SEX)
    score_data <- dplyr::select(age_temp, ID, SEX, YEAR_OF_BIRTH, matches("PC"))

    score_data <- add_phers_endpt_data(score_data=score_data,
                                       score_type=score_type,
                                       phers_data=phers_data,
                                       endpt=endpt)
    score_data <- add_prs_endpt_data(score_data=score_data,
                                     score_type=score_type,
                                     prs_data=prs_data,
                                     endpt=endpt)
    score_data <- add_edu_cont_data(score_data=score_data,
                                    score_type=score_type,
                                    pheno_data=pheno_data)
    score_data <- add_cci_data(score_data=score_data,
                               score_type=score_type,
                               pheno_data=pheno_data,
                               icd_data=icd_data,
                               study_setup=study_setup)

    if(!all(score_type[!(score_type %in% c("BMI", "EDU_UKBB", "EDUCATION_11", "EDUCATION_97", "EDU", "ZIP", "SMOKING"))] %in% colnames(score_data))) {
        missing_score <- score_type[!(score_type %in% c("EDU_UKBB", "EDUCATION_11","EDUCATION_97", "EDU",  "ZIP", "BMI", "SMOKING")) & !(score_type %in% colnames(score_data))]
        write_to_error_file(error_file, paste0("Something went wrong when getting the score data for endpoint ", endpt, ". Missing ", paste0(missing_score, collapse=", "), " data.\n"))
        score_data <- NULL
    }

    score_data <- dplyr::select(score_data, -SEX, -YEAR_OF_BIRTH, -matches("PC"))
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
        } 
        score_data <- dplyr::filter(score_data, !is.na(CCI), !is.na(PheRS), !is.na(YEAR_OF_BIRTH), !is.na(PC1), !is.na(PC2), !is.na(PC3), !is.na(PC4), !is.na(PC5), !is.na(PC6), !is.na(PC7), !is.na(PC8), !is.na(PC9), !is.na(PC10), !is.na(SEX))

        score_data$CCI[is.na(score_data$CCI)] <- 0

        score_data$CCI_orig <- score_data$CCI

        if(length(unique(score_data$SEX)) > 1) {
            score_data$CCI <- scale(residuals(glm(scale(CCI_orig)~scale(YEAR_OF_BIRTH)+SEX+scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10), data=score_data)))[,1]
        } else {
            score_data$CCI <- scale(residuals(glm(scale(CCI_orig)~scale(YEAR_OF_BIRTH)+scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10), data=score_data)))[,1]
        }

        #Assign PRS into percentiles
        q <- quantile(score_data$CCI, probs=c(0,0.9,1), na.rm=TRUE)

        score_data["CCI_group"] <- cut(score_data$CCI, q, include.lowest=TRUE, labels=c("0", "1"))
        #score_data["CCI_group"] <- ifelse(score_data$CCI < 2, "0", "1")
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
        } 
        score_data <- dplyr::filter(score_data, !is.na(PRS), !is.na(PheRS), !is.na(YEAR_OF_BIRTH), !is.na(PC1), !is.na(PC2), !is.na(PC3), !is.na(PC4), !is.na(PC5), !is.na(PC6), !is.na(PC7), !is.na(PC8), !is.na(PC9), !is.na(PC10), !is.na(SEX))

        score_data$PRS_orig <- score_data$PRS

        if(length(unique(score_data$SEX)) > 1) {
            score_data$PRS <- scale(residuals(glm(scale(PRS_orig)~scale(YEAR_OF_BIRTH)+SEX+scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10), data=score_data)))[,1]
        } else {
            score_data$PRS <- scale(residuals(glm(scale(PRS_orig)~scale(YEAR_OF_BIRTH)+scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10), data=score_data)))[,1]
        }

        #Assign PRS into percentiles
        p<-c(0,0.1,0.2,0.4)
        q <- quantile(score_data$PRS, probs=c(p,rev(1-p)), na.rm=TRUE)

        score_data["PRS_group"] <- cut(score_data$PRS, q, include.lowest=TRUE, labels=paste(1:(2*length(p)-1)))
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
    if(("PheRS" %in% score_type)) {
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
        } 
        score_data <- dplyr::filter(score_data, !is.na(PheRS), !is.na(YEAR_OF_BIRTH), !is.na(PC1), !is.na(PC2), !is.na(PC3), !is.na(PC4), !is.na(PC5), !is.na(PC6), !is.na(PC7), !is.na(PC8), !is.na(PC9), !is.na(PC10), !is.na(SEX))
        score_data$PheRS_orig <- score_data$PheRS

        if(length(unique(score_data$SEX)) > 1) {
            score_data$PheRS <- scale(residuals(glm(scale(PheRS_orig)~scale(YEAR_OF_BIRTH)+SEX+scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10), data=score_data)))[,1]
        } else {
            score_data$PheRS <- scale(residuals(glm(scale(PheRS_orig)~scale(YEAR_OF_BIRTH)+scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10), data=score_data)))[,1]
        }

        #Assign PheRS into percentiles  
        p<-c(0,0.1,0.2,0.4)
        q <- quantile(score_data$PheRS, probs=c(p,rev(1-p)), na.rm=TRUE)

        score_data["PheRS_group"] <- cut(score_data$PheRS, q, include.lowest=TRUE, labels=paste(1:(2*length(p)-1)))
    }

    # Repeat for transfer
    if(("PheRS_transfer" %in% score_type)) {
        PheRS_data <- get_phers_endpt_data(score_data=phers_data,
                                           endpt=endpt,
                                           transfer=TRUE)
        if(!is.null(score_data) & !is.null(PheRS_data)) {
            new_score_data <- dplyr::full_join(PheRS_data, 
                                               score_data, 
                                               by="ID")
        } else {
            new_score_data <- PheRS_data
        }
        if(!is.null(new_score_data)) {
            score_data <- new_score_data
        }

        score_data <- dplyr::filter(score_data, !is.na(PheRS_transfer))
        #Assign PheRS into percentiles  
        p<-c(0,0.1,0.2,0.4)
        q <- quantile(score_data$PheRS_transfer, probs=c(p,rev(1-p)), na.rm=TRUE)

        score_data["PheRS_transfer_group"] <- cut(score_data$PheRS_transfer, q, include.lowest=TRUE, labels=paste(1:(2*length(p)-1)))
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
        } 
        score_data <- dplyr::filter(score_data, !is.na(EDU_cont), !is.na(PheRS), !is.na(YEAR_OF_BIRTH), !is.na(SEX))

        score_data$EDU_cont_orig <- score_data$EDU_cont
        if(length(unique(score_data$SEX)) > 1) {
            score_data$EDU_cont <- scale(residuals(glm(scale(EDU_cont_orig)~scale(YEAR_OF_BIRTH)+SEX+scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10), data=score_data)))[,1]
        } else {
            score_data$EDU_cont <- scale(residuals(glm(scale(EDU_cont_orig)~scale(YEAR_OF_BIRTH)+scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10), data=score_data)))[,1]
        }

        #Assign Education into percentiles
        q <- quantile(score_data$EDU_cont, probs=c(0,0.6,1), na.rm=TRUE)

        score_data["EDU_cont_group"] <- cut(score_data$EDU_cont, q, include.lowest=TRUE, labels=c("0", "1"))
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
        }
    }
    return(score_data)
}