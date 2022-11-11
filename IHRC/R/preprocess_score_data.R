preprocess_score_data <- function(score_type,
                                  study_data,
                                  icd_data=NULL,
                                  atc_data=NULL,
                                  prs_data=NULL,
                                  endpt=NULL) {
    score_data = NULL
    if("CCI" %in% score_type) {
        score_data <- get_study_CCI_data(study_data,
                                         icd_data,
                                         score_type="CCI")  
    } 
    if("EI" %in% score_type) {
        ei_data <- get_study_CCI_data(study_data,
                                      icd_data,
                                      score_type="EI")  
        if(!is.null(score_data)) {
            score_data <- dplyr::left_join(score_data, 
                                           ei_data, 
                                           by="ID")
        } else {
            score_data <- ei_data
        }
    }
    # Adding PRS column
    if("PRS" %in% score_type) {
        PRS_data <- get_prs_endpt_scores(score_data=prs_data,
                                               endpt=endpt)
        if(!is.null(score_data)) {
            score_data <- dplyr::left_join(score_data, 
                                           PRS_data, 
                                           by="ID")
        } else {
            score_data <- PRS_data
        }
    }
    if("MI" %in% score_type) {
        MI_data <- get_study_mi_data(study_data, atc_data)
        if(!is.null(score_data)) {
            score_data <- dplyr::left_join(score_data,
                                           MI_data)
        } else {
            score_data <- MI_data
        }
    }

    return(score_data)
}