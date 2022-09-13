preprocess_score_data <- function(score_type,
                                  study_data,
                                  icd_data=NULL,
                                  prs_data=NULL,
                                  endpt=NULL) {
    if("CCI" %in% score_type) {
        score_data <- get_study_CCI_data(study_data,
                                           icd_data)  
    } 
    # Adding PRS column
    if("PRS" %in% score_type) {
        PRS_data <- get_prs_endpt_scores(score_data=prs_data,
                                               endpt=endpt)
        if("CCI" %in% score_type) {
            score_data <- dplyr::left_join(score_data, 
                                           PRS_data, 
                                           by="ID")
        } else {
            score_data <- PRS_data
        }
    }

    return(score_data)
}