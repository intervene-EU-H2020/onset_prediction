preprocess_score_data <- function(score_type,
                                  study_data,
                                  icd_data=NULL,
                                  prs_data=NULL,
                                  endpt=NULL) {
    if("CCI" %in% score_type) {
        score_data <- get_study_cci_scores(study_data,
                                           icd_data)  
    } 
    # Adding PRS_SCORE column
    if("PRS" %in% score_type) {
        prs_score_data <- get_prs_endpt_scores(score_data=prs_data,
                                               endpt=endpt)
        if("CCI" %in% score_type) {
            score_data <- dplyr::left_join(score_data, 
                                           prs_score_data, 
                                           by="ID")
        } else {
            score_data <- prs_score_data
        }
    }

    return(score_data)
}