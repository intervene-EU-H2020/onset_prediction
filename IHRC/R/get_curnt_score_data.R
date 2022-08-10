#' Gets the score data for the survival analysis setup
#' 
#' Should be run before any analyis on the survival setup is run.
#' 
#' For `CCI` calculates the CCI score based on the ICD-data.
#' Also, renames the score column to `SCORE` and filters out NAs.
#' For the PRS data the score columsn have names in the
#' form of `J10_ASTHMA_PRS`. The function renames the
#' current PRS column of interest to `SCORE`. Then it
#' filters out all NAs in the column. See function 
#' \code{\link{get_prs_endpt_scores}}.
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return The data.frame with the score data for all eligible individuals
#'          under the study setup of the current survival analysis setup.
#' 
#' @author Kira E. Detrois
get_curnt_score_data <- function(surv_ana) {
    curnt_score_data <- surv_ana@elig_score_data
    # Adding CCI_SCORE column
    if("CCI" %in% surv_ana@score_type) {
        score_data <- get_study_cci_scores(surv_ana@study@study_data,
                                           curnt_score_data)  
    } 
    # Adding PRS_SCORE column
    if("PRS" %in% surv_ana@score_type) {
        curnt_score_data <- get_prs_endpt_scores(   
                                    score_data=surv_ana@elig_score_data,
                                    endpt=surv_ana@study@endpt)
        if("CCI" %in% surv_ana@score_type) {
            score_data <- dplyr::left_join(score_data, 
                                           curnt_score_data, 
                                           by="ID")
        } else {
            score_data <- curnt_score_data
        }
    }

    return(score_data)
}