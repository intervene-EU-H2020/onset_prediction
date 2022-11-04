#' Joins the two data.frames 
#' 
#' Joins the two data.frames and removes any individuals without
#' a score available.
#' 
#' @inheritParams run_surv_studies
#' @inheritParams get_n_group_cases
#' 
#' @return The joined data.frame
#' 
#' @export 
#' 
#' @author Kira E. Detrois
join_dfs <- function(study_data,
                     score_data,
                     score_type="CCI",
                     endpt=NULL) {
    elig_score_data <- dplyr::left_join(x=study_data,
                                        y=score_data,
                                        by="ID")
    if("CCI" %in% score_type) {
        elig_score_data$CCI[is.na(elig_score_data$CCI)] <- 0
    } else if("EI" %in% score_type) {
        elig_score_data$EI[is.na(elig_score_data$EI)] <- 0
    } 
    return(elig_score_data)
}