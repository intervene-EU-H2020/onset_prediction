#' Joins the two data.frames 
#' 
#' Joins the two data.frames and removes any individuals without
#' a score available.
#' 
#' @inheritParams calc_studies_hrs
#' @inheritParams run_coxph_ana
#' 
#' @return The joined data.frame
#' 
#' @export 
#' 
#' @author Kira E. Detrois
join_dfs <- function(pheno_data,
                     score_data,
                     score_type="CCI",
                     endpt=NA_character_) {
    score_data <- get_and_filter_endpt_scores(score_data, score_type, endpt)
    pheno_score_data <- dplyr::left_join(pheno_data,
                                         score_data,
                                         by="ID")
    pheno_score_data <- dplyr::filter(pheno_score_data, !is.na(SCORE))
    return(pheno_score_data)
}