#' Joins the two data.frames 
#' 
#' Joins the two data.frames and removes any individuals without
#' a score available.
#' 
#' @param pheno_data A data.frame. Left of the join. Needs at least column
#'                      `SCORE`.
#' @param score_data A data.frame. Left of the join. Needs at least column
#'                      `SCORE`.
#' @inheritParams get_coxph_mdl
#' 
#' @return The joined data.frame
#' 
#' @export 
#' 
#' @author Kira E. Detrois
join_dfs <- function(pheno_data,
                     score_data,
                     score_type="CCI",
                     endpt=NULL) {
    score_data <- get_and_filter_endpt_scores(score_data, 
                                              score_type, 
                                              endpt)
    pheno_score_data <- dplyr::left_join(x=pheno_data,
                                         y=score_data,
                                         by="ID")
    pheno_score_data <- dplyr::filter(pheno_score_data, !is.na(SCORE))
    return(pheno_score_data)
}