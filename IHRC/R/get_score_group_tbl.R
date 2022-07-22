#' Get's the score cutoff values 
#' 
#' @param score_data A data.frame with at least column `SCORE`.
#' @param quantiles A numeric. The probabilities for the quantiles.
#' 
#' @return A named numeric vector with the quantiles as names 
#'          and score-cutoffs as values.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_score_group_tbl <- function(score_data, 
                                score_type,
                                quantiles) {
    stats::quantile(x=dplyr::pull(score_data, paste0(score_type, "_SCORE")), 
                    probs=quantiles,
                    na.rm=TRUE)
}