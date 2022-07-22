#' Gets the risk score groups for each individual based on a single 
#' cutoff
#' 
#' @inheritParams run_surv_studies
#' @param cutoff A numeric. The score cutoff value.
#' 
#' @return A character. The group names.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_two_level_groups <- function(score_data,
                                 cutoff) {
    indv_score_groups <- factor(ifelse(score_data$CCI_SCORE <= cutoff,
                                       paste0("<=", cutoff),
                                       paste0(">", cutoff)),
                                levels=c(paste0("<=", cutoff), paste0(">", cutoff)))
    return(indv_score_groups)
}