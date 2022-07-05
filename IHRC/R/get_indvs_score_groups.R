#' Gets the risk score groups for each individual based on a score cutoff
#' table
#' 
#' @inheritParams add_risk_group_col
#' @inheritParams get_risk_group_labs
#' 
#' @return A factor. The risk score group for each individual.
#' 
#' @author Kira E. Detrois
get_indvs_score_groups <- function(score_data,
                                   score_group_tbl) {
    # Risk group left-open intervals for each individual
    indv_score_groups <- cut(score_data$SCORE,
                             breaks=score_group_tbl,
                             labels=get_risk_group_labs(score_group_tbl),
                             include.lowest=TRUE, # Include Group 0%
                             right=FALSE) # left-open intervals
    indv_score_groups <- stats::relevel(indv_score_groups, ref="(Group 40% - Group 60%]")

    return(indv_score_groups)
}
