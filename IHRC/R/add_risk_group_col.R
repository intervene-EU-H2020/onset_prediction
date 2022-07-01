
#' Adds a risk group column
#' 
#' The risk groups are quantiles based on the score distribution
#' of all individuals in the data. They are given as left-open
#' intervals i.e. first risk group could be an interval 0% <= x < 60%
#' and would be called `Group 60%`, the second risk group could then 
#' go from i.e. 60% <= x <= 80%  and would be indicated as `Group 80%`
#' etc. 
#' 
#' @param score_data A data.frame with at least column `SCORE`.
#' @param study An S4 class representing the study setup.
#' @inheritParams calc_studies_hrs
#' 
#' @return The data.frame with the added risk group column
#' 
#' @author Kira E. Detrois
add_risk_group_col <- function(score_data,
                               score_type,
                               study,
                               write_res=FALSE,
                               res_dir=NA) {
                                
    if(score_type != "CCI") {
        quantiles <- c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1)
        score_group_tbl <- get_score_groups(score_data, quantiles)
        indv_score_groups <- get_indvs_score_groups(score_data, score_group_tbl)
    } else {
        score_group_tbl <- c("low"="<=1", "high"= ">1")
        indv_score_groups <- get_two_level_groups(score_data)
    }
    score_data <- tibble::add_column(score_data, 
                                     SCORE_GROUP=indv_score_groups)

    write_score_groups_to_log(score_group_tbl,
                              score_type,
                              study,
                              write_res,
                              res_dir)
    return(score_data)
}

#' 
#' 
#' @inheritParams add_risk_group_col
#' @inheritParams check_has_mid_group
get_indvs_score_groups <- function(score_data, 
                                   score_group_tbl) {

    # Risk group left-open intervals for each individual
    indv_score_groups <- cut(score_data$SCORE,
                             breaks=score_group_tbl,
                             labels=get_group_labs(score_group_tbl),
                             include.lowest=TRUE, # Include last break
                             right=FALSE) # left-open intervals
    indv_score_groups <- droplevels(indv_score_groups)

    indv_score_groups <- stats::relevel(indv_score_groups, ref="[Group 40% - Group 60%)")

    return(indv_score_groups)
}

get_group_labs <- function(score_groups) {
    down_group <- paste("Group", names(score_groups[1:length(score_groups)-1]))
    down_group <- paste0("[", down_group)
    up_group <-  paste("Group", names(score_groups[2:length(score_groups)]))
    up_group[length(up_group)] <- paste0(up_group[length(up_group)], "]")
    up_group[1:(length(up_group)-1)] <- paste0(up_group[1:(length(up_group)-1)], ")")
    group_labs <- paste0(down_group, " - " , up_group)
}

get_two_level_groups <- function(score_data) {
    indv_score_groups <- factor(ifelse(score_data$SCORE <= 1,
                                       "<=1",
                                       ">1"),
                                levels=c("<=1", ">1"))
    return(indv_score_groups)
}

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
get_score_groups <- function(score_data, 
                             quantiles) {
    stats::quantile(score_data$SCORE, 
                    probs=quantiles,
                    na.rm=TRUE)
}