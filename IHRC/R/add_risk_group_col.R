#' Adds a risk group column
#' 
#' For CCI gets two risk groups based on the cutoff value `bin_cut`.
#' The groups are then `<= bin_cut` and `>bin_cut`.
#' 
#' For PRS bases the risk grouping on the quantiles 
#' `c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1)` for the score
#' distribution of all individuals in the data. 
#' The resulting intervals are left-open, except the first one which
#' is both right- and left-open. I.e. the first risk group is an 
#' interval 0% <= x <= 1% and the second risk group is then the interval
#' 1% < x <= 5% etc. 
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
                               bin_cut=1,
                               write_res=FALSE,
                               res_dir=NA_character_) {
    if(score_type != "CCI") {
        indv_score_groups <- get_indvs_score_groups(score_data)
        write_score_groups_to_log(score_group_tbl,
                                  score_type,
                                  study,
                                  write_res,
                                  res_dir)
    } else {
        indv_score_groups <- get_two_level_groups(score_data, 
                                                  bin_cut)
    }
    score_data <- tibble::add_column(score_data, 
                                     SCORE_GROUP=indv_score_groups)
    return(score_data)
}

#' Gets the risk score groups for each individual based on a score cutoff
#' table
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A factor. The risk score group for each individual.
#' 
#' @author Kira E. Detrois
get_indvs_score_groups <- function(score_data) {
    quantiles <- c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1)
    score_group_tbl <- get_score_group_tbl(score_data, 
                                           quantiles)
    # Risk group left-open intervals for each individual
    indv_score_groups <- cut(score_data$SCORE,
                             breaks=score_group_tbl,
                             labels=get_group_labs(score_group_tbl),
                             include.lowest=TRUE, # Include Group 0%
                             right=FALSE) # left-open intervals
    indv_score_groups <- stats::relevel(indv_score_groups, ref="(Group 40% - Group 60%]")

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
get_score_group_tbl <- function(score_data, 
                                quantiles) {
    stats::quantile(score_data$SCORE, 
                    probs=quantiles,
                    na.rm=TRUE)
}

#' Creates the labels for each risk score group
#' 
#' @param score_group_tbl A named numeric. The risk score cuts for the 
#'                          different quantiles.
#' 
#' @return A character. The group names.
#' 
#' @author Kira E. Detrois
get_group_labs <- function(score_group_tbl) {
    down_group <- paste("Group", 
                        names(score_group_tbl[1:length(score_group_tbl)-1]))
    down_group[1] <- paste0("[", down_group[1])
    down_group[2:length(down_group)] <- paste0("(", down_group[2:length(down_group)])
    up_group <-  paste("Group", 
                        names(score_group_tbl[2:length(score_group_tbl)]))
    up_group <- paste0(up_group, "]")
    group_labs <- paste0(down_group, " - " , up_group)
}

#' Gets the risk score groups for each individual based on a single 
#' cutoff
#' 
#' @inheritParams calc_studies_hrs 
#' @param cutoff A numeric. The score cutoff value.
#' 
#' @return A character. The group names.
#' 
#' @author Kira E. Detrois
get_two_level_groups <- function(score_data,
                                 cutoff) {
    indv_score_groups <- factor(ifelse(score_data$SCORE <= cutoff,
                                       paste0("<=", cutoff),
                                       paste0(">", cutoff)),
                                levels=c(paste0("<=", cutoff), paste0(">", cutoff)))
    return(indv_score_groups)
}