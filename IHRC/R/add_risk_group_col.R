
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
                               study,
                               write_res=FALSE,
                               res_dir=NA) {
    quantiles <- c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1)
    score_group_tbl <- get_unique_score_groups(score_data, quantiles)
    if(check_has_mid_group(score_group_tbl)) {
        indv_score_groups <- get_indvs_score_groups(score_data, score_group_tbl)
    } else {
        score_group_tbl <- c("low"="<=1", "high"= ">1")
        indv_score_groups <- get_two_level_groups(score_data)
    }
    score_data <- tibble::add_column(score_data, 
                                     SCORE_GROUP=indv_score_groups)

    write_score_groups_to_log(score_group_tbl,
                              study,
                              write_res,
                              res_dir)
    return(score_data)
}

#' Checks whether there is a 40-60% group in the score groups
#' 
#' @param score_group_tbl A named character. 
#'                        The score cutoffs that should be written to the log.
#' 
#' @return A bollean. Indicates whether the group exists.
#' 
#' @author Kira E. Detrois
check_has_mid_group <- function(score_group_tbl) {
    if("60%" %in% names(score_group_tbl) &
        "40%" %in% names(score_group_tbl)) {
            return(TRUE)
    } else {
        return(FALSE)
    }
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
                                       "low",
                                       "high"),
                                levels=c("low", "high"))
    return(indv_score_groups)
}

#' Gets the percentage of each group character as a numeric value
#' 
#' @inheritParams relevel_to_mid_group
#' 
#' @return  A numeric. The unique risk groups as a numeric vector.
#' 
#' @author Kira E. Detrois
get_groups_numeric <- function(indv_score_groups) {
    names_split <- strsplit(levels(indv_score_groups), split=" ")
    unlist(lapply(names_split, function(str) {
                        as.numeric(sub("(.+)%$", "\\1", str[2]))
                  }))
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

#' Get's the score cutoff values with empty quantiles removed
#' 
#' @inheritParams get_score_groups
#' 
#' @return A named numeric vector with the quantiles as names 
#'          and score-cutoffs as values.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_unique_score_groups <- function(score_data,
                                    quantiles) {
    score_group_tbl <- get_score_groups(score_data, quantiles)
    score_group_tbl_slim <- slim_score_groups(score_group_tbl)
}

slim_score_groups <- function(score_group_tbl) {
    score_group_tbl_slim = c()
    prev_quant <- min(score_group_tbl)
    prev_name <- names(score_group_tbl)[1]
    score_group_tbl_slim[[prev_name]] <- prev_quant

    for(idx in seq(2, length(score_group_tbl))) {
        curnt_name <- names(score_group_tbl)[idx]
        if(prev_quant != score_group_tbl[[curnt_name]] ) {
            score_group_tbl_slim[[curnt_name]] <- score_group_tbl[[curnt_name]]
            prev_quant <- score_group_tbl[[curnt_name]]
            prev_name <- curnt_name
        } else if(idx == length(score_group_tbl)) {
            score_group_tbl_slim <- score_group_tbl_slim[which(names(score_group_tbl_slim) != prev_name)]
            score_group_tbl_slim[[curnt_name]] <- score_group_tbl[[curnt_name]]
        } 
    }
    return(unlist(score_group_tbl_slim))
}