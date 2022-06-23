
#' Adds a risk group column
#' 
#' The risk groups are quantiles based on the score distribution
#' of all individuals in the data. They are given as left-open
#' intervals I.e. first risk group could be an interval 0% <= x < 60%
#' and would be called `Group 60%`, the second risk group could then 
#' go from i.e. 60% <= x <= 80%  and would be indicated as `Group 80%`
#' etc. 
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return The data.frame with the added risk group column
#' 
#' @author Kira E. Detrois
add_risk_group_col <- function(pheno_score_data) {
    score_groups <- get_unique_score_groups(pheno_score_data)
    group_labs <- paste("Group", names(score_groups[2:length(score_groups)]))
    # Risk group lef-open intervals for each individual
    indv_score_groups <- cut(pheno_score_data$SCORE,
                             breaks=score_groups,
                             labels=group_labs,
                             include.lowest=TRUE, # Include last break
                             right=FALSE) # left-open intervals
    indv_score_groups <- relevel_to_middle(indv_score_groups)
    pheno_score_data <- tibble::add_column(pheno_score_data, 
                                    SCORE_GROUP=indv_score_groups)
    return(pheno_score_data)
}

#' Gets the percentage of each group character as a numeric value
#' 
#' @inheritParams relevel_to_middle
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

get_unique_score_groups <- function(pheno_score_data) {
    score_groups <- stats::quantile(pheno_score_data$SCORE, 
                            probs=c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
                            na.rm=TRUE)

    score_groups_slim = c()
    prev_quant <- min(score_groups)
    prev_name <- names(score_groups)[1]
    score_groups_slim[[prev_name]] <- prev_quant

    for(quant in names(score_groups)) {
        if(prev_quant != score_groups[[quant]]) {
            score_groups_slim[[quant]] <- score_groups[[quant]]
            prev_quant <- score_groups[[quant]]
            prev_name <- quant
        } else {
            prev_name <- quant
        }
    }

    return(unlist(score_groups_slim))
}

#' Change score group levels reference to middle
#' 
#' Relevel score groups so that the reference group is the one closest
#' to 50%
#' 
#' @param indv_score_groups A character. The risk groups of each 
#'                                       individual.
#' 
#' @return The risk group character vector releveled.
#' 
#' @author Kira E. Detrois
relevel_to_middle <- function(indv_score_groups) {
    numeric_groups <- get_groups_numeric(indv_score_groups)
    new_ref <- levels(indv_score_groups)[which.min(abs(numeric_groups - 50))]
    indv_score_groups <- stats::relevel(indv_score_groups, ref=new_ref)
    return(indv_score_groups)
}