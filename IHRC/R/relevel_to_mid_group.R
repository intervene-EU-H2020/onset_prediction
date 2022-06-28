
#' Change score group levels reference to middle
#' 
#' Relevel score groups to the 40-60% group if it exists. Otherwise
#' sets the reference group is the group closest to 50%.
#' 
#' @param indv_score_groups A character. The risk groups of each 
#'                                       individual.
#' 
#' @return The risk group character vector releveled.
#' 
#' @author Kira E. Detrois
relevel_to_mid_group <- function(indv_score_groups) {
    if("Group 60%" %in% levels(indv_score_groups) &
        "Group 40%" %in% levels(indv_score_groups)) {
        new_ref <- "Group 60%"
    } else {
        numeric_groups <- get_groups_numeric(indv_score_groups)
        new_ref <- levels(indv_score_groups)[which.min(abs(numeric_groups - 50))]
        message(paste0("There is not 40-60% group to set the reference to.
        Setting the reference to the closest group to 50%. New reference is: ", new_ref))
    }
    indv_score_groups <- stats::relevel(indv_score_groups, ref=new_ref)
    return(indv_score_groups)
}

