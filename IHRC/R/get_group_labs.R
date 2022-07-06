#' Creates the labels for each risk score group
#' 
#' @param score_group_tbl A named numeric. The risk score cuts for the 
#'                          different quantiles.
#' 
#' @return A character. The group names.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_risk_group_labs <- function(score_group_tbl) {
    down_group <- paste("Group", 
                        names(score_group_tbl[1:length(score_group_tbl)-1]))
    down_group[1] <- paste0("[", down_group[1])
    down_group[2:length(down_group)] <- paste0("(", down_group[2:length(down_group)])
    up_group <-  paste("Group", 
                        names(score_group_tbl[2:length(score_group_tbl)]))
    up_group <- paste0(up_group, "]")
    group_labs <- paste0(down_group, " - " , up_group)
}