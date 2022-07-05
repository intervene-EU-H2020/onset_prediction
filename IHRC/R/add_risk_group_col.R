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
                               bin_cut=1,
                               study,
                               write_res=FALSE,
                               res_dir=NA_character_) {
    if(score_type != "CCI") {
        quantiles <- c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1)
        score_group_tbl <- get_score_group_tbl(score_data, 
                                               quantiles)
        indv_score_groups <- get_indvs_score_groups(score_data,
                                                    score_group_tbl)
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