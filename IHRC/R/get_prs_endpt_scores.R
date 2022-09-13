#' Renames the score column to `SCORE` and filters out NAs
#' 
#' For the PRS data the score columsn have names in the
#' form of `J10_ASTHMA_PRS`. The function renames the
#' current PRS column of interest to `SCORE`. Then it
#' filters out all NAs in the column.
#' 
#' @inheritParams run_surv_studies
#' @inheritParams get_n_group_cases
#' 
#' @return A tibble. The renamed and filtered score data.
#' 
#' @export 
#' 
#' @importFrom dplyr %>% 
get_prs_endpt_scores <- function(score_data,
                                 endpt) {
    
    prs_col_name <- paste0(endpt, "_PRS")
    score_data <- dplyr::select(.data=score_data, 
                                ID, 
                                {{ prs_col_name }}) %>% 
                    dplyr::rename("PRS" = {{ prs_col_name }})
}