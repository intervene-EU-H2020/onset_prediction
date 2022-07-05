#' @importFrom dplyr %>% 
get_and_filter_endpt_scores <- function(score_data,
                                        score_type,
                                        endpt) {
    if(score_type == "PRS") {
        prs_col_name <- paste0(endpt, "_PRS")
        score_data <- dplyr::select(score_data, 
                                    ID, 
                                    {{ prs_col_name }}) %>% 
                        dplyr::rename("SCORE" = {{ prs_col_name }})
    }
    score_data <- dplyr::filter(score_data, 
                                !is.na("SCORE"))
}