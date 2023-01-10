#' Joins the phenotypic and score data
#' 
#' Joins the dataframes `study_data`, and `score_data` using
#' the ID variable.
#' 
#' Sets missing values for the `CCI`, or `EI` to 0.
#' 
#' @inheritParams run_surv_studies
#' @inheritParams get_n_group_cases
#' 
#' @return A dataframe containing the merged study and score data.
#'  
#' @export 
#' 
#' @author Kira E. Detrois
join_dfs <- function(study_data,
                     score_data,
                     score_type="CCI",
                     endpt=NULL) {
    elig_score_data <- dplyr::left_join(x=study_data,
                                        y=score_data,
                                        by="ID")
    if("CCI" %in% score_type) {
        elig_score_data$CCI[is.na(elig_score_data$CCI)] <- 0
    } 
    if("EI" %in% score_type) {
        elig_score_data$EI[is.na(elig_score_data$EI)] <- 0
    } 
    return(elig_score_data)
}