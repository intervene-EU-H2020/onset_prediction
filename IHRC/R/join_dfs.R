#' Joins the phenotypic and score data
#' 
#' Joins the dataframes `pheno_data`, and `score_data` using
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
join_dfs <- function(pheno_data,
                     score_data,
                     score_type="CCI") {
    writeLines(paste0("pheno data ", nrow(pheno_data)))
    writeLines(paste0("score data ", nrow(score_data)))
    study_data <- dplyr::inner_join(pheno_data,
                                    score_data,
                                    by="ID")
    writeLines(paste0("joined data ", nrow(study_data)))
    if("CCI" %in% score_type) {
        study_data$CCI[is.na(study_data$CCI)] <- 0
    } 
    if("EI" %in% score_type) {
        study_data$EI[is.na(study_data$EI)] <- 0
    } 
    return(study_data)
}