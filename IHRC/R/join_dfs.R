#' Joins phenotype and score data
#' 
#' Joins the data.frames `pheno_data`, and `score_data` using
#' the `ID` variable.
#' 
#' Sets missing values for the `CCI`, or `EI` to 0.
#' 
#' @param pheno_data A data.frame. The phenotype data. Needs at least column `ID`.
#' @param score_data A data.frame. The score data. Needs at least column `ID` and 
#'                   if `score_type` contains `CCI` or `EI` then the selected columns.
#' @param score_type A string (vector). The score types used in the analysis.
#' 
#' @return A tibble containing the merged phenotype and score data.
#'  
#' @export 
#' 
#' @author Kira E. Detrois
join_dfs <- function(pheno_data,
                     score_data,
                     score_type="CCI") {
    study_data <- dplyr::full_join(pheno_data,
                                   score_data,
                                   by="ID")
    if("CCI" %in% score_type) {
        study_data$CCI[is.na(study_data$CCI)] <- 0
    } 
    if("EI" %in% score_type) {
        study_data$EI[is.na(study_data$EI)] <- 0
    } 
    return(study_data)
}