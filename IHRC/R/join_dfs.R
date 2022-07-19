#' Joins the two data.frames 
#' 
#' Joins the two data.frames and removes any individuals without
#' a score available.
#' 
#' @inheritParams run_surv_studies
#' @inheritParams get_n_group_cases
#' 
#' @return The joined data.frame
#' 
#' @export 
#' 
#' @author Kira E. Detrois
join_dfs <- function(pheno_data,
                     score_data,
                     score_type="CCI",
                     endpt=NULL) {
    pheno_score_data <- dplyr::left_join(x=pheno_data,
                                         y=score_data,
                                         by="ID")
    pheno_score_data <- dplyr::filter(pheno_score_data, 
                                      !is.na(SCORE))
    return(pheno_score_data)
}