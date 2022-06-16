#' Preprocesses the score data for the analysis
#' 
#' Checks that `score_col_name` is contained in the data and renames the column
#' to `SCORE`.
#' 
#' @param score_data A data.frame with the score results for each individuals.
#'                   Should have at least column defined in `score_col_name`.
#' @param score_col_name A character. The name of the column with the scores.
#' 
#' @return The data.frame with the score column renamed to `SCORE`.
#' 
#' @author Kira E. Detrois
preprocess_score_data <- function(score_data, 
                                  score_col_name) {
    assertthat::assert_that(score_col_name %in% colnames(score_data),
                            msg=paste0("The score_col_name ", score_col_name, " you gave is not a known column in the score_data data.frame. Have column names: ", paste0(colnames(score_data), collapse=", ")))
    dplyr::rename(score_data, "SCORE"={{ score_col_name }})
}

#' 95% confidence interval given the ML estimator and SE
#' 
#' @param ML maximum likelihood estimator of the parameter
#' @param SE standard error of the ML estimator
get_CI <- function(ML, SE) {
    CIneg <- exp(ML-1.96*SE)
    CIpos <- exp(ML+1.96*SE)

    return(c(neg=CIneg, pos=CIpos))
}
