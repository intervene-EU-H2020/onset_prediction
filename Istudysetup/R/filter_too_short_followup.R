#' Filters out too short follow-up interval
#' 
#' Filters out individuals where the follow-up interval does
#' not cover the whole study period. The whole study period is defined 
#' as the exposure, washout, and observation period. See function:
#' \code{\link{calc_study_time}}.
#' 
#' This is obsolete at the moment, because the follow-up data
#' doesn't seem to be accurat, i.e. in our data sometimes and
#' inidividual has an entry in the endpoint date column that
#' precedes the start of followup date.
#' 
#' The idea is that the data comes from a phenotype file in INTERVENE format: 
#' \href{https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit}{INTERVENE Phenotype File Definition}.
#' To add the columns `STUDY_TIME` and `FOLLOWUP` use either functions
#' \code{\link{calc_study_time}} and \code{\link{get_followup_time}}
#' directly, or \code{\link{add_study_interval_cols}}.
#' 
#' @param pheno_data A data.frame with at least the columns: 
#'                   `STUDY_TIME`, and `FOLLOWUP`.
#'                   
#' @return The filtered data.frame without the individuals where the
#'         the follow-up period doesn't cover the study period. 
#' 
#' @examples 
#' test_data <- create_test_df()
#' test_data <- add_study_interval_cols(test_data)
#' 
#' @author Kira E. Detrois
filter_too_short_followup <- function(pheno_data) {
    dplyr::filter(pheno_data, STUDY_TIME %within% FOLLOWUP)
}