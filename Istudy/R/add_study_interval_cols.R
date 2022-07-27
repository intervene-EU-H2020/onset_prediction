#' Adds columns for the study setup
#' 
#' Adds the three columns, `EXP_END`, `ENDPT_FREE`, and `STUDY_TIME`
#' to the phenotype data.frame. Columns `ENDPT_FREE`, and `STUDY_TIME`
#' each contain lubridate \code{\link[lubridate]{interval}}s.
#' 
#' The column `EXP_END` is mainly needed for studies that consider
#' variable exposure lengths for individuals selected at a given time point. 
#' Here the exposure length will be the time from birth until the time point
#' considered and should be provided in the S4 study object in slot `EXP_END`
#' in the same order as the individuals in the data.frame. A study setup like
#' this can be created using the function \code{\link{get_backward_study}}.
#' 
#' 
#' @param pheno_data A data.frame with at least the column `DATE_OF_BIRTH`.
#' @inheritParams get_study_elig_indv
#' 
#' @return The phenotype data.frame with added columns:
#'         \itemize{
#'          \item `ENDPT_FREE`: lubdridate intervals
#'                           which ranges from `DATE_OF_BIRTH` to the 
#'                           end of the washout period. In this time 
#'                           period there should be no endpoint 
#'                           occurance for eligible study participants.
#'          \item `STUDY_TIME`: lubdridate intervals which range from 
#'                           the start of the exposure period to the 
#'                           end of the prediction period.
#'          \item `EXP_END`: Exact time length of the end exposure 
#'                           period in years.
#'          \item `EXP_START`: Exact time until the start of the 
#'                             exposure period in years. }
#' 
#' @export 
#' 
#' @author Kira E. Detrois
add_study_interval_cols <- function(pheno_data,
                                    study) {
    check_cols_exist(pheno_data, 
                     c("DATE_OF_BIRTH"), 
                     "add_study_interval_cols")
    endpt_free <- calc_endpt_free_time(pheno_data$DATE_OF_BIRTH, study)
    total_study_time <- calc_study_time(pheno_data, study)

    pheno_data <- tibble::add_column(pheno_data, 
                                     ENDPT_FREE=endpt_free,
                                     STUDY_TIME=total_study_time)

    return(pheno_data)
}
