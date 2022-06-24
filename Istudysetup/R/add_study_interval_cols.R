#' Adds columns for the study setup
#' 
#' Adds the three columns, `FOLLOWUP`, `ENDPT_FREE`, and `STUDY_TIME`
#' to the phenotype data.frame. Each column contains lubridate 
#' \code{\link[lubridate]{interval}}s.
#' 
#' For the input data format see: 
#' \href{https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit}{INTERVENE Phenotype File Definition}
#' 
#' @param pheno_data A data.frame with at least the column `DATE_OF_BIRTH`.
#' @param study A study object with the current study setup.
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
#'          }
#' 
#' @export 
#' 
#' @author Kira E. Detrois
add_study_interval_cols <- function(pheno_data,
                                    study) {
    check_cols_exist(pheno_data, c("DATE_OF_BIRTH"), "add_study_interval_cols")

    endpt_free <- calc_endpt_free_time(pheno_data$DATE_OF_BIRTH, 
                                       study)
    total_study_time <- calc_study_time(pheno_data$DATE_OF_BIRTH, 
                                        study)

    pheno_data <- tibble::add_column(pheno_data, 
                                     ENDPT_FREE=endpt_free,
                                     STUDY_TIME=total_study_time)

    return(pheno_data)
}
