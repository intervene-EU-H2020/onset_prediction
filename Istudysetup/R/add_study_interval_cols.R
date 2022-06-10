#' Adds columns for the study setup
#' 
#' Adds the three columns, `FOLLOWUP`, `ENDPT_FREE`, and `STUDY_TIME`
#' to the phenotype data.frame. Each column contains lubridate 
#' \code{\link[lubridate]{interval}}s.
#' 
#' For the input data format see: 
#' \href{https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit}{INTERVENE Phenotype File Definition}
#' 
#' @param pheno_data A data.frame with at least the columns: 
#'                   `START_OF_FOLLOWUP`, `END_OF_FOLLOWUP`, and 
#'                   `DATE_OF_BIRTH`.
#' @inheritParams get_study_elig_indv
#' 
#' @return The phenotype data.frame with added columns:
#'         \itemize{
#'          \item `FOLLOWUP`: lubdridate intervals created using columns 
#'                         `START_OF_FOLLOWUP` and `END_OF_FOLLOWUP`.
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
                                    exp_age=30, 
                                    exp_len=10, 
                                    wash_len=2, 
                                    obs_len=8) {
    test_length_vars_are_integers(as.list(environment()))

    followup <- get_followup_time(pheno_data)
    
    endpt_free <- calc_endpt_free_time(pheno_data$DATE_OF_BIRTH, 
                                       exp_age, 
                                       exp_len, 
                                       wash_len)
    total_study_time <- calc_study_time(pheno_data$DATE_OF_BIRTH, 
                                        exp_age, 
                                        exp_len, 
                                        wash_len, 
                                        obs_len)

    pheno_data <- tibble::add_column(pheno_data, 
                                     FOLLOWUP=followup,
                                     ENDPT_FREE=endpt_free,
                                     STUDY_TIME=total_study_time)

    return(pheno_data)
}
