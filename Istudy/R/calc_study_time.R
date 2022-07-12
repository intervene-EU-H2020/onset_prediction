#' Calculates total study time interval
#' 
#' Calculates the total study time for each individual. The study time
#' is the time from the begining of the exposure period to the end 
#' of the prediction period.
#' 
#' Creates lubridate \code{\link[lubridate]{interval}}s.
#' 
#' @inheritParams calc_end_of_study
#' 
#' @return A lubdridate interval. The total study time interval.
#' 
#' @importFrom lubridate %--%
#' @export
#' 
#' @examples 
#' test_data <- Istudy::create_test_df(30)
#' study <- methods::new("study", endpt="J10_ASTHMA", exp_age=30, 
#'                       exp_len=10, wash_len=2, obs_len=8, ancs="EUR")
#' test_data <- Istudy::add_study_interval_cols(test_data, study)
#' calc_study_time(test_data, study)
#' 
#' @author Kira E. Detrois
calc_study_time <- function(pheno_data, 
                            study) {

    test_date_var_correct(pheno_data$DATE_OF_BIRTH, "bds")

    exp_start_date <- calc_exp_start_date(pheno_data$DATE_OF_BIRTH, study@exp_age)
    end_time <- calc_end_of_study(pheno_data, study)
    
    total_time <- exp_start_date %--% end_time
    return(total_time)
}