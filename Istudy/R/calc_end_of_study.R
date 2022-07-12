#' Calculates the end of the study time
#' 
#' Calculates the end of the study time for each individual. The study time
#' is the time from the begining of the exposure period to the end 
#' of the prediction period.
#' 
#' @param bds A Date. The birth days of the individuals.
#' @inheritParams get_study_elig_indv
#' 
#' @return A lubdridate interval. The total study time interval.
#' 
#' @export
#' 
#' @examples 
#' test_data <- Istudy::create_test_df(30)
#' study <- methods::new("study", endpt="J10_ASTHMA", exp_age=30, 
#'                       exp_len=10, wash_len=2, obs_len=8, ancs="EUR")
#' test_data <- Istudy::add_study_interval_cols(test_data, study)
#' calc_end_of_study(test_data, study)
#' 
#' @author Kira E. Detrois
calc_end_of_study <- function(pheno_data,
                              study) {
    test_date_var_correct(pheno_data$DATE_OF_BIRTH, "bds")
    check_cols_exist(pheno_data, c("DATE_OF_BIRTH", "EXP_LEN"), "calc_end_of_study")  

    exp_start_date <- calc_exp_start_date(pheno_data$DATE_OF_BIRTH, study@exp_age)
    if(length(study@exp_len) == 1) {
        end_time <- exp_start_date + lubridate::years(study@exp_len + study@wash_len + study@obs_len)
    } else {
        end_time <- exp_start_date + lubridate::dyears(pheno_data$EXP_LEN) + lubridate::years(study@wash_len + study@obs_len)
        # Removing unecessary extra hms information
        end_time <- as.Date(end_time, "%Y%m%d")
    }

    return(end_time)
}