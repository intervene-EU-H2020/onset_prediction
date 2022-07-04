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
#' @importFrom lubridate %m+%
#' @importFrom lubridate %--%
#' @export
#' 
#' @examples 
#' bds <- c(as.Date("1923/07/01"), as.Date("1823/07/02"), as.Date("2002/04/01"))
#' study <- methods::new("study", endpt="J10_ASTHMA", exp_age=30, exp_len=10, wash_len=2, obs_len=8, ancs="EUR")
#' calc_study_time(bds, study)
#' 
#' @author Kira E. Detrois
calc_study_time <- function(bds, 
                            study) {
    test_date_var_correct(bds, "bds")

    exp_start_date <- calc_exp_start_date(bds, study@exp_age)
    end_time <- calc_end_of_study(bds, study)

    total_time <- exp_start_date %--% end_time
    return(total_time)
}