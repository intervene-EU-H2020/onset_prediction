#' Calculates total study time interval
#' 
#' Calculates the total study time for each individual. The study time
#' is the time from the begining of the exposure period to the end 
#' of the prediction period.
#' 
#' Creates lubridate \code{\link[lubridate]{interval}}s.
#' 
#' @param bds A Date. The birth days of the individuals.
#' @param obs_len An integer. Length of the prediction period.
#' @inheritParams get_study_elig_indv
#' 
#' @return A lubdridate interval. The total study time interval.
#' 
#' @importFrom lubridate %m+%
#' @importFrom lubridate %--%
#' @export
#' 
#' @examples 
#' bds <- c(as.Date("1923/07/01"), as.Date("1823/07/02"), as.Date("2002/04/01"))
#' calc_study_time(bds,
#'                 exp_age=30,
#'                 exp_len=10,
#'                 wash_len=2,
#'                 obs_len=8)
#' 
#' @author Kira E. Detrois
calc_study_time <- function(bds, 
                            exp_age, 
                            exp_len, 
                            wash_len, 
                            obs_len) {
    test_date_var_correct(bds, "bds")
    test_length_vars_are_integers(as.list(environment()))
    
    exp_start_date <- calc_exp_start_date(bds, exp_age)
    end_time <- exp_start_date %m+% lubridate::years(exp_len + wash_len + obs_len)
    total_time <- exp_start_date %--% end_time
    return(total_time)
}