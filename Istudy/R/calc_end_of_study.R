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
#' @importFrom lubridate %m+%
#' @export
#' 
#' @examples 
#' bds <- c(as.Date("1923/07/01"), as.Date("1823/07/02"), as.Date("2002/04/01"))
#' study <- methods::new("study", endpt="J10_ASTHMA", exp_age=30, 
#'                       exp_len=10, wash_len=2, obs_len=8, ancs="EUR")
#' calc_end_of_study(bds, study)
#' 
#' @author Kira E. Detrois
calc_end_of_study <- function(bds,
                              study) {
    test_date_var_correct(bds, "bds")
    exp_start_date <- calc_exp_start_date(bds, study@exp_age)
    end_time <- exp_start_date %m+% lubridate::years(study@exp_len + study@wash_len + study@obs_len)
    return(end_time)
}