#' Calcualtes endpoint free intervals
#' 
#' Calculates the endpoint free interval for each individual. The 
#' endpoint free interval is the period from birth until the 
#' prediction period begins. Thus, the interval also contains the 
#' exposure and washout period.
#' 
#' Creates lubridate \code{\link[lubridate]{interval}}s.
#' 
#' @inheritParams calc_end_of_study
#' 
#' @return A lubdridate interval. The endpoint free intervals.
#' 
#' @importFrom lubridate %--%
#' @importFrom lubridate %m+%
#' @export
#' 
#' @examples 
#' bds <- c(as.Date("1923/07/01"), as.Date("1823/07/02"), as.Date("2002/04/01"))
#' study <- methods::new("study", endpt="J10_ASTHMA", exp_age=30, 
#'                       exp_len=10, wash_len=2, obs_len=8, ancs="EUR")
#' calc_endpt_free_time(bds, study)
#' 
#' @author Kira E. Detrois
calc_endpt_free_time <- function(bds, 
                                 study) {
    test_date_var_correct(bds, "bds")

    exp_start_date <- calc_exp_start_date(bds, study@exp_age)

    if(length(study@exp_len) == 1) {
        endpt_free_end <- exp_start_date %m+% lubridate::years(study@exp_len + study@wash_len)
    } else {
        endpt_free_end <- as.Date(exp_start_date + lubridate::dyears(study@exp_len) + lubridate::years(study@wash_len), "%Y%m%d")
    }
    endpt_free_time <- bds %--% endpt_free_end

    return(endpt_free_time)
}