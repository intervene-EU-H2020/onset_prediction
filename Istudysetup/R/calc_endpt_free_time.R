#' Calcualtes endpoint free intervals
#' 
#' Calculates the endpoint free interval for each individual. The 
#' endpoint free interval is the period from birth until the 
#' prediction period begins. Thus, the interval also contains the 
#' exposure and washout period.
#' 
#' Creates lubridate \code{\link[lubridate]{interval}}s.
#' 
#' @param bds A Date. The birth days of the individuals.
#' @inheritParams get_study_elig_indv
#' 
#' @return A lubdridate interval. The endpoint free intervals.
#' 
#' @importFrom lubridate %m+%
#' @importFrom lubridate %--%
#' @export
#' 
#' @examples 
#' bds <- c(as.Date("1923/07/01"), as.Date("1823/07/02"), as.Date("2002/04/01"))
#' calc_endpt_free_time(bds,
#'                      exp_age=30,
#'                      exp_len=10,
#'                      wash_len=2)
#' 
#' @author Kira E. Detrois
calc_endpt_free_time <- function(bds, 
                                 exp_age=30, 
                                 exp_len=10, 
                                 wash_len=2) {
    test_date_var_correct(bds, "bds")
    test_length_vars_are_integers(as.list(environment()))

    exp_start_date <- calc_exp_start_date(bds, exp_age)
    endpt_free_end <- exp_start_date %m+% lubridate::years(exp_len + wash_len)
    endpt_free_time <- bds %--% endpt_free_end

    return(endpt_free_time)
}