#' Calculates start of exposure period
#' 
#' Calculates the date of the start of the exposure period for each 
#' individual.
#' 
#' @inheritParams calc_end_of_study
#' @param exp_age An integer. Age at which exposure period starts
#'                   (in years)
#' 
#' @return A Date. The dates of the start of the exposure period 
#'                 for each individual.
#' 
#' @importFrom lubridate %m+%
#' @export
#' 
#' @examples 
#' bds <- c(as.Date("1923/07/01"), as.Date("1823/07/02"), as.Date("2002/04/01"))
#' study <- methods::new("study", endpt="J10_ASTHMA", exp_age=30, exp_len=10, wash_len=2, obs_len=8)
#' calc_exp_start_date(bds, study@exp_age)
#' 
#' @author Kira E. Detrois
calc_exp_start_date <- function(bds, 
                                exp_age) {
    test_date_var_correct(bds, "bds")
    bds %m+% lubridate::years(exp_age)
}