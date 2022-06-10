#' Calculates start of exposure period
#' 
#' Calculates the date of the start of the exposure period for each 
#' individual.
#' 
#' @param bds A Date. The birth days of the individuals.
#' @inheritParams get_study_elig_indv
#' 
#' @return A Date. The dates of the start of the exposure period 
#'                 for each individual.
#' 
#' @importFrom lubridate %m+%
#' @export
#' 
#' @examples 
#' bds <- c(as.Date("1923/07/01"), as.Date("1823/07/02"), as.Date("2002/04/01"))
#' calc_exp_start_date(bds, exp_age=30)
#' 
#' @author Kira E. Detrois
calc_exp_start_date <- function(bds, 
                                exp_age) {
    test_date_var_correct(bds, "bds")
    test_length_vars_are_integers(as.list(environment()))
    
    bds %m+% lubridate::years(exp_age)
}