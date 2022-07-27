#' Calcualtes endpoint free intervals
#' 
#' Calculates the endpoint free interval for each individual. The 
#' endpoint free interval is the period from birth until the 
#' observation period begins. Thus, the interval contains the 
#' exposure and washout period.
#' 
#' Creates lubridate \code{\link[lubridate]{interval}}s.
#' 
#' @inheritParams calc_end_of_study
#' @param bds A Date (vector). The birth days of the individuals.
#' 
#' @return A lubdridate interval (vector). The endpoint free intervals.
#' 
#' @importFrom lubridate %--%
#' @importFrom lubridate %m+%
#' @export
#' 
#' @author Kira E. Detrois
calc_endpt_free_time <- function(pheno_data, 
                                 study) {
    exp_start_date <- calc_exp_start_date(pheno_data, study)

    if(length(study@exp_len) == 1) {
        endpt_free_end <- exp_start_date %m+% lubridate::years(study@exp_len + study@wash_len)
    } else {
        endpt_free_end <- as.Date(exp_start_date + lubridate::dyears(study@exp_len) + lubridate::years(study@wash_len), "%Y%m%d")
    }
    endpt_free_time <- bds %--% endpt_free_end

    return(endpt_free_time)
}