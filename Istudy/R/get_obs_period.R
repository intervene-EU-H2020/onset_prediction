#' Calculates the age at the start and end the observation period
#' 
#' @inheritParams get_study_elig_indv
#' 
#' @return A list with the `start` and `end` age in the
#' observation period
#' 
#' @export
#' @importFrom lubridate %m-% 
#' 
#' @author Kira E. Detrois
get_obs_period <- function(study) {
    if(is.null(end_date)) {
        obs_start <- study@exp_age + study@exp_len + study@wash_len
        obs_end <- obs_start + study@obs_len
    }
    return(list(start=obs_start, end=obs_end))
}