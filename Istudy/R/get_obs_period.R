#' Calculates the age in the observation period
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
get_obs_period <- function(study,
                           obs_end=NULL) {
    if(is.null(end_date)) {
        obs_start <- study@exp_age + study@exp_len + study@wash_len
        obs_end <- obs_start + study@obs_len
    } else {
        obs_start <- obs_end %m-% lubridate::years(obs_len)
    }
    return(list(start=obs_start, end=obs_end))
}