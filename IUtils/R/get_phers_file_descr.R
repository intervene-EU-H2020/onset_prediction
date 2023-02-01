#' Gets the study setup file description for the PheRS folders 
#' 
#' @param study_type A string. Currently only "backward" possible.
#' @param obs_end_date A Date. The end of the observation period.
#' @param exp_len An integer. Length of the exposure period (in years).
#' @param wash_len An integer. Length of the washout period (in years).
#' @param obs_len An integer. Length of the observation period 
#'                            (in years).
#' 
#' @importFrom lubridate %m-%
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_phers_file_descr <- function(study_type="backward",
                                 obs_end_date=as.Date("2019/01/01"),
                                 exp_len=10,
                                 wash_len=2,
                                 obs_len=8) {
    exp_start_date <- Istudy::calc_backward_exp_start_date(obs_end_date,
                                                           exp_len,
                                                           wash_len,
                                                           obs_len)
    wash_start_date <- Istudy::calc_backward_wash_start_date(obs_end_date,
                                                             wash_len,
                                                             obs_len) 
    exp_end_date <- wash_start_date %m-% lubridate::days(1)
    obs_start_date <- Istudy::calc_backward_obs_start_date(obs_end_date,
                                                           obs_len) 
    wash_end_date <- obs_start_date %m-% lubridate::days(1)

    paste0("exposure=",
           exp_start_date,
           "-",
           exp_end_date,
           "-washoutend=",
           wash_end_date,
           "-observationend=",
           obs_end_date)
}