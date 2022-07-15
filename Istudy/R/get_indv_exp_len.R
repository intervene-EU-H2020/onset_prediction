#' Gets the exact exposure times in years of each individual in the data
#' 
#' @param pheno_data A data.frame with columns i.e. `J10_ASTHMA_DATE`, and
#'                      `I9_VTE_DATE`. The diagnosis dates for different
#'                      endpoints.
#' @inheritParams get_backward_study
#' 
#' @importFrom lubridate %m-%
#' @export 
#' 
#' @author Kira E. Detrois
get_indv_exp_len <- function(pheno_data,
                             wash_len=2,
                             obs_len=8,
                             obs_end=NULL) {
    obs_end <- get_obs_end(pheno_data, obs_end)
    start_obs <- obs_end %m-% lubridate::years(wash_len + obs_len)
    exp_dur <- lubridate::as.duration(pheno_data$DATE_OF_BIRTH %--% start_obs)
    indv_exp_len <- lubridate::time_length(exp_dur, "years")
    
    return(indv_exp_len)
}

get_obs_end <- function(pheno_data, 
                        obs_end=NULL) {
    if(is.null(obs_end)) {
        obs_end <- get_max_date(pheno_data)
    }
    return(obs_end)
}

#' Get's the most recent date across different `_DATE` columns
#' 
#' @inheritParams get_indv_exp_len
#' 
#' @importFrom dplyr %>%
#' @export 
#' 
#' @author Kira E. Detrois
get_max_date <- function(pheno_data) {
    pheno_data <- filter_not_all_na_cols(pheno_data)
    all_dates <- dplyr::select(pheno_data, 
                               dplyr::matches("(*.)_DATE$"))
    column_max <- all_dates %>%
                    dplyr::summarise_all(.funs=function(x) {max(x, na.rm=TRUE)})
    max_date <- as.Date(apply(column_max, 1, max))
    return(max_date)
}

#' Filters out columns from the data with only NA entries
#' 
#' @param pheno_data A data.frame.
#' 
#' @author Kira E. Detrois
filter_not_all_na_cols <- function(pheno_data) {
    pheno_data[,!(colSums(is.na(pheno_data)) == nrow(pheno_data))]

}