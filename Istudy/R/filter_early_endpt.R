#' Filters out individuals with early endpoints
#' 
#' Filters out individuals where the endpoint occured before the 
#' observation period begins.
#' 
#' 
#' @param study_data A data.frame with at least columns `ENDPT_FREE_PERIOD`
#'                   and i.e. `J10_ASTHMA_DATE`, the study endpoint date,
#'                   which will differ depending on the input variable `endpt`.
#' @inheritParams adj_case_cntrl_status
#'                 
#' @return The filtered data.frame without individuals where the 
#'         endpoint occured before the prediction period. 
#' 
#' @importFrom lubridate %within%
#' @export
#'  
#' @author Kira E. Detrois
filter_early_endpt <- function(study_data, 
                               endpt) {    
    endpt_date_str <- paste0(endpt, "_DATE")
    check_cols_exist(study_data,
                     c(endpt_date_str, "ENDPT_FREE_PERIOD"),
                     "filter_early_endpt")

    dplyr::filter(study_data, 
                  !(get(endpt_date_str) %within% ENDPT_FREE_PERIOD) | 
                  is.na(get(endpt_date_str))) # If no endpoint date then NA 
}