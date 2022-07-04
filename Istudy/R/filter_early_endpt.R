#' Filters out early endpoint
#' 
#' Filters out individuals where the endpoint occured before the 
#' prediction period began.
#' 
#' The endpoint free interval is the period from birth until the 
#' prediction period begins. Thus, the interval also contains the 
#' exposure and washout period. See function 
#' \code{\link{calc_endpt_free_time}}.
#' 
#' For the input data format see: 
#' \href{https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit}{INTERVENE Phenotype File Definition}. To create the endpoint free interval, use either the
#' function \code{\link{calc_endpt_free_time}} directly or 
#' \code{\link{add_study_interval_cols}}.
#' 
#' @param pheno_data A data.frame with at least columns `ENDPT_FREE`
#'                   and i.e. `J10_ASTHMA_DATE` where the columns are the study 
#'                   endpoint and date, which will differ depending on the input 
#'                   variable `endpt`.
#' @inheritParams get_study_elig_indv
#' @inheritParams adj_case_cntrl_status
#'                 
#' @return The filtered data.frame without individuals where the 
#'         endpoint occured before the prediction period. 
#' 
#' @importFrom lubridate %within%
#' @export
#' 
#' @examples 
#' test_data <- Istudy::create_test_df(30)
#' study <- methods::new("study", endpt="J10_ASTHMA", exp_age=30, 
#'                       exp_len=10, wash_len=2, obs_len=8, ancs="EUR")
#' test_data <- add_study_interval_cols(test_data, study)
#' filter_early_endpt(test_data, "J10_ASTHMA")
#'  
#' @author Kira E. Detrois
filter_early_endpt <- function(pheno_data, 
                               endpt) {    
    endpt_date_str <- paste0(endpt, "_DATE")
    check_cols_exist(pheno_data,
                     c(endpt_date_str, "ENDPT_FREE"),
                     "filter_early_endpt")

    # Endpoint happens after Endpoint free interval
    dplyr::filter(pheno_data, 
                  !(get(endpt_date_str) %within% ENDPT_FREE) | 
                  is.na(get(endpt_date_str))) # If no endpoint date then NA 
}