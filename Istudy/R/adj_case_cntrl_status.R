#' Adjusts the case status of individuals with late onset endpoints
#'
#' Those individuals where the endpoint onset date is after the 
#' observation has ended are considered controls in this setup.
#' 
#' @param study_data A data.frame with at least columns `STUDY_TIME`
#'                   and i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE` 
#'                   where the columns are the study endpoint and 
#'                   date, which will differ depending on the input 
#'                   variable `endpt`.
#' @param endpt A character. The column name of the current endpoint of 
#'                           interest.
#'
#' @importFrom dplyr %>%
#' @export 
#' 
#' @author Kira E. Detrois
adj_case_cntrl_status <- function(study_data,
                                  endpt) {   
    endpt_date <- paste0(endpt, "_DATE")
    check_cols_exist(study_data, 
                     c(endpt, endpt_date, "STUDY_TIME", "ID"),
                     "adj_case_cntrl_status")  

    # Cases with endpoint after study ended                 
    cases_to_cntrls_ids <- dplyr::filter(study_data, 
                                         get(endpt_date)>lubridate::int_end(STUDY_TIME)) %>%
                                dplyr::pull(ID)
    study_data[study_data$ID %in% cases_to_cntrls_ids, endpt] = rep(0, length(cases_to_cntrls_ids))
    return(study_data)
}