#' Adjusts the case status of individuals with late onset endpoints
#'
#' Those individuals where the endpoint date is after the prediction
#' period are considered cases in our setup.
#' 
#' @param pheno_data A data.frame with at least columns `STUDY_TIME`
#'                   and i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE` 
#'                   where the columns are the study endpoint and 
#'                   date, which will differ depending on the input 
#'                   variable `endpt`.
#' @param endpt A string. The column name of the current endpoint of 
#'                        interest.
#'
#' @importFrom dplyr %>%
#' @export 
#' 
#' @author Kira E. Detrois
adj_case_cntrl_status <- function(pheno_data,
                                  endpt) {   
    endpt_date <- paste0(endpt, "_DATE")
    check_cols_exist(pheno_data, 
                     c(endpt, endpt_date, "STUDY_TIME", "ID"),
                     "adj_case_cntrl_status")  

    # Cases with endpoint after study ended                     
    cases_to_cntrls_ids <- dplyr::filter(pheno_data, 
                                        get(endpt_date) > 
                                            lubridate::int_end(STUDY_TIME)) %>%
                                dplyr::pull(ID)
    pheno_data[pheno_data$ID %in% cases_to_cntrls_ids, endpt] = rep(0, length(cases_to_cntrls_ids))
    return(pheno_data)
}