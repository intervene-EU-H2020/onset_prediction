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
#' @export 
#' 
#' @author Kira E. Detrois
adj_case_cntrl_status <- function(pheno_data,
                                  endpt) {                                  
    test_endpt_input_correct(as.list(environment()))

    endpt_date_str <- paste0(endpt, "_DATE")
    cases_to_cntrls <- dplyr::filter(pheno_data, 
                                     get(endpt_date_str) > 
                                        lubridate::int_end(STUDY_TIME))
    pheno_data[pheno_data$ID %in% cases_to_cntrls$ID, endpt] = rep(0, nrow(cases_to_cntrls))

    return(pheno_data)
}