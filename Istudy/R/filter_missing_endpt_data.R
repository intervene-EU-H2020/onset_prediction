#' Filters out missing information on the endpoint
#' 
#' Filters out individuals with missing information about the 
#' endpoint of interest, defined by variable `endpt`.
#' Missing information is defined as either no recorded endpoint 
#' (endpt: NA), or for cases no recorded date of endpoint (endpt: 1, 
#' endpt_date: NA).
#' 
#' @param study_data A data.frame with at least columns:
#'                   i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE` where 
#'                   the columns are the study endpoint and date, 
#'                   which will differ depending on the input variable 
#'                  `endpt`.
#' @inheritParams adj_case_cntrl_status
#'                  
#' @return The filtered data.frame without any missing information 
#'          on the endpoint of interest.
#' 
#' @export
#' 
#' @examples 
#' test_data <- create_test_df()
#' filter_missing_endpt_data(test_data, "J10_ASTHMA")
#' 
#' @author Kira E. Detrois
filter_missing_endpt_data <- function(study_data,       
                                      endpt) { 
    check_cols_exist(study_data,
                     c(endpt, paste0(endpt, "_DATE")),
                     "filter_missing_endpt_data")
    dplyr::filter(study_data, 
                  !is.na(get(endpt)),
                  !((get(endpt) == 1) & is.na(get(paste0(endpt, "_DATE")))))
}
