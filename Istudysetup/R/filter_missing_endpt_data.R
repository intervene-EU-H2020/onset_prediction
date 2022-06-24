#' Filters out missing information in the endpoint
#' 
#' Filters out individuals with missing information about the 
#' endpoint of interest, defined with variable `endpt`.
#' The missing information can either be no recorded endpoint 
#' (endpt: NA), or for cases no recorded date of endpoint (endpt: 1, 
#' endpt_date: NA).
#' 
#' For the input data format see: 
#' \href{https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit}{INTERVENE Phenotype File Definition}.
#' 
#' @param pheno_data A data.frame with at least the columns:
#'                   i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE` where 
#'                   the columns are the study endpoint and date, 
#'                   which will differ depending on the input variable 
#'                  `endpt`.
#' @param endpt A string. The column name of the current endpoint of interest.
#'                  
#' @return The filtered data.frame without any missing information about the endpoint of interest.
#' 
#' @export
#' 
#' @examples 
#' test_data <- create_test_df()
#' filter_missing_endpt_data(test_data, "J10_ASTHMA")
#' 
#' @author Kira E. Detrois
filter_missing_endpt_data <- function(pheno_data,       
                                      endpt) {  
    dplyr::filter(pheno_data, 
                  !is.na(get(endpt)),
                  !((get(endpt) == 1) & is.na(get(paste0(endpt, "_DATE")))))
}
