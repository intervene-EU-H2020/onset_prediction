#' Preprocesses the longitudinal data
#' 
#' Preprocesses the longidutdinal data by adding a numerical ID
#' to the data.frame and selecting the entries for the individuals 
#' inside the selected exposure period.
#' 
#' Adds a numerical ID to the data.frame because the 
#' \code{\link[comorbidity:comorbidity]{comordbidity}} 
#' function expects the IDs to be numeric.
#'  
#' @inheritParams calc_cci
#' 
#' @export 
#'
#' @author Kira E. Detrois
preprocess_icd_data <- function(icd_data,
                                exp_start=NULL,
                                exp_end=NULL) {
    icd_data <- IUtils::get_exposure_data(icd_data, 
                                          exp_start, 
                                          exp_end)
    icd_data <- filter_out_wrong_icd_vers(icd_data)
    icd_data <- add_num_id_col(icd_data)
    return(icd_data)
}
