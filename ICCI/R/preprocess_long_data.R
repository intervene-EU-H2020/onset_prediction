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
preprocess_long_data <- function(long_data,
                                 exp_start=NA,
                                 exp_end=NA) {
    long_data <- ILongDataUtils::get_exposure_data(long_data, 
                                                   exp_start, 
                                                   exp_end)
    long_data <- ILongDataUtils::add_num_id_col(long_data)
    return(long_data)
}