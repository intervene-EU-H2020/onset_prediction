#' Gets individuals matching exposure window
#' 
#' Subsets the data.frame to only include the entries from individuals
#' inside the exposure period. By default gets all data from the 
#' data.frame. The exposure has to be given as age. In this case the
#' data.frame also needs to have a column `Event_age`.
#' 
#' If either or both `exp_start` and `exp_end` are provided restricts
#' the entries to the exposure period for each individual.
#' 
#' @param long_data A data.frame with at least columns `ID`, and 
#'                  `PRIMARY_ICD`.
#' @param exp_start A numeric. Start of the exposure period. Can be 
#'                  used to restrict the timeframe on which the index 
#'                  should be calculated. In this case the data.frame
#'                  needs column `Event_age`. 
#'                  In case it is a vector has to have the length of
#'                  the number of rows in `long_data`.
#' @param exp_end A numeric. End of the exposure period.
#'                  Can be used to restrict the timeframe on which the index 
#'                  should be calculated. In this case the data.frame
#'                  needs column `Event_age`.
#'                  In case it is a vector has to have the length of
#'                  the number of rows in `long_data`.
#' 
#' @export
#' 
#' @author Kira E. Detrois 
get_exposure_data <- function(long_data,
                              exp_start=NA_real_,
                              exp_end=NA_real_) {
    
    if(!all(is.na(exp_start)) | !all(is.na(exp_end))) {
        assertthat::assert_that("Event_age" %in% colnames(long_data))
        # Replacing any NAs with values outside human life-spans
        if(all(is.na(exp_end))) {
            exp_end = 200 # Change this in case super humans exist
        } else if(any(is.na(exp_end))) {
            exp_end[is.na(exp_end)] = 200 # Change this in case super humans exist
        }
        if(all(is.na(exp_start))) {
            exp_start = 0
        } else if(any(is.na(exp_start))) {
            exp_start[is.na(exp_start)] = 0
        }

        long_data <- dplyr::filter(long_data, 
                                   Event_age >= exp_start & 
                                    Event_age <= exp_end)
    } 
    return(long_data)               
}
