#' Gets the follow-up period
#' 
#' Gets the follow-up period for each individual as a lubridate 
#' \code{\link[lubridate]{interval}}. 
#' 
#' Currently, if there is NAs in the `END_OF_FOLLOWUP` column replaces 
#' it with the current date. This assumes that the missingness comes 
#' from the fact that the followup has not yet ended and is not dues 
#' some other reason.
#' 
#' @param pheno_data A data.frame with at least the columns:
#'                   `END_OF_FOLLOWUP` and `START_OF_FOLLOWUP`.
#' 
#' @importFrom lubridate %--%
#' 
#' @author Kira E. Detrois
get_followup_time <- function(pheno_data) {
    # Replacing NAs with current date
    followup_complete = pheno_data$END_OF_FOLLOWUP
    followup_complete[is.na(followup_complete)] = lubridate::today()

    # Creating intervals
    followup_time <- pheno_data$START_OF_FOLLOWUP %--% followup_complete

    return(followup_time)
}