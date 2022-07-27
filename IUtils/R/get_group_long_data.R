#' Gets the data for the current ICD-version
#' 
#' Gets the entries of the data.frame for the selected ICD-version.
#' 
#' @param group_long_data A list. This should be created, using
#'                        function 
#'                        \code{\link{group_icd_data_by_ver}}.
#' @param icd_version The ICD-version in INTERVENE format.
#'                    This should be "10", "10CM", "9", or "9CM".
#' 
#' @return The data.frame with data for the chose ICD-version.
#' 
#' @export 
#' 
#' @examples 
#' long_data <- tibble::tibble(ICD_version=c("10", "9", "10", "10CM"),
#'                             primary_ICD=c("t1", "t2", "t3", "t4"))
#' group_long_data <- group_icd_data_by_ver(long_data)
#' get_group_icd_data(group_long_data, "10")
#' 
#' @author Kira E. Detrois
get_group_icd_data <- function(group_long_data, 
                                icd_version) {
    return(group_long_data$data[group_long_data$idxs[[icd_version]],])
}
