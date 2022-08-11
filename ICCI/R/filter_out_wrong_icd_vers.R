#' Filters out entries with incompatible ICD-version
#' 
#' Currently, the CCI can only be automatically calculated on ICD-9 and 10.
#' 
#' @param icd_data A data.frame with at least column `ICD_version`.
#' 
#' @return The data without rows with ICD-version entries other than "9", "9CM",
#'          "10", or "10CM".
#' 
#' @author Kira E. Detrois
filter_out_wrong_icd_vers <- function(icd_data) {
    if(any(!(icd_data$ICD_version %in% c("9", "10")))) {
        n_remove <- sum(!(icd_data$ICD_version %in% c("9", "10", "9CM", "10CM")))
        remove_icds <- setdiff(unique(icd_data$ICD_version), c(9, 10))
        icd_data <- dplyr::filter(icd_data, ICD_version %in% c(9, 10))
        warning(paste0("Careful, filtering out ", n_remove, " entries",
                        " because of incompatible ICD-versions removed entries with: ",
                        paste0(remove_icds, collapse=", "), ". The CCI is only defined on ICD-9 and 10 at the moment."))
    }
    return(icd_data)
}