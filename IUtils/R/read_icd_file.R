#' Reads in the ICD file according to INTERVENE standard
#' 
#' See \href{https://docs.google.com/document/d/1E2Jc72CmMItEchgQaCvfA4MhZUkQYjALwTu3dCl7qd8/edit}{INTERVENE ICD longitudinal file definition v1}. 
#' 
#' The needed columns are at least `ID`, `EVENT_AGE`, `ICD_VERSION`, 
#' `PRIMARY_ICD`, 
#' 
#' @param file_path A string (string). The path to the file.
#' 
#' @export 
#' 
#' @return A tibble with at least columns `ID`, `Event_age`, `ICD_version`, and 
#' `primary_ICD`. Additionally, can have column `secondary_ICD` if available in 
#' the data.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
read_icd_file <- function(file_path) {
    tryCatch({
        icd_data <- readr::read_delim(file_path, 
                                      delim="\t",
                                      col_types=c("cdccc"))
    }, error=function(e) {writeLines(paste0("Could not read ICD file ", file_path))})

    if(nrow(icd_data) == 0) {
        warning(paste0("Warning. ICD-file contained no entries. Given path: ", file_path, "\nGot: ", file_path))
    }
    expect_cols <- c("ID", "Event_age", "ICD_version", "primary_ICD")
    check_cols(expect_cols, colnames(icd_data), file_path)

    return(icd_data)
}

