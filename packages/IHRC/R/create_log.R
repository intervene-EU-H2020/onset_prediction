
#' Creates log file
#' 
#' @param .Object An S4 `surv_ana` object. The current survival analysis setup. 
#'                   See class definition [IHRC::surv_ana].
#' 
#' @return A string. The file path for the error file.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
create_log_file <- function(.Object) {
    error_dir <- paste0(.Object@res_dir, "log/")
    if(.Object@write_res) {
        if(!dir.exists(error_dir)) {
            writeLines(paste0("The file directory ", error_dir, " does not exist. Trying to create it."))
            dir.create(error_dir, recursive=TRUE)
        }
        error_file <- paste0(error_dir, "error.txt")
        file.create(error_file)
    } else {
        error_file <- NA_character_
    }
    return(error_file)
}

#' Writes error message to file if path is not Null or NA
#' 
#' @param error_file A string. The path to the error file. 
#' @param msg A string. The message to write to the error file.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
write_to_error_file <- function(error_file, 
                                msg) {
    if(!is.null(error_file) & !(any(is.na(error_file)))) {
        readr::write_file(x=msg, file=error_file, append=TRUE)
    }
}

