#' Check Column Names
#' 
#' Checks if certain columns with specific names are present in a data frame, 
#' and if they are not present, print a warning message indicating that the column is missing.
#' 
#' @param expect_cols A character (vector). The expected column names
#' @param cols A character (vector). The actual column name.
#' @param file_path A character (vector). The file from which the column names come from.
#' 
#' @return A modified vector of column names with the expected column names.
#'  
#' @export 
#' 
#' @author Kira E. Detrois
check_cols <- function(expect_cols, 
                       cols, 
                       file_path) {
  # Check for exact match of expected column names
  cols_regex <- paste0("(", paste0(cols, collapse=")|("), ")")
  detect_bins <- stringr::str_detect(expect_cols, cols_regex)
  # Print warning message for missing columns
  missing_columns <- expect_cols[!detect_bins]
  if(length(missing_columns) > 0) {
    warning(paste0("Warning. Columns named: ", paste(missing_columns, collapse = ", "), " are missing. Please change the file: ", file_path))
  }
}