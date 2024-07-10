#' Check Column Names
#' 
#' Checks if certain columns with specific names are present in a data frame, 
#' and if they are not present, print a warning message indicating that the column is missing.
#' 
#' @param expect_cols A string (vector). The expected column names
#' @param cols A string (vector). The actual column name.
#' @param file_path A string (vector). The file from which the column names come from.
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
  missing_columns <- c()
  for(expect_col in expect_cols) {
    if(!any(stringr::str_detect(cols, expect_col))) {
      missing_columns <- c(missing_columns, expect_col)
    }
  }
  # Print warning message for missing columns
  if(length(missing_columns) > 0) {
    warning(paste0("Warning. Columns named: ", paste(missing_columns, collapse = ", "), " are missing. \nHave columns: ", paste0(cols, collapse=", "), "\nPlease change the file: ", file_path))
  }
}