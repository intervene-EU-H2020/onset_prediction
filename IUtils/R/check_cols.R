#' Checks the column names of a file against the expected columns
#' 
#' Replaces the columns names and throws a warning for largely differing names.
#' 
#' @param expect_cols A character (vector). The expected column names
#' @param cols A character (vector). The received column name.
#' @param file_path A character (vector). The file from which the column names come from.
#' 
#' @return The expected column names.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
check_cols <- function(expect_cols,
                       cols,
                       file_path) {
    cols_letters <- unify_strs(cols)
    for(expect_col in expect_cols) {
        expect_col_letters <- unify_strs(expect_col)
        detect_bin <- stringr::str_detect(cols_letters, paste0("(", expect_col_letters, ")"))
        if(sum(detect_bin) == 0) {
            warning(paste0("Warning. Column named ", expect_col, " is missing. Please change the file structure.\n Given file path: ", file_path))
        }
        cols[detect_bin] <- expect_col
    }
    return(cols)
}

#' Casts columns names to lower charachter and removes all special signs
#' 
#' @param str A character. The string to be cast
#' 
#' @export 
#' 
#' @author Kira E. Detrois
unify_strs <- function(str) {
    return(stringr::str_to_lower(stringr::str_replace_all(str, "[[:punct:]]", " ")))
}