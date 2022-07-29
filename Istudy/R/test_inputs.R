#' Checks that the data.frame contains the chosen columns
#' 
#' @param study_data A data.frame.
#' @param cols A character. The columns `study_data` needs to contain.
#' @param func_name A character. The name of the function to print in the error message.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
check_cols_exist <- function(study_data,
                             cols,
                             func_name) {
    error_msg <- paste0("Error in ", func_name, " missing columns ", cols[!(cols %in% colnames(study_data))], " in the study_data data.frame.\nHave columns:\n", paste(colnames(study_data), collapse=" "))
    assertthat::assert_that(all(cols %in% colnames(study_data)), 
                            msg=error_msg)
}

#' Tests that variables that need to be a date are of correct type.
#' 
#' @param var Ideally a Date.
#' @param var_name A character. The name of the variable for more precise error printing.
test_date_var_correct <- function(var, var_name=NULL) {
    if(!is.null(var_name))
        message = paste0("The variable ", var_name, " should be of type Date.")
    else {
       message = paste0("Some variable needs to be of type Date but is not.")
    }
    assertthat::assert_that(lubridate::is.Date(var), 
                            msg=paste0("The variable ", var_name, " should be of type Date."))
}
