#' Tests that the input for endpt is of the correct type.
#' Meaning, a string and not a vector of strings. Also tests
#' that the endpoint is actually included in the data.frame
#' 
#' @param envir A list. The function call variables.
test_endpt_input_correct <- function(envir) {
    cols = colnames(envir$pheno_data)
    assertthat::assert_that(is.character(envir$endpt),
                            msg="The variable endpt nedds to be a character string.")
    assertthat::assert_that(length(envir$endpt) == 1, 
                            msg="The variable endpt nedds to be a character string and not a vector of characters.")
    assertthat::assert_that(envir$endpt %in% cols,
                            msg="The chosen endpoint is not part of the data.")
}

#' Tests that the length variables of the function call are integers.
#' 
#' @param var Ideally a numeric or integer.
test_length_vars_are_integers <- function(envir) {
    for(name in names(envir)) {
        if(!(name %in% c("pheno_data", "endpt", "bds"))) {
            var = envir[[name]]
            assertthat::assert_that(is.numeric(var) | is.integer(var),
                                    msg=paste0("The variable ", name, " needs to be an integer."))
            assertthat::assert_that(as.integer(var) == var,
                                    msg=paste0("The variable ", name, " needs to be an integer."))
        }
    }   
}

#' Tests that variables that need to be a date are of correct type.
#' 
#' @param var Ideally a Date.
test_date_var_correct <- function(var, var_name) {
    assertthat::assert_that(is.Date(var), msg=paste0("The variable ", var_name, " should be of type Date."))
}