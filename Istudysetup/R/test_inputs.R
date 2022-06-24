#' Tests that the input for endpt is of the correct type.
#' Meaning, a string and not a vector of strings. Also tests
#' that the endpoint is actually included in the data.frame
#' 
#' @param envir A list. The function call variables.
test_endpt_input_correct <- function(pheno_data, endpt) {
    cols = colnames(pheno_data)
    assertthat::assert_that(is.character(endpt),
                            msg=paste0("The variable endpt needs to be a character string. Instead got: ", class(endpt)))
    assertthat::assert_that(length(endpt) == 1, 
                            msg="The variable endpt needs to be a character string and not a vector of characters.")
    assertthat::assert_that(endpt %in% cols,
                            msg=paste0("The chosen endpoint ", endpt, " is not part of the data.\nThe data.frame pheno_data has columns:\n", paste0(cols, collapse=", ")))
}

# #' Tests that the length variables of the function call are integers.
# #' 
# #' @param envir A list. The function call variables.
# test_length_vars_are_integers <- function(envir) {
#     for(name in names(envir)) {
#         if(!(name %in% c("pheno_data", "endpt", "bds", "downsample_fctr", "write_res", "res_dir", "write_res", "res_dir"))) {
#             var = envir[[name]]
#             assertthat::assert_that(is.numeric(var) | is.integer(var),
#                                     msg=paste0("The variable ", name, " needs to be an integer."))
#             assertthat::assert_that(as.integer(var) == var,
#                                     msg=paste0("The variable ", name, " needs to be an integer."))
#         }
#     }   
# }

# #' Tests that variables that need to be a date are of correct type.
# #' 
# #' @param var Ideally a Date.
# #' @param var_name A character. The name of the variable for more precise error printing.
# test_date_var_correct <- function(var, var_name=NA) {
#     if(!is.na(var_name))
#         message = paste0("The variable ", var_name, " should be of type Date.")
#     else {
#        message = paste0("Some variable needs to be of type Date but is not.")
#     }
#     assertthat::assert_that(lubridate::is.Date(var), 
#                             msg=paste0("The variable ", var_name, " should be of type Date."))
# }