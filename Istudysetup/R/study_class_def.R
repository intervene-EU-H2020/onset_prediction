#' An S4 class representing the study setup
#' 
#' @slot data The actual data.frame with eligible individuals under the
#'       current study setup.
#' @slot exp_age An integer. Age at which exposure period starts (in years)
#' @slot exp_len asdf
#' @slot wash_len as
#' @slot n_cases asdf
#' @slot n_cntrls asdf
#' 
#' @export
study <- setClass("study", 
                  slots=list(elig_indv="tbl",
                             endpt="character",
                             exp_age="numeric",
                             exp_len="numeric",
                             wash_len="numeric",
                             obs_len="numeric",
                             n_cases="numeric",
                             n_cntrls="numeric",
                             downsample_fctr="numeric"),
                  prototype=list(elig_indv=tibble::tibble(),
                             endpt=NA_character_,
                             exp_age=NA_integer_,
                             exp_len=NA_integer_,
                             wash_len=NA_integer_,
                             obs_len=NA_integer_,
                             n_cases=NA_integer_,
                             n_cntrls=NA_integer_,
                             downsample_fctr=NA_integer_))

setValidity("study", function(object) {
    msg <- ""
    msg <- test_integer_correct(msg, object@exp_age, "exp_age")
    msg <- test_integer_correct(msg, object@exp_len, "exp_len")
    msg <- test_integer_correct(msg, object@wash_len, "wash_len")
    msg <- test_integer_correct(msg, object@obs_len, "obs_len")
    msg <- test_integer_correct(msg, object@n_cases, "n_cases")
    msg <- test_integer_correct(msg, object@n_cntrls, "n_cntrls")
    msg <- test_integer_correct(msg, object@downsample_fctr, "downsample_fctr")

    if(msg != "") {
        return(msg)
    } else {
        return(TRUE)
    }
})

test_integer_correct <- function(msg, var, var_name) {
    if(!is.na(var)) {
        if (length(var) != 1) {
            msg <- paste0(msg, "@", var_name, " needs to be a single integer not a vector of integers.\n")
        }
        if (!(is.numeric(var) | is.integer(var))) {
            msg <- paste0(msg, "@", var_name, " needs to be an integer. Instead got: ", class(var), "\n")
        } 
        if(!(as.integer(var) == var)) {
            msg <- paste0(msg, "@", var_name, " needs to be an integer. Instead got: ", var, "\n")
        }
    }
    return(msg)
}

test_endpt_correct <- function(msg, endpt, data) {
     cols = colnames(data)
     assertthat::assert_that(is.character(envir$study@endpt),
                             msg=paste0("The variable endpt needs to be a character string. Instead got: ", class(envir$study@endpt)))
     assertthat::assert_that(length(envir$study@endpt) == 1, 
                             msg="The variable endpt needs to be a character string and not a vector of characters.")
     assertthat::assert_that(envir$study@endpt %in% cols,
                             msg=paste0("The chosen endpoint ", envir$study@endpt, " is not part of the data.\nThe data.frame pheno_data has columns:\n", paste0(cols, collapse=", ")))
}