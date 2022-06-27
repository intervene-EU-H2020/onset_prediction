#' An S4 class representing the study setup
#' 
#' @slot elig_indv The actual data.frame with eligible individuals under the
#'       current study setup.
#' @slot endpt A character. The column name of the current endpoint of interest. 
#' @slot exp_age An integer. Age at which exposure period starts (in years)
#' @slot exp_len An integer. Length of the exposure period (in years)
#' @slot wash_len An integer. Length of the washout period (in years)
#' @slot obs_len  An integer. Length of the observation period (in years)
#' @slot downsample_fctr An integer. Defines how many controls there
#'                                   should be for every case.
#'                                   Default is NA, which means no
#'                                   downsampling is performed.
#' 
#' @export
#' 
#' @author Kira E. Detrois
study <- methods::setClass("study", 
                            slots=list(elig_indv="tbl",
                                        endpt="character",
                                        exp_age="numeric",
                                        exp_len="numeric",
                                        wash_len="numeric",
                                        obs_len="numeric",
                                        downsample_fctr="numeric"),
                            prototype=list(elig_indv=tibble::tibble(),
                                        endpt=NA_character_,
                                        exp_age=NA_integer_,
                                        exp_len=NA_integer_,
                                        wash_len=NA_integer_,
                                        obs_len=NA_integer_,
                                        downsample_fctr=NA_integer_))

setValidity("study", function(object) {
    msg <- ""
    msg <- test_integer_correct(msg, object@exp_age, "exp_age", TRUE)
    msg <- test_integer_correct(msg, object@exp_len, "exp_len", TRUE)
    msg <- test_integer_correct(msg, object@wash_len, "wash_len", TRUE)
    msg <- test_integer_correct(msg, object@obs_len, "obs_len", TRUE)
    msg <- test_integer_correct(msg, object@downsample_fctr, "downsample_fctr")
    msg <- test_endpt_input_correct(msg, object@endpt)
    if(msg != "") {
        return(msg)
    } else {
        return(TRUE)
    }
})

test_integer_correct <- function(msg, var, var_name, not_na=FALSE) {
    if(!is.na(var)) {
        if (length(var) != 1) {
            msg <- paste0(msg, "@", var_name, " needs to be a single integer not a vector of integers.\n")
        }
        if(!(as.integer(var) == var)) {
            msg <- paste0(msg, "@", var_name, " needs to be an integer. Instead got: ", var, "\n")
        }
    }
    if(is.na(var) & not_na) {
        msg <- paste0(msg, var_name, " needs to be provided for a valid study setup.")
    }
    return(msg)
}

test_endpt_input_correct <- function(msg, endpt) {
    if(!(length(endpt) == 1)) {
        msg <- paste0(msg, "The variable endpt needs to be a character string and not a vector of characters.")
    } else if(is.na(endpt)) {
        msg <- paste0(msg, "Endpoint needs to be provided for a valid study setup.")
    }   
     
    return(msg)
}