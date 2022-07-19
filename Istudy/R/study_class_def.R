#' An S4 class representing the study setup
#' 
#' @slot study_type A character. Can be either `forward` or `backward`. 
#'              `forward` considers individuals of a certain age 
#'              and a set exposure, washout, and observation periods 
#'              calcualted onwards from this age. It can simply be created
#'              by creating a S4 study object.
#'              `backward` considers all individuals at a set time
#'              point. The observation and washout period are calcualted 
#'              backwards from this time point. The exposure period will be 
#'              different for each individual depending on their birth date. 
#'              This setup can be created, using the function 
#'              \code{\link{get_backward_study}}.
#' @slot endpt A character. The column name of the current endpoint of interest. 
#' @slot exp_age An integer. Age at which exposure period starts (in years).
#' @slot exp_len An integer. Length of the exposure period (in years).
#' @slot exp_ids A character (vector). The IDs for the exposure lengths,
#'                   if they differ between individuals.
#' @slot wash_len An integer. Length of the washout period (in years).
#' @slot obs_len  An integer. Length of the observation period (in years).
#' @slot obs_end A Date. The end of the observation period.
#' @slot downsample_fctr An integer. Defines how many controls there
#'                                   should be for every case.
#'                                   Default is NA, which means no
#'                                   downsampling is performed.
#' @slot ancs A character (vector). The ancestries to consider.
#' @importFrom methods new
#' @export
#' 
#' @author Kira E. Detrois
study <- methods::setClass("study", 
                            slots=list(study_type="character",
                                       endpt="character",
                                       exp_age="numeric",
                                       exp_len="numeric",
                                       exp_ids="character",
                                       wash_len="numeric",
                                       obs_len="numeric",
                                       obs_end="Date",
                                       downsample_fctr="numeric",
                                       ancs="character"),
                            prototype=list(study_type="forward",
                                           endpt=NA_character_,
                                           exp_age=30,
                                           exp_len=10,
                                           exp_ids=NA_character_,
                                           wash_len=2,
                                           obs_len=8,
                                           obs_end=as.Date("3000/01/01"),
                                           downsample_fctr=NA_real_,
                                           ancs=NA_character_))

setValidity("study", function(object) {
    msg <- ""
    msg <- test_integer_correct(msg, object@exp_age, "exp_age", TRUE)
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
    if(!all(is.na(var))) {
        #if (length(var) != 1) {
        #    msg <- paste0(msg, "@", var_name, " needs to be a single integer not a vector of integers.\n")
        #} else 
        #if(!(all(as.integer(var) == var))) {
        #   msg <- paste0(msg, "@", var_name, " needs to be an integer. Instead got: ", paste0(var, collapse=" "), "\n")
        #}
    }
    if(all(is.na(var)) & not_na) {
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

#' Sets `endpt` of the S4 study object to a new value
#' 
#' @param theObject The S4 study object.
#' @param endpt A character. The new endpoint.
setGeneric(name="setEndpt",
           def=function(theObject, endpt) { standardGeneric("setEndpt") } 
)

#' Sets `endpt` of the S4 study object to a new value
#' 
#' @param theObject The S4 study object.
#' @param endpt A character. The new endpoint.
#' 
#' @export 
setMethod(f="setEndpt",
          signature="study",
          definition=function(theObject,endpt) {
                              theObject@endpt <- endpt
                              methods::validObject(theObject)
                              return(theObject)
                      }
)