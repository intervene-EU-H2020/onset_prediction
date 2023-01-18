#' An S4 class representing the study setup
#' 
#' @slot study_type A character. Can be either `forward` or `backward`. 
#'                  `forward` considers individuals of a certain age.
#'                  It calculates the exposure, washout, and observation 
#'                  period onwards from this age.
#'                  `backward` considers all individuals at a set time
#'                  point. The observation and washout period are calcualted 
#'                  backwards from this time point.
#' @slot exp_age An integer. Age at which exposure period starts 
#'              (in years).
#' @slot exp_len An integer. Length of the exposure period (in years).
#' @slot wash_len An integer. Length of the washout period (in years).
#' @slot obs_len  An integer. Length of the observation period (in years).
#' @slot obs_end_date A Date. The end of the observation period.
#' @slot down_fctr An integer. Defines how many controls there
#'                                   should be for every case.
#'                                   Default is NA, which means no
#'                                   downsampling is performed.
#' @slot ancs A character (vector). The ancestries to consider.
#' @slot obs_age_range A numeric. The age range of individuals in the observation
#'                                 period. Inclusive interval. 
#' 
#' @importFrom methods new
#' @importFrom lubridate %--%
#' 
#' @export
#' 
#' @author Kira E. Detrois
study_setup <- methods::setClass("study_setup", 
                           slots=list(study_type="character",
                                      exp_age="numeric",
                                      exp_len="numeric",
                                      wash_len="numeric",
                                      obs_len="numeric",
                                      obs_end_date="Date",
                                      down_fctr="numeric",
                                      exp_f1998="logical",
                                      ancs="character",
                                      obs_age_range="numeric"),
                           prototype=list(study_type="forward",
                                          exp_age=NA_integer_,
                                          exp_len=NA_integer_,
                                          wash_len=NA_integer_,
                                          obs_len=NA_integer_,
                                          obs_end_date=as.Date("2019/01/01"),
                                          down_fctr=NA_real_,
                                          exp_f1998=TRUE,
                                          ancs=NA_character_,
                                          obs_age_range=c(0,200)))

#' @importFrom methods callNextMethod
setMethod("initialize", "study_setup", function(.Object, ...) {
    # Check if the study type is "backward" and set the exp_age and exp_len values accordingly
    if (.Object@study_type == "backward") {
        if (!all(is.na(.Object@exp_len))) {
            .Object@exp_age <- NA_integer_
        } else {
            print("Warning: Carefuly the exposure length was not set. ")
            .Object@exp_len <- NA_integer_
        }
    }
    .Object <- callNextMethod()
})

#' An S4 class representing the study setup
#' 
#' @slot study_type A character. Can be either `forward` or `backward`. 
#'                  `forward` considers individuals of a certain age.
#'                  It calculates the exposure, washout, and observation 
#'                  period onwards from this age.
#'                  `backward` considers all individuals at a set time
#'                  point. The observation and washout period are calcualted 
#'                  backwards from this time point.
#' @slot study_data A tibble. The data on all study individuals.
#' @slot endpt A character. The column name of the current endpoint of 
#'             interest. 
#' @importFrom methods new
#' @importFrom lubridate %--%
#' 
#' @export
#' 
#' @author Kira E. Detrois
study <- methods::setClass("study", 
                           representation(study_setup="study_setup",
                                          study_data="data.frame",
                                          endpt="character"),
                           contains="study_setup")


#' @importFrom methods callNextMethod
setMethod("initialize", "study", function(.Object, ...) {
    .Object <- callNextMethod()
    check_cols_exist(.Object@study_data, .Object@endpt, "initialize")
    .Object@study_data <- process_ISCED_2011(.Object@study_data)
    .Object@study_data <- set_study_dates(study_data=.Object@study_data,
                                          study_setup=.Object@study_setup)
    .Object@study_data <- add_age_obs_cols(study_data=.Object@study_data)
    .Object@study_data <- get_study_elig_indv(study=.Object)

    return(.Object)
})

process_ISCED_2011 <- function(study_data) {
    study_data$ISCED_2011 <- as.integer(stringr::str_extract(study_data$ISCED_2011, "[0-9]"))
    return(study_data)
}

setValidity("study_setup", function(object) {
    msg <- ""
    msg <- test_integer_correct(msg, object@wash_len, "wash_len", TRUE)
    msg <- test_integer_correct(msg, object@obs_len, "obs_len", TRUE)
    msg <- test_integer_correct(msg, object@down_fctr, "down_fctr")
    msg <- ifelse(!object@study_type %in% c("backward", "forward"),
        paste0(object@study_type, " is not a valid study type. Can be either `backward`, or `forward`"),
        ""
    )
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
#' Sets `endpt` of the S4 study object to a new value
#' 
#' @param .Object The S4 study object.
#' @param endpt A character. The new endpoint.
setGeneric(name="setEndpt",
           def=function(.Object, endpt) { standardGeneric("setEndpt") } 
)

#' Sets `endpt` of the S4 study object to a new value
#' 
#' @param .Object The S4 study object.
#' @param endpt A character. The new endpoint.
#' 
#' @export 
setMethod(f="setEndpt",
          signature="study",
          definition=function(.Object,endpt) {
                              .Object@endpt <- endpt
                              methods::validObject(.Object)
                              return(.Object)
                      }
)

#' Sets `endpt` of the S4 study object to a new value
#' 
#' @param .Object The S4 study object.
#' @param endpt A character. The new endpoint.
#' 
#' @export 
setGeneric(name="updateStudyData",
           def=function(.Object, study_data) { standardGeneric("updateStudyData") } 
)

#' Sets `endpt` of the S4 study object to a new value
#' 
#' @param .Object The S4 study object.
#' @param endpt A character. The new endpoint.
#' 
#' @export 
setMethod(f="updateStudyData",
          signature="study",
          definition=function(.Object, study_data) {
                              .Object@study_data <- study_data
                              methods::validObject(.Object)
                              return(.Object)
                      }
)