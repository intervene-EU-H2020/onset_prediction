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
#' @slot exp_f1998 A boolean. Defines whether to filter out any data from before 1998.
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
                                          exp_f1998=FALSE,
                                          ancs=NA_character_,
                                          obs_age_range=c(0,200)))

#' Constructor for the `study_setup` class
#' 
#' @importFrom methods callNextMethod
setMethod("initialize", "study_setup", function(.Object, ...) {
    # Check if the study type is "backward" and set the exp_age and exp_len values accordingly
    if (.Object@study_type == "backward") {
        if (!all(is.na(.Object@exp_len))) {
            .Object@exp_age <- NA_integer_
        } else {
            writeLines("Warning: Carefuly the exposure length was not set. ")
            .Object@exp_len <- NA_integer_
        }
    }
    .Object <- callNextMethod()
})

#' An S4 class representing the study setup
#' 
#' @slot study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @slot study_data A tibble. The data on all study individuals.
#' @slot endpt A character. The current endpoint of interest. 
#'  
#' @author Kira E. Detrois
#' 
#' @export
study <- methods::setClass("study", 
                           representation(study_setup="study_setup",
                                          study_data="data.frame",
                                          endpt="character"))


#' Constructor for the `study` class
#' 
#' @importFrom methods callNextMethod
setMethod("initialize", "study", function(.Object, ...) {
    .Object <- callNextMethod()
    if(nrow(.Object@study_data) > 0) {
        check_cols_exist(.Object@study_data, .Object@endpt, "initialize")
        #.Object@study_data <- process_EDU(.Object@study_data)
        .Object@study_data <- set_study_dates(study_data=.Object@study_data,
                                            study_setup=.Object@study_setup)
        .Object@study_data <- add_age_obs_cols(study_data=.Object@study_data)
        .Object@study_data <- get_study_elig_indv(study=.Object)
    }

    return(.Object)
})

#' Turn EDU codes into integers
#' 
#' @param study_data A tibble. The data on all study individuals. Needs
#'                      at least column `EDU`.
#' 
#' @return The updated study data
#' 
#' @author Kira E. Detrois
#' 
#' @export 
process_EDU <- function(study_data) {
    study_data$EDU <- as.integer(stringr::str_extract(study_data$EDU, "[0-9]"))
    return(study_data)
}

#' Checks that the `study_setup` gets valid input
#' 
#' @param study_data A tibble. The data on all study individuals. Needs
#'                      at least column `EDU`.
#' 
#' @return The updated study data
#' 
#' @author Kira E. Detrois
#' 
#' @export 
setValidity("study_setup", function(.Object) {
    msg <- ""
    msg <- test_integer_correct(msg, .Object@wash_len, "wash_len", TRUE)
    msg <- test_integer_correct(msg, .Object@obs_len, "obs_len", TRUE)
    msg <- test_integer_correct(msg, .Object@down_fctr, "down_fctr")
    msg <- ifelse(!.Object@study_type %in% c("backward", "forward"),
        paste0(.Object@study_type, " is not a valid study type. Can be either `backward`, or `forward`"),
        ""
    )
    if(msg != "") {
        return(msg)
    } else {
        return(TRUE)
    }
})

#' Checks input integers
#' 
#' @param msg A string. Previous message to append the new message to.
#' @param var A numeric. The variable to check.
#' @param var_name A string. The variable name.
#' @param not_na A boolean. Whether the variable is allowed to be NA. 
#' 
#' @return The updated study data
#' 
#' @author Kira E. Detrois
#' 
#' @export 
test_integer_correct <- function(msg, var, var_name, not_na=FALSE) {
    if(!all(is.na(var))) {
        if(!(all(as.integer(var) == var))) {
            msg <- paste0(msg, "@", var_name, " needs to be an integer. Instead got: ", paste0(var, collapse=" "), "\n")
        }
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
#' 
#' @return The updated `study` object.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
setGeneric(name="setEndpt",
           def=function(.Object, endpt) { standardGeneric("setEndpt") } 
)

#' Sets `endpt` of the S4 study object to a new value
#' 
#' @param .Object The S4 study object.
#' @param endpt A character. The new endpoint.
#' 
#' @return The updated `study` object.
#' 
#' @author Kira E. Detrois
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
#' @return The updated `study` object.
#' 
#' @author Kira E. Detrois
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
#' @return The updated `study` object.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
setMethod(f="updateStudyData",
          signature="study",
          definition=function(.Object,study_data) {
                              .Object@study_data <- study_data
                              methods::validObject(.Object)
                              return(.Object)
                      }
)