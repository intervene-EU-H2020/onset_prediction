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
#' @slot max_age A numeric. The maximum age for individuals in the study. 
#'                          Individuals are censored, once they reach the maximum age. 
#' @slot write_res A boolean. Whether to write log files.
#' @slot res_dir A character. The path to the results directory.
#' 
#' @importFrom methods new
#' @importFrom lubridate %--%
#' 
#' @export
#' 
#' @author Kira E. Detrois
study <- methods::setClass("study", 
                           slots=list(study_type="character",
                                      study_data="data.frame",
                                      endpt="character",
                                      exp_age="numeric",
                                      exp_len="numeric",
                                      wash_len="numeric",
                                      obs_len="numeric",
                                      obs_end_date="Date",
                                      down_fctr="numeric",
                                      ancs="character",
                                      max_age="numeric",
                                      write_res="logical",
                                      res_dir="character"),
                           prototype=list(study_type="forward",
                                          endpt=NA_character_,
                                          exp_age=NA_integer_,
                                          exp_len=NA_integer_,
                                          wash_len=NA_integer_,
                                          obs_len=NA_integer_,
                                          obs_end_date=as.Date("2021/01/01"),
                                          down_fctr=NA_real_,
                                          ancs=NA_character_,
                                          max_age=200,
                                          write_res=FALSE,
                                          res_dir=NA_character_))
#' @importFrom methods callNextMethod
#' @importFrom lubridate %--%
setMethod("initialize", "study", function(.Object, ...) {
    .Object <- callNextMethod()
    check_cols_exist(.Object@study_data, .Object@endpt, "initialize")
    .Object@study_data <- apply_endpt_filter(.Object@study_data,
                                             .Object@endpt)
    .Object@study_data <- dplyr::select(.Object@study_data,
                                        ID, 
                                        SEX, 
                                        DATE_OF_BIRTH, 
                                        ANCESTRY, 
                                        # Otherwise dplyr will throw error. 
                                        # test_endpt_input_correct already 
                                        # checks that this is only a single 
                                        # string and not a vector.
                                        .Object@endpt,  
                                        paste0(.Object@endpt, "_DATE"),
                                        END_OF_FOLLOWUP,
                                        dplyr::starts_with("PC"),
                                        BATCH,
                                        EDU)
    .Object@study_data <- process_edu(.Object@study_data)
    .Object@study_data <- set_study_dates(study_data=.Object@study_data,
                                    study_type=.Object@study_type,
                                    exp_age=.Object@exp_age,
                                    exp_len=.Object@exp_len,
                                    wash_len=.Object@wash_len,
                                    obs_len=.Object@obs_len,
                                    obs_end_date=.Object@obs_end_date)
    .Object@study_data <- get_study_elig_indv(study=.Object)

    return(.Object)
})

process_edu <- function(study_data) {
    study_data$EDU <- as.integer(stringr::str_extract(study_data$EDU, "[0-9]"))
    return(study_data)
}

#' Filters out cohorts that were used to train the 
#' 
#' @export 
#' 
#' @author Kira E. Detrois
apply_endpt_filter <- function(study_data,
                               endpt) {
    if(endpt %in% c("I9_CHD", "I9_AF")) {
        study_data <- dplyr::filter(study_data,
                                     COHORT != "THL BIOBANK COROGENE")
    } else if(endpt == "J10_ASTHMA") {
        study_data <- dplyr::filter(study_data, 
                                    !stringr::str_detect(COHORT, "FINRISK") &
                                    COHORT != "THL BIOBANK HEALTH 2000")
    } else if(endpt == "C3_COLORECTAL") {
        study_data <- dplyr::filter(study_data,
                                    !(CHIP %in% c("Illumina_Human670_Human610", "Illumina_Human610-Quadv1_B")))
    } else if(endpt %in% c("ILD", "C3_BRONCHUS_LUNG")) {
        study_data <- dplyr::filter(study_data,
                                    !R3)
    }
    return(study_data)
}

setValidity("study", function(object) {
    msg <- ""
    msg <- test_integer_correct(msg, object@wash_len, "wash_len", TRUE)
    msg <- test_integer_correct(msg, object@obs_len, "obs_len", TRUE)
    msg <- test_integer_correct(msg, object@down_fctr, "down_fctr")
    msg <- test_endpt_input_correct(msg, object@endpt)
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