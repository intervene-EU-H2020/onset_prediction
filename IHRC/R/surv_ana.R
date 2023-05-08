#' An S4 class representing the survival analysis
#' 
#' Contains information about the score types 
#' 
#' @slot min_indvs An integer. The minimum number of individuals each 
#'                  group in the analyses needs to have. 
#' @slot score_types A string (vector). The score types used in the analysis.
#' @slot covs A string (vector). The added covariates for the Cox-PH model
#'               additional to the `score_types`.
#' @slot create_score_combos A boolean, whether or not to create all possible
#'          score type combinations from the score_type vector.
#'          see function [IUtils::get_all_possible_score_type_combs].
#' @slot score_combos A list. The score type combinations to be run in the
#'              analyses.
#' @slot preds A string (vector). The names of the all predictors to 
#'              be used in the Cox-PH model.
#' @slot write_res A boolean. Defines whether to save the results to 
#'                             files. 
#' @slot res_dir A string. The directory to write the results and
#'                             logs to. 
#' @slot error_file A string. The file path to the error file.
#' 
#' @importFrom methods new
#' 
#' @export
#' 
#' @author Kira E. Detrois
surv_ana <- methods::setClass("surv_ana", 
                slots=list(min_indvs="numeric",
                           score_types="character",
                           covs="character",
                           create_score_combos="logical",
                           score_combos="list",
                           preds="character",
                           write_res="logical",
                           res_dir="character",
                           error_file="character"),
                prototype=list(min_indvs=5,
                               create_score_combos=FALSE,
                               score_combos=list(),
                               preds=c("SEX", "YEAR_OF_BIRTH"),
                               write_res=FALSE,
                               res_dir=NA_character_,
                               error_file=NA_character_))

#' Constructor for the `surv_ana` class
#' 
#' @importFrom methods callNextMethod
setMethod("initialize", "surv_ana", function(.Object, ...) {
    .Object <- callNextMethod()
    .Object@error_file <- create_log_file(.Object)
    if(.Object@create_score_combos) {
        .Object@score_combos <- IUtils::get_score_types(.Object@score_types, 
                                                        .Object@create_score_combos)
    } else {
        .Object@score_combos <- list()
        .Object@score_combos[[1]] <- .Object@score_types
        .Object@score_combos[[2]] <- ""
    }
    .Object@preds <- get_all_preds_sorted(.Object@score_types, .Object@covs)
    return(.Object)
})

#' Creates log file
#' 
#' @param .Object An S4 `surv_ana` object. The current survival analysis setup. 
#'                   See class definition [IHRC::surv_ana].
#' 
#' @return A string. The file path for the error file.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
create_log_file <- function(.Object) {
    error_dir <- paste0(.Object@res_dir, "log/")
    if(.Object@write_res) {
        if(!dir.exists(error_dir)) {
            writeLines(paste0("The file directory ", error_dir, " does not exist. Trying to create it."))
            dir.create(error_dir, recursive=TRUE)
        }
        error_file <- paste0(error_dir, "error.txt")
        file.create(error_file)
    } else {
        error_file <- NA_character_
    }
    return(error_file)
}

#' Writes error message to file if path is not Null or NA
#' 
#' @param error_file A string. The path to the error file. 
#' @param msg A string. The message to write to the error file.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
write_to_error_file <- function(error_file, 
                                msg) {
    if(!is.null(error_file) & !(any(is.na(error_file)))) {
        readr::write_file(x=msg, file=error_file, append=TRUE)
    }
}


#' Sets the presictors of the S4 surv_ana object to a new value
#' 
#' @param .Object The S4 [IHRC::surv_ana] object.
#' @param crnt_score_types A string (vector). The current score types.
#' 
#' @return The updated S4 object
#' 
#' @author Kira E. Detrois
#' 
#' @export 
setGeneric(name="setPreds",
    def=function(.Object, crnt_score_types) { standardGeneric("setPreds") } 
)

#' Sets the presictors of the S4 surv_ana object to a new value
#' 
#' @param .Object The S4 [IHRC::surv_ana] object.
#' @param crnt_score_types A string (vector). The current score types.
#' 
#' @return The updated S4 object
#' 
#' @author Kira E. Detrois
#' 
#' @export 
setMethod(f="setPreds",
          signature="surv_ana",
          definition=function(.Object, crnt_score_types) {
                .Object@preds <- get_all_preds_sorted(crnt_score_types, .Object@covs)
                methods::validObject(.Object)
                return(.Object)
          }
)