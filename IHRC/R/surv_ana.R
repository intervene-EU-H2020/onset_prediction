#' An S4 class representing the survival analysis
#' 
#' Contains information about the score types 
#' 
#' @slot min_indvs An integer. The minimum number of individuals each 
#'                  group in the analyses needs to have. 
#' @slot score_types A string (vector). The score types used in the analysis.
#' @slot covs A string (vector). The added covariates for the Cox-PH model
#'               additional to the `score_types`.
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
                           models="list",
                           preds="character",
                           write_res="logical",
                           res_dir="character",
                           error_file="character",
                           write_progress="logical"),
                prototype=list(min_indvs=5,
                               models=list(),
                               preds=c("SEX", "YEAR_OF_BIRTH"),
                               write_res=FALSE,
                               res_dir=NA_character_,
                               error_file=NA_character_,
                               write_progress=FALSE))

#' Constructor for the `surv_ana` class
#' 
#' @importFrom methods callNextMethod
setMethod("initialize", "surv_ana", function(.Object, ...) {
    .Object <- callNextMethod()
    .Object@error_file <- create_log_file(.Object)
    .Object@preds <- get_all_preds_sorted(.Object@score_types, .Object@covs)

    .Object@models <- process_models(.Object@models)


    return(.Object)
})

process_models <- function(models) {
    for(model_nr in 1:length(models)) {
        if("PCs" %in% models[[model_nr]]) {
            models[[model_nr]] <- models[[model_nr]][-which(models[[model_nr]] == "PCs")]
            models[[model_nr]] <- c(models[[model_nr]], paste0("PC", 1:10))
        } 
    }
    return(models)
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
                crnt_covs <- crnt_score_types[crnt_score_types %in% .Object@covs]
                crnt_non_covs <- crnt_score_types[!(crnt_score_types %in% crnt_covs)]

                .Object@preds <- get_all_preds_sorted(crnt_non_covs, crnt_covs)
                methods::validObject(.Object)
                return(.Object)
          }
)