#' An S4 class representing the survival analysis
#' 
#' @slot study A S4 study object. The current study setups for on whicht
#'              the eligible indiviuals are selected.
#' @slot study_data A dataframe with the phenotypic and score data
#'                          of all eligible individuals under 
#'                          the current study setup and their scores. 
#' @slot min_indvs An integer. The minimum number of individuals each 
#'                  group in the analyses needs to have. 
#' @slot preds A character (vector). The names of the covariates 
#'              to use in the Cox-PH model.
#' @slot plot_preds A character (vector). 
#'                      The predictors to use when plotting the HRs.
#' @slot res_descr A character. An additional descriptive that can be 
#'                  used for distinguishing the results files. 
#' @slot write_res A boolean. Defines whether to save the results to 
#'                             files. Default: FALSE.
#' @slot res_dir A character. The directory to write the results and
#'                             logs to. Default: NA.
#' 
#' @importFrom methods new
#' @export
#' 
#' @author Kira E. Detrois
surv_ana <- methods::setClass("surv_ana", 
                slots=list(min_indvs="numeric",
                           score_types="character",
                           create_score_combos="logical",
                           score_combos="list",
                           covs="character",
                           preds="character",
                           plot_preds="character",
                           res_descr="character",
                           write_res="logical",
                           res_dir="character"),
                prototype=list(min_indvs=5,
                               create_score_combos=FALSE,
                               score_combos=list(),
                               preds=c("SEX", "YEAR_OF_BIRTH"),
                               plot_preds=NA_character_,
                               res_descr="",
                               write_res=FALSE,
                               res_dir=NA_character_))

#' @importFrom methods callNextMethod
setMethod("initialize", "surv_ana", function(.Object, ...) {
    .Object <- callNextMethod()
    if(.Object@create_score_combos) {
        .Object@score_combos <- IUtils::get_score_types(.Object@score_types, 
                                                        .Object@create_score_combos)
    } else {
        .Object@score_combos <- list()
        .Object@score_combos[[1]] <- .Object@score_types
        .Object@score_combos[[2]] <- ""
    }
    .Object@preds <- get_all_preds_sorted(.Object@score_types, .Object@covs)
    .Object@plot_preds <- .Object@score_types
    return(.Object)
})


#' Sets `endpt` of the S4 study object to a new value
#' 
#' @param .Object The S4 surv_ana object.
#' @param preds A character. The new predictors.
#' @param plot_preds A character. The new predictors for potting
setGeneric(name="setPreds",
    def=function(.Object, crnt_score_types) { standardGeneric("setPreds") } 
)

#' Sets `endpt` of the S4 study object to a new value
#' 
#' @param .Object The S4 study object.
#' @param endpt A character. The new endpoint.
#' 
#' @export 
setMethod(f="setPreds",
          signature="surv_ana",
          definition=function(.Object, crnt_score_types) {
                              .Object@preds <- get_all_preds_sorted(crnt_score_types, .Object@covs)
                              .Object@plot_preds <- crnt_score_types
                              methods::validObject(.Object)
                              return(.Object)
    }
)