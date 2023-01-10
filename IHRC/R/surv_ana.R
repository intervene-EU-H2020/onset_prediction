#' An S4 class representing the survival analysis
#' 
#' @slot study A S4 study object. The current study setups for on whicht
#'              the eligible indiviuals are selected.
#' @slot elig_score_data A dataframe with the phenotypic and score data
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
                slots=list(study="study",
                           elig_score_data="data.frame",
                           min_indvs="numeric",
                           preds="character",
                           plot_preds="character",
                           res_descr="character",
                           write_res="logical",
                           res_dir="character"),
                prototype=list(elig_score_data=tibble::tibble(),
                               min_indvs=5,
                               preds=c("SEX", "YEAR_OF_BIRTH"),
                               plot_preds=NA_character_,
                               res_descr="",
                               write_res=FALSE,
                               res_dir=NA_character_))

#' @importFrom methods callNextMethod
setMethod("initialize", "surv_ana", function(.Object, ...) {
    .Object <- callNextMethod()
    return(.Object)
})
