#' An S4 class representing the study setup
#' 
#' @slot pheno_data A data.frame with at least the columns: 
#'                   `ID`, the columns specified in `covs` and 
#'                   i.e. `J10_ASTHMA`, and `J10_ASTHMA_AGE_DAYS` where 
#'                   the columns are the study endpoint and date, which 
#'                   will differ depending on the chosen `endpts`.
#'                   The phenotypic data on the individuals.
#' @slot elig_indv A data.frame with all eligible individuals under the
#'                  chosen study setup. This will be created when
#'                  the object is initialized based on the rest of
#'                  the data.
#' @slot elig_score_data A data.frame with all eligible individuals under 
#'                  the chosen study setup and their scores. This will be 
#'                  created when the object is initialized based on the 
#'                  rest of the data.
#' @slot score_type A character. The score type. At the moment the two
#'                      options are `CCI`, and `PRS`.
#' @slot max_age A numeric. The maximum age at the end of the exposure
#'                  window of individuals.
#' @slot study A S4 study object. The current study setups for on whicht
#'              the eligible indiviuals are selected.
#' @slot covs A vector of characters. The column names of the covariates 
#'              to add to the predictor of the Cox-PH model.
#' @slot bin_cut A numeric. The binary cutoff value for classifying high
#'                  and low score individuals. Currently only in use if
#'                  the `score_type == CCI`.
#' @slot min_indvs An integer. The minimum number of individuals each 
#'                  group in the analyses needs to have. This is important 
#'                  for being able to export data from the FinnGen Sanbdox.

#' @slot write_res A boolean. Defines whether to save the results to 
#'                             files.
#' @slot res_dir A character. The directory to write the results and
#'                             log to.
#' 
#' @importFrom methods new
#' @export
#' 
#' @author Kira E. Detrois
surv_ana <- methods::setClass("surv_ana", 
                slots=list(study="study",
                           elig_score_data="data.frame",
                           score_type="character",
                           min_indvs="numeric",
                           covs="character",
                           bin_cut="numeric",
                           write_res="logical",
                           res_dir="character"),
                prototype=list(elig_score_data=tibble::tibble(),
                               score_type=NA_character_,
                               min_indvs=5,
                               covs=c("SEX", "YEAR_OF_BIRTH"),
                               bin_cut=1,
                               write_res=FALSE,
                               res_dir=NA_character_))

#' @importFrom methods callNextMethod
setMethod("initialize", "surv_ana", function(.Object, ...) {
    .Object <- callNextMethod()
    .Object@elig_score_data <- get_elig_score_data(.Object)
    if(nrow(.Object@elig_score_data) > 0) {
        .Object@elig_score_data <- add_risk_group_col(.Object@elig_score_data,
                                                      .Object)
    }
    return(.Object)
})
