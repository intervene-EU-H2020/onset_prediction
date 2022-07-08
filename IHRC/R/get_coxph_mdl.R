#' Fits the Cox-PH model for a given endpoint and covariates
#' 
#' For i.e. `endpt = "J10_ASTHMA"` and `covs = c("SEX", "YEAR_OF_BIRTH")` 
#' the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE_GROUP + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @param pheno_score_data A data.frame with at least the columns: 
#'                        `SCORE_GROUP`, the columns specified in `covs` 
#'                        and i.e. `J10_ASTHMA`, and `J10_ASTHMA_AGE_DAYS` 
#'                        where the columns are the study endpoint and 
#'                        date, which will differ depending on the input 
#'                        variable `endpt`.
#' @param endpt A character. The column name of the current endpoint of 
#'                           interest.
#' @inheritParams add_coxph_res_row
#' @inheritParams calc_endpt_studies_hrs
#' 
#' @return A Cox-PH model object or `NULL` if the model couldn't be fit.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_coxph_mdl <- function(pheno_score_data,
                          endpt,
                          covs,
                          pred_score="SCORE_GROUP") {
    pheno_score_data <- make_covs_fctrs(pheno_score_data, covs)
    coxph_formula <- get_coxph_formula(endpt, covs, pred_score)

    build_mdl <- TRUE
    if(pred_score == "SCORE_GROUP") {
        Istudy::check_cols_exist(pheno_score_data, "SCORE_GROUP", "get_coxph_mdl")
        if(length(unique(pheno_score_data$SCORE_GROUP)) < 2) {
            build_mdl <- FALSE
        }
    }
    if(build_mdl) {
        coxph_mdl <- survival::coxph(formula=coxph_formula, 
                                     data=pheno_score_data,
                                     # Larger fit object but no need to
                                     # other functions to reconstruct
                                     # which fails in this setup
                                     model=TRUE)
    } else {
        return(NULL)
    }
    return(coxph_mdl)
}