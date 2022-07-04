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
#' @inheritParams calc_studies_hrs
#' 
#' @return A Cox-PH model object or `NULL` if the model couldn't be fit.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
run_coxph_ana <- function(pheno_score_data,
                          endpt,
                          covs) {
    pheno_score_data <- make_covs_fctrs(pheno_score_data, covs)
    surv_mdl <- get_surv_model_formula(endpt, covs)

    if(length(unique(pheno_score_data$SCORE_GROUP)) >= 2) {
        coxph_mdl <- survival::coxph(surv_mdl,
                                     data=pheno_score_data)
        return(coxph_mdl)
    } else {
        return(NULL)
    }
}

#' Makes all character covariates a factor
#' 
#' @inheritParams run_coxph_ana
#' 
#' @return The tibble with character covariate columns now factors.
#' 
#' @author Kira E. Detrois
make_covs_fctrs <- function(pheno_score_data,
                            covs) {
    for(cov in covs) {
        if(is.character(pheno_score_data[,cov])) {
            pheno_score_data <- dplyr::mutate_at(pheno_score_data, 
                                                 cov, 
                                                 as.factor)
        }
    }
    return(pheno_score_data)
}

#' Creates the forumla string for the Cox-PH model.
#' 
#' @inheritParams run_coxph_ana
#' 
#' @return A character. The formula string for the Cox-PH model.
get_surv_model_formula <- function(endpt,
                                   covs) {
    pred_string <- get_pred_string(covs)
    stats::as.formula(paste0(
                        "survival::Surv(", endpt, "_AGE_DAYS, ",  endpt, ") ~ ",  pred_string))
}

#' Creates the predictor string for the Cox-PH model
#' 
#' @inheritParams run_coxph_ana
#' 
#' @return A character. The predictor string for the Cox-PH model.
#' 
#' @author Kira E. Detrois
get_pred_string <- function(covs) {
    paste0("SCORE_GROUP + ", paste0(covs, collapse=" + "))
}