#' Fits the Cox model for a given endpoint and predictor
#' 
#' @param pheno_score_data A data.frame with at least columns `DATE_OF_BIRTH`
#'                         and i.e. `J10_ASTHMA`, and `J10_ASTHMA_AGE_DAYS` 
#'                         where the columns are the study endpoint and 
#'                         date, which will differ depending on the input 
#'                         variable `endpt`. And the column defined in `predictor`.
#' @inheritParams add_coxph_row
#' 
#' @export 
#' 
#' @author Kira E. Detrois
run_coxph_ana <- function(pheno_score_data,
                          endpt,
                          covs,
                          write_res=FALSE) {
    pheno_score_data <- make_covs_fctrs(pheno_score_data, covs)
    pred_string <- get_pred_string(covs)
    surv_mdl <- stats::as.formula(paste0(
                        "survival::Surv(", endpt, "_AGE_DAYS, ",  endpt, ") ~ ",  pred_string))
    if(length(unique(pheno_score_data$SCORE_GROUP)) >= 2) {
        coxph_mdl <- survival::coxph(surv_mdl,
                                     data=pheno_score_data)
        return(coxph_mdl)
    } else {
        return(NULL)
    }
}

make_covs_fctrs <- function(pheno_score_data,
                            covs) {
    for(cov in covs) {
        if(is.character(pheno_score_data[cov,])) {
            pheno_score_data[cov,] <- as.factor(pheno_score_data[cov,])
        }
    }
    return(pheno_score_data)
}

get_pred_string <- function(covs) {
    paste0("SCORE_GROUP + ", paste0(covs, collapse=" + "))
}