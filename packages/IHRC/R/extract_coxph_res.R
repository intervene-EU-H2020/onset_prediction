#' Extract relevant results from the Cox-PH model
#' 
#' This function extracts the results of a Cox proportional hazards model 
#' fit and returns them in a list.
#' 
#' @param coxph_mdl A `coxph` object. See function [survival::coxph].
#' @param score_type A string (vector). The score types used in the analysis.
#' 
#' @return A list with elements 
#' \itemize{
#' \item beta: Estimated regression coefficients
#' \item std_err: Standard errors of the coefficients
#' \item p_val: P-values of the coefficients
#' \item HR: Hazard ratios
#' \item CI_neg: Negative confidence interval limits for the hazard ratios
#' \item CI_pos: Positive confidence interval limits for the hazard ratios
#' \item groups: Groupings for the predictors
#' \item preds: Predictor names
#' }
#' If the model is NULL, an empty list is returned
#' 
#' @export 
#' 
#' @author Kira E. Detrois
extract_coxph_res <- function(coxph_mdl, 
                              score_type) {
    if(!is.null(coxph_mdl)) {
        betas <- summary(coxph_mdl)$coefficients[,"coef"]
        SE <- summary(coxph_mdl)$coefficients[,"se(coef)"]
        pvals <- summary(coxph_mdl)$coefficients[,"Pr(>|z|)"]
        OR <- exp(betas)
        CI <- get_CI(betas, SE)
        preds <- rownames(summary(coxph_mdl)$coefficients)
        groups <- "no groups"

        return(list(beta=betas, std_err=SE, p_val=pvals, HR=OR, CI_neg=exp(CI$neg), CI_pos=exp(CI$pos), groups=groups, preds=preds))
    } else {
        return(list())
    }
}
