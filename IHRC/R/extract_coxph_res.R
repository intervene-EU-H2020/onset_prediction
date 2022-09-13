#' Extracts the relevant results from the Cox-PH model
#' 
#' @inheritParams add_coxph_res_row
#' @inheritParams run_surv_studies
#' 
#' @return A list(`beta`, `std_err`, `p_val`, `HR`, `CI`, `groups`).
#' 
#' @export 
#' 
#' @author Kira E. Detrois
extract_coxph_res <- function(coxph_mdl, 
                              score_type) {
    if(!is.null(coxph_mdl)) {
        betas <- summary(coxph_mdl)$coefficients[,"coef"]
        std_errs <- summary(coxph_mdl)$coefficients[,"se(coef)"]
        pvals <- summary(coxph_mdl)$coefficients[,"Pr(>|z|)"]
        OR <- exp(betas)
        CI <- get_CI(betas, std_errs)
        preds <- names(summary(coxph_mdl)$coefficients[,"coef"])
        groups <- "no groups"
        return(list(beta=betas, std_err=std_errs, p_val=pvals, HR=OR, CI_neg=exp(CI$neg), CI_pos=exp(CI$pos), groups=groups, preds=preds))
    } else {
        return(list())
    }
}

#' 95% confidence interval given the ML estimator and SE
#' 
#' @param ML maximum likelihood estimator of the parameter
#' @param SE standard error of the ML estimator
#' 
#' @return `c(neg, pos)` the upper and lower bounds of the CI.
#' @author Kira E. Detrois
get_CI <- function(ML, SE) {
    CIneg <- ML-1.96*SE
    CIpos <- ML+1.96*SE
    return(list(neg=CIneg, pos=CIpos))
}
