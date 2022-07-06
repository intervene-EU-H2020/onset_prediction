#' Extracts the relevant results from the Cox-PH model
#' 
#' @inheritParams add_coxph_res_row
#' 
#' @return A list(`beta`, `std_err`, `p_val`, `HR`, `CI`, `groups`).
#' 
#' @export 
#' 
#' @author Kira E. Detrois
extract_coxph_res <- function(coxph_mdl) {
    betas <- summary(coxph_mdl)$coefficients[,"coef"]
    betas <- betas[grep("SCORE", names(betas))]
    std_errs <- summary(coxph_mdl)$coefficients[,"se(coef)"]
    std_errs <- std_errs[grep("SCORE", names(std_errs))]
    pvals <- summary(coxph_mdl)$coefficients[,"Pr(>|z|)"]
    pvals <- pvals[grep("SCORE", names(pvals))]
    OR <- exp(betas)
    CI <- get_CI(betas, std_errs)
    if("SCORE_GROUP" %in% names(coxph_mdl$xlevels)) {
        groups <- coxph_mdl$xlevels$SCORE_GROUP[2:length(coxph_mdl$xlevels$SCORE_GROUP)]
    } else {
        groups <- "no groups"
    }
    return(list(beta=betas, std_err=std_errs, p_val=pvals, HR=OR, CI=CI, groups=groups))
}

#' 95% confidence interval given the ML estimator and SE
#' 
#' @param ML maximum likelihood estimator of the parameter
#' @param SE standard error of the ML estimator
get_CI <- function(ML, SE) {
    CIneg <- exp(ML-1.96*SE)
    CIpos <- exp(ML+1.96*SE)
    return(list(neg=CIneg, pos=CIpos))
}
