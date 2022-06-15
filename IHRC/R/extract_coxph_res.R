get_CI <- function(betas, std_errs) {
    CIneg <- exp(betas-1.96*std_errs)
    CIpos <- exp(betas+1.96*std_errs)

    return(c(neg=CIneg, pos=CIpos))
}

extract_surv_res <- function(coxph_res) {
    betas <- summary(coxph_res)$coefficients[,"coef"]
    std_errs <- summary(coxph_res)$coefficients[,"se(coef)"]
    pvals <- summary(coxph_res)$coefficients[,"Pr(>|z|)"]
    OR <- exp(betas)
    CI <- get_CI(betas, std_errs)
    
    return(list(BETAS=betas, STD_ERRS=std_errs, PVALS=pvals, HR=OR, CI=CI))
}