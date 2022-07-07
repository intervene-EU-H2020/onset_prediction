get_cidx <- function(coxph_mdl,
                     pheno_score_data,
                     endpt) {
    if(!is.null(coxph_mdl)) {
        preds <- predict(coxph_mdl, type="risk")
        surv_obj <- get_surv_obj(pheno_score_data, endpt)    
        c_idx <- Hmisc::rcorr.cens(preds,
                                surv_obj)
    } else {
        c_idx <- NULL
    }
    return(c_idx)
}