get_cidx <- function(coxph_mdl,
                     surv_ana) {
    if(!is.null(coxph_mdl)) {
        preds <- predict(coxph_mdl, type="risk")
        surv_obj <- get_surv_obj(surv_ana@elig_score_data, 
                                 surv_ana@study@endpt)    
        c_idx <- Hmisc::rcorr.cens(preds, surv_obj)
    } else {
        c_idx <- NULL
    }
    return(c_idx)
}