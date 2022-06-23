#' Fits the cox-ph model for the current endpoint
#' 
#' @param pheno_score_data A data.frame TODO.
#' @param endpt A string. The column name of the current endpoint of 
#'                        interest.
#' @export 
run_coxph_ana <- function(pheno_score_data,
                          endpt,
                          predictor="SCORE_GROUP") {
    coxph_formula <- stats::as.formula(paste0(
                        "survival::Surv(", endpt, "_AGE_DAYS,) ~ ",  predictor))
    if(length(unique(pheno_score_data$SCORE_GROUP)) >= 2) {
        coxph_res <- survival::coxph(coxph_formula,
                                     data=pheno_score_data)
        coxph_res <- extract_coxph_res(coxph_res)
        return(coxph_res)
    } else {
        return(list())
    }
}