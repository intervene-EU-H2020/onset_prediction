#' @export 
run_coxph_ana <- function(data,
                          endpt) {
    cox_formula <- stats::as.formula(paste0(
                    "survival::Surv(", endpt, "_AGE_DAYS,", endpt, ") ~ SCORE"
                   ))
    coxph_res <- survival::coxph(cox_formula,
                                 data=data)
    coxph_res <- extract_coxph_res(coxph_res)
    return(coxph_res)
}