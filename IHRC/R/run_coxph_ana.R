#' Fits the cox-ph model for the current endpoint
#' 
#' @param data TODO
#' @param endpt A string. The column name of the current endpoint of 
#'                        interest.
#' @export 
run_coxph_ana <- function(data,
                          endpt) {
    cox_formula <- stats::as.formula(paste0(
                    "survival::Surv(", endpt, "_AGE_DAYS,") ~ SCORE"
                   ))
    coxph_res <- survival::coxph(cox_formula,
                                 data=data)
    coxph_res <- extract_coxph_res(coxph_res)
    return(coxph_res)
}