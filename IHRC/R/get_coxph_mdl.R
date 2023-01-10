#' Fit a Cox Proportional Hazards Model
#'
#' This function fits a Cox Proportional Hazards Model for a 
#' given surivival analysis setup, using the [survival::coxph] function.
#' 
#' For i.e. `endpt = "J10_ASTHMA"` and `covs = c("SEX", "YEAR_OF_BIRTH")` 
#' the model would be 
#' `Surv(J10_ASTHMA_AGE_FROM_BASE, J10_ASTHMA) ~ PRS + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @param test_idxs An integer (vector). The indices of the test data
#'                  subset. Default is `NULL`, meaning the whole data
#'                  is used for fitting.
#' 
#' @return A Cox-PH model object or `NULL` if the model couldn't be fit.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_coxph_mdl <- function(surv_ana) {
    coxph_mdl <- NULL
    if(nrow(surv_ana@elig_score_data) > 0) {
        coxph_formula <- get_coxph_formula(surv_ana)  
        scaled_data <- scale_preds(surv_ana@plot_preds,
                                                surv_ana@elig_score_data)
        coxph_mdl <- suppressWarnings(
                            survival::coxph(formula=coxph_formula, 
                                            data=scaled_data,
                                            # Larger fit object but no need for
                                            # other functions to reconstruct
                                            # which fails in this setup
                                            model=TRUE))
    }
    return(coxph_mdl)
}