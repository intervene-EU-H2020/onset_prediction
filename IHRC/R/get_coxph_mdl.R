#' Fits the Cox-PH model for a survival analysis setup
#' 
#' For i.e. `endpt = "J10_ASTHMA"` and `covs = c("SEX", "YEAR_OF_BIRTH")` 
#' the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ PRS + SEX +
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
get_coxph_mdl <- function(surv_ana,
                          test_idxs=NULL) {
    coxph_mdl <- NULL
    if(nrow(surv_ana@elig_score_data) > 0) {
        coxph_formula <- get_coxph_formula(surv_ana)

        build_mdl <- TRUE
  
        surv_ana@elig_score_data <- scale_preds(surv_ana@plot_preds,
                                                surv_ana@elig_score_data)
        if(build_mdl) {
            test_data <- surv_ana@elig_score_data
            if(!is.null(test_idxs)) {
                test_data <- surv_ana@elig_score_data[test_idxs,]
            }
            if(nrow(test_data) > 0) {
                coxph_mdl <- suppressWarnings(
                                survival::coxph(formula=coxph_formula, 
                                                data=test_data,
                                                # Larger fit object but no need for
                                                # other functions to reconstruct
                                                # which fails in this setup
                                                model=TRUE))
            }
        } 
    }
    return(coxph_mdl)
}

#' @export 
scale_preds <- function(preds,
                        score_data) {
    for(pred in preds) {
        if(pred %in% colnames(score_data)) {
            if(pred %in% c("PRS", "CCI", "EI", "YEAR_OF_BIRTH", "MED", "EDU", "PheRS")) {
                score_data[,pred] <- scale(score_data[,pred])
            } else if(pred %in% c("SEX", "ANCESTRY")) {
                score_data <- dplyr::mutate_at(score_data, {{ pred }}, as.factor)
            }
        }
    }
    return(score_data)
}