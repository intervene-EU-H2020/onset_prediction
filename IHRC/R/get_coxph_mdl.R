#' Fits the Cox-PH model for a survival analysis setup
#' 
#' For i.e. `endpt = "J10_ASTHMA"` and `covs = c("SEX", "YEAR_OF_BIRTH")` 
#' the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE_GROUP + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @inheritParams add_risk_group_col
#' @param pred_score A character. The score type to predict. For direct
#'                      regression `SCORE`, and for regression on the
#'                      risk group `SCORE_GROUP`.
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
                          pred_score="SCORE_GROUP",
                          test_idxs=NULL) {
    coxph_mdl <- NULL
    if(nrow(surv_ana@elig_score_data) > 0) {
        surv_ana@elig_score_data <- make_covs_fctrs(
                                        surv_ana@elig_score_data,
                                        surv_ana@covs)
        coxph_formula <- get_coxph_formula(surv_ana, pred_score)
        build_mdl <- TRUE
        if(pred_score == "SCORE_GROUP") {
            Istudy::check_cols_exist(surv_ana@elig_score_data, 
                                    "SCORE_GROUP", 
                                    "get_coxph_mdl")
            if(length(unique(surv_ana@elig_score_data$SCORE_GROUP)) < 2) {
                build_mdl <- FALSE
            }
        } else if(pred_score == "SCORE") {
            score_col_name <- paste0(surv_ana@score_type, "_SCORE")
            Istudy::check_cols_exist(surv_ana@elig_score_data, 
                                     score_col_name, 
                                     "get_coxph_mdl")
            surv_ana@elig_score_data[,score_col_name] <- scale(surv_ana@elig_score_data[,score_col_name])
        }
        if(build_mdl) {
            test_data <- surv_ana@elig_score_data
            if(!is.null(test_idxs)) {
                test_data <- surv_ana@elig_score_data[test_idxs,]
            }
            coxph_mdl <- suppressWarnings(
                            survival::coxph(formula=coxph_formula, 
                                            data=test_data,
                                            # Larger fit object but no need for
                                            # other functions to reconstruct
                                            # which fails in this setup
                                            model=TRUE))
        } 
    }
    return(coxph_mdl)
}