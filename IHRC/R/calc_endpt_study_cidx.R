#' Calcualtes HR from a Cox-PH model for the current survival analysis 
#' setup
#' 
#' For i.e. a study with endpoint `J10_ASTHMA` and 
#' `covs = c("SEX", "YEAR_OF_BIRTH")` the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A tibble with columns `Endpoint`, `Score`, `Group`, 
#'          `N_controls`, `N_cases`, `beta`, `std_errs`, `p_val`, `HR`, 
#'          `CI_pos`, `CI_neg`. The Cox-PH analysis results for all 
#'          selected endpoints. 
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_endpt_study_cidx <- function(surv_ana) {
    set.seed(923)
    test_idxs <- sample(seq(nrow(surv_ana@elig_score_data)),
                        0.5*nrow(surv_ana@elig_score_data),
                        replace=FALSE)

    coxph_mdl <- get_coxph_mdl(surv_ana,
                               pred_score=paste0(surv_ana@score_type, "_SCORE", collapse=" + "),
                               test_idxs=test_idxs)
    if(!is.null(coxph_mdl)) {
        # Risk and survival have opposite directions
        preds_test <- (-1)*predict(
                                coxph_mdl, 
                                type="risk",
                                newdata=surv_ana@elig_score_data[test_idxs,])
        surv_obj_test <- get_surv_obj(
                            surv_ana@elig_score_data[test_idxs,], 
                            surv_ana@study@endpt)    
        c_idx_test <- Hmisc::rcorr.cens(preds_test, surv_obj_test)

        # Risk and survival have opposite directions
        preds_train <- (-1)*predict(
                              coxph_mdl, 
                              type="risk",
                              newdata=surv_ana@elig_score_data[-test_idxs,])
        surv_obj_train <- get_surv_obj(
                            surv_ana@elig_score_data[-test_idxs,],
                            surv_ana@study@endpt)    
        c_idx_train <- Hmisc::rcorr.cens(preds_train, surv_obj_train)
    } else {
        c_idx_test <- NULL
        c_idx_train <- NULL
    }
    return(list(test=c_idx_test, train=c_idx_train))
}


