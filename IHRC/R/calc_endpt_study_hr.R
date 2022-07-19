#' Calcualtes HR from a Cox-PH model for a survival analysis setup
#' 
#' Calcualtes HR from a Cox-PH model for a survival analysis setup
#' based on the risk groups.
#' 
#' For i.e. a study with endpoint `J10_ASTHMA` and 
#' `covs = c("SEX", "YEAR_OF_BIRTH")` the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE_GROUP + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A Cox-PH model.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_endpt_study_hr <- function(surv_ana) {
    coxph_mdl <- get_coxph_mdl(surv_ana=surv_ana,
                                     pred_score="SCORE_GROUP")
    return(coxph_mdl)
}


