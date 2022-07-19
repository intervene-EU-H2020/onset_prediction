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
    coxph_mdl <- get_coxph_mdl(surv_ana,
                               pred_score="SCORE")
    c_idx_res <- get_cidx(coxph_mdl, surv_ana)

    return(c_idx_res)
}


