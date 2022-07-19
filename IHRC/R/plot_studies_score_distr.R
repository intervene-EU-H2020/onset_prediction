 
 #' Calcualtes HR from a Cox-PH model for each endpoint
#' 
#' Fits a Cox-proportional hazards model for the different endpt_studies. 
#' 
#' Selects individuals from the data that are eligible given each of the 
#' endpt_studies. For more details see package `Istudy` and function
#' \link[Istudy]{get_study_elig_indv}. Runs the analyis based on risk 
#' groups. For `PRS` these are based on the quantiles, where the 40-60% 
#' group is the reference group. For `CCI` compares two groups based on a 
#' cutoff defined in `bin_cut`.
#' 
#' For i.e. endpoint `J10_ASTHMA` and `covs = c("SEX", "YEAR_OF_BIRTH")` 
#' the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE_GROUP + SEX +
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
plot_study_score_distr <- function(surv_ana) {
    if(nrow(surv_ana@elig_score_data) > 0) {
        plot_score_distr(score_data=surv_ana@elig_score_data, 
                         surv_ana)
        plot_endpt_score_distr(score_data=surv_ana@elig_score_data,
                               surv_ana)
    }
}