#' Calcualtes HR from a Cox-PH model for the current survival analysis 
#' setup
#' 
#' For i.e. a study with endpoint `J10_ASTHMA` and 
#' `covs = c("SEX", "YEAR_OF_BIRTH")` the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE + SEX +
#' YEAR_OF_BIRTH`.
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
    coxph_mdl <- get_coxph_mdl(surv_ana)
    if(!is.null(coxph_mdl)) {
        # Risk and survival have opposite directions
        preds <- (-1)*predict(coxph_mdl, 
                              type="risk")
        surv_obj <- get_surv_obj(get_non_na_pred_rows(surv_ana), 
                                 surv_ana@study@endpt)    
        c_idx <- Hmisc::rcorr.cens(preds, surv_obj)
    } else {
        c_idx <- NULL
    }
    return(c_idx)
}

get_non_na_pred_rows <- function(surv_ana) {
    score_data <- surv_ana@elig_score_data
    for(pred in surv_ana@preds) {
        if(pred %in% colnames(surv_ana@elig_score_data)) {
            score_data <- dplyr::filter(score_data, 
                                        !is.na(get(pred)))
        }
    }
    return(score_data)
}