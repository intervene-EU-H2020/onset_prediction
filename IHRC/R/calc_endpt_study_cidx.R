#' Calculate the c-index
#'  
#' This function calculates the c-index, also known as the concordance index 
#' or Harrell's C, for a Cox-PH model fit to survival data. The c-index is a 
#' measure of the predictive accuracy of the model and ranges from 0 to 1, 
#' with higher values indicating better predictive accuracy.
#' 
#' The c-index is calculated using the `Hmisc::rcorr.cens` function. For more 
#' information on the elements of the output, see the documentation for 
#' [Hmisc::rcorr.cens].
#' 
#' For i.e. a study with endpoint `J10_ASTHMA` and 
#' `covs = c("SEX", "YEAR_OF_BIRTH")` the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @inheritParams get_coxph_mdl
#'  
#' @return A numeric value representing the c-index for the survival 
#'           analysis.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_endpt_study_cidx <- function(surv_ana,
                                  coxph_mdl=NULL) {
    set.seed(923)
    if(is.null(coxph_mdl)) {
        coxph_mdl <- get_coxph_mdl(surv_ana)
    }
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

#' Exclude rows with missing values from a data frame
#' 
#' This function filters a data frame to exclude rows with missing values 
#' in the specified columns.
#'  
#' @inheritParams calc_endpt_study_cidx
#' 
#' @return A data frame with rows containing missing values in the specified
#'   columns excluded.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
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