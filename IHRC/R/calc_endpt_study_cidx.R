#' Calculates the c-index
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
#' For i.e. a `study` with endpoint `J10_ASTHMA` and `surv_ana` with covs
#' `covs = c("SEX", "YEAR_OF_BIRTH")` the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' @param study An S4 `study` object. The current study. 
#'                  See class definition [Istudy::study].
#' @param coxph_mdl A `coxph` object. See function [survival::coxph]. If `NULL`
#'                   creates a new object from the `surv_ana` and `study` information.
#'  
#' @return The c-index object from the Hmisc::rcorr.cens function
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_endpt_study_cidx <- function(surv_ana,
                                  study,
                                  coxph_mdl=NULL) {
    set.seed(923)
    if(is.null(coxph_mdl)) {
        coxph_mdl <- get_coxph_mdl(surv_ana, study)
    }
    if(!is.null(coxph_mdl)) {
        # Risk and survival have opposite directions
        preds <- (-1)*predict(coxph_mdl, 
                              type="lp")
        surv_obj <- get_surv_obj(get_non_na_pred_rows(surv_ana, study@study_data), 
                                 study@endpt)    
        c_idx <- Hmisc::rcorr.cens(preds, surv_obj)
    } else {
        c_idx <- NULL
    }
    return(c_idx)
}

#' Creates a survival object for the selected endpoint
#' 
#' Creates a survival object for the selected endpoint. With time
#' being the age in years until onset or the age at the end of the
#' observation period.
#' 
#' @param study_data A data.frame. The data for the Cox-PH model.
#' @param endpt A string. The current endpoint.
#' 
#' @author Kira E. Detrois
#' 
#' @export
get_surv_obj <- function(study_data,
                         endpt) {

    survival::Surv(time=dplyr::pull(study_data, 
                                    paste0(endpt, "_AGE_FROM_BASE")),
                   event=dplyr::pull(study_data, 
                                     endpt))
}

#' Exclude rows with missing values from a data frame
#' 
#' This function filters a data frame to exclude rows with missing values 
#' in the specified columns.
#'  
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' @param study_data A data.frame. The data for the Cox-PH model.
#' 
#' @return A data frame with rows containing missing values in the predictors excluded.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_non_na_pred_rows <- function(surv_ana,
                                 study_data) {
    score_data <- study_data
    for(pred in surv_ana@preds) {
        if(pred %in% colnames(study_data)) {
            score_data <- dplyr::filter(score_data, 
                                        !is.na(get(pred)))
        }
    }
    return(score_data)
}