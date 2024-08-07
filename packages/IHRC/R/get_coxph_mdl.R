#' Fit a Cox Proportional Hazards Model
#'
#' This function fits a Cox Proportional Hazards Model for a 
#' given surivival analysis setup and study, using the [survival::coxph] function.
#' 
#' For i.e. a `study` with endpoint `J10_ASTHMA` and `surv_ana` with covs
#' `covs = c("SEX", "YEAR_OF_BIRTH")` the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' @param study An S4 study object. The current study.
#' 
#' @return A Cox-PH model object or `NULL` if the model couldn't be fit.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_coxph_mdl <- function(surv_ana,
                          study) {

    coxph_mdl <- NULL
    if(nrow(study@study_data) > 0) {
        coxph_formula <- get_coxph_formula(preds=surv_ana@preds, endpt=study@endpt)  
        study@study_data <- scale_preds(preds=surv_ana@preds, study_data=study@study_data)

        coxph_mdl <- tryCatch({
            survival::coxph(formula=coxph_formula, 
                                             data=study@study_data,
                                             # Larger fit object but no need for
                                             # other functions to reconstruct
                                             # which fails in this setup
                                             model=TRUE)
                      }, error = function(e) {
                                write_to_error_file(surv_ana@error_file, msg=paste0(e, collapse="\n"))
                                return(NULL)})
        print(coxph_mdl)
        write_coxph_to_log(coxph_mdl, study, surv_ana) 
    }

    return(coxph_mdl)
}

#' Creates the forumla string for the Cox-PH model
#' 
#' Will be i.e. 
#' `Surv(J10_ASTHMA_AGE_FOR_BASE, J10_ASTHMA) ~ SEX + YEAR_OF_BIRTH`
#' for endpoint `J10_ASTHMA` and predictors `SEX` and `YEAR_OF_BIRTH`.
#' 
#' @param preds A string (vector). The predictors for the model.
#' @param endpt A string. The current endpoint. 
#' 
#' @return The formula for the Cox-PH model.
#' 
#' @export 
#' 
#' @return A string. The formula string for the Cox-PH model.
get_coxph_formula <- function(preds,
                              endpt) {
    pred_string <- paste0(preds, collapse="+")
    coxph_formula <- stats::as.formula(paste0("survival::Surv(", endpt, "_AGE_FROM_BASE, ",  endpt, ") ~ ",  pred_string))
    return(coxph_formula)
}
