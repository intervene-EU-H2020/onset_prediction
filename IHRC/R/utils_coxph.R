#' Makes all character covariates a factor
#'  
#' @param elig_score_data A data.frame with all eligible individuals under 
#'                  the chosen study setup and their scores. Needs at 
#'                  least the columns defines in `covs`.
#' @param covs A vector of characters. The column names of the covariates 
#'              to add to the predictor of the Cox-PH model.
#' 
#' @author Kira E. Detrois
make_covs_fctrs <- function(elig_score_data,
                            covs) {
    for(cov in covs) {
        if(is.character(elig_score_data[,cov])) {
            elig_score_data <- dplyr::mutate_at(
                                    .tbl=elig_score_data, 
                                    .vars=cov, 
                                    .funs=as.factor)
        }
    }
    return(elig_score_data)
}

#' Creates the forumla string for the Cox-PH model
#' 
#' @inheritParams add_risk_group_col
#' @inheritParams get_coxph_mdl
#' 
#' @export 
#' 
#' @return A character. The formula string for the Cox-PH model.
get_coxph_formula <- function(surv_ana,
                              pred_score="SCORE_GROUP") {
    pred_string <- get_pred_string(surv_ana@covs, pred_score)
    stats::as.formula(paste0("survival::Surv(", surv_ana@study@endpt, "_AGE, ",  surv_ana@study@endpt, ") ~ ",  pred_string))
}

#' Creates the predictor string for the Cox-PH model
#' 
#' @inheritParams make_covs_fctrs
#' @inheritParams get_coxph_mdl
#' 
#' @return A character. The predictor string for the Cox-PH model.
#' 
#' @author Kira E. Detrois
get_pred_string <- function(covs,
                            pred_score="SCORE_GROUP") {
    paste0(pred_score, " + ", paste0(covs, collapse=" + "))
}

#' Creates a survival object for the selected endpoint
#' 
#' Creates a survival object for the selected endpoint. With time
#' being the age in years until onset or the age at the end of the
#' observation period.
#' 
#' @inheritParams make_covs_fctrs
#' @inheritParams get_n_group_cases
#' 
#' @author Kira E. Detrois
#' 
#' @export
get_surv_obj <- function(elig_score_data,
                         endpt) {
     survival::Surv(time=dplyr::pull(elig_score_data, 
                                     paste0(endpt, "_AGE")),
                    event=dplyr::pull(elig_score_data, 
                                      endpt))
}