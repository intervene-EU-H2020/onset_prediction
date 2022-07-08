#' Makes all character covariates a factor
#' 
#' @inheritParams get_coxph_mdl
#' 
#' @return The tibble with character covariate columns now factors.
#' 
#' @author Kira E. Detrois
make_covs_fctrs <- function(pheno_score_data,
                            covs) {
    for(cov in covs) {
        if(is.character(pheno_score_data[,cov])) {
            pheno_score_data <- dplyr::mutate_at(.tbl=pheno_score_data, 
                                                 .vars=cov, 
                                                 .funs=as.factor)
        }
    }
    return(pheno_score_data)
}

#' Creates the forumla string for the Cox-PH model.
#' 
#' @inheritParams get_coxph_mdl
#' 
#' @export 
#' 
#' @return A character. The formula string for the Cox-PH model.
get_coxph_formula <- function(endpt,
                              covs,
                              pred_score="SCORE_GROUP") {
    pred_string <- get_pred_string(covs, pred_score)
    stats::as.formula(paste0(
                        "survival::Surv(", endpt, "_AGE, ",  endpt, ") ~ ",  pred_string))
}

#' Creates the predictor string for the Cox-PH model
#' 
#' @inheritParams get_coxph_mdl
#' 
#' @return A character. The predictor string for the Cox-PH model.
#' 
#' @author Kira E. Detrois
#' Creates the predictor string for the Cox-PH model
#' 
#' @inheritParams get_coxph_mdl
#' 
#' @return A character. The predictor string for the Cox-PH model.
#' 
#' @author Kira E. Detrois
get_pred_string <- function(covs,
                            pred_score="SCORE_GROUP") {
    paste0(pred_score, " + ", paste0(covs, collapse=" + "))
}

#' @export
get_surv_obj <- function(pheno_score_data,
                         endpt) {
     survival::Surv(time=dplyr::pull(pheno_score_data, 
                                     paste0(endpt, "_AGE")),
                    event=dplyr::pull(pheno_score_data, 
                                      endpt))
}
