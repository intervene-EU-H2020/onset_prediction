#' Fits the Cox model for a given endpoint and predictor
#' 
#' @param pheno_score_data A data.frame with at least columns `DATE_OF_BIRTH`
#'                         and i.e. `J10_ASTHMA`, and `J10_ASTHMA_AGE_DAYS` 
#'                         where the columns are the study endpoint and 
#'                         date, which will differ depending on the input 
#'                         variable `endpt`. And the column defined in `predictor`.
#' @param predictor A character. The predictor for the survival analysis.
#' @inheritParams add_coxph_row
#' 
#' @export 
#' 
#' @author Kira E. Detrois
run_coxph_ana <- function(pheno_score_data,
                          endpt,
                          predictor="SCORE_GROUP") {
    coxph_formula <- stats::as.formula(paste0(
                        "survival::Surv(", endpt, "_AGE_DAYS,) ~ ",  predictor))
    if(length(unique(pheno_score_data$SCORE_GROUP)) >= 2) {
        coxph_res_mdl <- survival::coxph(coxph_formula,
                                         data=pheno_score_data)
        coxph_res_mdl <- extract_coxph_res(coxph_res_mdl)
        return(coxph_res_mdl)
    } else {
        return(list())
    }
}

#' Fits the Cox model and adds the results to the data.frame `coxph_res_tib`
#' 
#' @inheritParams run_coxph_ana
#' @inheritParams add_coxph_row
#' 
#' @return A tibble the results data.frame `coxph_res_tib` with added 
#'         columns from the analysis run. 
run_and_add_coxph_ana <- function(coxph_res_tib,
                                  pheno_score_data,
                                  score_type,
                                  endpt,
                                  predictor) {
    curnt_coxph_res <- run_coxph_ana(pheno_score_data, 
                                     endpt,
                                     predictor)
    coxph_res_tib <- add_coxph_row(coxph_res_tib,
                                   curnt_coxph_res,
                                   score_type,
                                   endpt,
                                   pheno_score_data)
    return(coxph_res_tib)
}