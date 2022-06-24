#' Fits the Cox model for a given endpoint and predictor
#' 
#' @param pheno_score_data A data.frame TODO.
#' @param endpt A string. The column name of the current endpoint of 
#'                        interest.
#' @export 
run_coxph_ana <- function(pheno_score_data,
                          endpt,
                          predictor="SCORE_GROUP") {
    coxph_formula <- stats::as.formula(paste0(
                        "survival::Surv(", endpt, "_AGE_DAYS,) ~ ",  predictor))
    if(length(unique(pheno_score_data$SCORE_GROUP)) >= 2) {
        coxph_res <- survival::coxph(coxph_formula,
                                     data=pheno_score_data)
        coxph_res <- extract_coxph_res(coxph_res)
        return(coxph_res)
    } else {
        return(list())
    }
}

#' Fits the Cox model and adds the results to the data.frame `coxph_res`
#' 
#' @param envir A list with at least entries `coxph_res`, 
#'              `score_type`, `endpt`, and `pheno_score_data`.
#' @param predictor A character. The predictor for the survival.
#' 
#' @return A tibble the results data.fram `coxph_res` with added 
#'         columns from the analysis run. 
run_and_add_coxph_ana <- function(envir,
                                  predictor) {
    curnt_coxph_res <- run_coxph_ana(envir$pheno_score_data, 
                                     envir$endpt,
                                     predictor)
    envir$coxph_res <- add_coxph_row(envir$coxph_res,
                                     curnt_coxph_res,
                                     envir$score_type,
                                     envir$endpt,
                                     envir$pheno_score_data)
    return(envir$coxph_res)
}