#' Get's the predicted probability of the endpoint for the current model
#' 
#' @param coxph_mdl An S4 `coxph` object. The current coxph model.
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup.
#' @param study_data A data.frame. The current study data.
#' 
#' @return A tibble with the predicted probabilities of the endpoint for
#'              each individual for the current model.
get_indv_model_probs <- function(coxph_mdl,
                                 surv_ana,
                                 study_data,
                                 preds) {
    probs <- predict(coxph_mdl, type="lp")
    preds_tib <- tibble::tibble(ID=get_non_na_pred_rows(surv_ana, study_data)  %>% dplyr::pull(ID))
    preds_tib[,paste0(get_preds_file_name(preds), "_PROBS")] <- probs
    return(preds_tib)
}