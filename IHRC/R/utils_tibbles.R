#' Creates an empty tibble for endpoints hazard ratios
#' 
#' @return An empty tibble with the following columns:
#' \itemize{
#'   \item ENDPOINT: endpoint name
#'   \item EXP_AGE: exposure age
#'   \item VAR: predictor variable
#'   \item GROUP: group
#'   \item N_CONTROLS: number of controls
#'   \item N_CASES: number of cases
#'   \item BETA: beta coefficient
#'   \item SE: standard error
#'   \item P_VAL: p-value
#'   \item HR: hazard ratio
#'   \item CI_NEG: negative bound of the 95% confidence interval
#'   \item CI_POS: positive bound of the 95% confidence interval
#' }
#' 
#' @export 
#' 
#' @author Kira E. Detrois 
create_empty_hr_res <- function() {
    tibble::tibble(ENDPOINT=character(),
                   SURV_MODEL=character(),
                   VAR=character(),
                   GROUP=character(),
                   N_CONTROLS=numeric(),
                   N_CASES=numeric(),
                   BETA=numeric(),
                   SE=numeric(),
                   P_VAL=numeric(),
                   HR=numeric(),
                   CI_NEG=numeric(),
                   CI_POS=numeric())
}

#' Creates an empty tibble for the c-index results
#' 
#' @return An empty tibble with the following columns:
#' \itemize{
#'   \item ENDPOINT: endpoint name
#'   \item EXP_AGE: exposure age
#'   \item SURV_MODEL: A string describing the the survival model
#'   \item N_CASES: a numeric column for the number of cases
#'   \item N_CONTROLS: a numeric column for the number of controls
#'   \item C_IDX: a numeric column for the c-index
#'   \item SE: a numeric column for the standard error of the c-index
#'   \item C_IDX_CI_NEG: a numeric column for the lower bound of the 
#'                          c-index's confidence interval
#'   \item C_IDX_CI_POS: a numeric column for the upper bound of the 
#'                         c-index's confidence interval
#' }
#' 
#' @export 
#' 
#' @author Kira E. Detrois 
create_empty_cidx_res <- function() {
    tibble::tibble(
        ENDPOINT = character(),
        SURV_MODEL = character(),
        N_CASES = numeric(),
        N_CONTROLS = numeric(),
        C_IDX = numeric(),
        SE = numeric(),
        C_IDX_CI_NEG = numeric(),
        C_IDX_CI_POS = numeric()
    )
}

#' Adds a row to the Cox-PH HR results tibble
#' 
#' @param hr_res A tibble with the results for previous endpts, 
#'                        with columns: `ENDPOINT`, `EXP_AGE`, `VAR`, 
#'                        `GROUP`, `N_CONTROLS`, `N_CASES`, `BETA`, 
#'                        `SE`, `P_VAL`, `HR`, `CI_NEG`, `CI_POS`.
#' @param coxph_mdl A `coxph` object. See function [survival::coxph].
#' @param study An S4 `study` object. The current study. 
#'                  See class definition [Istudy::study].
#' 
#' @return A tibble. The updated `hr_res` with the added row for
#'          the HRs of the current endpoint.
#' 
#' @export 
#'                        
#' @author Kira E. Detrois 
add_coxph_res_row <- function(hr_res,
                              coxph_mdl,
                              study,
                              preds) {
    if(!is.null(coxph_mdl)) {
        surv_descr <- stringr::str_remove_all(get_surv_descr(preds), " ") 
        coxph_res_list <- get_min_indvs_data(coxph_mdl=coxph_mdl,
                                             study=study)
         if(!is.null(coxph_res_list)) {
            hr_res <- tibble::add_row(hr_res, 
                                      ENDPOINT=study@endpt,
                                      SURV_MODEL=surv_descr,
                                      VAR=coxph_res_list$preds,
                                      GROUP=coxph_res_list$groups,
                                      N_CONTROLS=coxph_res_list$n_cntrl,
                                      N_CASES=coxph_res_list$n_case,
                                      BETA=coxph_res_list$beta,
                                      SE=coxph_res_list$std_err,
                                      P_VAL=coxph_res_list$p_val,
                                      HR=coxph_res_list$HR,
                                      CI_NEG=coxph_res_list$CI_neg,
                                      CI_POS=coxph_res_list$CI_pos)
            }
    } 
    return(hr_res)
}

#' Get's the Cox-PH model results for runs where each group has at
#' least 5 controls and cases in each group
#' 
#' @param coxph_mdl A `coxph` object. See function [survival::coxph].
#' @param study An S4 `study` object. The current study. 
#'                  See class definition [Istudy::study]. 
#' 
#' @return A list. The filtered results.
#' 
#' @author Kira E. Detrois
get_min_indvs_data <- function(coxph_mdl,
                               study) {
    coxph_res_list <- extract_coxph_res(coxph_mdl)
    n_cases_vec <- c()
    n_cntrls_vec <- c()
    for(group_var in names(coxph_mdl$xlevels)) {
        for(group in unique(dplyr::pull(study@study_data, get(group_var)))) {
            group_data <- dplyr::filter(study@study_data, get(group_var) == group)
            n_cntrls_vec <- c(n_cntrls_vec, Istudy::get_n_cntrls(group_data, study@endpt))
            names(n_cntrls_vec)[length(n_cntrls_vec)] = paste0(group_var, group)
            n_cases_vec <- c(n_cases_vec, Istudy::get_n_cases(group_data, study@endpt))
            names(n_cases_vec)[length(n_cases_vec)] = paste0(group_var, group)
        }
    }
    coxph_res_list$n_case <- rep(Istudy::get_n_cases(study@study_data, study@endpt), length(coxph_res_list$HR))
    names(coxph_res_list$n_case) <- names(coxph_res_list$HR)
    coxph_res_list$n_cntrl <- rep(Istudy::get_n_cntrls(study@study_data, study@endpt), length(coxph_res_list$HR))
    names(coxph_res_list$n_cntrl) <- names(coxph_res_list$HR)

    for(var in names(coxph_res_list$n_case)) {
        if(var %in% names(n_cases_vec)) {
            coxph_res_list$n_case[[var]] <- n_cases_vec[[var]]
            coxph_res_list$n_cntrl[[var]] <- n_cntrls_vec[[var]]
        }
    }
    return(coxph_res_list)
}

#' Adds a row to the C-index results tibble
#' 
#' @param c_idxs_res A tibble with the results for previous endpts.
#' @param c_idx_res A tibble with the new results to add.
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' @param study An S4 `study` object. The current study. 
#'                  See class definition [Istudy::study].
#' 
#' @return A tibble. The updated `c_idxs_res` with the added results for
#'          the current endpoint.
#'                        
#' @author Kira E. Detrois 
add_cidx_res_row <- function(c_idxs_res,
                             c_idx_res,
                             surv_ana,
                             study) {
    if(!is.null(c_idx_res)) {
        surv_descr <- stringr::str_remove_all(get_surv_descr(surv_ana@preds), " ")
        # In docu it says to use se=sd/2
        c_idx_ci <- get_CI(c_idx_res["C Index"], c_idx_res["S.D."]/2)
        c_idxs_res <- tibble::add_row(
                            c_idxs_res,
                            ENDPOINT=study@endpt,
                            SURV_MODEL=surv_descr,
                            N_CASES=Istudy::get_n_cases(get_non_na_pred_rows(surv_ana, study@study_data), study@endpt),
                            N_CONTROLS=Istudy::get_n_cntrls(get_non_na_pred_rows(surv_ana, study@study_data), study@endpt),
                            SE=c_idx_res["S.D."]/2,
                            C_IDX=c_idx_res["C Index"],
                            C_IDX_CI_NEG=c_idx_ci[[1]],
                            C_IDX_CI_POS=c_idx_ci[[2]])
    }
    return(c_idxs_res)
}