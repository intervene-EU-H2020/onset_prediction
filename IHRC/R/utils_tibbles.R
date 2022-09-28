#' Creates an empty tibble for the Cox-PH HR results
#' 
#' @return An empty tibble with all relevant columns for the final 
#'            results.
#' 
#' @export 
#' 
#' @author Kira E. Detrois 
create_empty_endpt_hrs_tib <- function() {
    tibble::tibble(ENDPOINT=character(),
                   EXP_AGE=numeric(),
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

#' Creates an empty tibble for the C-index results
#' 
#' @return An empty tibble with all relevant columns for the final 
#'            results.
#' 
#' @export 
#' 
#' @author Kira E. Detrois 
create_empty_cidx_tib <- function() {
    tibble::tibble(
        ENDPOINT = character(),
        EXP_AGE = numeric(),
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
#' @param endpt_hrs_tib A tibble with the results for previous endpts.
#' @param coxph_mdl A Cox-PH model. 
#' 
#' @return A tibble. The updated `endpt_hrs_tib` with the added results for
#'          the current endpoint.
#'                        
#' @author Kira E. Detrois 
add_coxph_res_row <- function(endpt_hrs_tib,
                              coxph_mdl,
                              surv_ana) {
    if(!is.null(coxph_mdl)) {
        coxph_res_list <- get_min_indvs_data(coxph_mdl=coxph_mdl,
                                             surv_ana=surv_ana,
                                             var="SEX")
         if(!is.null(coxph_res_list)) {
            endpt_hrs_tib <- tibble::add_row(
                                            endpt_hrs_tib, 
                                            ENDPOINT=surv_ana@study@endpt,
                                            EXP_AGE=surv_ana@study@exp_age,
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
    return(endpt_hrs_tib)
}

#' Get's the Cox-PH model results for runs where each group has at
#' least 5 controls and cases in each group
#' 
#' @inheritParams add_coxph_res_row
#' 
#' @return A list. The filtered results.
#' 
#' @author Kira E. Detrois
get_min_indvs_data <- function(coxph_mdl,
                               surv_ana,
                               var) {
    coxph_res_list <- extract_coxph_res(coxph_mdl)
    coxph_res_list$n_case <- Istudy::get_n_cases(surv_ana@study@study_data, surv_ana@study@endpt)
    coxph_res_list$n_cntrl <- Istudy::get_n_cntrls(surv_ana@study@study_data, surv_ana@study@endpt)
    return(coxph_res_list)
}

#' Adds a row to the C-index results tibble
#' 
#' @param endpt_c_idxs_tib A tibble with the results for previous endpts.
#' @param c_idx_res A tibble with the new results to add.
#' 
#' @return A tibble. The updated `endpt_c_idxs_tib` with the added results for
#'          the current endpoint.
#'                        
#' @author Kira E. Detrois 
add_cidx_res_row <- function(endpt_c_idxs_tib,
                             c_idx_res,
                             surv_ana) {
    if(!is.null(c_idx_res)) {
        surv_descr=get_surv_descr(surv_ana@preds)  
        # In docu it says to use se=sd/2
        c_idx_ci <- get_CI(c_idx_res["C Index"], c_idx_res["S.D."]/2)
        endpt_c_idxs_tib <- tibble::add_row(
                            endpt_c_idxs_tib,
                            ENDPOINT=surv_ana@study@endpt,
                            EXP_AGE=surv_ana@study@exp_age,
                            SURV_MODEL=surv_descr,
                            N_CASES=Istudy::get_n_cases(surv_ana@study@study_data, surv_ana@study@endpt),
                            N_CONTROLS=Istudy::get_n_cntrls(surv_ana@study@study_data, surv_ana@study@endpt),
                            SE=c_idx_res["S.D."]/2,
                            C_IDX=c_idx_res["C Index"],
                            C_IDX_CI_NEG=c_idx_ci[[1]],
                            C_IDX_CI_POS=c_idx_ci[[2]])
    }
    return(endpt_c_idxs_tib)
}