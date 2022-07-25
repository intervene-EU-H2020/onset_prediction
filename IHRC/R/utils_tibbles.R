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
                   SCORE=character(),
                   GROUP=character(),
                   N_CONTROLS=numeric(),
                   N_CASES=numeric(),
                   BETA=numeric(),
                   STD_ERRS=numeric(),
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
        C_IDX_TEST = numeric(),
        CI_TEST_NEG = numeric(),
        CI_TEST_POS = numeric(),
        C_IDX_TRAIN = numeric(),
        CI_TRAIN_NEG = numeric(),
        CI_TRAIN_POS = numeric())
}

#' Adds a row to the Cox-PH HR results tibble
#' 
#' @param endpt_hrs_tib A tibble with the results for previous endpts.
#' @param coxph_mdl A Cox-PH model. 
#' @inheritParams add_risk_group_col
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
                                             surv_ana=surv_ana)
        if(!is.null(coxph_res_list)) {
            endpt_hrs_tib <- tibble::add_row(
                                endpt_hrs_tib, 
                                ENDPOINT=surv_ana@study@endpt,
                                EXP_AGE=surv_ana@study@exp_age,
                                SCORE=surv_ana@score_type,
                                GROUP=coxph_res_list$groups,
                                N_CONTROLS=coxph_res_list$n_cntrl,
                                N_CASES=coxph_res_list$n_case,
                                BETA=coxph_res_list$beta,
                                STD_ERRS=coxph_res_list$std_err,
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
#' @inheritParams add_risk_group_col
#' 
#' @return A list. The filtered results.
#' 
#' @author Kira E. Detrois
get_min_indvs_data <- function(coxph_mdl,
                               surv_ana) {
    coxph_res_list <- extract_coxph_res(coxph_mdl,
                                        surv_ana@score_type)
    n_cntrls_vec <- get_n_group_cntrls(surv_ana@elig_score_data, 
                                       levels(surv_ana@elig_score_data$SCORE_GROUP),
                                       surv_ana@study@endpt)
    n_cases_vec <- get_n_group_cases(surv_ana@elig_score_data, 
                                     levels(surv_ana@elig_score_data$SCORE_GROUP),
                                     surv_ana@study@endpt)
    ref_level = levels(surv_ana@elig_score_data$SCORE_GROUP)[1]
    
    # Reference group has enough cases and controls
    if((n_cntrls_vec[1] > surv_ana@min_indvs) & 
            (n_cases_vec[1] > surv_ana@min_indvs)) {
        coxph_res_tib <- tibble::as_tibble(coxph_res_list)
        coxph_res_tib <- tibble::add_row(coxph_res_tib,
                                         .before=1,
                                         beta=NA_real_,
                                         std_err=NA_real_,
                                         p_val=NA_real_,
                                         HR=NA_real_,
                                         CI_neg=NA_real_,
                                         CI_pos=NA_real_,
                                         groups=ref_level) 
        coxph_res_tib <- tibble::add_column(coxph_res_tib,
                                            n_cntrl=n_cntrls_vec)
        coxph_res_tib <- tibble::add_column(coxph_res_tib,
                                            n_case=n_cases_vec)
        coxph_res_tib <- coxph_res_tib[coxph_res_tib$n_cntrl > surv_ana@min_indvs &
                                        coxph_res_tib$n_case > surv_ana@min_indvs,]
        if(nrow(coxph_res_tib) < 2) {
            return(NULL)
        } else {
            return(as.list(coxph_res_tib))
        }
    } else {
        return(NULL)
    }
}

#' Adds a row to the C-index results tibble
#' 
#' @param endpt_c_idxs_tib A tibble with the results for previous endpts.
#' @param c_idx_res A tibble with the new results to add.
#' @inheritParams add_risk_group_col
#' 
#' @return A tibble. The updated `endpt_c_idxs_tib` with the added results for
#'          the current endpoint.
#'                        
#' @author Kira E. Detrois 
add_cidx_res_row <- function(endpt_c_idxs_tib,
                             c_idx_res,
                             surv_ana) {
    if(!is.null(c_idx_res)) {
        surv_descr=get_surv_descr(surv_ana,
                                  surv_type="surv")  
        c_idx_ci_test <- get_CI(c_idx_res$test["C Index"], c_idx_res$test["S.D."]/2)
        c_idx_ci_train <- get_CI(c_idx_res$train["C Index"], c_idx_res$train["S.D."]/2)
        endpt_c_idxs_tib <- tibble::add_row(
                            endpt_c_idxs_tib,
                            ENDPOINT=surv_ana@study@endpt,
                            EXP_AGE=surv_ana@study@exp_age,
                            SURV_MODEL=surv_descr,
                            N_CASES=Istudy::get_n_cases(surv_ana@elig_indv, surv_ana@study@endpt),
                            N_CONTROLS=Istudy::get_n_cntrls(surv_ana@elig_indv, surv_ana@study@endpt),
                            C_IDX_TEST=c_idx_res$test["C Index"],
                            CI_TEST_NEG=c_idx_ci_test[[1]],
                            CI_TEST_POS=c_idx_ci_test[[2]],
                            C_IDX_TRAIN=c_idx_res$train["C Index"],
                            CI_TRAIN_NEG=c_idx_ci_train[[1]],
                            CI_TRAIN_POS=c_idx_ci_train[[2]])
    }
    return(endpt_c_idxs_tib)
}