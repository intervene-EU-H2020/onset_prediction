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
get_empty_cidx_tib <- function() {
    tibble::tibble(
        ENDPOINT = character(),
        C_INDEX = numeric(),
        DXY = numeric(),
        SD = numeric(),
        N = numeric(),
        MISSING = numeric(),
        UNCESORED = numeric(),
        RELEVANT_PAIRS = numeric(),
        CONCORDANT = numeric(),
        UNCERTAIN = numeric()
    )
}

#' Adds a row to the Cox-PH HR results tibble
#' 
#' @param endpt_hrs_tib A tibble with the results for previous endpts.
#' @param coxph_mdl A Cox-PH model. 
#' @inheritParams extract_coxph_res
#' @inheritParams calc_endpt_studies_hrs
#' @inheritParams get_coxph_mdl
#' @param elig_indv A tibble. The individuals which were eligble under
#'                        the current study setup. Needs to at least 
#'                        contain the column defined in `endpt`.
#' 
#' @return A tibble. The updated `endpt_hrs_tib` with the added results for
#'          the current endpoint.
#'                        
#' @author Kira E. Detrois 
add_coxph_res_row <- function(endpt_hrs_tib,
                              coxph_mdl,
                              score_type,
                              study,
                              elig_indv,
                              min_cases=5) {
    if(!is.null(coxph_mdl)) {
        coxph_res_list <- extract_coxph_res(coxph_mdl)
        n_cases <- get_n_group_cases(elig_indv, 
                                     coxph_res_list$groups,
                                     study@endpt)
        n_cntrls <- get_n_group_cntrls(elig_indv, 
                                       coxph_res_list$groups,
                                       study@endpt)
        # TODO filter min cases / controls here.
        endpt_hrs_tib <- tibble::add_row(endpt_hrs_tib, 
                                         ENDPOINT=study@endpt,
                                         EXP_AGE=study@exp_age,
                                         SCORE=score_type,
                                         GROUP=coxph_res_list$groups,
                                         N_CONTROLS=n_cntrls,
                                         N_CASES=n_cases,
                                         BETA=coxph_res_list$beta,
                                         STD_ERRS=coxph_res_list$std_err,
                                         P_VAL=coxph_res_list$p_val,
                                         HR=coxph_res_list$HR,
                                         CI_NEG=coxph_res_list$CI$neg,
                                         CI_POS=coxph_res_list$CI$pos
                          )
    } 
    return(endpt_hrs_tib)
}

#' Adds a row to the C-index results tibble
#' 
#' @param endpt_hrs_tib A tibble with the results for previous endpts.
#' @param coxph_mdl A Cox-PH model. 
#' @inheritParams extract_coxph_res
#' @inheritParams calc_endpt_studies_hrs
#' @inheritParams get_coxph_mdl
#' @param elig_indv A tibble. The individuals which were eligble under
#'                        the current study setup. Needs to at least 
#'                        contain the column defined in `endpt`.
#' 
#' @return A tibble. The updated `endpt_hrs_tib` with the added results for
#'          the current endpoint.
#'                        
#' @author Kira E. Detrois 
add_cidx_res_row <- function(c_idxs_tib,
                             c_idx_res,
                             endpt) {
    if(!is.null(c_idx_res)) {
        c_idx_tib <- tibble::add_row(c_idxs_tib,
                                     ENDPOINT=endpt,
                                     C_INDEX=c_idx_res["C Index"],
                                     DXY=c_idx_res["Dxy"],
                                     SD=c_idx_res["S.D."],
                                     N=c_idx_res["n"],
                                     MISSING=c_idx_res["missing"],
                                     UNCESORED=c_idx_res["uncensored"],
                                     RELEVANT_PAIRS=c_idx_res["Relevant Pairs"],
                                     CONCORDANT=c_idx_res["Concordant"],
                                     UNCERTAIN=c_idx_res["Uncertain"])
    }
    return(c_idx_tib)
}