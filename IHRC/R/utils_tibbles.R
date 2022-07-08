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
        EXP_AGE = numeric(),
        SURV_MODEL = character(),
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
                              min_indvs=5) {
    if(!is.null(coxph_mdl)) {
        coxph_res_list <- get_min_indvs_data(
                                    elig_indv=elig_indv,
                                    coxph_mdl=coxph_mdl,
                                    study=study,
                                    min_indvs=min_indvs)
        if(!is.null(coxph_res_list)) {
            endpt_hrs_tib <- tibble::add_row(
                                         endpt_hrs_tib, 
                                         ENDPOINT=study@endpt,
                                         EXP_AGE=study@exp_age,
                                         SCORE=score_type,
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
#' least 5 controls and cases in each group. 
#' 
#' @inheritParams add_coxph_res_row
get_min_indvs_data <- function(elig_indv,
                               coxph_mdl,
                               study,
                               min_indvs=5) {
    coxph_res_list <- extract_coxph_res(coxph_mdl)
    n_cntrls_vec <- get_n_group_cntrls(elig_indv, 
                                       levels(elig_indv$SCORE_GROUP),
                                       study@endpt)
    n_cases_vec <- get_n_group_cases(elig_indv, 
                                     levels(elig_indv$SCORE_GROUP),
                                     study@endpt)
    ref_level = levels(elig_indv$SCORE_GROUP)[1]
    
    # Reference group has enough cases and controls

    if((n_cntrls_vec[1] > min_indvs) & 
            (n_cases_vec[1] > min_indvs)) {
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
        coxph_res_tib <- coxph_res_tib[coxph_res_tib$n_cntrl > min_indvs &
                                        coxph_res_tib$n_case > min_indvs,]
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
                             study,
                             score_type,
                             covs,
                             bin_cut) {
    if(!is.null(c_idx_res)) {
        surv_descr=get_surv_descr(score_type=score_type, 
                                  surv_type="surv", 
                                  covs=covs,
                                  bin_cut=bin_cut)
        c_idxs_tib <- tibble::add_row(
                            c_idxs_tib,
                            ENDPOINT=study@endpt,
                            EXP_AGE=study@exp_age,
                            SURV_MODEL=surv_descr,
                            C_INDEX=c_idx_res["C Index"],
                            DXY=c_idx_res["Dxy"],
                            SD=c_idx_res["S.D."],
                            N=c_idx_res["n"],
                            MISSING=c_idx_res["missing"],
                            UNCESORED=c_idx_res["uncensored"],
                            RELEVANT_PAIRS=c_idx_res["Relevant Pairs"],
                            CONCORDANT=c_idx_res ["Concordant"],
                            UNCERTAIN=c_idx_res["Uncertain"])
    }
    return(c_idxs_tib)
}