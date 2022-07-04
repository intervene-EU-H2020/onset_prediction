
#' Creates an empty tibble for the results
#' 
#' @return An empty tibble with all relevant columns for the final results.
#' 
#' @author Kira E. Detrois 
create_empty_coxph_res_tib <- function() {
    tibble::tibble(Endpoint=character(),
                   Score=character(),
                   Group=character(),
                   N_controls=numeric(),
                   N_cases=numeric(),
                   beta=numeric(),
                   std_errs=numeric(),
                   p_val=numeric(),
                   HR=numeric(),
                   CI_pos=numeric(),
                   CI_neg=numeric())
}
#' Adds a row to the results tibble
#' 
#' @param coxph_res_tib A tibble with the results for previous endpoints.
#' @param coxph_mdl A Cox-PH model. 
#' @inheritParams extract_coxph_res
#' @inheritParams calc_studies_hrs
#' @inheritParams run_coxph_ana
#' @param elig_indv A tibble. The individuals which were eligble under
#'                        the current study setup. Needs to at least 
#'                        contain the column defined in `endpt`.
#' 
#' @return A tibble. The updated `coxph_res_tib` with the added results for
#'          the current endpoint.
#'                        
#' @author Kira E. Detrois 
add_coxph_res_row <- function(coxph_res_tib,
                          coxph_mdl,
                          score_type,
                          endpt,
                          elig_indv) {

    if(!is.null(coxph_mdl)) {
        coxph_res_list <- extract_coxph_res(coxph_mdl)
        n_cases <- n_group_cases(elig_indv, 
                                 coxph_res_list$groups,
                                 endpt)
        n_ctrls <- get_group_ctrls(elig_indv, 
                                   coxph_res_list$groups,
                                   endpt)
        coxph_res_tib <- tibble::add_row(coxph_res_tib, 
                                         Endpoint=endpt,
                                         Score=score_type,
                                         Group=coxph_res_list$groups,
                                         N_controls=n_ctrls,
                                         N_cases=n_cases,
                                         beta=coxph_res_list$beta,
                                         std_errs=coxph_res_list$std_err,
                                         p_val=coxph_res_list$p_val,
                                         HR=coxph_res_list$HR,
                                         CI_neg=coxph_res_list$CI$neg,
                                         CI_pos=coxph_res_list$CI$pos
                          )
    } 
    return(coxph_res_tib)
}

#' Extracts the relevant results from the Cox-PH model
#' 
#' @inheritParams add_coxph_res_row
#' 
#' @return A list(`beta`, `std_err`, `p_val`, `HR`, `CI`, `groups`).
#' 
#' @author Kira E. Detrois
extract_coxph_res <- function(coxph_mdl) {
    betas <- summary(coxph_mdl)$coefficients[,"coef"]
    betas <- betas[grep("SCORE", names(betas))]
    std_errs <- summary(coxph_mdl)$coefficients[,"se(coef)"]
    std_errs <- std_errs[grep("SCORE", names(std_errs))]
    pvals <- summary(coxph_mdl)$coefficients[,"Pr(>|z|)"]
    pvals <- pvals[grep("SCORE", names(pvals))]
    OR <- exp(betas)
    CI <- get_CI(betas, std_errs)
    if("SCORE_GROUP" %in% names(coxph_mdl$xlevels)) {
        groups <- coxph_mdl$xlevels$SCORE_GROUP[2:length(coxph_mdl$xlevels$SCORE_GROUP)]
    } else {
        groups <- "no groups"
    }
    return(list(beta=betas, std_err=std_errs, p_val=pvals, HR=OR, CI=CI, groups=groups))
}
