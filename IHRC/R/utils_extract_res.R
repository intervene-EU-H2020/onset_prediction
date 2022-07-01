
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
#' @param all_coxph_res A tibble. The results for the previous endpoints.
#' @param coxph_res_tib A tibble with the results for the current endpoints.
#' @param score_type A character. The name of the score used for the model,
#'                      i.e. CCI, or PheRS.
#' @param endpt A character. The current enpoint of interest.
#' @param elig_indv A tibble. The individuals which were eligble under
#'                        the current study setup. Needs to at least contain
#'                        the column defined in `endpt`.
#' 
#' @return A tibble. TODO
#'                        
#' @author Kira E. Detrois 
add_coxph_row <- function(coxph_res_tib,
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
#' @param coxph_mdl A Cox-PH model. 
#'
#' @return A list(`beta`, `std_err`, `p_val`, `HR`, `CI`, `groups`).
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
