
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
add_coxph_row <- function(all_coxph_res,
                          coxph_res_tib,
                          score_type,
                          endpt,
                          elig_indv) {
    if(!is.null(names(coxph_res_tib))) {
        n_cases <- n_group_cases(elig_indv, 
                                 coxph_res_tib$groups,
                                 endpt)
        n_ctrls <- get_group_ctrls(elig_indv, 
                                   coxph_res_tib$groups,
                                   endpt)

        all_coxph_res <- tibble::add_row(all_coxph_res, 
                                         Endpoint=endpt,
                                         Score=score_type,
                                         Group=coxph_res_tib$groups,
                                         N_controls=n_ctrls,
                                         N_cases=n_cases,
                                         beta=coxph_res_tib$beta,
                                         std_errs=coxph_res_tib$std_err,
                                         p_val=coxph_res_tib$p_val,
                                         HR=coxph_res_tib$HR,
                                         CI_neg=coxph_res_tib$CI$neg,
                                         CI_pos=coxph_res_tib$CI$pos
                          )
    } 
    return(all_coxph_res)
}

#' Extracts the relevant results from the Cox-PH model
#' 
#' @param coxph_res_mdl A Cox-PH model. 
#'
#' @return A list(`beta`, `std_err`, `p_val`, `HR`, `CI`, `groups`).
extract_coxph_res <- function(coxph_res_mdl) {
    betas <- summary(coxph_res_mdl)$coefficients[,"coef"]
    std_errs <- summary(coxph_res_mdl)$coefficients[,"se(coef)"]
    pvals <- summary(coxph_res_mdl)$coefficients[,"Pr(>|z|)"]
    OR <- exp(betas)
    CI <- get_CI(betas, std_errs)

    if("SCORE_GROUP" %in% names(coxph_res_mdl$xlevels)) {
        groups <- coxph_res_mdl$xlevels$SCORE_GROUP[2:length(coxph_res_mdl$xlevels$SCORE_GROUP)]
    } else {
        groups <- "all"
    }

    return(list(beta=betas, std_err=std_errs, p_val=pvals, HR=OR, CI=CI, groups=groups))
}
