
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
#' @param coxph_res A tibble with the results for the current endpoints.
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
                          coxph_res,
                          score_type,
                          endpt,
                          elig_indv) {
    if(!is.null(names(coxph_res))) {
        n_cases <- n_group_cases(elig_indv, 
                                coxph_res$groups,
                                endpt)
        n_ctrls <- get_group_ctrls(elig_indv, 
                                coxph_res$groups,
                                endpt)

        all_coxph_res <- tibble::add_row(all_coxph_res, 
                                         Endpoint=endpt,
                                         Score=score_type,
                                         Group=coxph_res$groups,
                                         N_controls=n_ctrls,
                                         N_cases=n_cases,
                                         beta=coxph_res$beta,
                                         std_errs=coxph_res$std_err,
                                         p_val=coxph_res$p_val,
                                         HR=coxph_res$HR,
                                         CI_neg=coxph_res$CI$neg,
                                         CI_pos=coxph_res$CI$pos
                          )
    } 
    return(all_coxph_res)
}

#' Extracts the relevant results from the cox-ph model
#' 
#' @param coxph_res A coxph model. 
#'
#' @return A list(`beta`, `std_err`, `p_val`, `HR`, `CI`, `groups`).
extract_coxph_res <- function(coxph_res) {
    betas <- summary(coxph_res)$coefficients[,"coef"]
    std_errs <- summary(coxph_res)$coefficients[,"se(coef)"]
    pvals <- summary(coxph_res)$coefficients[,"Pr(>|z|)"]
    OR <- exp(betas)
    CI <- get_CI(betas, std_errs)

    if("SCORE_GROUP" %in% names(coxph_res$xlevels)) {
        groups <- coxph_res$xlevels$SCORE_GROUP[2:length(coxph_res$xlevels$SCORE_GROUP)]
    } else {
        groups <- "all"
    }

    return(list(beta=betas, std_err=std_errs, p_val=pvals, HR=OR, CI=CI, groups=groups))
}
