#' Writes model results to a tab-delim file
#'  
#' @param envir A list with at least entries `write_coxph_res`, `res_dir` ,
#'              `all_coxph_res`, `endpt`, `exp_age`, `exp_len`, `wash_len`, 
#'              and `obs_len`.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_coxph_res <- function(envir) {
    if(envir$write_coxph_res) {
        if(is.na(envir$res_dir)) {
            message("Variable write_coxph_res was set to TRUE but res_dir was not provided. Cannot write results to file.")
        } else {
            if(!dir.exists(envir$res_dir)) {
                message(paste0("The file directory ", envir$res_dir, " does not exist. Trying to create it."))
                dir.create(envir$res_dir, recursive=TRUE)
            } 
            envir$endpt <- "study" # Cheating the system for the file name
            res_file_name <- paste0(Istudysetup::get_study_file_name(envir), 
                                    "_coxph_res.tsv")
            readr::write_delim(envir$all_coxph_res, 
                               paste0(envir$res_dir, res_file_name),
                               delim="\t")
            }

        }
}

#' Extracts the relevant results from the cox-ph model
#' 
#' @param coxph_res A coxph model. 
#'
#' @return A list(`beta`, `std_err`, `p_val`, `HR`, `CI`).
extract_coxph_res <- function(coxph_res) {
    betas <- summary(coxph_res)$coefficients[,"coef"]
    std_errs <- summary(coxph_res)$coefficients[,"se(coef)"]
    pvals <- summary(coxph_res)$coefficients[,"Pr(>|z|)"]
    OR <- exp(betas)
    CI <- get_CI(betas, std_errs)
    
    return(list(beta=betas, std_err=std_errs, p_val=pvals, HR=OR, CI=CI))
}

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
#' @param elig_endpt_indv A tibble. The individuals which were eligble under
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
                         elig_endpt_indv) {

    n_cases <- Istudysetup::get_n_cases(elig_endpt_indv, endpt)
    n_ctrls <- Istudysetup::get_n_ctrls(elig_endpt_indv, endpt)

    tibble::add_row(all_coxph_res, 
                    Endpoint=endpt,
                    Score=score_type,
                    Group="all",
                    N_controls=n_ctrls,
                    N_cases=n_cases,
                    beta=coxph_res$beta,
                    std_errs=coxph_res$std_err,
                    p_val=coxph_res$p_val,
                    HR=coxph_res$HR,
                    CI_pos=coxph_res$CI["pos"],
                    CI_neg=coxph_res$CI["neg"]
    )
}


