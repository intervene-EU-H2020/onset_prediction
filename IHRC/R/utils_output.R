#' Writes model results to a tab-delim file
#'  
#' @param envir A list with at least entries `write_res`, `res_dir` ,
#'              `coxph_res`, `endpt`, `exp_age`, `exp_len`, 
#'              `wash_len`, and `obs_len`.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res <- function(envir) {
    check_res_dir(envir$write_res, envir$res_dir)
    envir$endpt <- "study" # Cheating the system for the file name
    res_file_name <- paste0(Istudysetup::get_study_file_name(envir), 
                                    "_coxph_res.tsv")
    readr::write_delim(envir$coxph_res, 
                        paste0(envir$res_dir, res_file_name),
                               delim="\t")
}

#' Checks whether the results directory exists, otherwise creates it
#' 
#' @param write_res A boolean. Whether to write the results to a file.
#' @param res_dir A character. The directory path for the results.
#' 
#' @export
#' 
#' @author Kira E. Detrois
check_res_dir <- function(write_res,
                          res_dir) {
    if(write_res) {
        if(is.na(res_dir)) {
            message("Variable write_res was set to TRUE but res_dir was not provided. Cannot write results to file.")
    } else {
        if(!dir.exists(res_dir)) {
            message(paste0("The file directory ", res_dir, " does not exist. Trying to create it."))
                dir.create(res_dir, recursive=TRUE)
            } 
        } 
    }
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
    if(!is.null(names(coxph_res))) {
        n_cases <- n_group_cases(elig_endpt_indv, 
                                coxph_res$groups,
                                endpt)
        n_ctrls <- get_group_ctrls(elig_endpt_indv, 
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

#' Creates a string of the current study setup
#' 
#' Only used inside `write_log` function.
#' 
#' @param envir A list with at least entry `pheno_score_data`.
#' 
#' @return A tibble with columns `GROUP`, `SCORE_CUT`
#' 
#' @author Kira E. Detrois
log_msg_table <- function(envir) {
    score_groups_table <- get_unique_score_groups(envir$pheno_score_data)
    score_tib <- tibble::tibble(GROUP=names(score_groups_table), 
                                SCORE_CUT=score_groups_table)

    return(score_tib)
}

#' Write score cut-off values and groups to log
#' 
#' 
#' @param envir A list with at least entry `write_res` and `res_dir`, 
#'  
#' @author Kira E. Detrois
write_score_groups_to_log <- function(envir) {
    if(envir$write_res) {
        if(is.na(envir$res_dir)) {
            message("Variable write_log was set to `file` but no log file path was provided. Printing instead.")
            print(log_msg_table(envir))
        } else {
            if(!dir.exists(envir$res_dir)) {
                message(paste0("The log file directory ", envir$res_dir, " does not exist. Trying to create it."))
                dir.create(envir$res_dir, recursive=TRUE)
            }
            file_path <- paste0(envir$res_dir, 
                                Istudysetup::get_study_file_name(envir), 
                                "_log.txt")
            if(file.exists(file_path)) {
                append_mode <- TRUE
            } else {
                append_mode <- FALSE
            }
            readr::write_delim(log_msg_table(envir), 
                            append=append_mode,
                            col_names=TRUE,
                            delim=" ",
                            file_path)
        }
    }
}