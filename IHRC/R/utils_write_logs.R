
#' Writes results and log file
#' 
#' Prints to console if something goes wrong.
#' 
#' @param surv_ana An S4 study object. The current survival analysis.
#' 
#' @author Kira E. Detrois
write_pheno_score_files <- function(pheno_score_data,
                                    study_setup,
                                    endpt,
                                    surv_ana) {
    if(check_res_dir(surv_ana@write_res, surv_ana@res_dir)) {
        file_path <- check_and_get_file_path(res_type="pheno_score",
                                             study_setup=study_setup,
                                             endpt=endpt,
                                             surv_ana=surv_ana)
        print(file_path)
        print(colnames(pheno_score_data))
        print(pheno_score_data)
        readr::write_delim(x=pheno_score_data,
                           file=file_path,
                           delim="\t")
    }

}


write_log_file <- function(study,
                           surv_ana) {
    if(check_res_dir(surv_ana@write_res, surv_ana@res_dir)) {

        file_path <- check_and_get_file_path(res_type="log",
                                            study_setup=study@study_setup,
                                            endpt=study@endpt,
                                            surv_ana=surv_ana)
        readr::write_file(log_msg_string(study), file_path)
    }
}

#' Creates a string of the current study setup
#' 
#' @inheritParams write_pheno_score_files
#' 
#' @return The log message character string
#' 
#' @export 
#' 
#' @author Kira E. Detrois
log_msg_string <- function(study) {
    n_cases <- Istudy::get_n_cases(study@study_data, study@endpt)
    n_cntrls <- Istudy::get_n_cntrls(study@study_data, study@endpt)

    paste0("Endpoint: ", study@endpt, "\n",
           "No of cases: ", n_cases, "\n",
           "No of ctrls: ", n_cntrls, "\n",   
           "Age at exposure start:        ", study@study_setup@exp_age, "\n",
           "Length of exposure period:    ", study@study_setup@exp_len, "\n", 
           "Length of washout period:     ", study@study_setup@wash_len, "\n",
           "Length of observation period: ", study@study_setup@obs_len, "\n")
}

append_log_file <- function(coxph_mdl,
                            study,
                            surv_ana) {
    if(check_res_dir(surv_ana@write_res, surv_ana@res_dir)) {
        file_path <- check_and_get_file_path(res_type="log",
                                             study_setup=study@study_setup,
                                             endpt=study@endpt,
                                             surv_ana=surv_ana)
        readr::write_file(paste0("\nSummary cox-PH model:\n\nNo of cases:", summary(coxph_mdl)$nevent, "\nNo of ctrls:", summary(coxph_mdl)$n-summary(coxph_mdl)$nevent, "\n\nzph:\n\n"),
                          file_path)
        try(readr::write_delim(tibble::as_tibble(survival::cox.zph(coxph_mdl)$table, rownames="PRED"),
                           file_path,
                           append=TRUE,
                           col_names=TRUE))

    }
}

#' Checks whether to write a results file and if the directory exists
#' 
#' If the directory does not exists tries to create it, recursively.
#' 
#' @param write_res A boolean. Whether to write log files.
#' @param res_dir A character. The path to the results directory.
#' 
#' @return A boolean. Whether to write the results to the `res_dir`.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
check_res_dir <- function(write_res=NULL,
                          res_dir=NULL) { 
   if(write_res) {
        if(is.na(res_dir)) {
            message("Variable write_res was set to TRUE but res_dir was not provided. Cannot write results to file.")
            return(FALSE)
        } else {
            if(!dir.exists(res_dir)) {
                message(paste0("The file directory ", res_dir, " does not exist. Trying to create it."))
                dir.create(res_dir, recursive=TRUE)
            } 
            return(TRUE)
        }
    } else {
        return(FALSE)
    }
}