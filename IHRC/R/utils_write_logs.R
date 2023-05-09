
#' Writes results and log file
#' 
#' Prints to console if something goes wrong.
#' 
#' @param pheno_score_data A data.frame. The phenotype-score data to be
#'                          written to file.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param endpt A string. The current endpoint.
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' 
#' @author Kira E. Detrois
write_pheno_score_files <- function(pheno_score_data,
                                    study_setup,
                                    endpt,
                                    surv_ana) {
    if(!is.null(pheno_score_data)) {
        file_path <- get_full_file_name_path(res_type="pheno_score",
                                            study_setup=study_setup,
                                            endpt=endpt,
                                            surv_ana=surv_ana)
        readr::write_delim(x=pheno_score_data,
                           file=file_path,
                           delim="\t")
    } else {
        warning("No phenotype-score data to write to file for endpoint ", endpt, ".")
    }
}

#' Creates a string of the current study setup
#' 
#' @param study An S4 `study` object. The current study. 
#'                  See class definition [Istudy::study].
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



#' Checks whether to write a results file and if the directory exists
#' 
#' If the directory does not exists tries to create it, recursively.
#' 
#' @param write_res A boolean. Whether to write log files.
#' @param res_dir A string. The path to the results directory.
#' 
#' @return A boolean. Whether to write the results to the `res_dir`.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
check_res_dir <- function(write_res,
                          res_dir) { 
   if(write_res) {
        if(is.na(res_dir)) {
            writeLines("Variable write_res was set to TRUE but res_dir was not provided. Cannot write results to file.")
            return(FALSE)
        } else {
            if(!dir.exists(res_dir)) {
                writeLines(paste0("The file directory ", res_dir, " does not exist. Trying to create it."))
                dir.create(res_dir, recursive=TRUE)
            }
            return(TRUE)
        }
    } else {
        return(FALSE)
    }
}