#' Creates a file name for the current study setup
#' 
#' @param study An S4 study object. The current study setup.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_study_file_name <- function(study) {
    if(study@study_type == "forward") {
        file_name <- paste0(study@endpt, "_a", study@exp_age, "_e", study@exp_len, "_w", study@wash_len, "_o", study@obs_len)
    } else {
        file_name <- paste0(study@endpt, "_", study@obs_end_date, "_o", study@obs_len, "_w", study@wash_len)
        if(!is.na(study@exp_len)) {
            file_name <- paste0(file_name, "_e", study@exp_len)
        }
    }
}   

#' Writes results and log file
#' 
#' Prints to console if something goes wrong.
#' 
#' @param study@study_data A tibble with the information for the eligible 
#'                  individuals that should be written to the file.
#' @inheritParams get_study_file_name
#' @inheritParams get_study_study@study_data
#' 
#' @author Kira E. Detrois
write_res_files <- function(study) {
    write_res_file(study=study)
    write_log_file(study=study)
}

#' Writes results to a tab-delim file
#' 
#' @inheritParams write_res_files
#' 
#' @author Kira E. Detrois
write_res_file <- function(study) {
    if(check_res_dir(study@write_res, study@res_dir) & 
        get_n_cases(study@study_data, study@endpt) > 0) {
            elig_res_dir <- paste0(study@res_dir, "elig_indv/")
            if(!dir.exists(elig_res_dir)) {
                dir.create(elig_res_dir)
            }
            res_file_name <- paste0(get_study_file_name(study), 
                                    "_elig_indv.tsv")
            readr::write_delim(study@study_data, 
                               paste0(elig_res_dir, res_file_name),
                               delim="\t")
    }
}

#' Creates a string of the current study setup
#' 
#' @inheritParams write_res_files
#' 
#' @return The log message character string
#' 
#' @author Kira E. Detrois
log_msg_string <- function(study) {
    n_cases <- get_n_cases(study@study_data, study@endpt)
    n_cntrls <- get_n_cntrls(study@study_data, study@endpt)

    if(study@study_type == "forward") {
        paste0("Endpoint: ", study@endpt, "\n",
               "No of cases: ", n_cases, "\n",
               "No of ctrls: ", n_cntrls, "\n",   
               "Age at exposure start:        ", study@exp_age, "\n",
               "Length of exposure period:    ", study@exp_len, "\n", 
               "Length of washout period:     ", study@wash_len, "\n",
               "Length of observation period: ", study@obs_len, "\n")
    } else {
        paste0("Endpoint: ", study@endpt, "\n",
               "No of cases: ", n_cases, "\n",
               "No of ctrls: ", n_cntrls, "\n",   
               "Length of washout period:     ", study@wash_len, "\n",
               "Length of observation period: ", study@obs_len, "\n",
               "End of observation:        ", study@obs_end_date, "\n")      
    }
}

#' Writes study setup to a file
#' 
#' @inheritParams write_res_files
#' 
#' @author Kira E. Detrois
write_log_file <- function(study) {
    if(check_res_dir(study@write_res, study@res_dir)) {
        log_res_dir <- paste0(study@res_dir, "log/")
        if(!dir.exists(log_res_dir)) {
            dir.create(log_res_dir)
        }
        readr::write_file(log_msg_string(study), 
                          paste0(log_res_dir, 
                                 get_study_file_name(study), 
                                 "_log.txt"))
    }
}

#' Checks whether to write a results file and if the directory exists
#' 
#' If the directory does not exists tries to create it, recursively.
#' 
#' @inheritParams write_res_files
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