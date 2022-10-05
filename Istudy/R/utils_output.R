#' Creates a file name for the current study setup
#' 
#' @param study An S4 study object. The current study setup.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_study_file_name <- function(study) {
    if(study@study_type == "forward") {
        file_name <- paste0(study@endpt, "_a", study@exp_age)
    } else {
        file_name <- paste0(study@endpt, "_", study@obs_end_date)
    }
    file_name <- paste0(file_name, "_", get_ewo_file_name(study@study_type,
                                                          study@exp_len,
                                                          study@wash_len,
                                                          study@obs_len))
    return(file_name)
}   

#' Creates file name describing the exposure, wash, and observation period.
#' 
#' @inheritParams set_study_dates
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_ewo_file_name <- function(study_type,
                              exp_len,
                              wash_len,
                              obs_len) {
    if(study_type == "forward") {
        file_name <- paste0("e", exp_len, "_w", wash_len, "_o", obs_len)
    } else {
        file_name <- paste0("o", obs_len, "_w", wash_len, "_e", exp_len)
    }
    return(file_name)
}

#' Writes results and log file
#' 
#' Prints to console if something goes wrong.
#' 
#' @param study An S4 study object. The current study setup.
#' 
#' @author Kira E. Detrois
write_res_files <- function(study) {
    write_res_file(study=study)
    write_log_file(study=study)
}

#' Writes results to a tab-delim file
#' 
#' @param study An S4 study object. The current study setup.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res_file <- function(study) {
    if(check_res_dir(study@write_res, study@res_dir) & 
        (get_n_cases(study@study_data, study@endpt) > 0)) {
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
#' @export 
#' 
#' @author Kira E. Detrois
log_msg_string <- function(study) {
    n_cases <- get_n_cases(study@study_data, study@endpt)
    n_cntrls <- get_n_cntrls(study@study_data, study@endpt)

    paste0("Endpoint: ", study@endpt, "\n",
           "No of cases: ", n_cases, "\n",
           "No of ctrls: ", n_cntrls, "\n",   
           "Age at exposure start:        ", study@exp_age, "\n",
           "Length of exposure period:    ", study@exp_len, "\n", 
           "Length of washout period:     ", study@wash_len, "\n",
           "Length of observation period: ", study@obs_len, "\n")
}

#' Writes study setup to a file
#' 
#' @inheritParams write_res_files
#' 
#' @export 
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