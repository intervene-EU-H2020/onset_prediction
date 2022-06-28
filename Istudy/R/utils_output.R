
#' Creates a list with relevant information of the selected data
#' 
#' Gets only the columns relevant for the current endpoint.
#' Creates a list with the data, as well as the selected
#' study period intervals.
#' 
#' @inheritParams get_study_elig_indv
#' @inheritParams adj_case_cntrl_status
#' 
#' @return The results data.frame to return
#' 
#' @author Kira E. Detrois
create_return_tib <- function(pheno_data,
                              endpt) {
    check_cols_exist(pheno_data, endpt, "create_return_tib")
    elig_data <- dplyr::select(pheno_data, 
                               ID, 
                               SEX, 
                               DATE_OF_BIRTH, 
                               ANCESTRY, 
                               # Otherwise dplyr will throw error. 
                               # test_endpt_input_correct already checks that
                               # this is only a single string and not
                               # a vector
                               dplyr::all_of(endpt), 
                               paste0(endpt, "_AGE_DAYS"),
                               paste0(endpt, "_DATE"))
}

#' Creates a file name for the current study setup
#' 
#' @inheritParams add_study_interval_cols
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_study_file_name <- function(study) {
    paste0(study@endpt, "_", study@exp_age, "_", study@exp_len, "_", study@wash_len, "_", study@obs_len)
}

#' Writes results and log file
#' 
#' Prints to console if something goes wrong.
#' 
#' @param elig_indv A tibble with the information for the eligible 
#'                  individuals that should be written to the file.
#' @inheritParams get_study_file_name
#' @inheritParams get_study_elig_indv
#' 
#' @author Kira E. Detrois
write_res_files <- function(elig_indv,
                      study,
                      write_res,
                      res_dir) {
    write_res_file(elig_indv, study, write_res, res_dir)
    write_log_file(elig_indv, study, write_res, res_dir)

    if(get_n_cases(elig_indv, study@endpt) == 0) {
        message("Message: Careful there are no cases eligible under the current study setup.")
        message(log_msg_string(elig_indv, study))
    }
}

#' Writes results to a tab-delim file
#' 
#' @inheritParams write_res_files
#' 
#' @author Kira E. Detrois
write_res_file <- function(elig_indv,
                           study,
                           write_res,
                           res_dir) {
 
    if(check_res_dir(write_res, res_dir) & 
        get_n_cases(elig_indv, study@endpt) > 0) {
        res_file_name <- paste0(get_study_file_name(study), 
                                "_elig_indv.tsv")
        readr::write_delim(elig_indv, 
                           paste0(res_dir, res_file_name),
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
log_msg_string <- function(elig_indv,
                           study) {
    n_cases <- get_n_cases(elig_indv, study@endpt)
    n_cntrls <- get_n_cntrls(elig_indv, study@endpt)
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
#' @author Kira E. Detrois
write_log_file <- function(elig_indv,
                           study,
                           write_res,
                           res_dir) {
    if(check_res_dir(write_res, res_dir)) {
        n_cases <- get_n_cases(elig_indv, study@endpt)
        n_cntrls <- get_n_cntrls(elig_indv, study@endpt)

        readr::write_file(log_msg_string(elig_indv, study), 
                          paste0(res_dir, 
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
check_res_dir <- function(write_res,
                          res_dir) { 
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