
#' Creates a list with relevant information of the selected data
#' 
#' Gets only the columns relevant for the current endpoint.
#' Creates a list with the data, as well as the selected
#' study period intervals.
#' 
#' @inheritParams get_study_elig_indv
#' 
#' @return A list(`data`, `exp_age`, `exp_age`, `exp_age`, `exp_age`):
#'         \itemize{
#'          \item `data`: The actual data.frame.
#'          \item `exp_age` An integer. Age at which exposure period  
#'                            starts (in years).
#'          \item `exp_age` An integer. Length of the exposure period
#'                               (in years).
#'          \item `exp_age` An integer. Length of the washout period
#'                                (in years).
#'          \item `exp_age` An integer. Length of the prediction period
#'                               (in years).
#'          }
#' 
#' @author Kira E. Detrois
create_return_dt <- function(pheno_data,
                             endpt) {
    #test_endpt_input_correct(as.list(environment()))
    #test_length_vars_are_integers(as.list(environment()))
    
    elig_data <- dplyr::select(pheno_data, 
                               ID, 
                               SEX, 
                               DATE_OF_BIRTH, 
                               ANCESTRY, 
                               # Otherwise dplyr will throw error. 
                               # test_endpt_correct already checks that
                               # this is only a single string and not
                               # a vector
                               dplyr::all_of(endpt), 
                               paste0(endpt, "_AGE_DAYS"),
                               paste0(endpt, "_DATE"))
}

#' Creates a file name for the current study setup
#' 
#' @param envir A list with at least entries `endpt`, `exp_age`, `exp_age`, 
#'              `exp_age`, and `exp_age`.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_study_file_name <- function(envir) {
    paste0(envir$endpt, "_", envir$study@exp_age, "_", envir$study@exp_len, "_", envir$study@wash_len, "_", envir$study@obs_len)
}

#' Writes results to a tab-delim file
#' 
#' This should be called after `create_return_df`. 
#' 
#' @param envir A list with at least entries `write_res`, `res_dir` ,
#'              `elig_data`, `endpt`, `exp_age`, `exp_age`, `exp_age`, 
#'              and `exp_age`.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_res <- function(envir) {
    if(envir$write_res) {
        if(is.na(envir$res_dir)) {
            message("Variable write_res was set to TRUE but res_dir was not provided. Cannot write results to file.")
        } else {
            if(!dir.exists(envir$res_dir)) {
                message(paste0("The file directory ", envir$res_dir, " does not exist. Trying to create it."))
                dir.create(envir$res_dir, recursive=TRUE)
            } 
            if(get_n_cases(envir$pheno_data, envir$endpt) > 0) {
                res_file_name <- paste0(get_study_file_name(envir), 
                                    "_elig_indv.tsv")
                readr::write_delim(envir$elig_data@elig_indv, 
                                   paste0(envir$res_dir, res_file_name),
                                   delim="\t")
            }

        }
    }
}

#' Prints the current study setup or writes it to a file
#' 
#' Only used inside `create_test_df` function.
#' 
#' @param envir A list with at least entries `write_res`, `pheno_data`, 
#'              `endpt`, `exp_age`, `exp_age`, `exp_age`, and `exp_age`.
write_res <- function(envir) {
    if(envir$write_res) {
        write_res_file(envir)
    } else if(get_n_cases(envir$pheno_data, envir$endpt) == 0) {
        message("Message: Careful there are no cases eligible under the current study setup.")
        message(log_msg_string(envir))
    }
}

#' Creates a string of the current study setup
#' 
#' Only used inside `write_res` function.
#' 
#' @param envir A list with at least entries `pheno_data`, `endpt`, 
#'              `exp_age`, `exp_age`, `exp_age`, and `exp_age`.
log_msg_string <- function(envir) {
    n_cases <- get_n_cases(envir$pheno_data, envir$endpt)
    n_cntrls <- get_n_cntrls(envir$pheno_data, envir$endpt)
    paste0("Endpoint: ", envir$endpt, "\n",
           "No of cases: ", n_cases, "\n",
           "No of ctrls: ", n_cntrls, "\n",   
           "Age at exposure start:        ", envir$study@exp_age, "\n",
           "Length of exposure period:    ", envir$study@exp_len, "\n", 
           "Length of washout period:     ", envir$study@wash_len, "\n",
           "Length of observation period: ", envir$study@obs_len, "\n")
}

#' Creates a string of the current study setup
#' 
#' Only used inside `write_res` function.
#' 
#' @inheritParams log_msg_string
print_log_msg <- function(envir) {
    if(nrow(envir$pheno_data) == 0) {
        message("Careful no eligible individuals were found.")
    } else {
        writeLines(log_msg_string(envir))
    }
}

#' Prints the current study setup or writes it to a file
#' 
#' @inheritParams log_msg_string
write_res_file <- function(envir) {
    n_cases <- get_n_cases(envir$pheno_data, envir$endpt)
    n_cntrls <- get_n_cntrls(envir$pheno_data, envir$endpt)
    
    if(is.na(envir$res_dir)) {
        message("Variable write_res was set to `file` but no log file path was provided. Printing instead.")
        print_log_msg(envir)
    } else {
        if(!dir.exists(envir$res_dir)) {
            message(paste0("The log file directory ", envir$res_dir, " does not exist. Trying to create it."))
            dir.create(envir$res_dir, recursive=TRUE)
        }
        readr::write_file(log_msg_string(envir), 
                          paste0(envir$res_dir, 
                                 get_study_file_name(envir), 
                                 "_log.txt"))
    }
}