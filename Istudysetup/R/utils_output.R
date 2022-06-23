
#' Creates a list with relevant information of the selected data
#' 
#' Gets only the columns relevant for the current endpoint.
#' Creates a list with the data, as well as the selected
#' study period intervals.
#' 
#' @inheritParams get_study_elig_indv
#' 
#' @return A list(`data`, `exp_age`, `exp_len`, `wash_len`, `obs_len`):
#'         \itemize{
#'          \item `data`: The actual data.frame.
#'          \item `exp_age` An integer. Age at which exposure period  
#'                            starts (in years).
#'          \item `exp_len` An integer. Length of the exposure period
#'                               (in years).
#'          \item `wash_len` An integer. Length of the washout period
#'                                (in years).
#'          \item `obs_len` An integer. Length of the prediction period
#'                               (in years).
#'          }
#' 
#' @author Kira E. Detrois
create_return_dt <- function(pheno_data,
                             endpt,
                             exp_age=30,
                             exp_len=10,
                             wash_len=2,
                             obs_len=8) {
    test_endpt_input_correct(as.list(environment()))
    test_length_vars_are_integers(as.list(environment()))
    
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

    elig_data <- list(data=elig_data, 
                      exp_age=exp_age,
                      exp_len=exp_len,
                      wash_len=wash_len,
                      obs_len=obs_len,
                      n_cases=get_n_cases(pheno_data, endpt),
                      n_ctrls=get_n_ctrls(pheno_data, endpt))
}

#' Creates a file name for the current study setup
#' 
#' @param envir A list with at least entries `endpt`, `exp_age`, `exp_len`, 
#'              `wash_len`, and `obs_len`.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_study_file_name <- function(envir) {
    paste0(envir$endpt, "_", envir$exp_age, "_", envir$exp_len, "_", envir$wash_len, "_", envir$obs_len)
}

#' Writes results to a tab-delim file
#' 
#' This should be called after `create_return_df`. 
#' 
#' @param envir A list with at least entries `write_res`, `res_dir` ,
#'              `elig_data`, `endpt`, `exp_age`, `exp_len`, `wash_len`, 
#'              and `obs_len`.
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
                readr::write_delim(envir$elig_data$data, 
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
#'              `endpt`, `exp_age`, `exp_len`, `wash_len`, and `obs_len`.
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
#'              `exp_age`, `exp_len`, `wash_len`, and `obs_len`.
log_msg_string <- function(envir) {
    n_cases <- get_n_cases(envir$pheno_data, envir$endpt)
    n_ctrls <- get_n_ctrls(envir$pheno_data, envir$endpt)
    paste0("Endpoint: ", envir$endpt, "\n",
           "No of cases: ", n_cases, "\n",
           "No of ctrls: ", n_ctrls, "\n",   
           "Age at exposure start:        ", envir$exp_age, "\n",
           "Length of exposure period:    ", envir$exp_len, "\n", 
           "Length of washout period:     ", envir$wash_len, "\n",
           "Length of observation period: ", envir$obs_len, "\n")
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
    n_ctrls <- get_n_ctrls(envir$pheno_data, envir$endpt)
    
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