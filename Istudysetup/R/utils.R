#' A helper function to calculate the age at diagnosis.
#' 
#' @param bds A Date. The birth days of the individuals.
#' @param diag_dates A Date. The dates of diagnosis.
#' 
#' @importFrom lubridate %--%
#' @export
#' 
#' @author Kira E. Detrois
calc_age_at_diag <- function(bds, diag_dates) {
    test_date_var_correct(bds, "bds")
    test_date_var_correct(diag_dates, "diag_dates")

    interval = bds %--% diag_dates
    age = interval %/% lubridate::years(1)
    return(age)
}

#' A helper function to add a column with the age at diagnosis
#' for each individual
#' 
#' @inheritParams filter_missing_endpt_data
#' 
#' @importFrom lubridate %--%
#' @export
#' 
#' @author Kira E. Detrois
add_age_at_diag_col <- function(pheno_data, endpt) {
    test_endpt_input_correct(as.list(environment()))

    age_at_diag <- calc_age_at_diag(pheno_data$DATE_OF_BIRTH, 
                                    pheno_data[[paste0(endpt, "_DATE")]])
    pheno_data <- tibble::add_column(pheno_data, 
                                     AGE_AT_DIAG=age_at_diag)

    return(pheno_data)
}

#' Counts the number of cases in the data 
#' 
#' Counts the number of cases in the data for a given endpoints. A case
#' is considered to be an individual with a one in the respective endpoint 
#' column. This does not consider things like the observation period from
#' the study setup. This should be handled prior using functions 
#' `add_study_interval_cols`, and `adj_case_cntrl_status`.
#' 
#' @inheritParams downsample_cntrls
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_n_cases <- function(pheno_data, 
                        endpt) {
    sum(pheno_data[[endpt]], na.rm=TRUE)
}

#' Counts the number of controls in the data 
#' 
#' Counts the number of controls in the data for a given endpoints. A 
#' control is considered to be an individual with a zero in the respective 
#' endpoint column. This does not consider things like the observation
#' period from the study setup. This should be handled prior using 
#' functions `add_study_interval_cols`, and `adj_case_cntrl_status`.
#' 
#' @inheritParams downsample_cntrls
#' 
#' @export 
#' 
#' @author Kira E. Detrois 
get_n_ctrls <- function(pheno_data, 
                        endpt) {
    sum(pheno_data[[endpt]] == 0, na.rm=TRUE)
}

#' Prints the current study setup or writes it to a file
#' 
#' Only used inside `create_test_df` function.
#' 
#' @param envir A list. The environment of the `create_test_df` function.
write_log <- function(envir) {
    if(!is.na(envir$write_log)) {
        if("print" %in% envir$write_log) {
            print_log_msg(envir)
        } 
        if("file" %in% envir$write_log) {
            write_log_file_path(envir)
        }
    } else if(get_n_cases(envir$pheno_data, envir$endpt) == 0) {
        message("Message: Careful there are no cases eligible under the current study setup.")
        message(log_msg_string(envir))
    }
}

#' Creates a string of the current study setup
#' 
#' Only used inside `write_log` function.
#' 
#' @param envir A list. The environment of the `create_test_df` function.
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
#' Only used inside `write_log` function.
#' 
#' @param envir A list. The environment of the `create_test_df` function.
print_log_msg <- function(envir) {
    if(nrow(envir$pheno_data) == 0) {
        message("Careful no eligible individuals were found.")
    } else {
        writeLines(log_msg_string(envir))
    }
}

#' Prints the current study setup or writes it to a file
#' 
#' Only used inside `create_test_df` function.
#' 
#' @param envir A list. The environment of the `create_test_df` function.
write_log_file_path <- function(envir) {
    n_cases <- get_n_cases(envir$pheno_data, envir$endpt)
    n_ctrls <- get_n_ctrls(envir$pheno_data, envir$endpt)
    
    if(is.na(envir$log_file_path)) {
        message("Variable write_log was set to `file` but no log file path was provided. Printing instead.")
        print_log_msg(envir)
    } else {
        file_dir <- sub("(.+/)*(.+)$", "\\1", envir$log_file_path)
        if(!dir.exists(file_dir)) {
            message(paste0("The log file directory ", file_dir, " does not exist. Trying to create it."))
            dir.create(file_dir, recursive=TRUE)
        }
        readr::write_file(log_msg_string(envir), envir$log_file_path)
    }
}