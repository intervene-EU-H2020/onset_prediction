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
#' @inheritParams adj_case_cntrl_status
#' 
#' @importFrom lubridate %--%
#' @export
#' 
#' @author Kira E. Detrois
add_age_at_diag_col <- function(study_data,
                                endpt) {
    check_cols_exist(study_data, 
                     c("DATE_OF_BIRTH", paste0(endpt, "_DATE")),
                     "add_age_at_diag_col")
    age_at_diag <- calc_age_at_diag(study_data$DATE_OF_BIRTH, 
                                    study_data[[paste0(endpt, "_DATE")]])
    study_data <- tibble::add_column(study_data, 
                                     AGE_AT_DIAG=age_at_diag)

    return(study_data)
}

#' Counts the number of cases in the data 
#' 
#' Counts the number of cases in the data for a given endpoints. A case
#' is considered to be an individual with a one in the respective endpoint 
#' column. This does not consider things like the observation period from
#' the study setup. This should be handled prior using functions 
#' `add_study_interval_cols`, and `adj_case_cntrl_status`.
#' 
#' @param study_data A data.frame with at least the column defined in `endpt`.
#' @inheritParams adj_case_cntrl_status
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_n_cases <- function(study_data, 
                        endpt) {
    check_cols_exist(study_data, endpt, "get_n_cases")
    sum(study_data[[endpt]], na.rm=TRUE)
}

#' Counts the number of controls in the data 
#' 
#' Counts the number of controls in the data for a given endpoints. A 
#' control is considered to be an individual with a zero in the respective 
#' endpoint column. This does not consider things like the observation
#' period from the study setup. This should be handled prior using 
#' functions `add_study_interval_cols`, and `adj_case_cntrl_status`.
#' 
#' @inheritParams get_n_cases
#' 
#' @export 
#' 
#' @author Kira E. Detrois 
get_n_cntrls <- function(study_data, 
                         endpt) {
    check_cols_exist(study_data, endpt, "get_n_cntrls")
    sum(study_data[[endpt]] == 0, na.rm=TRUE)
}

