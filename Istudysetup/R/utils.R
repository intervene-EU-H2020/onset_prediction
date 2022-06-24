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
add_age_at_diag_col <- function(pheno_data,
                                endpt) {
    check_cols_exist(pheno_data, 
                     c("DATE_OF_BIRTH", paste0(endpt, "_DATE")),
                     "add_age_at_diag_col")
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
#' @param pheno_data A data.frame with at least the column defined in `endpt`.
#' @inheritParams get_study_elig_indv
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
#' @inheritParams get_n_cases
#' 
#' @export 
#' 
#' @author Kira E. Detrois 
get_n_cntrls <- function(pheno_data, 
                        endpt) {
    sum(pheno_data[[endpt]] == 0, na.rm=TRUE)
}

