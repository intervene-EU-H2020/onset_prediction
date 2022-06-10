#' A helper function to calculate the age at diagnosis.
#' 
#' @param bds A Date. The birth days of the individuals.
#' @param diag_dates A Date. The dates of diagnosis.
#' 
#' @importFrom lubridate %--%
#' @export
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
add_age_at_diag_col <- function(pheno_data, endpt) {
    test_endpt_input_correct(as.list(environment()))

    age_at_diag <- calc_age_at_diag(pheno_data$DATE_OF_BIRTH, 
                                    pheno_data[[paste0(endpt, "_DATE")]])
    pheno_data <- tibble::add_column(pheno_data, 
                                     AGE_AT_DIAG=age_at_diag)

    return(pheno_data)
}

