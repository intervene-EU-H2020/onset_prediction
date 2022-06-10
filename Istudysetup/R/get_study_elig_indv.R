#' Gets the individuals which are eligble given the study setup
#' 
#' \itemize{
#'  \item The study setup consist of an exposure window, 
#'        a washout period, and a observation period. See function 
#'        \code{\link{calc_study_time}}.
#'  \item Eligible individuals must have a follow-up period that
#'        covers the whole study time interval. See function
#'        \code{\link{filter_too_short_followup}}.
#'  \item Eligible individuals cannot have missing date in the
#'        column for the endpoint of interest. See function 
#'        \code{\link{filter_missing_endpt_data}}.
#'  \item Eligible individuals cannot have the
#'        selected endpoint of interest inside the endpoint free
#'        period. The endpoint free interval is the period from birth 
#'        until the observation period begins. See function
#'        \code{\link{filter_early_endpt}}. 
#'  
#' }
#' 
#' @param pheno_data A data.frame with at least the columns: 
#'                   `ID`, `SEX`, `DATE_OF_BIRTH`, `ANCESTRY`, 
#'                   `START_OF_FOLLOWUP`, `END_OF_FOLLOWUP`, 
#'                   `DATE_OF_BIRTH`, and i.e. `J10_ASTHMA`, and
#'                   `J10_ASTHMA_DATE` where the columns are the study 
#'                   endpoint and date, which will differ depending on 
#'                   the input variable `endpt`.
#' @param exp_age An integer. Age at which exposure period starts 
#'                            (in years).
#' @param exp_length An integer. Length of the exposure period
#'                               (in years).
#' @param wash_length An integer. Length of the washout period
#'                                (in years).
#' @param out_length An integer. Length of the observation period
#'                               (in years).
#' @param endpt A string. The column name of the study endpoint.
#' 
#' @return A tibble with the information for the eligible individuals 
#'         with columns: `ID`, `SEX`, `DATE_OF_BIRTH`, 
#'         `START_OF_FOLLOWUP`, `END_OF_FOLLOWUP`, `ANCESTRY`, 
#'         and i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE.
#'         where the last two columns are the study endpoint and date, 
#'         which will differ depending on the input variable `endpt`.
#' 
#' @importFrom lubridate %m+%
#' @importFrom lubridate %--%
#' @importFrom lubridate %within%
#' @export
#' 
#' @author Kira E. Detrois
get_study_elig_indv <- function(pheno_data,
                                exp_age=30,
                                exp_length=10,
                                wash_length=2,
                                out_length=8,
                                endpt="J10_ASTHMA",
                                downsample_fctr=NA) {
    test_length_vars_are_integers(as.list(environment()))             
    test_endpt_input_correct(as.list(environment()))

    pheno_data <- add_study_interval_cols(pheno_data,
                                          exp_age, 
                                          exp_length, 
                                          wash_length, 
                                          out_length)

    pheno_data <- filter_missing_endpt_data(pheno_data, endpt)
    pheno_data <- filter_early_endpt(pheno_data, endpt)
    pheno_data <- adj_case_cntrl_status(pheno_data, endpt)

    if(!is.na(downsample_fctr)) {
        pheno_data <- downsample_cntrls(pheno_data, endpt)
    }
    elig_data <- create_return_dt(pheno_data)

    return(elig_data)
}

create_return_dt <- function(pheno_data,
                             exp_age=30,
                             exp_length=10,
                             wash_length=2,
                             out_length=8,
                             endpt="J10_ASTHMA") {
    test_endpt_input_correct(as.list(environment()))
    test_length_vars_are_integers(as.list(environment()))
    
    elig_data <- dplyr::select(pheno_data, 
                               ID, 
                               SEX, 
                               DATE_OF_BIRTH, 
                               START_OF_FOLLOWUP,
                               END_OF_FOLLOWUP, 
                               ANCESTRY, 
                               # Otherwise dplyr will throw error. 
                               # test_endpt_correct already checks that
                               # this is only a single string and not
                               # a vector
                               all_of(endpt), 
                               paste0(endpt, "_DATE"))
    elig_data <- list(data=elig_data, 
                      exp_age=exp_age,
                      exp_length=exp_length,
                      wash_length=wash_length,
                      out_length=out_length)
}

look_at_cases <- function(pheno_data, endpt, exp_age, show_cols=NA) {
    case_data_1 = dplyr::filter(pheno_data, get(endpt) == 1)
    case_data_1 <- add_age_at_diag_col(case_data_1, endpt)
    case_data_1 <- tibble::add_column(case_data_1, DATE_EXP=calc_exp_start_date(case_data_1$DATE_OF_BIRTH, exp_age))
    if(is.na(show_cols))
        print(dplyr::select(case_data_1, DATE_OF_BIRTH, DATE_EXP, paste0(endpt, "_DATE"), AGE_AT_DIAG), width=500)
    else {
        print(dplyr::select(case_data_1, ID, DATE_OF_BIRTH, DATE_EXP, FOLLOWUP, paste0(endpt, "_DATE"), AGE_AT_DIAG, show_cols), width=500)
    }
}



