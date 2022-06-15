#' Gets the individuals which are eligble given the study setup
#' 
#' \itemize{
#'  \item The study setup consist of an exposure window, 
#'        a washout period, and a prediction period. See function 
#'        \code{\link{calc_study_time}}.
#'  \item Eligible individuals cannot have missing date in the
#'        column for the endpoint of interest. See function 
#'        \code{\link{filter_missing_endpt_data}}.
#'  \item Eligible individuals cannot have the
#'        selected endpoint of interest inside the endpoint free
#'        period. The endpoint free interval is the period from birth 
#'        until the prediction period begins. See function
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
#' @param endpt A string. The column name of the current endpoint of 
#'                        interest.
#' @param exp_age An integer. Age at which exposure period starts 
#'                            (in years).
#' @param exp_len An integer. Length of the exposure period
#'                               (in years).
#' @param wash_len An integer. Length of the washout period
#'                                (in years).
#' @param obs_len An integer. Length of the prediction period
#'                               (in years).
#' @param downsample_fctr A numeric. Defines how many controls there
#'                                   should be for every case.
#'                                   Default is NA, which means no
#'                                   downsampling is performed.
#' @param write_res A boolean that defines whether to write the results to
#'                  a file or not. Default is FALSE.
#' @param res_dir A character. The directory to write the results to.
#' @param write_log A character or NA. How to write the log. Can be either
#'                  `NA`: No log. `file`: Write to file, or `print`: Print
#'                  to console. `file`, and `print` can be used at the same
#'                  time.
#' @param log_dir A character. Has to be set when `write_log = file`
#'                      Complete path, including file name of the log file.
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
                                endpt,
                                exp_age=30,
                                exp_len=10,
                                wash_len=2,
                                obs_len=8,
                                downsample_fctr=NA,
                                write_res=FALSE,
                                res_dir=NA,
                                write_log=NA,
                                log_dir=NA) {
    test_length_vars_are_integers(as.list(environment()))             
    test_endpt_input_correct(as.list(environment()))

    pheno_data <- add_study_interval_cols(pheno_data,
                                          exp_age, 
                                          exp_len, 
                                          wash_len, 
                                          obs_len)

    pheno_data <- filter_missing_endpt_data(pheno_data, endpt)
    pheno_data <- filter_early_endpt(pheno_data, endpt)
    pheno_data <- adj_case_cntrl_status(pheno_data, endpt)

    if(!is.na(downsample_fctr)) {
        pheno_data <- downsample_cntrls(pheno_data, endpt)
    }

    onset_time <- calc_onset_time(pheno_data,
                                  endpt,
                                  exp_age, 
                                  exp_len, 
                                  wash_len, 
                                  obs_len)

    pheno_data[,paste0(endpt, "_AGE_DAYS")] <- onset_time$age_days
    pheno_data[,paste0(endpt, "_DATE")] <- onset_time$onset_date

    elig_data <- create_return_dt(pheno_data,
                                  endpt,
                                  exp_age, 
                                  exp_len, 
                                  wash_len, 
                                  obs_len)

    write_log(as.list(environment()))
    write_res(as.list(environment()))

    return(elig_data)
}