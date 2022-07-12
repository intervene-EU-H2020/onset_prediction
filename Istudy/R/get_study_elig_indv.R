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
#' @param study An S4 class representing the study setup.
#' @param write_res A boolean. Defines whether to write the results to
#'                  a file or not. Default is FALSE.
#' @param res_dir A character. The directory to write the results to.
#' 
#' @return A tibble with the information for the eligible individuals 
#'         with columns: `ID`, `SEX`, `DATE_OF_BIRTH`, 
#'         `START_OF_FOLLOWUP`, `END_OF_FOLLOWUP`, `ANCESTRY`, 
#'         and i.e. `J10_ASTHMA`, and `J10_ASTHMA_DATE.
#'         where the last two columns are the study endpoint and date, 
#'         which will differ depending on the input variable `endpt`.
#' 
#' @importFrom lubridate %--%
#' @importFrom lubridate %within%
#' @export
#' 
#' @author Kira E. Detrois
get_study_elig_indv <- function(pheno_data,
                                study,
                                max_age=90,
                                write_res=FALSE,
                                res_dir=NULL) {

    check_cols_exist(pheno_data, study@endpt, "get_study_elig_indv")
    pheno_data <- add_study_interval_cols(pheno_data, study)
    pheno_data <- filter_too_old_and_young(pheno_data, max_age)
    pheno_data <- filter_missing_endpt_data(pheno_data, study@endpt)
    pheno_data <- filter_early_endpt(pheno_data, study@endpt)
    pheno_data <- adj_case_cntrl_status(pheno_data, study@endpt)
    if(!is.na(study@downsample_fctr)) {
        pheno_data <- downsample_cntrls(pheno_data, study)
    }
    pheno_data <- add_diag_time_cols(pheno_data, study)
    pheno_data <- filter_ancestry(pheno_data, study@ancs)
    elig_indv <- create_return_tib(pheno_data, study@endpt)
    write_res_files(elig_indv, study, write_res, res_dir)

    return(elig_indv)
}

