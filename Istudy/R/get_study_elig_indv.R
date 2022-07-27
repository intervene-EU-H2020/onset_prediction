#' Gets the individuals which are eligble given the study setup
#' 
#' There are two types of study setup:
#' 
#' \itemize{
#'  \item The first one considers individuals of a certain age 
#'        and a set exposure, washout, and observation periods 
#'        calcualted onwards from this age. It can simply be created
#'        by creating a S4 study object.
#'  \item The second one considers all individuals at a set time
#'        point. The observation and washout period are calcualted 
#'        backwards from this time point. The exposure period will be 
#'        different for each individual depending on their birth date. 
#'        This setup can be created, using the function 
#'        \code{\link{get_backward_study}}.
#' }
#' 
#' \itemize{
#'  \item The study setup consists of an exposure window, 
#'        a washout period, and a prediction period. See function 
#'        \code{\link{calc_study_time}}.
#'  \item Eligible individuals cannot have missing data in the
#'        column for the endpoint of interest. See function 
#'        \code{\link{filter_missing_endpt_data}}.
#'  \item Eligible individuals cannot have the
#'        selected endpoint of interest inside the endpoint free
#'        period. The endpoint free interval is the period from birth 
#'        until the prediction period begins. See function
#'        \code{\link{filter_early_endpt}}. 
#'  \item Individuals where the endpoint onset date is after the 
#'        observation has ended are considered controls in this setup. 
#' }
#' 
#' @param study@study_data A data.frame with at least the columns: 
#'                   `ID`, `SEX`, `DATE_OF_BIRTH`, `ANCESTRY`, 
#'                   `START_OF_FOLLOWUP`, `END_OF_FOLLOWUP`, 
#'                   `DATE_OF_BIRTH`, and i.e. `J10_ASTHMA`, and
#'                   `J10_ASTHMA_DATE` where the columns are the study 
#'                   endpoint and date, which will differ depending on 
#'                   the input variable `endpt`.
#' @inheritParams filter_too_old_and_young
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
#' @export
#' 
#' @author Kira E. Detrois
get_study_elig_indv <- function(study,
                                max_age=200,
                                write_res=FALSE,
                                res_dir=NULL) {
    check_cols_exist(study@study_data, study@endpt, "get_study_elig_indv")

    study@study_data <- filter_too_old_and_young(study@study_data,
                                                 study@obs_end_date, 
                                                 study@study_type,
                                                 max_age)

    study@study_data <- filter_missing_endpt_data(study@study_data, 
                                                  study@endpt)
    study@study_data <- filter_early_endpt(study@study_data, 
                                           study@endpt)
    study@study_data <- adj_case_cntrl_status(study@study_data, 
                                              study@endpt)
    study@study_data <- downsample_cntrls(study@study_data, study)
    study@study_data <- add_diag_time_cols(study@study_data, study)
    study@study_data <- filter_ancestry(study@study_data, study@ancs)
    write_res_files(study@study_data, study, write_res, res_dir)

    return(study@study_data)
}