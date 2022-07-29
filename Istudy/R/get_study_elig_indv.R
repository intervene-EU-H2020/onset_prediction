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
#' @param study An S4 object with the current study setup.
#' 
#' @export
#' 
#' @author Kira E. Detrois
get_study_elig_indv <- function(study) {
    study@study_data <- filter_too_old_and_young(
                                study_data=study@study_data,
                                obs_end_date=study@obs_end_date, 
                                study_type=study@study_type,
                                max_age=study@max_age,
                                filter_1998=study@filter_1998)
    study@study_data <- filter_missing_endpt_data(
                                study_data=study@study_data, 
                                endpt=study@endpt)
    study@study_data <- filter_early_endpt(
                                study_data=study@study_data, 
                                endpt=study@endpt)
    study@study_data <- adj_case_cntrl_status(
                                study_data=study@study_data, 
                                endpt=study@endpt)
    study@study_data <- downsample_cntrls(
                                study_data=study@study_data,
                                endpt=study@endpt,
                                downsample_fctr=study@downsample_fctr)
    study@study_data <- add_diag_time_cols(
                                study_data=study@study_data,
                                endpt=study@endpt)
    study@study_data <- filter_ancestry(study@study_data, 
                                        study@ancs)
    write_res_files(study=study)

    return(study@study_data)
}