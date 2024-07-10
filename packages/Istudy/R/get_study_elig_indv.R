#' Gets the individuals which are eligble given the study setup
#' 
#' There are two types of study setup:
#' 
#' \itemize{
#'  \item `forward` considers individuals of a certain age.
#'        It calculates the exposure, washout, and observation period
#'        onwards from this age.
#'  \item `backward` considers all individuals at a set time
#'        point. The observation and washout period are calcualted 
#'        backwards from this time point.
#' }
#' 
#' \itemize{
#'  \item The study setup consists of an exposure period, 
#'        a washout period, and a observation period.
#'  \item Eligible individuals cannot have missing data in the
#'        column for the endpoint of interest. See function 
#'        \code{\link{filter_missing_endpt_data}}.
#'  \item Eligible individuals cannot have the endpoint of interest 
#'        inside the endpoint free period. The endpoint free interval 
#'        ranges from birth until the observation period begins. 
#'        See function \code{\link{filter_early_endpt}}. 
#'  \item Individuals where the endpoint diagnosis date is after the 
#'        study has ended, are considered controls. 
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
                                study_type=study@study_setup@study_type,
                                obs_age_range=study@study_setup@obs_age_range,
                                exp_f1998=study@study_setup@exp_f1998)
    study@study_data <- filter_missing_endpt_data(
                                study_data=study@study_data, 
                                endpt=study@endpt)
    study@study_data <- filter_early_endpt(
                                study_data=study@study_data, 
                                endpt=study@endpt)
    study@study_data <- adj_case_cntrl_status(
                                study_data=study@study_data, 
                                endpt=study@endpt)
    study@study_data <- filter_ancestry(study@study_data, 
                                        study@study_setup@ancs)
    study@study_data <- downsample_cntrls(
                                study_data=study@study_data,
                                endpt=study@endpt,
                                down_fctr=study@study_setup@down_fctr)
    study@study_data <- complete_endpt_date_info(
                                study_data=study@study_data,
                                endpt=study@endpt)
    study@study_data <- add_age_event_cols(study_data=study@study_data,
                                           endpt=study@endpt)
    return(study@study_data)
}