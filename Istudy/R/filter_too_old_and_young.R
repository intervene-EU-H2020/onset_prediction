#' Filters out individuals that are too old or young for the study
#' 
#' This is mainly needed for studies that consider variable exposure lengths for 
#' individuals selected at a given time point. 
#' Here the exposure length will be the time from birth until the time point
#' considered and should be provided in the S4 study object in slot `EXP_END`
#' in the same order as the individuals in the data.frame. A study setup like
#' this can be created using the function \code{\link{get_backward_study}}.
#' 
#' It might be that the birth of an individual is after the selected time point
#' so their exposure lengths are negative. These individuals are always removed.
#' Additionally, individuals are filtered out based on some maximum exposure 
#' length which can be set using the input variable `max_age`.
#' 
#' @param pheno_data A data.frame with at least column `EXP_END`.
#' @param max_age A numeric. The maximum length of the exposure period.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
filter_too_old_and_young <- function(pheno_data,
                                     obs_end_date,
                                     study_type,
                                     max_age=200) {
    check_cols_exist(pheno_data, 
                     c("DATE_OF_BIRTH", "EXP_START_DATE", "OBS_END_DATE"),
                     "filter_too_old_and_young")
    # Too young
    if(study_type == "backward") {
        pheno_data <- dplyr::filter(pheno_data, 
                                    EXP_START_DATE <= obs_end_date)
    }
    # Too old
    pheno_data <- dplyr::filter(pheno_data, lubridate::time_length(DATE_OF_BIRTH %--% OBS_END_DATE, "years") < max_age)

    return(pheno_data)
}