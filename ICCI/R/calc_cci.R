#' Calculates the Charlson Comorbidity Index based on ICD-codes
#'
#' Maps the ICD-codes onto the relevant conditions for the 
#' charlson-deyo comorbidity index, using the function 
#' \code{\link[comorbidity]{comorbidity}}.
#' Then calculates the scores, using the function 
#' \code{\link[comorbidity]{score}}.
#'
#' Can handle different ICD-versions also for a single patient by
#' adding up the results.
#'
#' @param long_data A data.frame with at least columns `ID`, and 
#'                  `PRIMARY_ICD`.
#' @param exp_start A numeric. Start of the exposure period. Can be 
#'                  used to restrict the timeframe on which the index 
#'                  should be calculated. In this case the data.frame
#'                  needs column `Event_age`.
#' @param exp_end A numeric. End of the exposure period.
#'                   Can be 
#'                  used to restrict the timeframe on which the index 
#'                  should be calculated. In this case the data.frame
#'                  needs column `Event_age`.
#' 
#' @return A tibble with columns `ID` and `score`.
#'         Contains the charlson weighted comorbidity scores for each 
#'         individual.
#'
#' @importFrom dplyr %>%
#' @export
#' 
#' @author Kira E. Detrois
calc_cci <- function(long_data,
                     exp_start=NA,
                     exp_end=NA) {
    long_data <- preprocess_long_data(long_data)
    group_long_data <- ILongDataUtils::group_long_data_by_icd_ver(long_data)

    # For adding up different ICD-version scores
    all_cci_scores <- tibble::tibble()
    for (icd_version in as.character(group_long_data$keys)) {
        curnt_long_data <- ILongDataUtils::get_group_long_data(group_long_data, 
                                                               icd_version)
        curnt_cci_scores <- calc_icd_ver_spec_ccis(curnt_long_data, 
                                                   icd_version)
        all_cci_scores <- dplyr::bind_rows(all_cci_scores, 
                                           curnt_cci_scores)
    }
    # Adds up the scores from the same individual with different ICD-version
    # entries.
    total_cci_scores <- add_up_cci_scores(all_cci_scores)
    total_cci_scores <- add_back_orig_ids(total_cci_scores, 
                                          long_data) 
    return(dplyr::select(total_cci_scores, ID, score))
}
