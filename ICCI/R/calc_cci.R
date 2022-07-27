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
#' @param icd_data A data.frame with at least columns `ID`, and 
#'                  `PRIMARY_ICD`.
#' @param exp_start A numeric. Start of the exposure period. Can be 
#'                  used to restrict the timeframe on which the index 
#'                  should be calculated. In this case the data.frame
#'                  needs column `Event_age`. 
#'                  In case it is a vector has to have the length of
#'                  the number of rows in `icd_data`.
#' @param exp_end A numeric. End of the exposure period.
#'                  Can be used to restrict the timeframe on which the index 
#'                  should be calculated. In this case the data.frame
#'                  needs column `Event_age`.
#'                  In case it is a vector has to have the length of
#'                  the number of rows in `icd_data`.
#' 
#' @return A tibble with columns `ID` and `score`.
#'         Contains the charlson weighted comorbidity scores for each 
#'         individual.
#'
#' @importFrom dplyr %>%
#' @export
#' 
#' @author Kira E. Detrois
calc_cci <- function(icd_data,
                     exp_start=NULL,
                     exp_end=NULL) {
    process_icd_data <- preprocess_icd_data(icd_data, exp_start, exp_end)
    if(nrow(process_icd_data) > 0) {
        group_icd_data <- IUtils::group_icd_data_by_ver(process_icd_data)

        # For adding up different ICD-version scores
        all_cci_scores <- tibble::tibble()
        for (icd_version in as.character(group_icd_data$keys)) {
            curnt_icd_data <- IUtils::get_group_icd_data(group_icd_data, 
                                                                icd_version)
            curnt_cci_scores <- calc_icd_ver_spec_ccis(curnt_icd_data, 
                                                    icd_version)
            all_cci_scores <- dplyr::bind_rows(all_cci_scores, 
                                            curnt_cci_scores)
        }
        # Adds up the scores from the same individual with different ICD-version
        # entries.
        total_cci_scores <- add_up_cci_scores(all_cci_scores)
        total_cci_scores <- add_back_orig_ids(total_cci_scores, 
                                            process_icd_data) 
        full_scores <- add_back_missing_indvs(icd_data, total_cci_scores)
    } else {
        full_scores <- tibble::add_column(icd_data, CCI_score=rep(0, nrow(icd_data)))
    }

    return(dplyr::select(full_scores, ID, CCI_score))
}
