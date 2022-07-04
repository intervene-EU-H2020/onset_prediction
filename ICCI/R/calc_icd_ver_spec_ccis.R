#' Calculates the Charlson Comorbidity Index for a specific 
#' ICD-version
#' 
#' Maps the ICD-codes onto the relevant conditions for the 
#' charlson-deyo comorbidity index, using the function
#' \code{\link[comorbidity]{comorbidity}}.
#' Then calculates the scores, using the function
#' \code{\link[comorbidity]{score}}.
#' 
#' Make sure that the ICD-codes in the data  all come from the same 
#' ICD-version.
#'
#' @param icd_version The ICD-version in INTERVENE format.
#'                    Can be either "10", "10CM", "9", or "9CM".
#' @inheritParams calc_cci
#'
#' @return A tibble with columns `ID` and `score`.
#'         Contains the charlson weighted comorbidity scores for each 
#'         individual.
#'
#' @importFrom dplyr %>%
#' @export
#' 
#' @author Kira E. Detrois
calc_icd_ver_spec_ccis <- function(icd_data,
                                   icd_version) {
    test_icd_ver_correct(icd_data)
    comorb_tbl <- comorbidity::comorbidity(icd_data,
                                           "ID_num",
                                           "primary_ICD",
                                           map=get_comorb_icd_ver_str(icd_version),
                                           assign0=FALSE)
    cci_scores <- comorbidity::score(comorb_tbl,
                                     weights = "charlson",
                                     assign0 = FALSE)
    cci_scores <- map_cci_scores_to_num_ids(cci_scores, icd_data$ID_num)

    return(cci_scores)
}

test_icd_ver_correct <- function(icd_data) {
    n_icd_versions <- length(unique(icd_data$ICD_version)) 
    assertthat::assert_that(n_icd_versions == 1)            
}