#' Gets the current endpoint PRS score column
#' 
#' For the PRS data the score columns have names in the
#' form of `J10_ASTHMA_PRS`. The function renames the
#' current PRS column of interest to `PRS`. 
#' 
#' @param score_data A data.frame. The PRS data. 
#'                   Contains a column of individual IDs and 
#'                   a column named according to the selected
#'                   endpoint. 
#' @param endpt A string. The current endpoint.
#' 
#' @return A tibble. The renamed and filtered score data.
#' 
#' @importFrom dplyr %>% 
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_prs_endpt_data <- function(score_data,
                               endpt) {
    
    prs_col_name <- paste0(endpt, "_PRS")
    if(prs_col_name %in% colnames(score_data)) {
        score_data <- dplyr::select(.data=score_data, 
                                    ID, 
                                    {{ prs_col_name }}) %>% 
                        dplyr::rename("PRS" = {{ prs_col_name }})
    } else {
        score_data <- NULL
    }
    return(score_data)
}

#' Gets the current endpoint PheRS score column
#' 
#' For the PheRS data the score columns have names in the
#' form of `J10_ASTHMA_PheRS`. The function renames the
#' current PheRS column of interest to `PheRS`. 
#' 
#' @param score_data A data.frame. The PheRS data. 
#'                   Contains a column of individual IDs and 
#'                   a column named according to the selected
#'                   endpoint. 
#' @param endpt A string. The current endpoint.
#' 
#' @return A tibble. The renamed and filtered score data.
#' 
#' @importFrom dplyr %>% 
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_phers_endpt_data <- function(score_data,
                                 endpt) {
    
    phers_col_name <- paste0(endpt, "_PheRS")
    if(phers_col_name %in% colnames(score_data)) {
        score_data <- dplyr::select(.data=score_data, 
                                    ID, 
                                    {{ phers_col_name }}) %>% 
                        dplyr::rename("PheRS" = {{ phers_col_name }})
    } else {
        score_data <- NULL
    }
    return(score_data)
}

#' Gets the CCI score on the exposure window from ICD data
#' 
#' @param pheno_data A data.frame. The phenotype data. Needs at least columns `ID` 
#'                   and `DATE_OF_BIRTH`.
#' @param icd_data A data.frame. The ICD diagnoses codes. Needs at least columns 
#'                 `ID`, `EVENT_AGE`, and `PRIMARY_ICD`.
#' @param score_type A string. Can be either `charlson` or `elixhauser`.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#'  
#' @return A tibble with columns `ID` and `CCI`, or `EI` depending on the 
#'         `score_type` used. Contains the charlson weighted comorbidity 
#'          scores for each individual.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_study_cci_data <- function(pheno_data,
                               icd_data,
                               score_type,
                               study_setup) {
    # Adds info to be able to calculate exposure start and end age
    pheno_data <- Istudy::set_study_dates(pheno_data, study_setup)
    cci_data <- ICCI::calc_cci(icd_data,
                               exp_start=calc_exp_start_age(pheno_data),
                               exp_end=calc_exp_end_age(pheno_data),
                               score_type=ifelse(score_type == "CCI", "charlson", "elixhauser"))  
    return(cci_data)
}

#' Gets the CCI score on the exposure window from ICD data
#' 
#' @param pheno_data A data.frame. The phenotype data. Needs at least columns `ID` 
#'                   and `DATE_OF_BIRTH`.
#' @param atc_data A data.frame. The ATC codes. Needs at least columns 
#'                 `ID`, `ATC`, and `WEIGHT`.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#'  
#' @return A tibble with columns `ID` and `MED`. Contains the weighted medication
#'          scores for each individual.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_study_med_data <- function(pheno_data,
                               study_setup,
                               atc_data) {
    # Adds info to be able to calculate exposure start and end age
    pheno_data <- Istudy::set_study_dates(pheno_data, study_setup)
    med_data <- ICCI::calc_med(atc_data,
                               exp_start=calc_exp_start_age(pheno_data),
                               exp_end=calc_exp_end_age(pheno_data)) 
    return(med_data)
}

#' Calcualtes the age of individuals at exposure start
#' 
#' Calculates the length of the exposure period in (exact) years
#' from birth until the start date of the exposure period.
#'
#' @param elig_indv A data.frame of individuals with columns 
#'                   `ID`, `DATE_OF_BIRTH`, and `EXP_START_DATE`.
#' 
#' @return tibble with columns `ID` and `EXP_START`,
#'           which is the ID of the individual and the corresponding 
#'           calculated exposure start date.
#' 
#' @import dplyr
#' @importFrom lubridate %--%
#' 
#' @author Kira E. Detrois
#' 
#' @export
calc_exp_start_age <- function(elig_indv) {
    elig_indv <- dplyr::mutate(elig_indv, 
                               EXP_START = lubridate::time_length(DATE_OF_BIRTH %--% EXP_START_DATE, "years"))
    return(dplyr::select(elig_indv, ID, EXP_START))
}

#' Calcualtes the age of individuals at exposure end
#' 
#' Calculates the length of the exposure period in years
#' from birth until the end date of the exposure period.
#'
#' @param elig_indv A data.frame of individuals with columns 
#'                   `ID`, `DATE_OF_BIRTH`, and `EXP_END_DATE`.
#' 
#' @return tibble with columns `ID` and `EXP_END`,
#'           which is the ID of the individual and the corresponding 
#'           calculated exposure end date.
#' 
#' @export
#' 
#' @import dplyr
#' @importFrom lubridate %--%
#' 
#' @author Kira E. Detrois
calc_exp_end_age <- function(elig_indv) {
    elig_indv <- dplyr::mutate(elig_indv, 
                               EXP_END = lubridate::time_length(DATE_OF_BIRTH %--% EXP_END_DATE, "years"))
    return(dplyr::select(elig_indv, ID, EXP_END))
}

#' Gets the current endpoint ZIP probability columns
#' 
#' For the ZIP data the score columns have names in the
#' form of `J10_ASTHMA_ZIPprobs`. The function renames the
#' current ZIP column of interest to `ZIP`. 
#' 
#' @param score_data A data.frame. The ZIP code probability data. 
#'                   Contains a column of individual IDs and 
#'                   a column named according to the selected
#'                   endpoint. 
#' @param endpt A string. The current endpoint.
#' 
#' @return A tibble. The renamed and filtered score data.
#' 
#' @importFrom dplyr %>% 
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_prob_data <- function(score_data,
                         endpt) {
    
    prob_col_name <- paste0(endpt, "_prob")
    if(prob_col_name %in% colnames(score_data)) {
        score_data <- dplyr::select(.data=score_data, 
                                    ID, 
                                    {{ prob_col_name }}) %>% 
                        dplyr::rename("Prob" = {{ zip_col_name }})
    } else {
        score_data <- NULL
    }
    return(score_data)
}
