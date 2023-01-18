#' Renames the PRS score column to `PRS`
#' 
#' For the PRS data the score columns have names in the
#' form of `J10_ASTHMA_PRS`. The function renames the
#' current PRS column of interest to `PRS`. 
#' 
#' @inheritParams run_surv_studies
#' @inheritParams get_n_group_cases
#' 
#' @return A tibble. The renamed and filtered score data.
#' 
#' @export 
#' 
#' @importFrom dplyr %>% 
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

#' Renames the PRS score column to `PheRS`
#' 
#' For the PRS data the score columns have names in the
#' form of `J10_ASTHMA_PRS`. The function renames the
#' current PRS column of interest to `PheRS`. 
#' 
#' @inheritParams run_surv_studies
#' @inheritParams get_n_group_cases
#' 
#' @return A tibble. The renamed and filtered score data.
#' 
#' @export 
#' 
#' @importFrom dplyr %>% 
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

#' Calcualtes the CCI score on the exposure window
#' 
#' @param elig_indv A data.frame. The individuals which were eligble under
#'                        the current study setup. Needs to at least 
#'                        contain the column defined in `ID`, `DATE_OF_BIRTH`,
#'                        and `EXP_END_PERIOD`.
#' @param icd_data A data.frame with at least columns `ID`, `Event_age`, 
#'                  and `PRIMARY_ICD`.
#' 
#' @return A data.frame with columns `ID`, and `SCORE`.
#' 
#' @author Kira E. Detrois
get_study_cci_data <- function(pheno_data,
                               icd_data,
                               score_type,
                               study_setup) {
    pheno_data <- Istudy::set_study_dates(pheno_data, study_setup)
    cci_data <- ICCI::calc_cci(icd_data,
                               exp_start=calc_exp_start_age(pheno_data),
                               exp_end=calc_exp_end_age(pheno_data),
                               score_type=ifelse(score_type == "CCI", "charlson", "elixhauser"))  
    return(cci_data)
}

#' Calcualtes the MI score on the exposure window
#' 
#' @param elig_indv A data.frame. The individuals which were eligble under
#'                        the current study setup. Needs to at least 
#'                        contain the column defined in `ID`, `DATE_OF_BIRTH`,
#'                        and `EXP_END_PERIOD`.
#' @param icd_data A data.frame with at least columns `ID`, `Event_age`, 
#'                  and `PRIMARY_ICD`.
#' 
#' @return A data.frame with columns `ID`, and `SCORE`.
#' 
#' @author Kira E. Detrois
get_study_med_data <- function(elig_indv,
                               atc_data) {
    med_data <- ICCI::calc_med(atc_data,
                               exp_start=calc_exp_start_age(elig_indv),
                               exp_end=calc_exp_end_age(elig_indv)) 
    return(med_data)
}

#' Calcualtes the age of individuals at exposure start
#' 
#' Calculates the length of the exposure period in (exact) years
#' from birth until the start date of the exposure period.
#'
#' @param elig_indv A dataframe of individuals with columns 
#'                   `ID`, `DATE_OF_BIRTH`, and `EXP_START_DATE`.
#' 
#' @return tibble with columns `ID` and `EXP_START`,
#'           which is the ID of the individual and the corresponding 
#'           calculated exposure start date.
#' 
#' @export
#' 
#' @import dplyr
#' @importFrom lubridate %--%
#' 
#' @author Kira E. Detrois
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
#' @param elig_indv A dataframe of individuals with columns 
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

