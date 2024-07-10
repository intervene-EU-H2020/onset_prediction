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
                                 endpt,
                                 transfer=FALSE) {
    if(!transfer) {
        phers_col_name <- paste0(endpt, "_PheRS")
        train_col_name <- paste0(endpt, "_TRAIN_STATUS")
        if(phers_col_name %in% colnames(score_data)) {
            score_data <- dplyr::select(.data=score_data, 
                                        ID, 
                                        {{ phers_col_name }},
                                        {{ train_col_name }}) %>% 
                            dplyr::rename("PheRS" = {{ phers_col_name }}, "TRAIN_STATUS" = {{ train_col_name }})
        } else {
            score_data <- NULL
        }
    } else {
        phers_col_name <- paste0(endpt, "_PheRS_transfer")
        if(phers_col_name %in% colnames(score_data)) {
            score_data <- dplyr::select(.data=score_data, 
                                        ID, 
                                        {{ phers_col_name }}) %>% 
                            dplyr::rename("PheRS_transfer" = {{ phers_col_name }})
        } else {
            score_data <- NULL
        }
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
    writeLines(paste0("Calculating CCIs"))
    pheno_data <- Istudy::set_study_dates(pheno_data, study_setup)
    cci_data <- ICCI::calc_cci(icd_data,
                               exp_start=calc_exp_start_age(pheno_data),
                               exp_end=calc_exp_end_age(pheno_data),
                               score_type=ifelse(score_type == "CCI", "charlson", "elixhauser"))
    return(cci_data)
}
