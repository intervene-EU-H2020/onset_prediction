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
        if(phers_col_name %in% colnames(score_data)) {
            score_data <- dplyr::select(.data=score_data, 
                                        ID, 
                                        {{ phers_col_name }}) %>% 
                            dplyr::rename("PheRS" = {{ phers_col_name }})
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
