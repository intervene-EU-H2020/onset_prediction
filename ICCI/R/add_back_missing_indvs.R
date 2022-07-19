#' Adds individuals without scores, setting their CCI to 0
#' 
#' If some individuals have not diagnosis in the exposure window
#' they will get exluded from the calculation. This function adds
#' them back setting their score to 0. 
#' 
#' @param icd_data A data.frame with at least columns `ID`, the original
#'                   data.
#' @param total_cci_scores A data.frame with at least columsn `ID`, and 
#'                          `CCI_score`.
#' 
#' @return  A data.frame with all individuals 
#' 
#' @author Kira E. Detrois
add_back_missing_indvs <- function(icd_data, 
                                   total_cci_scores) {
    id_tib <- dplyr::select(icd_data, ID) %>% dplyr::distinct()
    full_scores <- dplyr::left_join(id_tib, total_cci_scores, by="ID")
    full_scores$CCI_score[is.na(full_scores$CCI_score)] <- 0
    return(full_scores)
}