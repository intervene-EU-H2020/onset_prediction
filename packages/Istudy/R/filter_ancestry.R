#' Gets only the individuals from the specified ancestries
#' 
#' @inheritParams adj_case_cntrl_status
#' @param ancs A character (vector). The ancestries to select.
#' 
#' @return The filtered data.frame.
#' @export 
#' 
#' @author Kira E. Detrois
filter_ancestry <- function(study_data,
                            ancs="EUR") {
    if(!all(is.na(ancs))) {
        study_data <- dplyr::filter(study_data, 
                                    ANCESTRY %in% ancs)
    }
    return(study_data)
}