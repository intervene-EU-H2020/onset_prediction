#' Gets only the individuals from the specified ancestries
#' 
#' @inheritParams get_study_elig_indv
#' @param ancs A character (vector). The ancestries to select.
#' 
#' @return The filtered data.frame.
#' 
#' @author Kira E. Detrois
filter_ancestry <- function(pheno_data,
                            ancs="EUR") {
    if(!all(is.na(ancs))) {
        pheno_data <- dplyr::filter(pheno_data, 
                                    ANCESTRY %in% ancs)
    }
    return(pheno_data)
}