#' Gets the individuals for a given endpoint study
#' 
#' This is mostly relevant for FinnGen where we have some overalp for 
#' the PRS studies and training and testing sets. However, it could
#' also be relevant for the training and testing of the PheRS.
#' 
#' @param endpt_indvs_mat A dataframe. Contains a column of individual 
#'                        IDs and a binary column for each endpoint.
#' @param endpt A character. The endpoint of interest, 
#'                must match a column name in `endpt_indvs_mat`
#' @param all_ids A character (vector). The IDs of all individuals.
#' 
#' @examples
#' endpt_indvs_mat <- data.frame(ID = c(1, 2, 3), J10_ASTHMA = c(1, 0, 1))
#' get_endpt_ids(endpt_indvs_mat, "J10_ASTHMA", c(1, 2, 3))
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_endpt_ids <- function(endpt_indvs_mat,
                          endpt,
                          all_ids) {
    if(!is.null(endpt_indvs_mat)) {
        endpt_ids <- dplyr::filter(endpt_indvs_mat, get(endpt) == 1)
        if(nrow(endpt_ids) > 0) {
            endpt_ids <- dplyr::pull(endpt_ids, ID)
        } else {
            endpt_ids <- NULL
        }
    } else {
        endpt_ids <- all_ids
    }
    return(endpt_ids)
}
