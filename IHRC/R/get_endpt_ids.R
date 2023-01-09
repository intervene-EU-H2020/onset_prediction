#' Gets the individuals that are part of each study
#' 
#' This is mostly relevant for FinnGen where we have some overalp for 
#' the PRS studies and training and testing sets. However, it could
#' also be relevant for the training and testing of the PheRS.
#' 
#' @param endpt_indvs_mat A data.frame. Contains at least column `ID`,
#'                          and the current endpt. The endpoint column
#'                          has either 1 or 0, depending on whether
#'                          the individual is part of the study for
#'                          this endpoint.
#' @inheritParams run_surv_studies
#' @param all_ids A character (vector). The ids of all individuals.
#' 
#' @export 
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