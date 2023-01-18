#' Joins two matrices for selecting individuals for an endpoint
#' 
#' @param prev_endpt_indvs_mat A dataframe. Contains a column of individual 
#'                                  IDs and a binary column named by the endpoint.
#' @param crnt_endpt_indvs_mat A dataframe. Contains a column of individual 
#'                                  IDs and a binary column named i.e. "J10_ASTHMA_crnt"
#'                                  if the current endpoint is "J10_ASTHMA".
#' @param endpt A character. The current endpoint of interest.
#' @param set_nas_true A boolean. Defines whether missing individuals in either matrix
#'                      are set to TRUE or FALSE. Default: TRUE
#'  
#' @export 
#' 
#' @author Kira E. Detrois
join_endpt_indvs_mats <- function(crnt_endpt_indvs_mat,
                                  endpt,
                                  prev_endpt_indvs_mat=NULL,
                                  set_nas_true=FALSE) {
    if(!is.null(prev_endpt_indvs_mat)) {            
        endpt_indvs_mat <- dplyr::full_join(prev_endpt_indvs_mat, 
                                            crnt_endpt_indvs_mat, 
                                            by="ID",
                                            na_matches="na")

        # Setting NA values from either matrix to the selected fill
        endpt_indvs_mat[,paste0(endpt)][is.na(endpt_indvs_mat[,paste0(endpt)])] <- set_nas_true
        endpt_indvs_mat[,paste0(endpt, "_crnt")][is.na(endpt_indvs_mat[,paste0(endpt, "_crnt")])] <- set_nas_true

        # Combining booleans
        endpt_indvs_mat[,paste0(endpt)] <- endpt_indvs_mat[,paste0(endpt, "_crnt")] & endpt_indvs_mat[,paste0(endpt)]
    } else {
        endpt_indvs_mat <- crnt_endpt_indvs_mat
        endpt_indvs_mat[,paste0(endpt)] <- as.logical(dplyr::pull(endpt_indvs_mat, paste0(endpt, "_crnt")))
    }
    endpt_indvs_mat <- dplyr::select(endpt_indvs_mat, ID, all_of(endpt)) # endpt is alway only one element
    return(endpt_indvs_mat)
}