#' Gets the data of individuals for a given endpoint study
#' 
#' This is mostly relevant for FinnGen where we have some overalp for 
#' the PRS studies and training and testing sets. However, it could
#' also be relevant for the training and testing of the PheRS.
#' 
#' @param endpts_indvs_mat A dataframe. Contains a column of individual 
#'                        IDs and a binary column for each endpoint.
#' @param endpt A character. The endpoint of interest, 
#'                must match a column name in `endpts_indvs_mat`
#' @param all_ids A character (vector). The IDs of all individuals.
#' 
#' @return A data.frame that contains the data for individuals that can be used
#'           for the current endpoint.
#' 
#' @export
#' 
#' @author Kira E. Detrois
get_crnt_endpt_data <- function(endpts_indvs_mat=NULL,
                                endpt,
                                pheno_data) {
    if(!is.null(endpts_indvs_mat)) {
        if(sum(endpts_indvs_mat[,endpt]) < nrow(endpts_indvs_mat)) {
            endpt_ids <- dplyr::pull(endpts_indvs_mat[dplyr::pull(endpts_indvs_mat, endpt),], ID)
            pheno_data <- pheno_data[pheno_data$ID %in% endpt_ids, ]
        }
    }
    return(pheno_data)
}
