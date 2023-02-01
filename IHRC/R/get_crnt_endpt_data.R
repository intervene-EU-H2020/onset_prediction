#' Gets the data of individuals for a given endpoint study
#' 
#' This is mostly relevant for FinnGen where we have some overalp for 
#' the PRS studies and training and testing sets. However, it could
#' also be relevant for the training and testing of the PheRS.
#' 
#' @param endpts_indvs_mat A data.frame. Contains a column of individual 
#'                          IDs and a binary column for each endpoint.
#' @param endpt A string. The endpoint of interest, 
#'                must match a column name in `endpts_indvs_mat`.
#' @param pheno_data A data.frame. The phenotype data. Needs at least column `ID`.
#' @param error_file A string. The path to the error file. 
#' 
#' @return A data.frame that contains the data for individuals that can be used
#'           for the current endpoint.
#' 
#' @export
#' 
#' @author Kira E. Detrois
get_crnt_endpt_data <- function(endpts_indvs_mat=NULL,
                                endpt,
                                pheno_data,
                                error_file=NULL) {
    if(!is.null(endpts_indvs_mat)) {
        # Checking endpoint column exists
        if(endpt %in% colnames(endpts_indvs_mat)) {
            # Checking if there is a need to select individuals, i.e. not all individuals are selected
            if(sum(endpts_indvs_mat[,endpt]) < nrow(endpts_indvs_mat)) {
                endpt_ids <- endpts_indvs_mat[dplyr::pull(endpts_indvs_mat, endpt),]$ID
                pheno_data <- pheno_data[pheno_data$ID %in% endpt_ids, ]
            }
        } else {
            write_to_error_file(error_file, msg=paste0("Warning. Endpoint: ", endpt, " was not found in the endpts_indvs_mat.\n"))
        }
    }
    return(pheno_data)
}
