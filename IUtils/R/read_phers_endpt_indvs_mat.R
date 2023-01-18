#' Title
#' 
#' @param dir_path A character. The path to the PheRS file directory.
#' @param indvs_ids A character (vector). All individuals IDs. This is useful
#'                   if the endpoint selection information is not known for all
#'                   all individuals. 
#' @param set_nas_true A boolean. Defines whether missing individuals in either matrix
#'                      are set to TRUE or FALSE. Default: TRUE.
#' @param endpts A character (vector). The endpoints of interest.
#' @param prev_endpt_indvs_mat A dataframe. Contains a column of individual 
#'                                  IDs and a binary column named by the endpoint.
#'   
#' @export 
#' 
#' @author Kira E. Detrois
read_phers_endpt_indvs_mat <- function(dir_path,
                                       indvs_ids=character(),
                                       set_nas_true=TRUE,
                                       endpts=NULL,
                                       study_descr=NULL,
                                       indv_status="test",
                                       prev_endpt_indvs_mat=NULL) {
    if(is.null(study_descr)) {
        study_descr <- get_phers_file_descr()
    }
    if(is.null(endpts)) {
        endpts <- get_endpts()
    }
    endpt_indvs_mat <- tibble::tibble(ID=indvs_ids)
    for(endpt in endpts) {
        file_path <- paste0(dir_path, endpt, "_", study_descr, "/target-", endpt, "-PheRS-ML-input.txt.gz")
        if(file.exists(file_path)) {
            # Reading in data
            info_data <- readr::read_delim(file_path, delim="\t", show_col_types=FALSE) %>% 
                                dplyr::select(1:6) %>%
                                dplyr::rename(ID=`#ID`)
            # Initializing dataframe
            crnt_endpt_indvs_mat <- tibble::tibble(ID=info_data$ID)

            if(indv_status == "test") {
                # Selecting test individuals
                crnt_endpt_indvs_mat[,paste0(endpt, "_crnt")] <- !info_data$train_status
                # Getting only relevant info
            } else if(indv_status == "train") {
                # Selecting train individuals
                crnt_endpt_indvs_mat[,paste0(endpt, "_crnt")] <- info_data$train_status
            } else {
                stop(paste0("Unknown indv_status ", indv_status, " can be `test`, `train`."))
            }
            crnt_endpt_indvs_mat <- dplyr::select(crnt_endpt_indvs_mat, 
                                                  ID, 
                                                  all_of(paste0(endpt, "_crnt"))) # endpt is alway only one element

            # Selecting individuals only selected based on both matrices
            crnt_endpt_indvs_mat <- join_endpt_indvs_mats(
                                        crnt_endpt_indvs_mat=crnt_endpt_indvs_mat,
                                        endpt=endpt,
                                        set_nas_true=set_nas_true,
                                        prev_endpt_indvs_mat=prev_endpt_indvs_mat)
            endpt_indvs_mat <- dplyr::full_join(endpt_indvs_mat, 
                                                crnt_endpt_indvs_mat, 
                                                by="ID", 
                                                na_matches="na")
            endpt_indvs_mat[,paste0(endpt)][is.na(endpt_indvs_mat[,paste0(endpt)])] <- set_nas_true
        }
    }
    return(endpt_indvs_mat)
}