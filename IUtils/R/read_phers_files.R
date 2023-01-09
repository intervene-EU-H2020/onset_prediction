#' Reads the PRS files for the endpoints from a common directory
#' 
#' @param dir_path A character (string). The path to the directory.
#' @param endpts A character (vector). The endpoint names. If not provided
#'                  uses default endpoints from function [IUtils::get_endpts()].
#' @param prs_endpt_descr A character (vector). The PRS files endpoint 
#'                         descriptions. If not provided, uses default endpoints
#'                         from function [IUtils::prs_endpt_descr()].
#' 
#' The files need to be named in a format `prsendptdescr_PRS_hm3.sscore`. 
#' For example, for endpoint `J10_ASTHMA` the file would be named 
#' `Asthma_PRS_hm3.sscore`. 
#' 
#' For the PRS endpoint descriptions, see function [IUtils::get_prs_endpt_descr()].
#' 
#' It is possible to provide custom endpoints or PRS endpoint descriptions, 
#' using the parameters `endpts` and/or `prs_endpt_descr`. However, make sure
#' that the two vectors are in the same order and length. For the default
#' endpoint vector see [IUtils::get_endpts()].
#' 
#' The PRS files should at least contain columns `IID` and `SCORE1_AVG`.
#'
#' @return A tibble with columns `ID` and a column for each selected endpoint
#'          named `endpt_PRS` i.e. `J10_ASTHMA_PRS`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
read_phers_files <- function(dir_path,
                             study_descr=NULL,
                             endpts=NULL) {
    if(is.null(study_descr)) {
        study_descr <- get_phers_file_descr()
    }
    if(is.null(endpts)) {
        endpts <- get_endpts()
    }
    phers_data <- tibble::tibble(ID=character())
    for(endpt in endpts) {
        file_path <- paste0(dir_path, endpt, "_", study_descr, "/")
        if(dir.exists(file_path)) {
            # Reading
            file_name_phers <- paste0(file_path, "pred_probas.txt.gz")
            phers_probs <- vroom::vroom(file_name_phers, delim="\t",show_col_types = FALSE)
            # Renaming
            phers_probs <- dplyr::rename(phers_probs, ID = `#ID`) %>%
                            dplyr::select(ID, pred_class1_prob)
            names(phers_probs)[names(phers_probs) == "pred_class1_prob"] <- paste0(endpt, "_PheRS")
            # Joining
            phers_data <- dplyr::full_join(phers_probs, 
                                           phers_data, 
                                           by="ID", 
                                           na_matches="na")
        }
    }
    return(phers_data)
}

