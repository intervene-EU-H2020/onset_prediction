#' Reads the PheRS files for the endpoints from a common directory
#' 
#' @param dir_path A string (string). The path to the directory.
#' @param study_descr A string. The study description in the PheRS files. If not 
#'                      provided uses standard call to [IUtils::get_phers_file_descr].
#' @param endpts A string (vector). The endpoint names. If not provided
#'                  uses default endpoints from function [IUtils::get_endpts()].
#'
#' @return A tibble with columns `ID` and a column for each selected endpoint
#'          named `endpt_PRS` i.e. `J10_ASTHMA_PRS`.
#' 
#' @importFrom dplyr %>% 
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
            phers_probs <- readr::read_delim(file_name_phers, 
                                             delim="\t",
                                             show_col_types=FALSE,
                                             col_types=list(`#ID`="c"))
            # Renaming
            phers_probs <- dplyr::rename(phers_probs, ID = `#ID`) %>%
                            dplyr::select(ID, pred_class1_prob)
            names(phers_probs)[names(phers_probs) == "pred_class1_prob"] <- paste0(endpt, "_PheRS")
            # Joining
            phers_data <- dplyr::full_join(phers_probs, 
                                           phers_data, 
                                           by="ID", 
                                           na_matches="na")
        } else {
            stop(paste0("Directory for PheRS files ", file_path, " does not exist."))
        }
    }
    return(phers_data)
}
