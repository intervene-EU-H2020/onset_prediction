
#' Reads the PheRS files for the endpoints from a common directory
#' 
#' @param dir_path A string (string). The path to the directory.
#' @param endpts A string (vector). The endpoint names. If not provided
#'                  uses default endpoints from function [IUtils::get_endpts()].
#'
#' @return A tibble with columns `ID` and a column for each selected endpoint
#'          named `endpt_PRS` i.e. `J10_ASTHMA_PRS`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
read_zip_files <- function(dir_path,
                           endpts=NULL) {
    if(is.null(endpts)) {
        endpts <- get_endpts()
    }
    zip_data <- tibble::tibble(ID=character())
    for(endpt in endpts) {
        if(dir.exists(dir_path)) {
            # Reading
            file_name_zip <- paste0(dir_path,  endpt, "_2019-01-01_o8_w2_e10_32_70.tsv")
            zip_probs <- readr::read_delim(file_name_zip, delim="\t",show_col_types = FALSE)
            zip_probs <- dplyr::mutate(zip_probs, ID=as.character(ID))
            names(zip_probs)[names(zip_probs) == "ZIP_probs"] <- paste0(endpt, "_ZIPprobs")
            names(zip_probs)[names(zip_probs) == "ZIP"] <- paste0(endpt, "_ZIP")

            # Joining
            zip_data <- dplyr::full_join(zip_probs, 
                                         zip_data, 
                                         by="ID", 
                                         na_matches="na")
        }
    }
    return(zip_data)
}