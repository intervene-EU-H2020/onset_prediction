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
read_prs_files <- function(dir_path,
                           endpts=NULL,
                           prs_endpt_descr=NULL) {
    if(is.null(endpts)) {
        endpts <- get_endpts()
    }
    if(is.null(prs_endpt_descr)) {
        prs_endpt_descr <- get_prs_endpt_descr()
    }
    col_map <- tibble::tibble(endpt=endpts, 
                              prs=prs_endpt_descr)

    prs_data <- tibble::tibble(ID=character())

    file_names <- list.files(dir_path)
    for(file_name in file_names) {
        file_path <- paste0(dir_path, "/", file_name)
        disease <- sub("(.*)(_PRS_hm3.sscore)$", "\\1", file_name)
        if(disease %in% col_map$prs) {
            prs_data <- add_prs_col(file_path, disease, col_map, prs_data)
        }
    }
    return(prs_data)
}

#' Reads in PRS file and adds it as a column to the data.frame
#' 
#' @param file_path A character (string). The path to the PRS file.
#' @param disease A character (string). The PRS endpoint description.
#' @param col_map A tibble. The mapping from endpoint names to PRS
#'                  endpoint description.
#' @param prs_data A tibble. The whole PRS data for all previous endpoints.
#' 
#' @return A tibble. The `prs_data` with the added PRS results.
#' 
#' @author Kira E. Detrois
#' 
#' @export
add_prs_col <- function(file_path,
                        disease,
                        col_map,
                        prs_data) {
    crnt_prs <- vroom::vroom(file_path, delim="\t", show_col_types = FALSE)
    endpt <- col_map[col_map$prs == disease,]$endpt
    crnt_prs <- dplyr::rename(crnt_prs, ID=IID) %>%
                    dplyr::select(ID, SCORE1_AVG)
    names(crnt_prs)[names(crnt_prs) == "SCORE1_AVG"] <- paste0(endpt, "_PRS")
    prs_data <- dplyr::full_join(crnt_prs, prs_data, by="ID", na_matches="na")
    return(prs_data)
}