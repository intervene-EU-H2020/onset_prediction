#' Reads the PRS files for the endpoints from a common directory
#' 
#' The files need to be named in a format `Asthma_PRS_hm3.sscore`, 
#' for example, for endpoint `J10_ASTHMA`.
#' 
#' For the PRS endpoint descriptions, see function [IUtils::get_prs_endpt_descr].
#' 
#' The PRS files should at least contain columns `IID` and `SCORE1_AVG`.
#' 
#' @param dir_path A string. The path to the PRS file directory.
#' @param prs_endpts_map A tibble. Mapping the endpoint names to the ones used
#'                          in the PRS files. If not provided uses standard 
#'                          mapping from [IUtils::get_endpts] to 
#'                          [IUtils::get_prs_endpt_descr].  
#'
#' @return A tibble with columns `ID` and a column for each selected endpoint
#'          named `endpt_PRS` i.e. `J10_ASTHMA_PRS`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
read_prs_files <- function(dir_path,
                           prs_endpts_map=NULL,
                           prs_file_end="_PRS_hm3.sscore") {
    if(is.null(prs_endpts_map)) {
        endpts <- get_endpts()
        get_prs_endpt_descr <- get_prs_endpt_descr()  
        prs_endpts_map <- tibble::tibble(endpt=endpts, 
                                         prs=get_prs_endpt_descr)
    }
    prs_data <- tibble::tibble(ID=character())

    file_names <- list.files(dir_path)
     for(file_name in file_names) {
        file_path <- paste0(dir_path, file_name)
        disease <- sub(paste0("(.*)", prs_file_end, "$"), "\\1", file_name)
        print(disease)
        if(disease %in% prs_endpts_map$prs) {
            prs_data <- add_prs_col(file_path, disease, prs_endpts_map, prs_data)
        }
    }
    return(prs_data)
}

#' Reads in PRS file and adds it as a column to the data.frame
#' 
#' @param file_path A string (string). The path to the PRS file.
#' @param disease A string (string). The PRS endpoint description.
#' @param col_map A tibble. The mapping from endpoint names to PRS
#'                  endpoint description.
#' @param prs_data A tibble. The whole PRS data for all previous endpoints.
#' 
#' @return A tibble. The `prs_data` with the added PRS results.
#' 
#' @importFrom dplyr %>% 
#' 
#' @author Kira E. Detrois
#' 
#' @export
add_prs_col <- function(file_path,
                        disease,
                        col_map,
                        prs_data) {
    prs_data <- tibble::tibble()
    tryCatch({
        crnt_prs <- readr::read_delim(file_path, 
                                      delim="\t", 
                                      show_col_types=FALSE,
                                      col_types=list(IID="c"))
        endpt <- col_map[col_map$prs == disease,]$endpt
        crnt_prs <- dplyr::rename(crnt_prs, ID=IID) %>%
                        dplyr::select(ID, SCORE1_AVG) 
        names(crnt_prs)[names(crnt_prs) == "SCORE1_AVG"] <- paste0(endpt, "_PRS")
        prs_data <- dplyr::full_join(crnt_prs, prs_data, by="ID", na_matches="na")
    }, error=function(e) {writeLines(paste0("Could not read PRS file ", file_path))})
    if(nrow(prs_data) == 0) {
        warning(writeLines(paste0("PRS file ", file_path, " is empty.")))
    }
    return(prs_data)
}