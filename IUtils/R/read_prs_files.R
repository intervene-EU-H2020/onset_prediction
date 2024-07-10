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
                           prs_endpts_names=NULL,
                           prs_file_end="_PRS_hm3.sscore",
                           prs_score_col_name="SCORE1_AVG",
                           prs_id_col_name="#IID") {
    if(is.null(prs_endpts_names)) {
        endpts <- get_endpts()
        get_prs_endpt_descr <- get_prs_endpt_descr()  
        prs_endpts_map <- tibble::tibble(endpt=endpts, 
                                         prs=get_prs_endpt_descr)
        print(prs_endpts_map)
        writeLines(paste0("This is the mapping from endpoint names to PRS endpoint descriptions. 
                            Please check that it is correct. If not, please provide also a comma separated list prs_endpts_names matching the order printed here.")) 
        
    }
    prs_data <- tibble::tibble(ID=character())

    file_names <- list.files(dir_path)
     for(file_name in file_names) {
        file_path <- paste0(dir_path, file_name)
        disease <- sub(paste0("(.*)", prs_file_end, "$"), "\\1", file_name)
        if(disease %in% prs_endpts_map$prs) {
            prs_data <- add_prs_col(file_path=file_path, 
                                    disease=disease, 
                                    col_map=prs_endpts_map, 
                                    prs_data=prs_data, 
                                    prs_score_col_name=prs_score_col_name,
                                    prs_id_col_name=prs_id_col_name)
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
                        prs_data,
                        prs_score_col_name,
                        prs_id_col_name) {

    tryCatch({
        crnt_prs <- readr::read_delim(file_path, 
                                      delim="\t", 
                                      show_col_types=FALSE)
        if(disease %in% col_map$prs) {
            endpt <- col_map[col_map$prs == disease,]$endpt
        } else if(disease %in% col_map$endpt) {
            endpt <- disease
        } else {
            stop(paste0("Could not find PRS endpoint description for ", disease, ". Please check the PRS file name."))
        }
        colnames(crnt_prs)[colnames(crnt_prs) == prs_id_col_name] <- "ID"
        colnames(crnt_prs)[colnames(crnt_prs) == prs_score_col_name] <- paste0(endpt, "_PRS")
        crnt_prs <- dplyr::select(crnt_prs, ID, paste0(endpt, "_PRS")) %>% 
                        dplyr::mutate(ID=as.character(ID))
        prs_data <- dplyr::full_join(crnt_prs, prs_data, by="ID", na_matches="na")
    }, error=function(e) {writeLines(paste0("Could not read PRS file ", file_path, "\nerror: ", e$message))})
    if(nrow(prs_data) == 0) {
        warning(writeLines(paste0("PRS data read from ", file_path, " is empty. Either the file is empty or the column names are wrong. Please change paramters `prs_score_col_name`and `prs_id_col_name` accordingly in the setup file.")))
        writeLines(paste0("Have columns: ", paste0(colnames(crnt_prs), collapse=", ")))
        writeLines(paste0("Expected columns: ", paste0(prs_id_col_name, ", ", prs_score_col_name)))
    }
    return(prs_data)
}