#' Reads in the ATC file in FinnGen
#' 
#' Combines the ATC codes with the weights defined in `inst/extdata/ATC_weights.txt`
#' 
#' @param atc_file_path A string (string). The path to the file.
#' 
#' @return A tibble the ATC data with columns `ID`, `ATC`, `WEIGHT`.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
read_atc_file <- function(atc_file_path) {
    atc_data <- data.table::fread(atc_file_path, sep="\t")
    atc_data <- dplyr::filter(atc_data, SOURCE == "PURCH")
    atc_data <- dplyr::select(atc_data, FINNGENID, EVENT_AGE, CODE1)
    atc_data <- dplyr::rename(atc_data, ID=FINNGENID)
    atc_data <- dplyr::rename(atc_data, Event_age=EVENT_AGE)
    atc_data <- dplyr::rename(atc_data, ATC=CODE1)

    file_path <- system.file("extdata", "ATC_weights.txt", package = "IUtils")
    weights <- readr::read_delim(file_path, delim="\t", show_col_types=FALSE)
    atc_data <- dplyr::left_join(atc_data, weights, by="ATC")

    return(atc_data)
}