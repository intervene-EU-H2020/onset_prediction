#' Maps ISCED 2011 codes to age modes
#' 
#' Maps the ISCED 2011 standard codes to the age modes better suited for the 
#' downstream analysis by joining with a provided mapping data and renaming 
#' the variable. Returns a single variable data frame with ID and the mapped 
#' education level average ages.
#' 
#' The average ages are based on FinnGen R10. For more details see:
#' `/finngen/red/detrois/scoio_processed/` in the SES Sandbox.
#'
#' @param study_data A tibble containing a column `EDU` 
#' 
#' @return A tibble containing columns `ID` and `EDU`
#' 
#' @references The International Standard Classification of Education (ISCED) 2011 
#'              is a framework developed by UNESCO for classifying and comparing education 
#'              levels and programmes. For more information see: 
#'              [uis.onesco.org](https://uis.unesco.org/en/topic/international-standard-classification-education-isced)
#' @examples
#' 
#' study_data <- tibble::tibble(ID = 1:7, 
#'                              EDU = c("1","2","3","4","5","6","7"))
#' get_study_edu_data(study_data)
#' 
#' @param study_data A data frame containing columns `ID` and `EDU`
#' 
#' @importFrom dplyr %>% 
#' 
#' @author Kira E. Detrois
#' 
#' @export
get_study_edu_data <- function(study_data) {
    # Read in ISCED 2011 mapping
    file_path <- system.file("extdata", "finngen_age_modes.tsv", package = "IHRC")
    isced_map <- readr::read_delim(file_path, delim="\t", show_col_types=FALSE)
    class(isced_map$ISCED_2011) <- class(study_data$ISCED_2011)

    # Add age mode based on Finngen R10 to the data
    edu_data <- dplyr::left_join(study_data, 
                                 isced_map,
                                 by=c("EDU"="ISCED_2011"))
    edu_data <- dplyr::rename(edu_data, EDU=FIN_AGE_MODE) %>% 
                    dplyr::select(ID, EDU)
    return(edu_data)
}