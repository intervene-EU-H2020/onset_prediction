#' Reads in the phenotype file according to INTERVENE standard
#' 
#' See \href{https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit}{INTERVENE phenotype file definition v1}. 
#' 
#' @param file_path A string. The path to the file.
#' @param endpts A string (vector). The endpoints of interest.
#' 
#' @return A tibble with the longitudinal phenotype data
#' 
#' @export 
#' 
#' @author Kira E. Detrois
read_pheno_file <- function(file_path="",
                            endpts=NULL) {
    assertthat::assert_that(file_path != "", 
                            msg="Error. Phenotype file path needs to always be provided.")
    tryCatch({
            pheno_data <- readr::read_delim(file_path, 
                                            delim="\t", 
                                            col_types=list(SEX="f", DATE_OF_BIRTH="D", END_OF_FOLLOWUP="D", ANCESTRY="f", ID="c"),
                                            show_col_types = FALSE)
            }, error=function(e) {writeLines(paste0("Could not read phenotype file ", file_path))})
    if(nrow(pheno_data) == 0) {
        error(paste0("Error. Phenotype file contained no entries. Given path: ", file_path))
    }

    if(is.null(endpts)) {
        endpts <- get_endpts()
    }
    expect_cols <- c("ID", "SEX", "DATE_OF_BIRTH", paste0("PC", 1:10), "ANCESTRY", "START_OF_FOLLOWUP", "END_OF_FOLLOWUP", endpts, paste0(endpts, "_DATE"))
    check_cols(expect_cols, colnames(pheno_data), file_path)

    return(pheno_data)
}


