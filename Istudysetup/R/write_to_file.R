#' Writes results to a tab-delim file
#' 
#' @param final_data The data.frame to be saved.
#' @param file_dir A character. Directory path where file should be
#'                              written to.
#' @param file_name A character. File name, without file-type ending.
#'                               so do not add i.e. ".tsv" to the name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
write_to_file <- function(final_data, 
                          file_dir=paste0(getwd(), "/results/"),
                          file_name="eligible_indv") {
    if(!dir.exists(file_dir))
        dir.create(file_dir)
    readr::write_delim(final_data, 
                       paste0(file_dir, file_name, ".tsv"),
                       delim="\t")
    return()
}