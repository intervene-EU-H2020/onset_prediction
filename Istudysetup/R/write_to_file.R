write_to_file <- function(pheno_data, 
                          file_dir=paste0(getwd(), "/results/"),
                          file_name="eligible_indv") {
    if(!dir.exists(file_dir))
        dir.create(file_dir)
    readr::write_delim(pheno_data, 
                       paste0(file_dir, file_name, ".tsv"),
                       delim="\t")
    return()
}