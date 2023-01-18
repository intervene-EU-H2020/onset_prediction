#' @export
#' 
#' @author Kira E. Detrois
get_all_data <- function(score_type,
                         endpts=NULL,
                         pheno_file_path="",
                         icd_file_path="",
                         prs_dir_path="",
                         prs_endpts_map=NULL,
                         phers_dir_path="",
                         phers_study_descr=NULL) {
    if(is.null(endpts)) {
        endpts <- get_endpts()
    }
    pheno_data <- read_pheno_file(pheno_file_path, endpts)
    icd_data <- NULL
    prs_data <- NULL
    phers_data <- NULL

    if(any(stringr::str_detect(score_type, "CCI|EI"))) {
        if(file.exists(icd_file_path)) {
            icd_data <- read_icd_file(icd_file_path)
        } else {
            stop(paste0("\nError. CCI or EI selected as predictors, selected: ",
            paste0(score_type, collapse=", "),
            "\nHowever, the icd_file_path is not provided or incorrect.\n", icd_file_path))
            stop()
        }
    }
    if(any(stringr::str_detect(score_type, "PRS"))) {
        if(dir.exists(prs_dir_path)) {
            prs_data <- read_prs_files(prs_dir_path, prs_endpts_map)
        } else {
            stop(paste0("nError. PRS selected as predictor, selected: ",
            paste0(score_type, collapse=", "),
            "\nHowever, the prs_dir_path is not provided or incorrect.\n", prs_dir_path))
        }
    }
    if(any(stringr::str_detect(score_type, "PheRS"))) {
        if(dir.exists(phers_dir_path)) {
            if(is.null(phers_study_descr)) {
                phers_study_descr <- get_phers_file_descr()
            }
            phers_data <- read_phers_files(phers_dir_path, phers_study_descr, endpts)
        } else {
            stop(paste0("Error. PheRS selected as predictor, selected: ",
            paste0(score_type, collapse=", "),
            "\nHowever, the phers_dir_path is not provided or incorrect.\n", phers_dir_path))
        }
    }
    return(list(pheno=pheno_data,
                icd=icd_data,
                prs=prs_data,
                phers=phers_data))
}