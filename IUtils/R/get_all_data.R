#' Reads in all the data files
#' 
#' @param score_type A string (vector). The score types in the analysis.
#'                   Available options include:
#'                   \itemize{
#'                      \item `CCI` for Charlson comorbidity index, then the
#'                              `icd_file_path` should be provided.
#'                      \item `EI` for Elixhauser index, then the
#'                              `icd_file_path` should be provided.
#'                      \item `PRS` for polygenic risk score, then the
#'                              `prs_file_path` should be provided.
#'                      \item `PheRS` for PheRS results score, then the
#'                              `phers_file_path` should be provided.
#'                      \item `MED` for medication data, then the
#'                              `atc_file_path` should be provided.
#'                      \item `EDU` for educational level. The column for this needs to 
#'                                  be called `EDU` in the `pheno_data`.
#'                      \item `ZIP` for zip scores, then the
#'                              `zip_file_path` should be provided.
#'                  }
#' @param endpts A string (vector). The endpoints of interest.
#' @param pheno_file_path A string. Path to the phenotype data. Needs to be provided.
#'                          The file needs at least columns as described 
#'                          in [IHRC::get_relevant_pheno_data_cols].
#' @param icd_file_path A string. Path to the ICD data. Needs at least the 
#'                           columns as described in [IHRC::get_study_cci_data].
#' @param atc_file_path A string. Path to the ATC data. The file needs at least the 
#'                           columns as described in [IHRC::get_study_med_data].
#' @param prs_dir_path A string. Path to the directory containing the PRS files.
#'                       The file needs at least the columns as described in 
#'                       [IHRC::get_prs_endpt_data].
#' @param prs_endpts_map A tibble. Mapping the endpoint names to the ones used
#'                          in the PRS files. If not provided uses standard 
#'                          mapping from [IUtils::get_endpts] to 
#'                          [IUtils::get_prs_endpt_descr].  
#' @param phers_dir_path A string. Path to the directory containing the PRS files.
#'                       The file needs at least the columns as described in 
#'                       [IHRC::get_prs_endpt_data].
#' @param phers_study_descr A string. The study description in the PheRS files. If not 
#'                      provided uses standard call to [IUtils::get_phers_file_descr].
#' @param zip_dir_path A string. Path to the ZIP score data. The file needs at least 
#'                       the columns as described in [IHRC::get_zip_data].
#'  
#' @return A list all the data. At least `pheno` will contain data.
#'          All others will contain data if in the `score_type` list(`pheno`,
#'          `icd`, `atc`, `prs`, `phers`, `zip`)
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_all_data <- function(score_type,
                         endpts=NULL,
                         pheno_file_path="",
                         icd_file_path="",
                         atc_file_path="",
                         prs_dir_path="",
                         prs_endpts_map=NULL,
                         phers_dir_path="",
                         phers_study_descr=NULL,
                         zip_dir_path="",
<<<<<<< HEAD
                         prs_file_end=".sscore",
                         prs_id_col_name="#IID",
                         prs_score_col_name="SCORE1_AVG") {
=======
                         prs_file_end) {
>>>>>>> 1c65450bc0672f1bf58fdb82a5f41f123c6e9bfe
    if(is.null(endpts)) {
        endpts <- get_endpts()
    }
    pheno_data <- read_pheno_file(pheno_file_path, endpts)
    icd_data <- NULL
    prs_data <- NULL
    phers_data <- NULL
    zip_data <- NULL
    atc_data <- NULL

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
    if(any(stringr::str_detect(score_type, "MED"))) {
        if(file.exists(atc_file_path)) {
            atc_data <- read_atc_file(atc_file_path)
        } else {
            stop(paste0("\nError. MED selected as predictors, selected: ",
            paste0(score_type, collapse=", "),
            "\nHowever, the atc_file_path is not provided or incorrect.\n", atc_file_path))
            stop()
        }
    }
    if(any(stringr::str_detect(score_type, "PRS"))) {
        if(dir.exists(prs_dir_path)) {
<<<<<<< HEAD
            prs_data <- read_prs_files(dir_path=prs_dir_path, 
                                       prs_endpts_map=prs_endpts_map, 
                                       prs_file_end=prs_file_end, 
                                       prs_score_col_name=prs_score_col_name, 
                                       prs_id_col_name=prs_id_col_name)
=======
            prs_data <- read_prs_files(prs_dir_path, prs_endpts_map, prs_file_end)
>>>>>>> 1c65450bc0672f1bf58fdb82a5f41f123c6e9bfe
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
    if(any(stringr::str_detect(score_type, "ZIP_prob"))) {
        if(dir.exists(zip_dir_path)) {
            zip_data <- read_zip_files(zip_dir_path, endpts)
        } else {
            stop(paste0("Error. ZIP selected as predictor, selected: ",
            paste0(score_type, collapse=", "),
            "\nHowever, the zip_dir_path is not provided or incorrect.\n", zip_dir_path))
        }
    }
    return(list(pheno=pheno_data,
                icd=icd_data,
                atc=atc_data,
                prs=prs_data,
                phers=phers_data,
                zip=zip_data))
}