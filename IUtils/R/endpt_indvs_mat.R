#' Joins two matrices for selecting individuals for an endpoint
#' 
#' @param prev_endpts_indvs_mat A dataframe. Contains a column of individual 
#'                                  IDs and a binary column named by the endpoint.
#' @param crnt_endpts_indvs_mat A dataframe. Contains a column of individual 
#'                                  IDs and a binary column named i.e. "J10_ASTHMA_crnt"
#'                                  if the current endpoint is "J10_ASTHMA".
#' @param endpt A string. The current endpoint of interest.
#' @param set_nas_true A boolean. Defines whether missing individuals in either matrix
#'                      are set to TRUE or FALSE. Default: TRUE
#'  
#' @export 
#' 
#' @author Kira E. Detrois
join_endpts_indvs_mats <- function(crnt_endpts_indvs_mat,
                                  endpt,
                                  prev_endpts_indvs_mat=NULL,
                                  set_nas_true=FALSE) {
    if(!is.null(prev_endpts_indvs_mat)) {            
        endpts_indvs_mat <- dplyr::full_join(prev_endpts_indvs_mat, 
                                             crnt_endpts_indvs_mat, 
                                             by="ID",
                                             na_matches="na")

        # Setting NA values from either matrix to the selected fill
        endpts_indvs_mat[,paste0(endpt)][is.na(endpts_indvs_mat[,paste0(endpt)])] <- set_nas_true
        endpts_indvs_mat[,paste0(endpt, "_crnt")][is.na(endpts_indvs_mat[,paste0(endpt, "_crnt")])] <- set_nas_true

        # Combining booleans
        endpts_indvs_mat[,paste0(endpt)] <- endpts_indvs_mat[,paste0(endpt, "_crnt")] & endpts_indvs_mat[,paste0(endpt)]
    } else { # Potential problem
        endpts_indvs_mat <- crnt_endpts_indvs_mat
        endpts_indvs_mat[,paste0(endpt)] <- as.logical(dplyr::pull(endpts_indvs_mat, paste0(endpt, "_crnt")))
    }
    endpts_indvs_mat <- dplyr::select(endpts_indvs_mat, ID, dplyr::all_of(endpt)) # endpt is alway only one element
    return(endpts_indvs_mat)
}

#' Reads in the test individuals for each endpoint of the PheRS data
#' 
#' Reads in all files in the form of a matrix, with columns `ID` and
#' boolean columns for each endpoint with the selected testing individuals
#' for each endpoint.
#' 
#' @param dir_path A string. The path to the PheRS file directory.
#' @param indvs_ids A string (vector). All individuals IDs. This is useful
#'                   if the endpoint selection information is not known for all
#'                   all individuals. 
#' @param set_nas_true A boolean. Defines whether missing individuals in either matrix
#'                      are set to TRUE or FALSE. 
#' @param endpts A string (vector). The endpoints of interest.
#' @param study_descr A string. The study description in the PheRS files. If not 
#'                      provided uses standard call to [IUtils::get_phers_file_descr].
#' @param prev_endpts_indvs_mat A dataframe. Contains a column of individual 
#'                                  IDs and a binary column named by the endpoint.
#'   
#' @importFrom dplyr %>% 
#' 
#' @author Kira E. Detrois
#' 
#' @export 
read_phers_endpts_indvs_mat <- function(dir_path,
                                        indvs_ids=character(),
                                        set_nas_true=TRUE,
                                        endpts=NULL,
                                        study_descr=NULL,
                                        prev_endpts_indvs_mat=NULL) {
    if(is.null(study_descr)) {
        study_descr <- get_phers_file_descr()
    }
    if(is.null(endpts)) {
        endpts <- get_endpts()
    }
    endpts_indvs_mat <- tibble::tibble(ID=indvs_ids)
    for(endpt in endpts) {
        file_path <- paste0(dir_path, endpt, "_", study_descr, "/target-", endpt, "-PheRS-ML-input.txt.gz")
        if(file.exists(file_path)) {
            # Reading in data
            info_data <- readr::read_delim(file_path, delim="\t", show_col_types=FALSE) %>% 
                                dplyr::rename(ID=`#ID`)
            # Initializing dataframe
            crnt_endpts_indvs_mat <- tibble::tibble(ID=info_data$ID)
            crnt_endpts_indvs_mat[,paste0(endpt, "_crnt")] <- !info_data$train_status
            crnt_endpts_indvs_mat <- dplyr::select(crnt_endpts_indvs_mat, 
                                                   ID, 
                                                   dplyr::all_of(paste0(endpt, "_crnt"))) # endpt is alway only one element
            # Selecting individuals only selected based on both matrices
            crnt_endpts_indvs_mat <- join_endpts_indvs_mats(
                                        crnt_endpts_indvs_mat=crnt_endpts_indvs_mat,
                                        endpt=endpt,
                                        set_nas_true=set_nas_true,
                                        prev_endpts_indvs_mat=prev_endpts_indvs_mat)
            endpts_indvs_mat <- dplyr::full_join(endpts_indvs_mat, 
                                                 crnt_endpts_indvs_mat, 
                                                 by="ID", 
                                                 na_matches="na")
            endpts_indvs_mat[,paste0(endpt)][is.na(endpts_indvs_mat[,paste0(endpt)])] <- set_nas_true
        }
    }
    return(endpts_indvs_mat)
}

#' Reads in the test individuals for each endpoint of the PRS data in FinnGen
#' 
#' These are individuals not used in training the PRS.
#' 
#' @param pheno_data A data.frame. The phenotype data. Needs at least column `ID`.
#' @param endpts A string (vector). The endpoints of interest.
#' @param ver A string. The FinnGen version. Used for function 
#'            [IUtils::add_cohort_chip_info]
#' 
#' @author Kira E. Detrois
#' 
#' @export 
read_finngen_endpts_indvs_mat <- function(pheno_data,
                                          endpts,
                                          ver) {
  pheno_data <- add_cohort_chip_info(pheno_data, ver)
  endpts_indvs_mat <- tibble::tibble(ID = pheno_data$ID)
  for(endpt in endpts) {
    if(endpt %in% c("I9_CHD", "I9_AF")) {
      bin_vec <- (pheno_data$COHORT != "THL BIOBANK COROGENE")
    } else if(endpt == "J10_ASTHMA") {
      bin_vec <- (!stringr::str_detect(pheno_data$COHORT, "FINRISK") & pheno_data$COHORT != "THL BIOBANK HEALTH 2000")
    } else if(endpt == "C3_COLORECTAL") {
      bin_vec <- !(pheno_data$CHIP %in% c("Illumina_Human670_Human610", "Illumina_Human610-Quadv1_B"))
    } else if(endpt %in% c("ILD", "C3_BRONCHUS_LUNG")) {
      bin_vec <- !pheno_data$R3
    } else {
      bin_vec <- TRUE
    }
    endpts_indvs_mat[,paste0(endpt)] <- bin_vec
  }
  return(endpts_indvs_mat)
}
 
#' Adds Cohort and Chip information for the individuals in FinnGen
#' 
#' Used for getting in the test individuals for each endpoint of the PRS data 
#' in FinnGen. These are individuals not used in training the PRS.
#' 
#' @param pheno_data A data.frame. The phenotype data. Needs at least column `ID`.
#' @param ver A string. The FinnGen version.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
add_cohort_chip_info <- function(pheno_data, 
                                 ver) {
  ## ADDING BATCH
  if(ver == "R10") {
    file_name <- "/finngen/library-red/finngen_R10/analysis_covariates/R10_COV_PHENO_V1.txt.gz"
  } else if(ver == "R8") {
    file_name <- "/finngen/library-red/finngen_R8/analysis_covariates/finngen_R8_cov_1.0.txt.gz"
  } else {
    warning(paste0("Unknown version: ", ver))
    return(NULL)
  }
  cohort_chip_info <- data.table::fread(file_name, sep="\t")
  cohort_chip_info <- dplyr::select(cohort_chip_info, 
                                    FINNGENID, chip, cohort)
  cohort_chip_info <- dplyr::rename(cohort_chip_info, ID=FINNGENID)
  cohort_chip_info <- dplyr::rename(cohort_chip_info, CHIP=chip)
  cohort_chip_info <- dplyr::rename(cohort_chip_info, COHORT=cohort)
  pheno_data <- dplyr::left_join(pheno_data, cohort_chip_info, 
                                 by="ID", na_matches="na")
  
  ## ADDING R3 COHORT INFORMATION
  R8_ids_with_R3_info <- readr::read_delim("/home/ivm/onset_pred/data/R8_ids_with_R3_info.tsv",
                                           delim="\t",
                                           show_col_types = FALSE)
  pheno_data <- dplyr::left_join(pheno_data, 
                                 R8_ids_with_R3_info, 
                                 by="ID", 
                                 na_matches="na")
  return(pheno_data)
}
