create_endpt_indvs_mat <- function(pheno_data,
                                   endpts,
                                   ver) {
  pheno_data <- add_cohort_chip_info(pheno_data, ver)
  endpt_indvs_mat <- tibble::tibble(ID = pheno_data$ID)
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
    endpt_indvs_mat[,paste0(endpt)] <- bin_vec
  }
  return(endpt_indvs_mat)
}
 
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
  cohort_chip_info <- fread(file_name, sep="\t")
  cohort_chip_info <- dplyr::select(cohort_chip_info, 
                                    FINNGENID, chip, cohort)
  cohort_chip_info <- dplyr::rename(cohort_chip_info, ID=FINNGENID)
  cohort_chip_info <- dplyr::rename(cohort_chip_info, CHIP=chip)
  cohort_chip_info <- dplyr::rename(cohort_chip_info, COHORT=cohort)
  pheno_data <- dplyr::left_join(pheno_data, cohort_chip_info, 
                                 by="ID", na_matches="na")
  
  ## ADDING R3 COHORT INFORMATION
  R8_ids_with_R3_info <- readr::read_delim("/finngen/red/detrois/onset_pred/data/R8_ids_with_R3_info.tsv",
                                           delim="\t")
  pheno_data <- dplyr::left_join(pheno_data, 
                                 R8_ids_with_R3_info, 
                                 by="ID", 
                                 na_matches="na")
  return(pheno_data)
}
