#' Creates a data.frame with random ICD-code entries
#' 
#' Creates a tibble with random ICD-codes entries from different 
#' ICD-versions using the function \code{\link{create_test_df}}. 
#' The results are meant to mimic INTERVENE longitudinal files for
#' testing.
#'
#' It is possible to create a mix of ICD-10 and ICD-9 codes to create
#' a more realistic testing dataset.
#'
#' @param n_icd10 Number of draws from ICD10
#' @param n_icd9 Number of draws from ICD9
#' @param icd10_indv A character vector.
#'                   The names of the individuals with ICD10 codes.
#' @param icd9_indv A character vector.
#'                  The names of the individuals with ICD9 codes.
#' 
#' @return A tibble with columns `ID`, `Event_age`, `primary_ICD`,
#' and `secondary_ICD`. Filled with random ICD codes, and age at event.
#' 
#' @importFrom dplyr %>%
#' @export
#' 
#' @author Kira E. Detrois
create_test_df_multi_icd_ver <- function(n_icd10=50,
                                         n_icd9=20,
                                         icd10_indv=c("KT0000001", "KT0000002", "KT0000004", "KT0000005","KT0000006"), 
                                         icd9_indv=c("KT0000005", "KT0000007", "KT0000002", "KT0000008", "KT0000009")) { 
    samples_icd10 <- create_test_df(icd10_indv, n_icd10, "ICD10_2011")
    samples_icd9 <- create_test_df(icd9_indv, n_icd9, "ICD9_2015")
    icd_versions <- c(rep("10", n_icd10), rep("9", n_icd9))
    samples <- dplyr::bind_rows(samples_icd10, samples_icd9) %>%
                tibble::add_column(ICD_version = icd_versions)
}
