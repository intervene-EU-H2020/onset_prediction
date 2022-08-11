#' Creates a tibble with random ICD-codes entries
#' 
#' Creates a tibble with random ICD-codes entries, 
#' using the function \code{\link[comorbidity]{sample_diag}}. The 
#' results are meant to mimic INTERVENE longitudinal files for
#' testing.
#'
#' For each entry the age at event is drawn from a uniform 
#' distribution. The functions also adds a certain percentage of 
#' secondary ICD-codes.
#'
#' @param indv_ids A string. 
#'                 IDs of the individuals.
#' @param n_samples An integer. 
#'                  Total number of samples to draw for the individuals
#' @param icd_version The ICD version to draw the samples from.
#'                    Can be either `ICD10_2009`, `ICD10_2011`, or
#'                    `ICD9_2015`. The default is `ICD10_2011`.
#' @param percent_sec_icd Percentage of secondary ICD-codes to add
#'                        to the records.
#' 
#' @return A tibble with columns `ID`, `Event_age`, `primary_ICD`,
#' and `secondary_ICD`. Filled with random ICD codes, and age at event.
#'
#' @examples
#' indv_ids = c("KT001","KT002","KT004","KT005","KT006")
#' create_test_df(indv_ids, 50, "ICD10_2011")
#'
#' @export
#' 
#' @author Kira E. Detrois
create_test_df <- function(indv_ids,
                           n_samples,
                           icd_version="ICD10_2011",
                           percent_sec_icd=0.05) {

    n_secondary_icd <- ceiling(percent_sec_icd * n_samples)
    id_samples <- sample(indv_ids, size = n_samples, replace = TRUE)
    age_samples <- round(stats::runif(n_samples, min = 0, max = 100), 1)
    prim_icd_samples <- comorbidity::sample_diag(n = n_samples,
                                                 version = icd_version)
    sec_icd_samples <- c(comorbidity::sample_diag(n = n_secondary_icd,
                                                  version = icd_version),
                         rep(NA, n_samples - n_secondary_icd))
    tibble::tibble(
        ID = id_samples,
        Event_age = age_samples,
        primary_ICD = prim_icd_samples,
        secondary_ICD = sec_icd_samples
    )
}

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
                                         n_icd9=0,
                                         icd10_indv=c("KT0000001", "KT0000002", "KT0000004", "KT0000005","KT0000006"), 
                                         icd9_indv=c("KT0000005", "KT0000007", "KT0000002", "KT0000008", "KT0000009")) { 
    samples_icd10 <- create_test_df(icd10_indv, n_icd10, "ICD10_2011")
    samples_icd9 <- create_test_df(icd9_indv, n_icd9, "ICD9_2015")
    icd_versions <- c(rep("10", n_icd10), rep("9", n_icd9))
    samples <- dplyr::bind_rows(samples_icd10, samples_icd9) %>%
                tibble::add_column(ICD_version = icd_versions)
}
