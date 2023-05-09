create_prs_test <- function(n_indv=25, indv_ids=NULL) {
    if(is.null(indv_ids))
        indv_ids <- paste0("KT00000", seq(n_indv))
    tibble::tibble(ID=indv_ids,
                   J10_ASTHMA_PRS=rnorm(n_indv, 0, 0.01), 
                   I9_VTE_PRS=rnorm(n_indv, 0, 0.01),  
                   C3_BREAST_PRS=rnorm(n_indv, 0, 0.01),
                   COVHOSP_PRS=rnorm(n_indv, 0, 0.01))
}

test_that("PRS + CCI works", {
  if (requireNamespace("ICCI", quietly = TRUE)) {
      set.seed(919923)
      library("ICCI")

    pheno_data <- Istudy::create_test_df(10000)
    icd_data <- ICCI::create_test_df_multi_icd_ver(
                                                n_icd10=50000, 
                                                icd10_indv=pheno_data$ID)
    prs_data <- create_prs_test(length(pheno_data$ID), pheno_data$ID)
    expect_error(run_surv_studies(
                         pheno_data=pheno_data, 
                         icd_data=icd_data,
                         prs_data=prs_data,
                         score_type=c("CCI", "PRS", "EDU"),
                         create_score_combos=FALSE,
                         study_type="backward",
                         endpts=c("J10_ASTHMA", 
                                  "I9_VTE",
                                  "C3_BREAST", 
                                  "COVHOSP"),
                         exp_len=10,
                         wash_len=2,
                         obs_len=8,
                         covs=c("SEX", "YEAR_OF_BIRTH"),
                         write_res=TRUE,
                         res_dir="/Users/detrokir/Documents/Projects/onset_prediction/code/onset_prediction/IHRC/tests/results/"), regexp=NA)
 
   } else {
      message("Could not run tests, because ICCI is not available.")
   }
})