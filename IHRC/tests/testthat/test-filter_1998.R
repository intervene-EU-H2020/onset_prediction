
test_that("calc_endpt_studies_hrs works", {
  if (requireNamespace("ICCI", quietly = TRUE)) {
      set.seed(919923)
      library("ICCI")

      pheno_data <- Istudy::create_test_df(10000)
      icd_data <- IUtils::create_test_df_multi_icd_ver(
                                                n_icd10=50000, 
                                                icd10_indv=pheno_data$ID)
      exp_ages <- c(20,30,40)
      expect_error(run_surv_studies(
                         pheno_data=pheno_data, 
                         score_data=icd_data,
                         score_type="CCI",
                         study_type="forward",
                         endpts=c("J10_ASTHMA", 
                                  "I9_VTE",
                                  "C3_BREAST", 
                                  "COVHOSP"),
                         exp_ages=exp_ages,
                         exp_len=10,
                         wash_len=2,
                         obs_len=8,
                         max_age=50,
                         ancs=NA_character_,
                         filter_1998 = TRUE,
                         covs=c("SEX", "YEAR_OF_BIRTH"),
                         bin_cut=1,
                         write_res=TRUE,
                         res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/"), regexp=NA)
 
   } else {
      message("Could not run tests, because ICCI is not available.")
   }
})
