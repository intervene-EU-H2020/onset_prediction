test_that("calc_endpt_studies_hrs works", {
  if (requireNamespace("ICCI", quietly = TRUE)) {
      set.seed(919923)
      library("ICCI")

      pheno_data <- Istudy::create_test_df(1000)
      icd_data <- ILongDataUtils::create_test_df_multi_icd_ver(
                                                n_icd10=5000, 
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
                         downsample_fctr=4,
                         ancs=NA_character_,
                         covs=c("SEX", "YEAR_OF_BIRTH"),
                         bin_cut=1,
                         write_res=TRUE,
                         res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/"), regexp=NA)
                         
      readr::write_delim(pheno_data, "pheno_data.tsv", delim="\t")
      pheno_data_true <- readr::read_delim("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/pheno_data.tsv", delim="\t")
      expect_equal(pheno_data, pheno_data_true)
      print(pheno_data, n=10)
      print(pheno_data_true, n=10)
      elig_true_vte <- readr::read_delim("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/I9_VTE_a20_e10_w2_o8_elig_indv.tsv", delim="\t", show_col_types = FALSE)
      elig_test <- readr::read_delim("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/CCI_logs/elig_indv/I9_VTE_a20_e10_w2_o8_elig_indv.tsv", delim="\t", show_col_types = FALSE)
      expect_equal(elig_test$ID, elig_true_vte$ID)

      elig_true_breast <- readr::read_delim("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/C3_BREAST_a20_e10_w2_o8_elig_indv.tsv", delim="\t", show_col_types = FALSE)
      elig_test <- readr::read_delim("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/CCI_logs/elig_indv/C3_BREAST_a20_e10_w2_o8_elig_indv.tsv", delim="\t", show_col_types = FALSE)
      expect_equal(elig_test$ID, elig_true_breast$ID)

      elig_true_asth <- readr::read_delim("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/J10_ASTHMA_a20_e10_w2_o8_elig_indv.tsv", delim="\t", show_col_types = FALSE)
      elig_test <- readr::read_delim("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/CCI_logs/elig_indv/J10_ASTHMA_a20_e10_w2_o8_elig_indv.tsv", delim="\t", show_col_types = FALSE)
      expect_equal(elig_test$ID, elig_true_asth$ID)
   } else {
      message("Could not run tests, because ICCI is not available.")
   }
})


