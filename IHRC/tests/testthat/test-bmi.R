
test_that("run_back_endpt_studies BMI works", {
   if(requireNamespace("ICCI", quietly = TRUE)) {
      set.seed(919923)
      library("ICCI")

      pheno_data <- Istudy::create_test_df(10000)
      icd_data <- ICCI::create_test_df_multi_icd_ver(
                                                n_icd10=50000, 
                                                icd10_indv=pheno_data$ID)

      expect_error(run_surv_studies(pheno_data=pheno_data, 
                                    icd_data=icd_data,
                                    score_type=c("BMI", "CCI"),
                                    study_type="backward",
                                    endpts=c("J10_ASTHMA", "I9_VTE"),
                                    exp_len=10,
                                    wash_len=2,
                                    obs_len=8,
                                    obs_end_date=as.Date("2019/01/01"),
                                    ancs=NA_character_,
                                    covs=c("SEX", "YEAR_OF_BIRTH", "PC1", "PC2"),
                                    min_indvs=50,
                                    write_res=TRUE,
                                    res_dir="/Users/detrokir/Documents/Projects/onset_prediction/code/onset_prediction/IHRC/tests/results/"),
                  regexp=NA)
   }
})
