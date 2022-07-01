get_cci_score_age_data <- function(icd_data,
                                   exp_ages=c(20,30,40,50),
                                   exp_len=10) {
   score_age_data <- c()
   for(exp_age in exp_ages) {
      score_age_data[[exp_age]] <- ICCI::calc_cci(icd_data, exp_start=exp_age, exp_end=exp_age+exp_len) %>% dplyr::rename(SCORE=CCI_score)
   }
   return(score_age_data)
}

test_that("calc_studies_hrs works", {
  if (requireNamespace("ICCI", quietly = TRUE)) {
      set.seed(919923)
      library("ICCI")

      pheno_data <- Istudy::create_test_df(100000)
      icd_data <- ILongDataUtils::create_test_df_multi_icd_ver(n_icd10=50000, 
                                                               icd10_indv=pheno_data$ID)
      exp_ages <- c(20,30)
      score_age_data <- get_cci_score_age_data(icd_data, exp_ages, exp_len=10)
      run_age_exp_studies(pheno_data, 
                          score_age_data,
                          score_type="CCI",
                          exp_ages=exp_ages,
                          exp_len=10,
                          wash_len=2,
                          obs_len=8,
                          endpts=c("J10_ASTHMA", 
                                   "I9_VTE",
                                   "C3_BREAST", 
                                   "COVHOSP"),
                          downsample_fctr=4,
                          write_res=TRUE,
                          res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/")

   } else {
      message("Could not run tests, because ICCI is not available.")
   }
})


