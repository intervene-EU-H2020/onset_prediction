test_that("calc_endpt_hr works", {

  if (requireNamespace("ICCI", quietly = TRUE)) {
      set.seed(919923)
      library("ICCI")

      pheno_data <- Istudysetup::create_test_df(10000)
      icd_data <- ILongDataUtils::create_test_df_multi_icd_ver(n_icd10=50000, 
                                                    icd10_indv=pheno_data$ID)

      score_data <- ICCI::calc_cci(icd_data,
                                  exp_start=30,
                                  exp_end=40)
      calc_endpt_hr(pheno_data, 
                    score_data,
                    score_col_name="score", 
                    score_type="CCI",
                    exp_age=30,
                    exp_len=10,
                    wash_len=2,
                    obs_len=8,
                    endpts=c("J10_ASTHMA", 
                            "I9_VTE",
                            "C3_BREAST", 
                            "COVHOSP"),
                    downsample_fctr=4,
                    write_coxph_res=TRUE,
                    write_study_res=TRUE,
                    write_study_log=TRUE,
                    res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/results/")
   } else {
      message("Could not run tests, because ICCI is not available.")
   }
})
