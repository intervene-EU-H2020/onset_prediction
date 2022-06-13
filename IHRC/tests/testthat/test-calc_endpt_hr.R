test_that("calc_endpt_hr works", {
  library("Istudysetup")
  library("ICCI")

  pheno_data <- Istudysetup::create_test_df(1000)
  icd_data <- ILongDataUtils::create_test_df_multi_icd_ver(n_icd10=10000, 
                                                 icd10_indv=pheno_data$ID)

  score_data <- ICCI::calc_cci(icd_data,
                              exp_start=30,
                              exp_end=40)
  calc_endpt_hr(pheno_data, 
                score_data, 
                exp_age=30,
                exp_len=10,
                wash_len=2,
                obs_len=8,
                endpts=c("J10_ASTHMA", "I9_VTE"),
                downsample_fctr=4)
})
