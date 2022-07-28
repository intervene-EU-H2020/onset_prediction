# test_that("calc_endpt_studies_hrs works", {
#   if (requireNamespace("ICCI", quietly = TRUE)) {
#       set.seed(919923)
#       library("ICCI")

#       pheno_data <- Istudy::create_test_df(500)
#       icd_data <- IUtils::create_test_df_multi_icd_ver(
#                                                 n_icd10=1000, 
#                                                 icd10_indv=pheno_data$ID)
#       expect_error(run_surv_studies(
#                          pheno_data=pheno_data, 
#                          score_data=icd_data,
#                          score_type="CCI",
#                          study_type="backward",
#                          endpts=c("C3_BREAST"),
#                          wash_len=2,
#                          obs_len=8,
#                          ancs=NA_character_,
#                          downsample_fctr=1,
#                          covs=c("SEX", "YEAR_OF_BIRTH"),
#                          bin_cut=1,
#                          write_res=TRUE,
#                          res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/"), regexp=NA)
 
#    } else {
#       message("Could not run tests, because ICCI is not available.")
#    }
# })