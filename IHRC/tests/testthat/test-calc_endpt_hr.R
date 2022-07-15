

# test_that("calc_endpt_studies_hrs works", {
#   if (requireNamespace("ICCI", quietly = TRUE)) {
#       set.seed(919923)
#       library("ICCI")

#       pheno_data <- Istudy::create_test_df(1000)
#       icd_data <- ILongDataUtils::create_test_df_multi_icd_ver(n_icd10=5000, 
#                                                                icd10_indv=pheno_data$ID)
#       exp_ages <- c(20,30,40,50,60)
#       score_age_data <- ICCI::calc_cci_for_mult_exp_ages(icd_data, 
#                                                          exp_ages, 
#                                                          exp_len=10)
#       expect_error(run_age_exp_endpt_studies(pheno_data, 
#                           score_age_data,
#                           score_type="CCI",
#                           exp_ages=exp_ages,
#                           exp_len=10,
#                           wash_len=2,
#                           obs_len=8,
#                           endpts=c("J10_ASTHMA", 
#                                    "I9_VTE",
#                                    "C3_BREAST", 
#                                    "COVHOSP"),
#                           downsample_fctr=4,
#                           ancs=NA_character_,
#                           covs=c("SEX", "YEAR_OF_BIRTH"),
#                           bin_cut=1,
#                           write_res=TRUE,
#                           res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/"), regexp=NA)

#    } else {
#       message("Could not run tests, because ICCI is not available.")
#    }
# })


