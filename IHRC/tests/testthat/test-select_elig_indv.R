
# test_that("select_elig_indv of surv_ana works", {
#   if (requireNamespace("ICCI", quietly = TRUE)) {
#       set.seed(919923)
#       library("ICCI")

#          pheno_data <- Istudy::create_test_df(10000)
#          icd_data <- IUtils::create_test_df_multi_icd_ver(
#                                                 n_icd10=50000, 
#                                                 icd10_indv=pheno_data$ID)
#          study <- methods::new("study",
#                               endpt="I9_VTE",
#                               exp_age=20,
#                               exp_len=10,
#                               wash_len=2,
#                               obs_len=8,
#                               downsample_fctr=4) 
#          surv_ana <- methods::new("surv_ana",
#                                  pheno_data=pheno_data,
#                                  elig_score_data=icd_data,
#                                  score_type="CCI",
#                                  study=study)
#          elig_indv <- Istudy::get_study_elig_indv(pheno_data, study)
#          cci_scores <- ICCI::calc_cci(icd_data, exp_start=20, exp_end=30)
#          elig_cci_scores <- dplyr::left_join(elig_indv, cci_scores, by="ID")
#          elig_cci_scores$CCI_score[is.na(elig_cci_scores$CCI_score)] <- 0
#          expect_equal(table(elig_cci_scores$CCI_score), table(surv_ana@elig_score_data$CCI_SCORE))
#    } else {
#       message("Could not run tests, because ICCI is not available.")
#    }
# })

