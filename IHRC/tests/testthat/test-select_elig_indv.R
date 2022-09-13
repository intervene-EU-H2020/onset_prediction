
# test_that("select_elig_indv of surv_ana works", {
#   if (requireNamespace("ICCI", quietly = TRUE)) {
#       set.seed(919923)
#       library("ICCI")

#          pheno_data <- Istudy::create_test_df(10000)
#          icd_data <- IUtils::create_test_df_multi_icd_ver(
#                                                 n_icd10=50000, 
#                                                 icd10_indv=pheno_data$ID)
#          study <- methods::new("study",
#                                study_data=pheno_data,
#                                endpt="I9_VTE",
#                                exp_age=20,
#                                exp_len=10,
#                                wash_len=2,
#                                obs_len=8,
#                                downsample_fctr=4) 
#          score_data <- get_elig_score_data(score_type="CCI", 
#                                            study_data=study@study_data, 
#                                            icd_data=icd_data,
#                                            endpt="I9_VTE")
#          surv_ana <- methods::new("surv_ana",
#                                   elig_score_data=score_data,
#                                   preds=c("SEX", "YEAR_OF_BIRTH", "CCI"),
#                                   study=study)
#          CCIs <- ICCI::calc_cci(icd_data, 
#                                       exp_start=20, 
#                                       exp_end=30)

#          elig_CCIs <- dplyr::left_join(study@study_data, CCIs, by="ID")
#          elig_CCIs$CCI[is.na(elig_CCIs$CCI)] <- 0

#          expect_equal(table(elig_CCIs$CCI), table(surv_ana@elig_score_data$CCI))
#    } else {
#       message("Could not run tests, because ICCI is not available.")
#    }
# })

