# create_prs_test <- function(n_indv=25, indv_ids=NULL) {
#     if(is.null(indv_ids))
#         indv_ids <- paste0("KT00000", seq(n_indv))
#     tibble::tibble(ID=indv_ids,
#                    J10_ASTHMA_PRS=rnorm(n_indv, 0, 0.01), 
#                    I9_VTE_PRS=rnorm(n_indv, 0, 0.01),  
#                    C3_BREAST_PRS=rnorm(n_indv, 0, 0.01),
#                    COVHOSP_PRS=rnorm(n_indv, 0, 0.01))
# }

# test_that("PRS + CCI works", {
#   if (requireNamespace("ICCI", quietly = TRUE)) {
#       set.seed(919923)
#       library("ICCI")

#     pheno_data <- Istudy::create_test_df(10000)
#     icd_data <- ICCI::create_test_df_multi_icd_ver(
#                                                 n_icd10=50000, 
#                                                 icd10_indv=pheno_data$ID)
#     prs_data <- create_prs_test(length(pheno_data$ID), pheno_data$ID)
#     exp_ages <- c(20,30,40)
#     expect_error(run_surv_studies(
#                          pheno_data=pheno_data, 
#                          icd_data=icd_data,
#                          prs_data=prs_data,
#                          score_type=c("CCI", "PRS"),
#                          study_type="forward",
#                          endpts=c("J10_ASTHMA", 
#                                   "I9_VTE",
#                                   "C3_BREAST", 
#                                   "COVHOSP"),
#                          exp_ages=exp_ages,
#                          exp_len=10,
#                          wash_len=2,
#                          obs_len=8,
#                          ancs=NA_character_,
#                          covs=c("SEX", "YEAR_OF_BIRTH"),
#                          write_res=TRUE,
#                          res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/"), regexp=NA)
 
#    } else {
#       message("Could not run tests, because ICCI is not available.")
#    }
# })


# test_that("PRS + CCI backwards works", {
#   if (requireNamespace("ICCI", quietly = TRUE)) {
#       set.seed(919923)
#       library("ICCI")

#     pheno_data <- Istudy::create_test_df(10000)
#     icd_data <- ICCI::create_test_df_multi_icd_ver(
#                                                 n_icd10=50000, 
#                                                 icd10_indv=pheno_data$ID)
#     prs_data <- create_prs_test(length(pheno_data$ID), pheno_data$ID)
#     expect_error(run_surv_studies(
#                          pheno_data=pheno_data, 
#                          icd_data=icd_data,
#                          prs_data=prs_data,
#                          score_type=c("PRS", "CCI"),
#                          study_type="backward",
#                          endpts=c("J10_ASTHMA", 
#                                   "I9_VTE",
#                                   "C3_BREAST", 
#                                   "COVHOSP"),
#                          exp_len=10,
#                          wash_len=2,
#                          obs_len=8,
#                          ancs=NA_character_,
#                          covs=c("SEX", "YEAR_OF_BIRTH"),
#                          write_res=TRUE,
#                          res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/"), regexp=NA)
 
#    } else {
#       message("Could not run tests, because ICCI is not available.")
#    }
# })

# test_that("PRS * CCI works", {
#   if (requireNamespace("ICCI", quietly = TRUE)) {
#       set.seed(919923)
#       library("ICCI")

#     pheno_data <- Istudy::create_test_df(10000)
#     icd_data <- ICCI::create_test_df_multi_icd_ver(
#                                                 n_icd10=50000, 
#                                                 icd10_indv=pheno_data$ID)
#     prs_data <- create_prs_test(length(pheno_data$ID), pheno_data$ID)
#     expect_error(run_surv_studies(
#                          pheno_data=pheno_data, 
#                          icd_data=icd_data,
#                          prs_data=prs_data,
#                          score_type=c("CCI", "PRS", "CCI*PRS", "CCI*YEAR_OF_BIRTH"),
#                          plot_preds=c("CCI", "PRS", "CCI*PRS", "CCI*YEAR_OF_BIRTH", "YEAR_OF_BIRTH"),
#                          study_type="backward",
#                          endpts=c("J10_ASTHMA", "I9_VTE"),
#                          exp_len=10,
#                          wash_len=2,
#                          obs_len=8,
#                          obs_end_date=as.Date("2021/01/01"),
#                          write_res=TRUE,
#                          res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/"), regexp=NA)
 
#    } else {
#       message("Could not run tests, because ICCI is not available.")
#    }
# })