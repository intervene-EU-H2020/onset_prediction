# create_prs_test <- function(n_indv=25, indv_ids=NULL) {
#     if(is.null(indv_ids))
#         indv_ids <- paste0("KT00000", seq(n_indv))
#     tibble::tibble(ID=indv_ids,
#                    J10_ASTHMA_PRS=rnorm(n_indv, 0, 0.01), 
#                    I9_VTE_PRS=rnorm(n_indv, 0, 0.01),  
#                    C3_BREAST_PRS=rnorm(n_indv, 0, 0.01),
#                    COVHOSP_PRS=rnorm(n_indv, 0, 0.01))
# }

# test_that("run_back_endpt_studies PRS works", {
 
#    pheno_data <- Istudy::create_test_df(10000)
#    test_data <- create_prs_test(length(pheno_data$ID), pheno_data$ID)

#    expect_error(run_surv_studies(pheno_data=pheno_data, 
#                                  prs_data=test_data,
#                                  score_type="PRS",
#                                  study_type="backward",
#                                  endpts=c("J10_ASTHMA", "I9_VTE"),
#                                  exp_len=10,
#                                  wash_len=2,
#                                  obs_len=8,
#                                  obs_end_date=as.Date("2019/01/01"),
#                                  down_fctr=4,
#                                  ancs=NA_character_,
#                                  covs=c("SEX", "YEAR_OF_BIRTH", "PC1"),
#                                  min_indvs=5,
#                                  write_res=TRUE,
#                                  res_dir="/Users/detrokir/Documents/Projects/onset_prediction/code/onset_prediction/IHRC/tests/results/"),
#                 regexp=NA)
# })


# test_that("run_back_endpt_studies exposure from birth works", {
 
#    pheno_data <- Istudy::create_test_df(10000)
#    test_data <- create_prs_test(length(pheno_data$ID), pheno_data$ID)

#    expect_error(run_surv_studies(pheno_data=pheno_data, 
#                                  prs_data=test_data,
#                                  score_type="PRS",
#                                  study_type="backward",
#                                  endpts=c("J10_ASTHMA", "I9_VTE"),
#                                  exp_age=0,
#                                  wash_len=2,
#                                  obs_len=8,
#                                  obs_end_date=as.Date("2021/01/01"),
#                                  down_fctr=4,
#                                  ancs=NA_character_,
#                                  covs=c("SEX", "YEAR_OF_BIRTH", "PC1"),
#                                  min_indvs=5,
#                                  write_res=TRUE,
#                                  res_dir="/Users/detrokir/Documents/Projects/onset_prediction/code/onset_prediction/IHRC/tests/results/"),
#                 regexp=NA)
# })