# test_that("join_dfs less score data works", {
#     set.seed(1291)

#     pheno_data <- Istudy::create_test_df(5)

#     icd_data <- ICCI::create_test_df_multi_icd_ver(n_icd10=50, n_icd9=0, icd10_indv=pheno_data$ID[2:nrow(pheno_data)])
#     cci_data <- ICCI::calc_cci(icd_data)
#     pheno_score_data <- join_dfs(pheno_data, cci_data)
#     # join removed individual with no score data.
#     expect_equal(pheno_score_data$CCI, c(0,0,3,4,0))
# })

# test_that("join_dfs less pheno data works", {
#     set.seed(1291)
#     pheno_data <- Istudy::create_test_df(5)
#     icd_data <- ICCI::create_test_df_multi_icd_ver(n_icd10=50, n_icd9=0, icd10_indv=pheno_data$ID)
#     cci_data <- ICCI::calc_cci(icd_data)
#     pheno_score_data <- join_dfs(pheno_data, cci_data)
#     # join removed individual with no pheno data
#     expect_equal(sort(pheno_score_data$ID), sort(pheno_data$ID))
# })
# # 