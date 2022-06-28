test_that("add_risk_group_col works", {
    set.seed(191)

    pheno_data <- Istudy::create_test_df(5)
    icd_data <- ILongDataUtils::create_test_df_multi_icd_ver(n_icd10=70, n_icd9=0, icd10_indv=pheno_data$ID)
    cci_data <- ICCI::calc_cci(icd_data)
    cci_data <- preprocess_score_data(cci_data, "CCI_score")
    pheno_score_data <- join_dfs(pheno_data, cci_data)
    pheno_score_data <- add_risk_group_col(pheno_score_data)

    expect_res <- factor(c("low", "low", "low", "low", "high"),
                         levels=c("low", "high"))
    expect_equal(pheno_score_data$SCORE_GROUP, expect_res)
})

test_that("add_risk_group_col works", {
    set.seed(191)

    pheno_data <- Istudy::create_test_df(10)
    icd_data <- ILongDataUtils::create_test_df_multi_icd_ver(n_icd10=300, n_icd9=0, icd10_indv=pheno_data$ID)
    cci_data <- ICCI::calc_cci(icd_data)
    cci_data <- preprocess_score_data(cci_data, "CCI_score")
    pheno_score_data <- join_dfs(pheno_data, cci_data)

    pheno_score_data <- add_risk_group_col(pheno_score_data)

    expect_res <- factor(c("[Group 20% - Group 40%)", "[Group 80% - Group 100%]", "[Group 40% - Group 60%)", "[Group 40% - Group 60%)", "[Group 0% - Group 1%)", "[Group 80% - Group 100%]", "[Group 20% - Group 40%)", "[Group 40% - Group 60%)", "[Group 60% - Group 80%)", "[Group 60% - Group 80%)"))
    expect_res <- stats::relevel(expect_res, ref="[Group 40% - Group 60%)")
    expect_equal(pheno_score_data$SCORE_GROUP, expect_res)
})
