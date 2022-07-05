test_that("add_risk_group_col works", {
    set.seed(191)

    pheno_data <- Istudy::create_test_df(5)
    icd_data <- ILongDataUtils::create_test_df_multi_icd_ver(n_icd10=30, icd10_indv=pheno_data$ID)
    cci_data <- ICCI::calc_cci(icd_data) %>% dplyr::rename(SCORE=CCI_score)
    pheno_score_data <- join_dfs(pheno_data, cci_data)
    pheno_score_data <- add_risk_group_col(pheno_score_data, score_type="CCI")
    expect_res <- factor(c("<=1", "<=1", ">1", ">1", ">1"),
                         levels=c("<=1", ">1"))
    expect_equal(pheno_score_data$SCORE_GROUP, expect_res)
})

test_that("get_risk_group_labs works", {
    set.seed(191)

    test_tbl <- c(0, 0.25, 2.3, 4.9, 5)
    names(test_tbl) <- c(0, 0.1, 0.5, 0.9, 1)

    expect_res <- c("[Group 0 - Group 0.1]", "(Group 0.1 - Group 0.5]", "(Group 0.5 - Group 0.9]", "(Group 0.9 - Group 1]")
    group_labels <- get_risk_group_labs(test_tbl)
    expect_equal(group_labels, expect_res)
})


