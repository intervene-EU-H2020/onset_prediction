test_that("calc_icd_ver_spec_ccis fails correctly", {
    sample_data <- ILongDataUtils::create_test_df_multi_icd_ver() 
    expect_error(calc_icd_ver_spec_ccis(sample_data))
})
