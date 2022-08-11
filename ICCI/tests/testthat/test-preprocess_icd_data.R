test_that("preprocess_icd_data works", {
  sample_data <- create_test_df_multi_icd_ver(n_icd10 = 5)
  sample_data <- tibble::add_row(sample_data, ID="KT0000005", Event_age=12.3, primary_ICD="asdf", secondary_ICD=NA, ICD_version=NA)
    sample_data <- tibble::add_row(sample_data, ID="KT0000005", Event_age=12.3, primary_ICD="asdf", secondary_ICD=NA, ICD_version="8")

  expect_warning(preprocess_icd_data(icd_data=sample_data), regexp="Careful, filtering out 2 entries because of incompatible ICD-versions removed entries with: NA, 8. The CCI is only defined on ICD-9 and 10 at the moment.")
})
