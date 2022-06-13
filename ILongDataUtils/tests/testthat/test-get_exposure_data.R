test_that("get_exposure_data throws correct error", {
  sample_data <- create_test_df_multi_icd_ver() 
  # No error
  expect_error(get_exposure_data(sample_data, exp_start=10), regexp=NA)
  expect_error(get_exposure_data(sample_data, exp_end=10), regexp=NA)
  expect_error(get_exposure_data(sample_data, 
                                exp_start=10.3,
                                exp_end=30), regexp=NA)
  sample_data <- dplyr::select(sample_data, -Event_age)
  expect_error(get_exposure_data(sample_data), regexp=NA)
  # error
  expect_error(get_exposure_data(sample_data, exp_start=10))
  expect_error(get_exposure_data(sample_data, exp_end=10))
  expect_error(get_exposure_data(sample_data, 
                                exp_start=10.3,
                                exp_end=30))
})

test_that("get_exposure_data works", {
  set.seed(9123)
  sample_data <- create_test_df_multi_icd_ver(n_icd10=5, 
                                              n_icd9=5,
                                              icd10_ind=c("KT02", "KT03"),
                                              icd9_indv=c("KT01", "KT02"))

  # Only start
  res <- get_exposure_data(sample_data, exp_start=75.3)
  expect_ids <- c("KT02", "KT02", "KT02")
  expect_codes <- c("1469", "V6284", "31322")
  expect_equal(res$ID, expect_ids)
  expect_equal(res$primary_ICD, expect_codes)

  # Only end
  res <- get_exposure_data(sample_data, exp_end=50)
  expect_ids <- c("KT03", "KT02", "KT02")
  expect_codes <- c("S827", "2980", "63500")
  expect_equal(res$ID, expect_ids)
  expect_equal(res$primary_ICD, expect_codes)

  # Both
  res <- get_exposure_data(sample_data, exp_start=70, exp_end=85)
  expect_ids <- c("KT02", "KT03", "KT02", "KT02")
  expect_codes <- c("T172", "D521", "V6284", "31322")
  expect_equal(res$ID, expect_ids)
  expect_equal(res$primary_ICD, expect_codes)
})
