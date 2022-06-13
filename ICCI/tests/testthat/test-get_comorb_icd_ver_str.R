test_that("get_comorb_idc_ver_str works", {
  expect_equal(get_comorb_icd_ver_str("10"), "charlson_icd10_quan")
  expect_equal(get_comorb_icd_ver_str("10CM"), "charlson_icd10_quan")
  expect_equal(get_comorb_icd_ver_str("9"), "charlson_icd9_quan")
  expect_equal(get_comorb_icd_ver_str("9CM"), "charlson_icd9_quan")
})
