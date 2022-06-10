library(lubridate)

test_that("create_test_df date drawing works", {
  set.seed(9231)
  test_df = create_test_df(10)

  # Birth comes before followup period
  followup_interval = get_followup_time(test_df)
  expect_true(all(!(test_df$DATE_OF_BIRTH %within% followup_interval)))

  # End of followup comes after start of followup
  expected_bool = c(TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, NA, TRUE)
  expect_equal(test_df$END_OF_FOLLOWUP > test_df$START_OF_FOLLOWUP, expected_bool)
  
  # Diagnosis comes in followup period
  date_cols = colnames(dplyr::select(test_df, matches("(*.)_DATE$")))
  for(date_col in date_cols) {
    not_nas = !is.na(test_df[[date_col]]) & !is.na(test_df$END_OF_FOLLOWUP)
    if(sum(not_nas) > 0) {
      expect_true(all(test_df[[date_col]][not_nas] %within% followup_interval[not_nas]))
    }
  }
})
