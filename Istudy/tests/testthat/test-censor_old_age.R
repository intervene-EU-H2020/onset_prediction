test_that("censor_old_age works", {
  set.seed(1923)
  test_df <- create_test_df(10)
  test_df$OBS_END_DATE <- as.Date("2019/01/01")
  censored <- censor_old_age(test_df, 70)


  expect_eof <- c(as.Date("2021-09-04"), NA, as.Date("2008-10-04"), NA, NA, as.Date("2016-06-13"), as.Date("2021-04-18"), as.Date("2019-01-01"), as.Date("2019-01-01"), as.Date("2014-07-26"))
  expect_equal(censored$END_OF_FOLLOWUP, expect_eof)
})
