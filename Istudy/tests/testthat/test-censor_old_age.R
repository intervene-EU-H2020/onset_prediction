test_that("filter_old_age works", {
  set.seed(1923)
  test_df <- create_test_df(10)
  test_df$OBS_END_DATE <- as.Date("2019/01/01")
  censored <- filter_old_age(test_df, 70)

  expect_oe_age <- c(0.4, 38.1, 33.6, 63.4, 13.5, 66.9)
  expect_equal(round(censored$OBS_END_AGE, 1), expect_oe_age)
})
