test_that("filter_old_age works", {
  set.seed(1923)
  test_df <- create_test_df(10)
  test_df$OBS_END_DATE <- as.Date("2019/01/01")
  test_df$AGE_AT_BASE <- lubridate::time_length(test_df$DATE_OF_BIRTH %--% test_df$OBS_END_DATE, "years")
  censored <- filter_old_age(test_df, 70)
  expect_oe_age <- c(0.4, 38.1, 33.6, 63.4, 13.5, 66.9)
  expect_equal(round(censored$AGE_AT_BASE, 1), expect_oe_age)
})

test_that("filter_young_age works", {
  set.seed(1923)
  test_df <- create_test_df(10)
  test_df$OBS_START_DATE <- rep(as.Date("2011/01/01"), 10)
  test_df$OBS_END_DATE <- rep(as.Date("2019/01/01"), 10)
  test_df$EXP_START_DATE <- rep(as.Date("1999/01/01"), 10)


  test_df$AGE_AT_BASE <- lubridate::time_length(test_df$DATE_OF_BIRTH %--% test_df$OBS_START_DATE, "years") 
  censored <- filter_young_age(test_df, "backward", 30)
  expect_oe_age <- c(30.1, 55.4, 74.7, 58.9, 72.6, 73.1, 119.8)
  expect_equal(round(censored$AGE_AT_BASE, 1), expect_oe_age)
})
