test_that("test endpt input checks work", {
  # No error
  expect_error(methods::new("study", endpt="J10_ASTHMA", exp_age=30, exp_len=10, wash_len=2, obs_len=8), regexp=NA)

  # Errors
  expect_error(methods::new("study", endpt=2, exp_age=30, exp_len=10, wash_len=2, obs_len=8))
  expect_error(methods::new("study", endpt=c("J10_ASTHMA", "C3_BREAST"),exp_age=30, exp_len=10, wash_len=2, obs_len=8), regexp="The variable endpt needs to be a character string and not a vector of characters.", fixed=TRUE)
})

test_that("test_date_var_correct works", {
  bds = c(as.Date("2020/01/01", "1977/01/01"))
  bds_wrong = c(1,2,3)
  # No error
  study <- methods::new("study", endpt="J10_ASTHMA", exp_age=30, exp_len=10, wash_len=2, obs_len=8)
  expect_error(calc_endpt_free_time(bds, study), regexp=NA)

  # Errors
  bds_wrong = c(1,2,3)
  expect_error(calc_endpt_free_time(bds_wrong, study))
})

test_that("test_length_vars_are_integers works", {
  set.seed(9123)
  test_data = create_test_df(10)

  # No error
  expect_error(methods::new("study",
                                   endpt="J10_ASTHMA", 
                                   exp_age=30,
                                   exp_len=10,
                                   wash_len=2,
                                   obs_len=8), 
                regexp=NA)

  # Errors
  expect_error(methods::new("study",
                            endpt="J10_ASTHMA",
                            exp_age=30.3,
                            exp_len=10,
                            wash_len=2,
                            obs_len=8))

  expect_error(methods::new("study",
                                   endpt="J10_ASTHMA",
                                   exp_age="bla",
                                   exp_len=10,
                                   wash_len=2,
                                   obs_len=8))
})

