test_that("test endpt input checks work", {
  # No error
  expect_error(methods::new("study", 
                            study_data=create_test_df(5),
                            endpt="J10_ASTHMA", 
                            exp_age=30, 
                            exp_len=10, 
                            wash_len=2, 
                            obs_len=8, 
                            ancs="EUR"), regexp=NA)

  # Errors
  expect_error(methods::new("study", 
                            study_data=create_test_df(5),
                            endpt=2, 
                            exp_age=30, 
                            exp_len=10, 
                            wash_len=2, 
                            obs_len=8, 
                            ancs="EUR"))
  expect_error(methods::new("study", 
                            study_data=create_test_df(5),
                            endpt=c("J10_ASTHMA", "C3_BREAST"),
                            exp_age=30, 
                            exp_len=10, 
                            wash_len=2, 
                            obs_len=8, 
                            ancs="EUR"), regexp="The variable endpt needs to be a character string and not a vector of characters.", fixed=TRUE)
})

test_that("test_length_vars_are_integers works", {
  set.seed(9123)
  test_data = create_test_df(10)

  # No error
  expect_error(methods::new("study",
                            study_data=create_test_df(5),
                            endpt="J10_ASTHMA", 
                            exp_age=30,
                            exp_len=10,
                            wash_len=2,
                            obs_len=8, ancs="EUR"), 
                regexp=NA)

  # Errors
  expect_error(methods::new("study",
                            study_data=create_test_df(5),
                            endpt="J10_ASTHMA",
                            exp_age=30.3,
                            exp_len=10,
                            wash_len=2,
                            obs_len=8, ancs="EUR"))

  expect_error(methods::new("study",
                            study_data=create_test_df(5),
                            endpt="J10_ASTHMA",
                            exp_age="bla",
                            exp_len=10,
                            wash_len=2,
                            obs_len=8, 
                            ancs="EUR"))
})

