test_that("get_study_elig_indv adj case control works", {
  set.seed(9231)
  test_data <- create_test_df(5)


  study <- methods::new("study",
                        study_type="forward",
                        study_data=test_data, 
                        endpt="J10_ASTHMA",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8)

  study_f1998 <- methods::new("study",
                        study_type="forward",
                        study_data=test_data, 
                        endpt="J10_ASTHMA",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8,
                        filter_1998=TRUE)

    expect_equal(study_f1998@study_data$ID, "KT000005")
})