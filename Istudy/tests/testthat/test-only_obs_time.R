test_that("get_study_elig_indv with only observation time works", {
  set.seed(9231)
  test_data <- create_test_df(25)
  study_setup <- methods::new("study_setup",
                        study_type="forward",
                        exp_age=0,
                        exp_len=0,
                        wash_len=0,
                        obs_len=100) 
  study <- methods::new("study",
                        study_data=test_data,
                        study_setup=study_setup,
                        endpt="I9_VTE")


    expect_equal(study@study_data$EXP_START_DATE, study@study_data$DATE_OF_BIRTH)
    expect_equal(study@study_data$EXP_END_DATE, study@study_data$DATE_OF_BIRTH)
    expect_equal(study@study_data$WASH_START_DATE, study@study_data$DATE_OF_BIRTH)
    expect_equal(study@study_data$WASH_END_DATE, study@study_data$DATE_OF_BIRTH)
    expect_equal(study@study_data$OBS_START_DATE, study@study_data$DATE_OF_BIRTH)

    # Expect no error
    expect_equal(nrow(get_study_elig_indv(study)), 7)
})
