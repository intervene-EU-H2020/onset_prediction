test_that("get_study_elig_indv with only observation time works", {
  set.seed(9231)
  test_data <- create_test_df(25)
  test_data$ANCESTRY <- rep("EUR", 25)

  study <- methods::new("study", 
                        study_type="forward",
                        study_data=test_data,
                        endpt="I9_VTE",
                        exp_age=0,
                        exp_len=0,
                        wash_len=0,
                        obs_len=100)
 # Expect no error
 expect_equal(nrow(get_study_elig_indv(study)), 7)
})
