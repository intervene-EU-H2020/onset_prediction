test_that("get_study_elig_indv downsampling works", {
  set.seed(9231)
  test_data <- create_test_df(25)

  # Case control adjustment tests
  # Control because of late diagnosis
  test_data[test_data$ID == "KT0000022",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT0000022",]$J10_ASTHMA_DATE = as.Date("2021/01/01")
  # Case because of correct diagnosis time frame
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA_DATE = as.Date("1983/01/01")
  test_data[test_data$ID == "KT0000013","J10_ASTHMA_DATE"] = as.Date("1956/01/01")


  study <- methods::new("study", 
                        endpt="J10_ASTHMA",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8,
                        downsample_fctr=4)
  res_1 = get_study_elig_indv(test_data, study)
  expect_equal(get_n_cntrls(res_1, "J10_ASTHMA"), 8)

  # Rerunning should give exact same results
  res_2 = get_study_elig_indv(test_data, study)
  expect_equal(res_1$ID, res_2$ID)
})