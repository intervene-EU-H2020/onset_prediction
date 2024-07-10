test_that("get_study_elig_indv downsampling works", {
  set.seed(9231)
  test_data <- create_test_df(25)

  # Case control adjustment tests
  # Control because of late diagnosis
  test_data[test_data$ID == "KT0000022",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT0000022",]$J10_ASTHMA_DATE = as.Date("2021/01/01")
  # Case because of correct diagnosis time frame
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT000007",]$DATE_OF_BIRTH = as.Date("1998/01/01")
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA_DATE = as.Date("2015/01/01")
  test_data[test_data$ID == "KT0000013","J10_ASTHMA_DATE"] = as.Date("1956/01/01")

  study_setup_1 <- methods::new("study_setup", 
                         study_type="backward",
                         exp_len=10,
                         wash_len=2,
                         obs_len=8,
                         obs_end_date=as.Date("2019/01/01"),
                         down_fctr=4)
  study_1 <- methods::new("study",
                           study_setup=study_setup_1,
                           study_data=test_data,
                           endpt="J10_ASTHMA")
  expect_equal(get_n_cntrls(study_1@study_data, "J10_ASTHMA"), 4)

  study_setup_2 <- methods::new("study_setup", 
                         study_type="backward",
                         exp_len=10,
                         wash_len=2,
                         obs_len=8,
                         obs_end_date=as.Date("2019/01/01"),
                         down_fctr=4)
  study_2 <- methods::new("study",
                           study_setup=study_setup_2,
                           study_data=test_data,
                           endpt="J10_ASTHMA")
  expect_equal(study_1@study_data$ID, study_2@study_data$ID)
})