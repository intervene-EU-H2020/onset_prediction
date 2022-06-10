test_that("get_study_elig_indv works", {
  set.seed(9231)
  test_data <- create_test_df(25)

  # Removed because of early diagnosis
  test_data[test_data$ID == "FG000001",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "FG000001",]$J10_ASTHMA_DATE = as.Date("1930/01/01")

  # REMOVE missing date case works
  test_data[test_data$ID == "FG0000016",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "FG0000016",]$J10_ASTHMA_DATE = NA


  res = get_study_elig_indv(test_data,
                            exp_age=30,
                            exp_len=2,
                            wash_len=2,
                            obs_len=8,
                            endpt="J10_ASTHMA")$data

  expected_res_ids = c("FG000004", "FG000005", "FG000006", "FG000007", "FG000009", "FG0000010",
                       "FG0000012", "FG0000013", "FG0000014", "FG0000015", "FG0000017", 
                       "FG0000018", "FG0000019", "FG0000022", "FG0000023", "FG0000025")

  expect_equal(res$ID, expected_res_ids)
})

test_that("get_study_elig_indv adj case control works", {
  set.seed(9231)
  test_data <- create_test_df(25)
  # Case control adjustment tests
  # Control because of late diagnosis
  test_data[test_data$ID == "FG0000022",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "FG0000022",]$J10_ASTHMA_DATE = as.Date("2021/01/01")
  # Case because of correct diagnosis time frame
  test_data[test_data$ID == "FG000007",]$J10_ASTHMA_DATE = as.Date("1980/01/01")
  test_data[test_data$ID == "FG0000013",]$J10_ASTHMA_DATE = as.Date("1955/01/01")

  res = get_study_elig_indv(test_data,
                            exp_age=30,
                            exp_len=2,
                            wash_len=2,
                            obs_len=8,
                            endpt="J10_ASTHMA")$data

  expected_res = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)   
  expect_equal(res$J10_ASTHMA, expected_res)
})


test_that("get_study_elig_indv downsampling works", {
  set.seed(9231)
  test_data <- create_test_df(25)
  # Case control adjustment tests
  # Control because of late diagnosis
  test_data[test_data$ID == "FG0000022",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "FG0000022",]$J10_ASTHMA_DATE = as.Date("2021/01/01")
  # Case because of correct diagnosis time frame
  test_data[test_data$ID == "FG000007",]$J10_ASTHMA_DATE = as.Date("1980/01/01")
  test_data[test_data$ID == "FG0000013",]$J10_ASTHMA_DATE = as.Date("1955/01/01")

  res = get_study_elig_indv(test_data,
                            exp_age=30,
                            exp_len=2,
                            wash_len=2,
                            obs_len=8,
                            endpt="J10_ASTHMA",
                            downsample_fctr=4)$data

  expected_res_ids = c("FG000004", "FG000005", "FG000007", "FG000009", 
                       "FG0000012", "FG0000013", "FG0000014", "FG0000017", "FG0000023", "FG0000025")

  expect_equal(res$ID, expected_res_ids)
})
