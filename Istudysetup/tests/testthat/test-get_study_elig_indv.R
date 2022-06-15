test_that("get_study_elig_indv works", {
  set.seed(9231)
  test_data <- create_test_df(25)

  # Removed because of early diagnosis
  test_data[test_data$ID == "FG000001",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "FG000001",]$J10_ASTHMA_DATE = as.Date("1930/01/01")

  # REMOVE missing date case works
  test_data[test_data$ID == "FG0000016",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "FG0000016",]$J10_ASTHMA_DATE = NA


  res = suppressMessages(get_study_elig_indv(test_data,
                            endpt="J10_ASTHMA",
                            exp_age=30,
                            exp_len=2,
                            wash_len=2,
                            obs_len=8)$data)

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
                            endpt="J10_ASTHMA",
                            exp_age=30,
                            exp_len=2,
                            wash_len=2,
                            obs_len=8)$data

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


  res = suppressMessages(get_study_elig_indv(test_data,
                            endpt="J10_ASTHMA",
                            exp_age=30,
                            exp_len=2,
                            wash_len=2,
                            obs_len=8,
                            downsample_fctr=4)$data)

  expected_res_ids = c("FG000004", "FG000005", "FG000007", "FG000009", 
                       "FG0000012", "FG0000013", "FG0000014", "FG0000017", "FG0000023", "FG0000025")

  expect_equal(res$ID, expected_res_ids)
})

test_that("get_study_elig_indv works also with other endpoints", {
  set.seed(9231)
  test_data <- create_test_df(25)

 # Expect no error
 expect_error(suppressMessages(get_study_elig_indv(test_data, endpt="I9_VTE")),
              regexp=NA)
})

test_that("get_study_elig_indv log messages work", {
  set.seed(9231)
  test_data <- create_test_df(500)

  get_study_elig_indv(test_data, 
                      endpt="I9_VTE", 
                      write_res=TRUE,
                      res_dir = "/home/kira/duni/helsinki/DSGE/Code/onset_prediction/Istudysetup/results/",
                      write_log="file",
                      log_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/Istudysetup/results/")
})