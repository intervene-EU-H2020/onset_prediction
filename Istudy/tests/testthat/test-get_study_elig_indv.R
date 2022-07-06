test_that("get_study_elig_indv works", {
  set.seed(9231)
  test_data <- create_test_df(25)
  test_data$ANCESTRY <- rep("EUR", 25)
  # Removed because of early diagnosis
  test_data[test_data$ID == "KT000001",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT000001",]$J10_ASTHMA_DATE = as.Date("1930/01/01")

  # REMOVE missing date case works
  test_data[test_data$ID == "KT0000016",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT0000016",]$J10_ASTHMA_DATE = NA

  study <- methods::new("study", 
                        endpt="J10_ASTHMA",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8, 
                        ancs="EUR")

  res = suppressMessages(get_study_elig_indv(test_data, study))
  expected_res_ids = c("KT000004", "KT000005", "KT000006", "KT000007", "KT000009", "KT0000010",
                       "KT0000012", "KT0000013", "KT0000014", "KT0000015", "KT0000017", 
                       "KT0000018", "KT0000019", "KT0000022", "KT0000023", "KT0000025")

  expect_equal(res$ID, expected_res_ids)
})

test_that("get_study_elig_indv adj case control works", {
  set.seed(9231)
  test_data <- create_test_df(25)
  test_data$ANCESTRY <- rep("EUR", 25)

  # Case control adjustment tests
  # Control because of late diagnosis
  test_data[test_data$ID == "KT0000022",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT0000022",]$J10_ASTHMA_DATE = as.Date("2021/01/01")
  # Case because of correct diagnosis time frame
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA_DATE = as.Date("1983/01/01")
  test_data[test_data$ID == "KT0000013","J10_ASTHMA_DATE"] = as.Date("1956/01/01")

  study <- methods::new("study", 
                        endpt="J10_ASTHMA",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8, 
                        ancs="EUR")
  res = suppressMessages(get_study_elig_indv(test_data, study))

  expected_res = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)   
  expect_equal(res$J10_ASTHMA, expected_res)
})




test_that("get_study_elig_indv works also with other endpoints", {
  set.seed(9231)
  test_data <- create_test_df(25)
  test_data$ANCESTRY <- rep("EUR", 25)

  study <- methods::new("study", 
                        endpt="I9_VTE",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8, 
                        ancs="EUR")
 # Expect no error
 expect_error(suppressMessages(get_study_elig_indv(test_data, study)),
              regexp=NA)
})


test_that("get_study_elig_indv unkown cols works", {
  set.seed(9231)
  test_data <- create_test_df(25)
  test_data$ANCESTRY <- rep("EUR", 25)

  study <- methods::new("study", 
                        endpt="F5_DEPRESSIO",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8, 
                        ancs="EUR")
 expect_error(suppressMessages(get_study_elig_indv(test_data, study)))
})

# test_that("get_study_elig_indv log messages work", {
#   set.seed(9231)
#   test_data <- create_test_df(500)

#   get_study_elig_indv(test_data, 
#                       endpt="I9_VTE", 
#                       write_res=TRUE,
#                       res_dir = "/home/kira/duni/helsinki/DSGE/Code/onset_prediction/Istudy/results/")
# })