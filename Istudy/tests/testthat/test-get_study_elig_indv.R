test_that("get_study_elig_indv works", {
  set.seed(9231)
    test_data <- create_test_df(25)
  # Removed because of early diagnosis
  test_data[test_data$ID == "KT000001",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT000001",]$J10_ASTHMA_DATE = as.Date("1930/01/01")

  # Case because of correct diagnosis time frame
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA_DATE = as.Date("1983/01/01")

  # REMOVE missing date case works
  test_data[test_data$ID == "KT0000016",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT0000016",]$J10_ASTHMA_DATE = NA

  study_setup <- methods::new("study_setup",
                        study_type="forward",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8,
                        exp_f1998=FALSE) 
  study <- methods::new("study",
                        study_data=test_data,
                        study_setup=study_setup,
                        endpt="J10_ASTHMA")
  expected_res_ids = c("KT000002", "KT000005", "KT000006", "KT000007", "KT000008", 
                       "KT000009", "KT0000013",  "KT0000014", "KT0000015", "KT0000017", "KT0000019",
                       "KT0000020", "KT0000021", "KT0000022", "KT0000023")
  expect_equal(study@study_data$ID, expected_res_ids)
})

test_that("study setup works", {
  set.seed(9231)
  test_data <- create_test_df(25)

  # Case
  test_data[test_data$ID == "KT000001",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT000001",]$J10_ASTHMA_DATE = as.Date("1945/01/01")

  # Removed because of early diagnosis
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA_DATE = as.Date("1949/01/01")

  # REMOVE missing date case works
  test_data[test_data$ID == "KT0000016",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT0000016",]$J10_ASTHMA_DATE = NA
  test_data$END_OF_FOLLOWUP[lubridate::year(test_data$DATE_OF_BIRTH) > 2000] <- as.Date("2020/01/01")

  study_setup <- methods::new("study_setup",
                        study_type="forward",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8,
                        exp_f1998=FALSE,
                        ancs=NA_character_) 
  study <- methods::new("study",
                        study_data=test_data,
                        study_setup=study_setup,
                        endpt="J10_ASTHMA")
  true_res <- readr::read_delim("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/Istudy/tests/true_res/study_test_results.tsv", delim="\t", show_col_types = FALSE) %>% dplyr::select(-ENDPT_FREE_PERIOD, -STUDY_TIME)

  expect_equal(study@study_data$ID, true_res$ID)
  expect_equal(study@study_data$EXP_START_DATE, true_res$EXP_START_DATE)
})

test_that("get_study_elig_indv adj case control works", {
  set.seed(9231)
  test_data <- create_test_df(25)
  test_data$ANCESTRY <- rep("EUR", 25)
  test_data$END_OF_FOLLOWUP[lubridate::year(test_data$DATE_OF_BIRTH) > 2000] <- as.Date("2020/01/01")

  # Case control adjustment tests
  # Control because of late diagnosis
  test_data[test_data$ID == "KT0000022",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT0000022",]$J10_ASTHMA_DATE = as.Date("2021/01/01")
  # Case because of correct diagnosis time frame
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT000007",]$J10_ASTHMA_DATE = as.Date("1983/01/01")
  test_data[test_data$ID == "KT0000013",]$J10_ASTHMA = 1 
  test_data[test_data$ID == "KT0000013","J10_ASTHMA_DATE"] = as.Date("1956/01/01")

  study_setup <- methods::new("study_setup",
                        study_type="forward",
                        exp_age=30,
                        exp_len=10,
                        wash_len=2,
                        obs_len=8,
                        exp_f1998=FALSE,
                        ancs="EUR") 
  study <- methods::new("study",
                        study_data=test_data,
                        study_setup=study_setup,
                        endpt="J10_ASTHMA")
  expected_res = c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)  
  expect_equal(study@study_data$J10_ASTHMA, expected_res)
})



# test_that("get_study_elig_indv unkown cols works", {
#   set.seed(9231)
#   test_data <- create_test_df(25)
#   study <- expect_error(methods::new("study", 
#                         study_type="forward",
#                         study_data=test_data,
#                         endpt="F5_DEPRESSIO",
#                         exp_age=30,
#                         exp_len=10,
#                         wash_len=2,
#                         obs_len=8))
# })