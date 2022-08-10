test_that("get_backward_study from birth works", {
  set.seed(21923)
  test_data <- create_test_df(10)

  study <- methods::new("study",
                        study_type="backward",
                        study_data=test_data,
                        endpt="I9_VTE",
                        exp_age=0,
                        wash_len=2,
                        obs_len=8,
                        obs_end_date=as.Date("2019-12-21"))
  date_expect <- c(as.Date("2019/11/24"), as.Date("2019/12/21"), as.Date("2019/12/21"), as.Date("2016-04-28"), as.Date("2019-08-12"), as.Date("2014-06-13"))
  expect_equal(study@study_data$I9_VTE_DATE, date_expect)
})

test_that("get_backward_study set exp len works", {
  set.seed(21923)
  test_data <- create_test_df(10)
  study <- methods::new("study",
                        study_type="backward",
                        study_data=test_data,
                        endpt="I9_VTE",
                        exp_len=5,
                        wash_len=2,
                        obs_len=8,
                        obs_end_date=as.Date("2019-12-21"))
  id_expect <- c("KT000002", "KT000005", "KT000006", "KT000008", "KT00000010")
  date_expect <- rep(as.Date("2004/12/21"), 5)
  expect_equal(study@study_data$EXP_START_DATE, date_expect)
})

test_that("filter_too_old_and_young works", {
  set.seed(21923)
  test_data <- create_test_df(10)
  test_data[test_data$ID == "KT000004", "DATE_OF_BIRTH"] <- as.Date("2020/01/01")

  back_study <- methods::new("study",
                        study_type="backward",
                        study_data=test_data,
                        endpt="I9_VTE",
                        exp_age=0,
                        wash_len=2,
                        obs_len=8,
                        obs_end_date=as.Date("2019-12-21"))

  date_expect <- c(as.Date("2019/11/24"), as.Date("2019/12/21"), as.Date("2016-04-28"), as.Date("2019-08-12"), as.Date("2014-06-13"))
  id_expect <- c("KT000002", "KT000005", "KT000006", "KT000008", "KT0000010")
  expect_equal(back_study@study_data$I9_VTE_DATE, date_expect)
  expect_equal(back_study@study_data$ID, id_expect)
})
