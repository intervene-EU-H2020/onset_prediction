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
                        obs_end_date=as.Date("2019-01-01"))
    date_expect <- c(as.Date("2019/01/01"))
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