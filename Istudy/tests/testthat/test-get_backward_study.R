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
  elig_indv <- get_study_elig_indv(study)
  date_expect <- c(as.Date("2019/11/24"), as.Date("2019/12/21"), as.Date("2019/12/21"), as.Date("2019/12/21"), as.Date("2019/12/21"), as.Date("2014/06/13"))
  expect_equal(elig_indv$I9_VTE_DATE, date_expect)
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
  elig_indv <- get_study_elig_indv(study)
  date_expect <- rep(as.Date("2004/12/21"), 6)
  expect_equal(elig_indv$EXP_START_DATE, date_expect)
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
  elig_indv <- get_study_elig_indv(back_study)

  date_expect <- c(as.Date("2019/11/24"), rep(as.Date("2019-12-21"), 3), as.Date("2014/06/13"))
  id_expect <- c("KT000002", "KT000005", "KT000006", "KT000008", "KT0000010")
  expect_equal(elig_indv$I9_VTE_DATE, date_expect)
  expect_equal(elig_indv$ID, id_expect)
})
