test_that("get_backward_study works", {
  set.seed(21923)
  test_data <- create_test_df(10)
  study <- get_backward_study(test_data, "I9_VTE")
  elig_indv <- get_study_elig_indv(test_data,
                                   study)
  date_expect <- c(as.Date("2019/11/24"), as.Date("2019/12/21"), as.Date("2019/12/21"), as.Date("2019/12/21"), as.Date("2019/12/21"), as.Date("2014/06/13"))
  expect_equal(elig_indv$I9_VTE_DATE, date_expect)
})

test_that("filter_too_old_and_young works", {
  set.seed(21923)
  test_data <- create_test_df(10)
  test_data[test_data$ID == "KT000004", "DATE_OF_BIRTH"] <- as.Date("2020/01/01")
  back_study <- get_backward_study(test_data, "I9_VTE")
  elig_indv <- get_study_elig_indv(test_data, back_study)
  date_expect <- c(as.Date("2019/11/24"), rep(get_max_date(test_data), 3), as.Date("2014/06/13"))
  id_expect <- c("KT000002", "KT000005", "KT000006", "KT000008", "KT0000010")
  expect_equal(elig_indv$I9_VTE_DATE, date_expect)
  expect_equal(elig_indv$ID, id_expect)
})
