library(tibble)
library(lubridate)

test_that("set_study_dates works for tibbles", {
  test_df <- create_test_df(2)
  test_df$DATE_OF_BIRTH <- c(as.Date("1993/03/01"), as.Date("1978/08/06"))
  test_df$J10_ASTHMA <- 
  expect_res <- tibble::add_column(test_df, 
                                    ENDPT_FREE_PERIOD=c(as.Date("1993/03/01") %--% as.Date("2015/03/01"),
                                                 as.Date("1978/08/06") %--% as.Date("2000/08/06")),
                                    STUDY_TIME=c(as.Date("2003/03/01") %--% as.Date("2023/03/01"),
                                                 as.Date("1988/08/06") %--% as.Date("2008/08/06")))
  res_df <- set_study_dates(test_df, exp_age=10, exp_len=10)

  expect_equal(res_df$ENDPT_FREE_PERIOD, expect_res$ENDPT_FREE_PERIOD)
  expect_equal(res_df$STUDY_TIME, expect_res$STUDY_TIME)
  expect_equal(class(res_df), class(test_df))

})