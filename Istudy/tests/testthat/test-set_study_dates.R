library(tibble)
library(lubridate)

test_that("set_study_dates forward works", {
  test_df <- create_test_df(2)
  test_df$DATE_OF_BIRTH <- c(as.Date("1993/01/01"), as.Date("1978/08/06"))
  expect_res <- tibble::add_column(test_df, 
                                    ENDPT_FREE_PERIOD=c(as.Date("1993/01/01") %--% as.Date("2014/12/31"),
                                                 as.Date("1978/08/06") %--% as.Date("2000/08/05")),
                                    STUDY_TIME=c(as.Date("2003/01/01") %--% as.Date("2023/01/01"),
                                                 as.Date("1988/08/06") %--% as.Date("2008/08/06")))
  res_df <- set_study_dates(test_df, study_type="forward", exp_age=10, exp_len=10)

  expect_equal(res_df$ENDPT_FREE_PERIOD, expect_res$ENDPT_FREE_PERIOD)
  expect_equal(res_df$STUDY_TIME, expect_res$STUDY_TIME)
  expect_equal(class(res_df), class(test_df))

})

test_that("set_study_dates backwards works", {
  test_df <- create_test_df(2)
  test_df$DATE_OF_BIRTH <- c(as.Date("1993/01/01"), as.Date("1978/08/06"))
  expect_res <- tibble::add_column(test_df, 
                                    ENDPT_FREE_PERIOD=c(as.Date("1993/01/01") %--% as.Date("2010/12/31"),
                                                 as.Date("1978/08/06") %--% as.Date("2010/12/31")),
                                    STUDY_TIME=c(as.Date("1999/01/01") %--% as.Date("2019/01/01"),
                                                 as.Date("1999/01/01") %--% as.Date("2019/01/01")))
  res_df <- set_study_dates(test_df, study_type="backward", exp_len=10, obs_end_date=as.Date("2019/01/01"))

  expect_equal(res_df$ENDPT_FREE_PERIOD, expect_res$ENDPT_FREE_PERIOD)
  expect_equal(res_df$STUDY_TIME, expect_res$STUDY_TIME)
  expect_equal(class(res_df), class(test_df))

})

test_that("set_study_dates backwards detailed works", {
  test_df <- create_test_df(1)
  test_df$DATE_OF_BIRTH <- c(as.Date("1993/01/01"))
  
  expect_dates <- tibble::tibble(
                    EXP_START_DATE=as.Date("1999/01/01"), EXP_END_DATE=as.Date("2008/12/31"),
                    WASH_START_DATE=as.Date("2009/01/01"), WASH_END_DATE=as.Date("2010/12/31"),
                    OBS_START_DATE=as.Date("2011/01/01"), OBS_END_DATE=as.Date("2019/01/01"))
  res_df <- set_study_dates(test_df, study_type="backward", exp_len=10, obs_end_date=as.Date("2019/01/01"))

  dates_unique <- dplyr::select(res_df,
                                EXP_START_DATE, EXP_END_DATE,
                                WASH_START_DATE, WASH_END_DATE,
                                OBS_START_DATE, OBS_END_DATE) 
  expect_equal(dates_unique, expect_dates)
})