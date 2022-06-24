library(tibble)
library(lubridate)

test_that("add_study_interval_cols works for tibbles", {
  test_df <- tibble(DATE_OF_BIRTH=c(as.Date("1993/03/01"), as.Date("1978/08/06")))
  expected_df <- tibble::add_column(test_df, 
                                    ENDPT_FREE=c(as.Date("1993/03/01") %--% as.Date("2015/03/01"),
                                                 as.Date("1978/08/06") %--% as.Date("2000/08/06")),
                                    STUDY_TIME=c(as.Date("2003/03/01") %--% as.Date("2023/03/01"),
                                                 as.Date("1988/08/06") %--% as.Date("2008/08/06")))
  study <- methods::new("study", 
                endpt="J10_ASTHMA",
                exp_age=10,
                exp_len=10,
                wash_len=2,
                obs_len=8)
  res_df = add_study_interval_cols(test_df, study)
  expect_equal(res_df$ENDPT_FREE, expected_df$ENDPT_FREE)
  expect_equal(res_df$STUDY_TIME, expected_df$STUDY_TIME)
  expect_equal(class(res_df), class(test_df))

})

test_that("add_study_interval_cols works equally for data.frames", {
  test_df <- data.frame(DATE_OF_BIRTH=c(as.Date("1993/03/01")))
  expected_df <- tibble::add_column(test_df, 
                                    ENDPT_FREE=c(as.Date("1993/03/01") %--% as.Date("2015/03/01")),
                                    STUDY_TIME=c(as.Date("2003/03/01") %--% as.Date("2023/03/01")))
    study <- methods::new("study", 
                endpt="J10_ASTHMA",
                exp_age=10,
                exp_len=10,
                wash_len=2,
                obs_len=8)
  res_df = add_study_interval_cols(test_df, study)
  expect_equal(res_df$ENDPT_FREE, expected_df$ENDPT_FREE)
  expect_equal(res_df$STUDY_TIME, expected_df$STUDY_TIME)
  expect_equal(class(res_df), class(test_df))
})

