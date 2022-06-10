library(tibble)
library(lubridate)

test_that("add_study_interval_cols works for tibbles", {
  test_df <- tibble(DATE_OF_BIRTH=c(as.Date("1993/03/01"), as.Date("1978/08/06")),
                    START_OF_FOLLOWUP=c(as.Date("1993/03/01"), as.Date("1978/08/06")),
                    END_OF_FOLLOWUP=c(as.Date("2022/01/07"), as.Date("2022/07/06")))
  expected_df <- tibble::add_column(test_df, 
                                    FOLLOWUP=c(as.Date("1993/03/01") %--% as.Date("2022/01/07"),
                                               as.Date("1978/08/06") %--% as.Date("2022/07/06")),
                                    ENDPT_FREE=c(as.Date("1993/03/01") %--% as.Date("2015/03/01"),
                                                 as.Date("1978/08/06") %--% as.Date("2000/08/06")),
                                    STUDY_TIME=c(as.Date("2003/03/01") %--% as.Date("2023/03/01"),
                                                 as.Date("1988/08/06") %--% as.Date("2008/08/06")))
  res_df = add_study_interval_cols(test_df, exp_age=10, exp_len=10, wash_len=2, obs_len=8)
  expect_equal(res_df$FOLLOWUP, expected_df$FOLLOWUP)
  expect_equal(res_df$ENDPT_FREE, expected_df$ENDPT_FREE)
  expect_equal(res_df$STUDY_TIME, expected_df$STUDY_TIME)
  expect_equal(class(res_df), class(test_df))

})

test_that("add_study_interval_cols works equally for data.frames", {
  test_df <- data.frame(DATE_OF_BIRTH=c(as.Date("1993/03/01")),
                        START_OF_FOLLOWUP=c(as.Date("1993/03/01")),
                        END_OF_FOLLOWUP=c(as.Date("2022/01/07")))
  expected_df <- tibble::add_column(test_df, 
                                    FOLLOWUP=c(as.Date("1993/03/01") %--% as.Date("2022/01/07")),
                                    ENDPT_FREE=c(as.Date("1993/03/01") %--% as.Date("2015/03/01")),
                                    STUDY_TIME=c(as.Date("2003/03/01") %--% as.Date("2023/03/01")))
  
  res_df = add_study_interval_cols(test_df, exp_age=10, exp_len=10, wash_len=2, obs_len=8)
  expect_equal(res_df$FOLLOWUP, expected_df$FOLLOWUP)
  expect_equal(res_df$ENDPT_FREE, expected_df$ENDPT_FREE)
  expect_equal(res_df$STUDY_TIME, expected_df$STUDY_TIME)
  expect_equal(class(res_df), class(test_df))
})

