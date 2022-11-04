test_that("parse_file_path backward no down no filter works", {
  ana_details <- parse_file_path("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/no_down/backward/coxph/2021-01-01_o8_w2_e10_CCI_PRS_CCIiPRS_CCIiYOB_SEX_YOB_coxph.tsv")

  expect_details <- list()
  expect_details$res_dir <- "/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/no_down/"
  expect_details$down_fctr <- NA_integer_
  expect_details$study_type <- "backward"
  expect_details$obs_end_date <- as.Date("2021-01-01")
  expect_details$endpt <- "None"
  expect_details$exp_len <- 10
  expect_details$wash_len <- 2
  expect_details$obs_len <- 8
  expect_details$preds <- c("CCI", "PRS", "CCI*PRS", "CCI*YEAR_OF_BIRTH", "SEX", "YEAR_OF_BIRTH")

  expect_equal(ana_details, expect_details)
})

