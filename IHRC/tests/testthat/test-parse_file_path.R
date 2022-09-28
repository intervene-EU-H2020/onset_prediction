test_that("parse_file_path backward no down no filter works", {
  ana_details <- parse_file_path("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/no_down/all/coxph/backward/2021-01-01_o8_w2_e10_CCI_PRS_CCIiPRS_CCIiYOB_SEX_YOB_coxph.tsv")

  expect_details <- list()
  expect_details$res_dir <- "/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/no_down/all/"
  expect_details$down_fctr <- NA_integer_
  expect_details$filter_1998 <- FALSE
  expect_details$study_type <- "backward"
  expect_details$obs_end_date <- as.Date("2021/01/01")
  expect_details$endpt <- "None"
  expect_details$exp_len <- 10
  expect_details$wash_len <- 2
  expect_details$obs_len <- 8
  expect_details$preds <- c("CCI", "PRS", "CCI*PRS", "CCI*YEAR_OF_BIRTH", "SEX", "YEAR_OF_BIRTH")

  expect_equal(ana_details, expect_details)
})


test_that("parse_file_path backward no down filter 1998 works", {
  ana_details <- parse_file_path("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/no_down/f1998/coxph/backward/2021-01-01_o8_w2_e10_CCI_PRS_CCIiPRS_CCIiYOB_SEX_YOB_coxph.tsv")

  expect_details <- list()
  expect_details$res_dir <- "/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/no_down/f1998/"
  expect_details$down_fctr <- NA_integer_
  expect_details$filter_1998 <- TRUE
  expect_details$study_type <- "backward"
  expect_details$obs_end_date <- as.Date("2021/01/01")
  expect_details$endpt <- "None"
  expect_details$exp_len <- 10
  expect_details$wash_len <- 2
  expect_details$obs_len <- 8
  expect_details$preds <- c("CCI", "PRS", "CCI*PRS", "CCI*YEAR_OF_BIRTH", "SEX", "YEAR_OF_BIRTH")

  expect_equal(ana_details, expect_details)
})

test_that("parse_file_path backward no down filter 1998 works", {
  ana_details <- parse_file_path("/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/no_down/f1998/coxph/forward/J10_ASTHMA_e10_w2_o8_CCI_PRS_CCIiPRS_CCIiYOB_SEX_YOB_coxph.tsv")

  expect_details <- list()
  expect_details$res_dir <- "/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/no_down/f1998/"
  expect_details$down_fctr <- NA_integer_
  expect_details$filter_1998 <- TRUE
  expect_details$study_type <- "forward"
  expect_details$obs_end_date <- as.Date("2021/01/01")
  expect_details$endpt <- "J10_ASTHMA"
  expect_details$exp_len <- 10
  expect_details$wash_len <- 2
  expect_details$obs_len <- 8
  expect_details$preds <- c("CCI", "PRS", "CCI*PRS", "CCI*YEAR_OF_BIRTH", "SEX", "YEAR_OF_BIRTH")

  expect_equal(ana_details, expect_details)
})