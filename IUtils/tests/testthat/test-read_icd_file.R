test_that("read_icd_files correct file works", {
  file_path <- "../data/icd_file_correct.tsv"

  true_res <- tibble::tibble(ID="KT00001",
                             Event_age=12.4,
                             ICD_version="10",
                             primary_ICD="HJ912",
                             secondary_ICD="LKJ23")
  read_res <- read_icd_file(file_path)
  expect_equal(read_res, true_res)
})

test_that("read_icd_files empty file works", {

  file_path <- "../data/icd_file_empty.tsv"
  read_res <- suppressWarnings(read_icd_file(file_path))
  true_res <- tibble::tibble(ID=character(),
                             Event_age=numeric(),
                             ICD_version=character(),
                             primary_ICD=character(),
                             secondary_ICD=character())
  expect_warning(read_icd_file(file_path))
  expect_equal(read_res, true_res)
})


test_that("read_icd_files missing secondary file works", {
  file_path <- "../data/icd_file_no_secondary.tsv"
  read_res <- read_icd_file(file_path)
  true_res <- tibble::tibble(ID="KT00001",
                             Event_age=12.4,
                             ICD_version="10",
                             primary_ICD="HJ912")
  expect_equal(read_res, true_res)
})

test_that("read_icd_files different but correct enough col names works", {
  file_path <- "../data/icd_file_correct_slighlty_different_cols.tsv"
  read_res <- read_icd_file(file_path)
  true_res <- tibble::tibble(ID="KT00001",
                             Event_age=12.4,
                             ICD_version="10",
                             primary_ICD="HJ912",
                             secondary_ICD="LKJ23")
  expect_equal(read_res, true_res)
})

test_that("read_icd_files wrong col names works", {
  file_path <- "../data/icd_file_wrong_cols.tsv"
  read_res <- suppressWarnings(read_icd_file(file_path))
  true_res <- tibble::tibble(ID="KT00001",
                             Event_age=12.4,
                             ICD_version="10",
                             primary_ICD="HJ912",
                             secondary_ICD="LKJ23")
  read_icd_file(file_path)
  expect_equal(read_res, true_res)
})


