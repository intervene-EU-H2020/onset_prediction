test_that("add_num_id_col simple scenario", {
  test_data <- tibble::tibble(ID = c("indiv_1", "indiv_2", "indiv_3"))
  expect_res <- tibble::tibble(ID = c("indiv_1", "indiv_2", "indiv_3"),
                       ID_num = seq_len(3))
  test_res <- add_num_id_col(test_data)
  expect_equal(test_res, expect_res)
})

test_that("add_num_id_col different order", {
  test_data <- tibble::tibble(ID = c("indiv_1", "indiv_3",  "indiv_2"))
  expect_res <- tibble::tibble(ID = c("indiv_1", "indiv_3", "indiv_2"),
                       ID_num = seq_len(3))
  test_res <- add_num_id_col(test_data)
  expect_equal(test_res, expect_res)
})

test_that("add_num_id_col double entries", {
  test_data <- tibble::tibble(ID = c("indiv_1", "indiv_1", "indiv_3"))
  expect_res <- tibble::tibble(ID = c("indiv_1", "indiv_1", "indiv_3"),
                       ID_num = c(1, 1, 2))
  test_res <- add_num_id_col(test_data)
  expect_equal(test_res, expect_res)
})

test_that("add_num_id_col empty tibble::tibble", {
  test_data <- tibble::tibble(ID = character())
  expect_res <- tibble::tibble(ID = character(), ID_num = numeric())
  test_res <- add_num_id_col(test_data)
  expect_equal(test_res, expect_res)
})