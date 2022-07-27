test_that("add_map_col simple test", {
  test_map <- tibble::tibble(ID = c("FG012", "FG0123", "FG012"), ID_num = c(1, 3, 1))
  test_data <- tibble::tibble(ID_num = c(3, 1), something = c("test1", "test2"))
  expect_res <- tibble::tibble(ID_num = c(3, 1),
                        something = c("test1", "test2"),
                        ID = c("FG0123", "FG012"))
  test_res <- add_map_col(test_data, test_map, "ID_num")
  expect_equal(test_res, expect_res)
})

test_that("add_map_col missing ID in data", {
  test_map <- tibble::tibble(ID = c("FG012", "FG0123", "FG012", "FG034"), ID_num = c(1, 3, 1, 2))
  test_data <- tibble::tibble(ID_num = c(3, 1), something = c("test1", "test2"))
  expect_res <- tibble::tibble(ID_num = c(3, 1),
                        something = c("test1", "test2"),
                        ID = c("FG0123", "FG012"))
  test_res <- add_map_col(test_data, test_map, "ID_num")
  expect_equal(test_res, expect_res)
})

test_that("add_map_col missing ID in map", {
  test_map <- tibble::tibble(ID = c("FG012", "FG0123", "FG012"), ID_num = c(1, 3, 1))
  test_data <- tibble::tibble(ID_num = c(3, 1, 2), something = c("test1", "test2", "test3"))
  expect_res <- tibble::tibble(ID_num = c(3, 1, 2),
                        something = c("test1", "test2", "test3"),
                        ID = c("FG0123", "FG012", NA))
  test_res <- add_map_col(test_data, test_map, "ID_num")
  expect_equal(test_res, expect_res)
})