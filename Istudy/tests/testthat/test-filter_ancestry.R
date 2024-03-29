test_that("filter_ancestry works", {
  set.seed(193)
  test_data <- create_test_df(6)
  res_data <- filter_ancestry(test_data, 
                              ancs="EUR")
  expect_ids <- c("KT000001", "KT000003")
  expect_equal(res_data$ID, expect_ids)
})

test_that("filter_ancestry works", {
  set.seed(193)
  test_data <- create_test_df(6)
  res_data <- filter_ancestry(test_data, 
                              ancs=c("EUR", "AMR"))
  expect_ids <- c("KT000001", "KT000002", "KT000003",
                  "KT000004", "KT000005", "KT000006")
  expect_equal(expect_ids, res_data$ID)

    res_data <- filter_ancestry(test_data, 
                              ancs=NA_character_)
  expect_ids <- c("KT000001", "KT000002", "KT000003",
                  "KT000004", "KT000005", "KT000006")
  expect_equal(expect_ids, res_data$ID)
})
