test_that("filter_ancestry works", {
  set.seed(193)
  test_data <- create_test_df(6)
  res_data <- filter_ancestry(test_data, 
                              ancs="EUR")
  expect_ids <- c("KT000002", "KT000004")
  expect_equal(expect_ids, res_data$ID)
    print(test_data %>% dplyr::select(ID, ANCESTRY), n=10)
    print(res_data %>% dplyr::select(ID, ANCESTRY), n=10)

})

test_that("filter_ancestry works", {
  set.seed(193)
  test_data <- create_test_df(6)
  res_data <- filter_ancestry(test_data, 
                              ancs=c("EUR", "AMR"))
  expect_ids <- c("KT000001", "KT000002", "KT000003",
                  "KT000004", "KT000005", "KT000006")
  expect_equal(expect_ids, res_data$ID)
})
