test_that("join_endpts_indvs_mats works", {
  prev_endpt <- tibble::tibble(ID=c("1","2","3","4","5","6"),
                               J10_ASTHMA=c(0,1,1,1,0,1))
  crnt_endpt <- tibble::tibble(ID=c("1","3","4","6","7","8"),
                               J10_ASTHMA_crnt=c(1,1,0,1,0,1))
  endpt_mat <- join_endpts_indvs_mats(crnt_endpt, endpt="J10_ASTHMA", prev_endpt, set_nas_true=TRUE)

  expect_mat <- tibble::tibble(ID=as.character(1:8),
                               J10_ASTHMA=c(FALSE,TRUE,TRUE,FALSE,FALSE, TRUE,FALSE, TRUE))
  expect_equal(endpt_mat, expect_mat)
})

test_that("join_endpts_indvs_mats works", {
  crnt_endpt <- tibble::tibble(ID=c("1","3","4","6","7","8"),
                               J10_ASTHMA_crnt=c(1,1,0,1,0,1))
  endpt_mat <- join_endpts_indvs_mats(crnt_endpt, endpt="J10_ASTHMA", set_nas_true=TRUE)
  expect_mat <- tibble::tibble(ID=c("1","3","4","6","7","8"),
                               J10_ASTHMA=c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE))
  expect_equal(endpt_mat, expect_mat)
})
