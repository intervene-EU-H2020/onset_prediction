test_that("read_phers_files works", {
  phers_data <- read_phers_files("../data/PheRS_R8/")
  phers_res_vec <- c(0.2, 0.9, 0.7, 0.3, 0.001, 0.9, 0.5, 0.4)
  expect_res <- tibble::tibble(ID=paste0("KT0000", 1:8),
                               C3_PROSTATE_PheRS=phers_res_vec,
                               I9_CHD_PheRS=phers_res_vec,
                               C3_BREAST_PheRS=phers_res_vec,
                               J10_ASTHMA_PheRS=phers_res_vec,
                               K11_APPENDACUT_PheRS=phers_res_vec)
  expect_equal(phers_data, expect_res)
})
