# test_that("read_prs_files works", {
#   expect_res <- tibble::tibble(ID=c("KT00001", "KT00002"),
#                  I9_CHD_PRS=c(3e-6, 1e-12),
#                  J10_ASTHMA_PRS=c(3e-6, 1e-12),
#                  K11_APPENDACUT_PRS=c(3e-6, 1e-12),
#                  C3_CANCER_PRS=c(3e-6, 1e-12))
#   expect_equal(read_prs_files("../data/PRS_R8/"), expect_res)
# })
