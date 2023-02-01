# test_that("read_phers_files works", {
#   phers_data <- read_phers_files("../data/PheRS_R8/")
#   phers_res_vec <- c(0.2, 0.9, 0.7, 0.3, 0.001, 0.9, 0.5, 0.4)
#   expect_res <- tibble::tibble(ID=paste0("KT0000", 1:8),
#                                C3_PROSTATE_PheRS=phers_res_vec,
#                                I9_CHD_PheRS=phers_res_vec,
#                                C3_BREAST_PheRS=phers_res_vec,
#                                J10_ASTHMA_PheRS=phers_res_vec,
#                                K11_APPENDACUT_PheRS=phers_res_vec)
#   expect_equal(phers_data, expect_res)
# })

# test_that("read_phers_endpts_indvs_mats works", {
#   res <- read_phers_endpts_indvs_mat(dir_path="../data/PheRS_R8/",
#                                     indvs_ids = c(paste0("KT0000", 1:10)),
#                                     endpts=c("C3_BREAST"))
#   expect_res <- tibble::tibble(ID=c(paste0("KT0000", 1:10)),
#                                C3_BREAST=c(rep(FALSE, 4), rep(TRUE, 6)))
#   expect_equal(res, expect_res)
# })

# test_that("read_phers_endpts_indvs_mat with prev endpt_indv_mat works", {
#   prev_mat <- tibble::tibble(ID=c(paste0("KT0000", c(3,4,6,8,9,10))),
#                              C3_BREAST=c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
#   res <- read_phers_endpts_indvs_mat(dir_path="../data/PheRS_R8/",
#                                     indvs_ids = c(paste0("KT0000", 1:10)),
#                                     endpts=c("C3_BREAST"),
#                                     prev_endpts_indvs_mat=prev_mat)
#   expect_res <- tibble::tibble(ID=c(paste0("KT0000", 1:10)),
#                                C3_BREAST=c(rep(FALSE, 4), rep(TRUE, 4), FALSE, TRUE))
#   expect_equal(res, expect_res)
# })

