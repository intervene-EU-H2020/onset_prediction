# test_that("get_endpts_ids works", {
#   endpts <- c("J10_ASTHMA", "I9_VTE", "C3_BREAST", "COVHOSP")
#   ids <- c("KT0001", "KT0002", "KT0003")
#   endpt_indvs_mat <- tibble::tibble(ID=ids,
#                                     J10_ASTHMA=c(0,1,1),
#                                     I9_VTE=c(1,1,0),
#                                     C3_BREAST=c(1,1,1),
#                                     COVHOSP=c(0,0,0))
#   expect_equal(get_endpt_ids(endpt_indvs_mat, "J10_ASTHMA", endpt_indvs_mat$ID),
#                c("KT0002", "KT0003"))
#   expect_equal(get_endpt_ids(endpt_indvs_mat, "I9_VTE", endpt_indvs_mat$ID),
#                c("KT0001", "KT0002"))
#   expect_equal(get_endpt_ids(endpt_indvs_mat, "C3_BREAST", endpt_indvs_mat$ID),
#                c("KT0001", "KT0002", "KT0003"))
#   expect_equal(get_endpt_ids(endpt_indvs_mat, "COVHOSP", endpt_indvs_mat$ID),
#                NULL)
# })
