test_that("read_pheno_file correct file works", {
  file_path <- "../data/pheno_data.tsv"
  read_res <- read_pheno_file(file_path, endpts=c("J10_ASTHMA", "T2D", "C3_BREAST", "C3_PROSTATE", "COVHOSP"))
    expect_cols <- c("ID", "SEX", "DATE_OF_BIRTH","START_OF_FOLLOWUP", "END_OF_FOLLOWUP", "ANCESTRY",  "C3_BREAST", "C3_BREAST_DATE", "T2D", "T2D_DATE", "C3_PROSTATE", "C3_PROSTATE_DATE", "COVHOSP", "COVHOSP_DATE", "I9_VTE", "I9_VTE_DATE", "J10_ASTHMA",  "J10_ASTHMA_DATE", "PC1", "BATCH", "EDU", "PC2", "PC3", "PC4", "PC5", "PC6","PC7", "PC8", "PC9", "PC10")
    expect_equal(colnames(read_res), expect_cols)
})
