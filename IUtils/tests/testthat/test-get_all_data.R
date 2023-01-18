test_that("get_all_data works", {
    pheno_file_path <- "../data/pheno_data.tsv"
    icd_file_path <- "../data/icd_file_correct.tsv"
    prs_dir_path <- "../data/PRS_R8/"
    phers_dir_path <- "../data/PheRS_R8/"

    pheno_expect_cols <- c("ID", "SEX", "DATE_OF_BIRTH","START_OF_FOLLOWUP", "END_OF_FOLLOWUP", "ANCESTRY",  "C3_BREAST", "C3_BREAST_DATE", "T2D", "T2D_DATE", "C3_PROSTATE", "C3_PROSTATE_DATE", "COVHOSP", "COVHOSP_DATE", "I9_VTE", "I9_VTE_DATE", "J10_ASTHMA",  "J10_ASTHMA_DATE", "PC1", "BATCH", "ISCED_2011", "PC2", "PC3", "PC4", "PC5", "PC6","PC7", "PC8", "PC9", "PC10")
    icd_expect <- tibble::tibble(ID="KT00001",
                                 Event_age=12.4,
                                 ICD_version="10",
                                 primary_ICD="HJ912",
                                 secondary_ICD="LKJ23") 
    prs_expect <- tibble::tibble(ID=c("KT00001", "KT00002"),
                                J10_ASTHMA_PRS=c(3e-6, 1e-12))
    phers_res_vec <- c(0.2, 0.9, 0.7, 0.3, 0.001, 0.9, 0.5, 0.4)
    phers_expect <- tibble::tibble(ID=paste0("KT0000", 1:8),
                               C3_PROSTATE_PheRS=phers_res_vec,
                               J10_ASTHMA_PheRS=phers_res_vec)

    res <- get_all_data(score_type=c("CCI", "PRS", "PheRS"),
                        endpts=c("J10_ASTHMA", "C3_PROSTATE"),
                        pheno_file_path=pheno_file_path,
                        icd_file_path=icd_file_path,
                        prs_dir_path=prs_dir_path,
                        prs_endpts_map=tibble::tibble(endpt=c("J10_ASTHMA", "C3_PROSTATE"),
                                               prs=c("Asthma", "Prostate_Cancer")),
                        phers_dir_path=phers_dir_path)
    expect_equal(colnames(res$pheno), pheno_expect_cols)
    expect_equal(res$icd, icd_expect)
    expect_equal(res$prs, prs_expect)
    expect_equal(res$phers, phers_expect)
})

test_that("get_all_data throws error for missing path", {
    pheno_file_path <- "../data/pheno_data.tsv"
    icd_file_path <- "../data/icd_file_correct.tsv"
    phers_dir_path <- "../data/PheRS_R8/"

    expect_error(get_all_data(score_type=c("CCI", "PRS", "PheRS"),
                        endpts=c("J10_ASTHMA", "C3_PROSTATE"),
                        pheno_file_path=pheno_file_path,
                        icd_file_path=icd_file_path,
                        phers_dir_path=phers_dir_path))
})

test_that("get_all_data EDU works", {
    pheno_file_path <- "../data/pheno_data.tsv"

    expect_error(get_all_data(score_type=c("EDU"),
                                endpts=c("J10_ASTHMA", "C3_PROSTATE"),
                                pheno_file_path=pheno_file_path), NA)
})

