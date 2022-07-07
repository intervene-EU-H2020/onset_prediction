create_prs_test <- function(n_indv=25, indv_ids=NA_character_) {
    if(all(is.na(indv_ids)))
        indv_ids <- paste0("KT00000", seq(n_indv))
    tibble::tibble(ID=indv_ids,
                   J10_ASTHMA_PRS=rnorm(n_indv, 0, 0.01), 
                   I9_VTE_PRS=rnorm(n_indv, 0, 0.01),  
                   C3_BREAST_PRS=rnorm(n_indv, 0, 0.01),
                   COVHOSP_PRS=rnorm(n_indv, 0, 0.01))
}

test_that("calc_endpt_studies_hrs for prs works", {
    set.seed(919923)

    pheno_data <- Istudy::create_test_df(10000)
    test_data <- create_prs_test(length(pheno_data$ID), pheno_data$ID)
    exp_ages <- c(20, 30, 40)

    run_age_exp_endpt_studies(pheno_data, 
                              test_data,
                              score_type="PRS",
                              exp_ages=exp_ages,
                              exp_len=10,
                              wash_len=2,
                              obs_len=8,
                              endpts=c("J10_ASTHMA", 
                                       "I9_VTE",
                                       "C3_BREAST", 
                                       "COVHOSP"),
                              downsample_fctr=4,
                              ancs="EUR",
                              covs=c("SEX", "YEAR_OF_BIRTH"),
                              write_res=TRUE,
                              res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/")

})



