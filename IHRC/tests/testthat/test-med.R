test_that("calc_endpt_studies_hrs works", {
    set.seed(919923)

    pheno_data <- Istudy::create_test_df(10000)
    atc_data <- ICCI::create_test_atc_data(pheno_data$ID, n_samples=5000)
    expect_error(run_surv_studies(
                         pheno_data=pheno_data, 
                         atc_data=atc_data,
                         score_type=c("MED", "EDU"),
                         study_type="backward",
                         endpts=c("J10_ASTHMA", 
                                  "I9_VTE",
                                  "C3_BREAST", 
                                  "COVHOSP"),
                         exp_len=10,
                         wash_len=2,
                         obs_len=8,
                         ancs=NA_character_,
                         covs=c("SEX", "YEAR_OF_BIRTH"),
                         write_res=TRUE,
                         res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/"), regexp=NA)
 
})
