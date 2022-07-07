test_that("plot_surv works", {
  if (requireNamespace("ICCI", quietly = TRUE)) {
    set.seed(919923)
    pheno_data <- Istudy::create_test_df(10000)
    icd_data <- ILongDataUtils::create_test_df_multi_icd_ver(n_icd10=500000, 
                                                             icd10_indv=pheno_data$ID)
    score_data <- ICCI::calc_cci(icd_data=icd_data, 
                                 exp_start=20, 
                                 exp_end=30) %>% 
                    dplyr::rename(SCORE=CCI_score)

    endpt_studies <- create_endpts_study_objs(endpts=c("J10_ASTHMA", "I9_VTE"), 
                                              exp_age=20,
                                              exp_len=10,
                                              wash_len=2,
                                              obs_len=8,
                                              downsample_fctr=4,
                                              ancs=unique(pheno_data$ANCESTRY))
        
    c_idxs <- calc_endpt_studies_cidxs(pheno_data=pheno_data,
                                       score_data=score_data,
                                       score_type="CCI",
                                       endpt_studies=endpt_studies,
                                       covs=c("SEX", "YEAR_OF_BIRTH"),
                                       write_res=TRUE,
                                       res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/")
  }
})
