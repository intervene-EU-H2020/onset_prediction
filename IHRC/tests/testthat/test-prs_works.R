create_prs_test <- function(n_indv=25, indv_ids=NA_character_) {
    if(all(is.na(indv_ids)))
        indv_ids <- paste0("KT00000", seq(n_indv))
    tibble::tibble(ID=indv_ids,
                   J10_ASTHMA_PRS=rnorm(n_indv, 0, 0.01), 
                   I9_VTE_PRS=rnorm(n_indv, 0, 0.01),  
                   C3_BREAST_PRS=rnorm(n_indv, 0, 0.01),
                   COVHOSP_PRS=rnorm(n_indv, 0, 0.01))
}

get_prs_score_age_data <- function(test_data,
                                   exp_ages=c(20,30,40,50)) {
   score_age_data <- c()
   for(exp_age in exp_ages) {
      score_age_data[[exp_age]] <- test_data
   }
   return(score_age_data)
}

test_that("calc_studies_hrs works", {
    set.seed(919923)

    pheno_data <- Istudy::create_test_df(100000)
    test_data <- create_prs_test(length(pheno_data$ID), pheno_data$ID)
    exp_ages <- c(20, 30, 40)
    score_ages_data <- get_prs_score_age_data(test_data, exp_ages)

    run_age_exp_studies(pheno_data, 
                        score_ages_data,
                        score_col_name="PRS", 
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
                        write_res=TRUE,
                        res_dir="/home/kira/duni/helsinki/DSGE/Code/onset_prediction/IHRC/tests/results/")

})



