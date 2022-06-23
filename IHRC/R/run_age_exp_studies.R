run_age_exp_studies <- function(pheno_data, 
                                score_data,
                                score_col_name,
                                score_type,
                                endpts,
                                exp_ages=c(20,30,40),
                                exp_len=10,
                                wash_len=2,
                                obs_len=8,
                                downsample_fctr=NA,
                                write_res=FALSE,
                                res_dir=NA) {
    score_data <- preprocess_score_data(score_data, 
                                        score_col_name)
    
    plt <- plot_score_distr(as.list(environment()))
    
    for(exp_age in exp_ages) {
        calc_endpt_hrs(pheno_data, 
                       score_data,
                       score_type,
                       endpts,
                       exp_age,
                       exp_len,
                       wash_len,
                       obs_len,
                       downsample_fctr,
                       write_res,
                       res_dir)
    }
}