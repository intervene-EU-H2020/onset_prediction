get_elig_pheno_score_data <- function(pheno_data,
                                      score_data,
                                      score_type,
                                      study,
                                      min_indvs=5,
                                      write_res=FALSE,
                                      res_dir=NULL) {
    elig_indv <- Istudy::get_study_elig_indv(pheno_data=pheno_data,
                                             study=study,
                                             write_res=write_res,
                                             res_dir=res_dir)
    if(Istudy::get_n_cases(elig_indv, study@endpt) > min_indvs &
        Istudy::get_n_cntrls(elig_indv, study@endpt) > min_indvs) {
        pheno_score_data <- join_dfs(pheno_data=elig_indv, 
                                     score_data=score_data,
                                     score_type=score_type,
                                     endpt=study@endpt)
        return(pheno_score_data)
    } else {
        return(NULL)
    }
}

get_study_coxph_mdl <- function(pheno_data,
                                score_data,
                                score_type,
                                study,
                                covs,
                                pred_score,
                                bin_cut=1,
                                write_res=FALSE,
                                res_dir=NULL) {
    pheno_score_data <- get_elig_pheno_score_data(pheno_data=pheno_data,
                                                  score_data=score_data,
                                                  score_type=score_type,
                                                  study=study,
                                                  write_res=write_res,
                                                  res_dir=res_dir)
    if(!is.null(pheno_score_data)) {
        if(pred_score == "SCORE_GROUP") {
            pheno_score_data <- add_risk_group_col(
                                    score_data=pheno_score_data,
                                    score_type=score_type,
                                    study=study,
                                    bin_cut=bin_cut,
                                    write_res=write_res,
                                    res_dir=res_dir)
        } 
        coxph_mdl <- get_coxph_mdl(pheno_score_data=pheno_score_data,
                                   endpt=study@endpt,
                                   covs=covs,
                                   pred_score=pred_score)
        return(list(mdl=coxph_mdl, data=pheno_score_data))
    } else {
        return(NULL)
    }
}