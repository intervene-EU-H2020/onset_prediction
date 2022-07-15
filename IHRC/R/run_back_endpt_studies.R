#' @export
run_back_endpt_studies <- function(pheno_data,
                                   score_data,
                                   score_type,
                                   endpts=c("J10_ASTHMA", "I9_VTE"),
                                   wash_len=2,
                                   obs_len=8,
                                   obs_end=NULL,
                                   downsample_fctr=NULL,
                                   ancs=NA_character_,
                                   covs=c("SEX", "YEAR_OF_BIRTH"),
                                   bin_cut=1,
                                   min_indvs=5,
                                   write_res=FALSE,
                                   res_dir=NULL) {
    endpt_hrs_tib <- create_empty_endpt_hrs_tib()   
    c_idxs_tib <- get_empty_cidx_tib()

    for(endpt in endpts) {
        back_study <- Istudy::get_backward_study(
                                    pheno_data=pheno_data,
                                    endpt=endpt,
                                    wash_len=wash_len,
                                    obs_len=obs_len,
                                    obs_end=obs_end,
                                    downsample_fctr=downsample_fctr,
                                    ancs=ancs)
        if(is.null(obs_end)) {
            obs_end <- Istudy::get_max_date(pheno_data)
        }
        elig_indv <- Istudy::get_study_elig_indv(pheno_data=pheno_data,
                                                 study=back_study)

        if(score_type == "CCI") {
            curnt_score_data <- get_indv_exp_cci_scores(score_data,
                                                        elig_indv)  
        } else {
            curnt_score_data <- get_and_filter_endpt_scores(score_data,
                                                            score_type,
                                                            endpt)
        }
 
        elig_score_data <- dplyr::inner_join(elig_indv,
                                             curnt_score_data,
                                             by="ID")
        plot_endpt_score_distr(score_data=elig_score_data,
                               score_type=score_type,
                               study=back_study,
                               obs_end=obs_end,
                               min_indvs=min_indvs,
                               write_res=write_res,
                               res_dir=res_dir)
        coxph <- get_study_coxph_mdl(pheno_score_data=elig_score_data,
                                     score_type=score_type,
                                     study=back_study,
                                     covs=covs,
                                     pred_score="SCORE_GROUP",
                                     bin_cut=bin_cut,
                                     obs_end=obs_end,
                                     write_res=write_res,
                                     res_dir=res_dir)
        endpt_hrs_tib <- add_coxph_res_row(endpt_hrs_tib=endpt_hrs_tib,
                                           coxph_mdl=coxph$mdl,
                                           score_type=score_type,
                                           study=back_study,
                                           elig_indv=coxph$data,
                                           min_indvs=min_indvs)
        c_idx_res <- get_cidx(coxph$mdl, 
                              coxph$data, 
                              endpt)
        c_idxs_tib <- add_cidx_res_row(c_idxs_tib=c_idxs_tib, 
                                       c_idx_res=c_idx_res, 
                                       study=back_study,
                                       score_type=score_type,
                                       covs=covs,
                                       bin_cut=bin_cut)
    }
    write_res_files(endpt_hrs_tib=endpt_hrs_tib,
                    endpt_cidx_tib=c_idxs_tib,
                    score_type=score_type,
                    study=back_study,
                    covs=covs,
                    bin_cut=bin_cut,
                    obs_end=obs_end,
                    write_res=write_res,
                    res_dir=res_dir)
    return(list(hrs=endpt_hrs_tib,
                cidxs=c_idxs_tib))
}

get_indv_exp_cci_scores <- function(icd_data,
                                    elig_indv) {
    if("EXP_LEN" %in% colnames(elig_indv)) {
        elig_indv <- dplyr::rename(elig_indv, 
                                   EXP_END=EXP_LEN)
    }
    cci_data <- ICCI::calc_cci(icd_data,
                               exp_start=0,
                               exp_end=dplyr::select(elig_indv, ID, EXP_END)) %>%
                    dplyr::rename(SCORE=CCI_score)
    return(cci_data)
}