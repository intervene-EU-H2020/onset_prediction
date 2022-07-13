#' @export
create_ggsurvplot <- function(coxph_mdl,
                              pheno_score_data,
                              study,
                              score_type,
                              covs,
                              write_res,
                              res_dir) {

    c_idx <- get_cidx(coxph_mdl, pheno_score_data, study@endpt)
    obs_period <- Istudy::get_obs_period(study)
    plt <- survminer::ggadjustedcurves(
                fit=coxph_mdl,
                data=pheno_score_data,
                title = paste0("Age at Onset of ", study@endpt),
                subtitle = IHRC::get_study_subtitle(study),
                caption = paste0(get_surv_descr(score_type, surv_type="surv", covs), "  C-index: ", round(c_idx[1], 2)),
                #ggtheme = ggplot2::theme_minimal(),
                # ####### Confidence Intervals ########
                conf.int = TRUE, # To Remove conf intervals use "FALSE"
                # ####### Format Axes #######
                xlim = c(obs_period$start, obs_period$end),
                xlab="Age at Onset", # changes xlabel,
                ylab = "Probability",
                )

    file_path <- check_and_get_file_path(score_type=score_type,
                                         study=study,
                                         write_res=write_res,
                                         res_dir=res_dir,
                                         res_type="surv",
                                         covs=covs)
    if(!is.null(file_path)) {
        ggsave(file_path,
               width=7,
               height=7,
               plot=plt, 
               device="png", 
               bg="white")
    }
    
    return(plt)
}
