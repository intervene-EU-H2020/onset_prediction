
plot_age_hrs <- function(coxph_hr_res,
                         surv_ana) {
                            
    coxph_hr_res <- dplyr::filter(coxph_hr_res, !is.na(HR))
    comp_group <- get_comp_group(surv_ana@score_type)
    endpts <- unique(coxph_hr_res$ENDPOINT)
    for(endpt in endpts) {
        endpt_coxph_hr_res <- dplyr::filter(coxph_hr_res,
                                            GROUP == comp_group,
                                            ENDPOINT == endpt)
        if(nrow(endpt_coxph_hr_res) > 0) {
            plt <- ggplot2::ggplot(endpt_coxph_hr_res, aes(x=as.character(EXP_AGE), y=HR)) +
                    labs(title=paste0(surv_ana@study@endpt),
                         subtitle=paste0("Exp: ", surv_ana@study@exp_len, " Wash: ", surv_ana@study@wash_len, " Obs: ",  surv_ana@study@obs_len, " Years"),
                         caption=get_surv_descr(surv_ana,
                                                surv_type="HR"),
                         x="Age",
                         y="Hazard Ratio (95% CI)") +
                    geom_point() + 
                    geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS), width=.1) +
                    geom_hline(yintercept=1.0) + 
                    coord_cartesian(ylim=c(-1,10)) +
                    theme_minimal() +
                    theme(text=element_text(size=21),
                          plot.caption=element_text(size=10, hjust=0))
            # Hacking the system a bit by putting random age that won't be 
            # used.
            file_path <- check_and_get_file_path(surv_ana,
                                                 res_type="HRs")
            if(!is.null(file_path)) {
                ggsave(file_path,
                    width=7,
                    height=7,
                    plot=plt, 
                    device="png", 
                    bg="white")
            }
        }
    }
}
