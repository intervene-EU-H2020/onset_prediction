plot_hrs <- function(coxph_hr_res,
                     surv_ana) {
    if(surv_ana@study@study_type == "forward") {
        plt <- plot_age_hrs(coxph_hr_res, surv_ana)
    } else {
        plt <- plot_endpt_hrs(coxph_hr_res, surv_ana)
    }
}

plot_endpt_hrs <- function(coxph_hr_res,
                           surv_ana) {
    if(nrow(coxph_hr_res) > 0) {
        top_group <- dplyr::filter(coxph_hr_res, GROUP == get_comp_group(surv_ana@score_type, surv_ana@bin_cut))
        plt <- ggplot2::ggplot(top_group,
                        aes(y=ENDPOINT, x=HR)) +
                        geom_point(show.legend = FALSE) +
                        labs(title=paste0(get_comp_descr(surv_ana, surv_type="HR")),
                            caption=paste0("Obs: ", surv_ana@study@obs_len, " Years until ", surv_ana@study@obs_end, " Wash: ", surv_ana@study@wash_len, " Years", get_surv_descr(surv_ana, surv_type="HR")),
                            x="Hazard Ratio (95%)",
                            y="") +
                        theme_minimal() +
                        coord_cartesian(xlim=c(0, 7.1)) +
                        geom_errorbar(aes(xmin=CI_NEG, xmax=CI_POS), width=0.2) +
                        geom_vline(xintercept = 1.0)+
                        theme(text=element_text(size=21))

        file_path <- check_and_get_file_path(surv_ana, res_type="HRs")
        if(!is.null(file_path) & !is.null(plt)) {
            ggsave(file_path,
                width=12,
                height=7,
                plot=plt, 
                device="png", 
                bg="white")
        }
    }
}

plot_age_hrs <- function(coxph_hr_res,
                         surv_ana) {
                            
    coxph_hr_res <- dplyr::filter(coxph_hr_res, !is.na(HR))
    comp_group <- get_comp_group(surv_ana@score_type)
    endpts <- unique(coxph_hr_res$ENDPOINT)
    for(endpt in endpts) {
        surv_ana@study@endpt <- endpt # Cheating the system for easier plotting here
        endpt_coxph_hr_res <- dplyr::filter(coxph_hr_res,
                                            GROUP == comp_group,
                                            ENDPOINT == endpt)
        if(nrow(endpt_coxph_hr_res) > 0) {
            plt <- ggplot2::ggplot(endpt_coxph_hr_res, aes(x=as.character(EXP_AGE), y=HR)) +
                    labs(subtitle=paste0(get_comp_descr(surv_ana, surv_type="HR")),
                         title=endpt,
                         caption=paste0("Exp: ", surv_ana@study@exp_len, " Wash: ", surv_ana@study@wash_len, " Obs: ",  surv_ana@study@obs_len, " Years", get_surv_descr(surv_ana, surv_type="HR")),
                         x="Age",
                         y="Hazard Ratio (95% CI)") +
                    geom_point() + 
                    geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS), width=.1) +
                    geom_hline(yintercept=1.0) + 
                    coord_cartesian(ylim=c(-1,10)) +
                    theme_minimal() +
                    theme(text=element_text(size=21),
                          plot.caption=element_text(size=10, hjust=0))

            file_path <- check_and_get_file_path(surv_ana, res_type="HRs")
            if(!is.null(file_path) & !is.null(plt)) {
                ggsave(file_path,
                    width=7,
                    height=7,
                    plot=plt, 
                    device="png", 
                    bg="white")
            }
        }
    }
    return(NULL)
}
