#' Plot the HR from the Cox-PH model
#' 
#' For forward study creates separate plots for each
#' endpoint for the different age studies. For 
#' the backward study creates a single plot with the
#' different endpoints.
#' 
#' @param coxph_hr_res A tibble. The Cox-PH HR results. Needs
#'                      to at least contain the columns `ENDPOINT`,
#'                      `HR`, `CI_NEG`, `CI_POS`, and `GROUP`.
#' @inheritParams add_risk_group_col
#' 
#' @export 
#' 
#' @author Kira E. Detrois
plot_hrs <- function(coxph_hr_res,
                     surv_ana) {
    if(surv_ana@study@study_type == "forward") {
        plt <- plot_age_hrs(coxph_hr_res, surv_ana)
    } else {
        plt <- plot_endpt_hrs(coxph_hr_res, surv_ana)
    }
}


#' Plots the HR from the Cox-PH model of a backwards study.
#'   
#' Creates a single plot with the different endpoints
#' 
#' @inheritParams plot_hrs
#' 
#' @export 
#' 
#' @author Kira E. Detrois
plot_endpt_hrs <- function(coxph_hr_res,
                           surv_ana) {
    if(nrow(coxph_hr_res) > 0) {
        top_group <- dplyr::filter(coxph_hr_res, GROUP == get_comp_group(surv_ana@score_type, surv_ana@bin_cut))
        plt <- ggplot2::ggplot(top_group,
                            # Plot basics
                            aes(y=ENDPOINT, x=HR)) +
                            geom_point(show.legend = FALSE) +
                            geom_errorbar(aes(xmin=CI_NEG, xmax=CI_POS), width=0.2) +
                            # Axis settings
                            coord_cartesian(xlim=c(0, 7.1)) +
                            geom_vline(xintercept = 1.0) +
                            # Legends and labels
                            labs(title=paste0(get_comp_descr(surv_ana, surv_type="HR")),
                                 caption=paste0("Obs: ", surv_ana@study@obs_len, " Years until ", surv_ana@study@obs_end, " Wash: ", surv_ana@study@wash_len, " Years", get_surv_descr(surv_ana, surv_type="HR")),
                                 x="Hazard Ratio (95%)",
                                 y="") +
                            # Theme
                            IUtils::theme_custom() +
                            theme(text=element_text(size=21))

        file_path <- check_and_get_file_path(surv_ana, res_type="HR RG")
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

#' Plots the HR from the Cox-PH model of a forward study
#'   
#' Creates separate plots for each endpoint for the different age studies.
#' Plots both the risk group stratified and continuous HRs.
#' 
#' @inheritParams plot_hrs
#' 
#' @export 
#' 
#' @author Kira E. Detrois
plot_age_hrs <- function(coxph_hr_res,
                         surv_ana) {
                            
    coxph_hr_res <- dplyr::filter(coxph_hr_res, !is.na(HR))
    endpts <- unique(coxph_hr_res$ENDPOINT)

    for(endpt in endpts) {
        surv_ana@study@endpt <- endpt # Cheating the system for easier plotting here
        endpt_coxph_hr_res <- dplyr::filter(coxph_hr_res,
                                            ENDPOINT == endpt)
        plot_risk_group_hr(surv_ana, endpt_coxph_hr_res, endpt)
        plot_sd_hr(surv_ana, endpt_coxph_hr_res, endpt)
    }
    return(NULL)
}

#' Plots the HR from the continuous Cox-PH model of a forward study
#'   
#' Creates separate plots for each endpoint for the different age studies.
#' 
#' @inheritParams plot_hrs
#' @param endpt_coxph_hr_res A tibble. The Cox-PH HR results. Needs
#'                           to at least contain the columns `ENDPOINT`,
#'                           `HR`, `CI_NEG`, `CI_POS`, and `GROUP`.
#' @inheritParams get_n_group_cases
#' 
#' @export 
#' 
#' @author Kira E. Detrois
plot_sd_hr <- function(surv_ana,
                       endpt_coxph_hr_res,
                       endpt) {
    endpt_coxph_hr_res <- dplyr::filter(endpt_coxph_hr_res, GROUP == "no groups")
    if(nrow(endpt_coxph_hr_res) > 0) {
        plt <- ggplot2::ggplot(endpt_coxph_hr_res, aes(x=as.character(EXP_AGE), y=HR)) +
                    # Points and error bars
                    geom_point() + 
                    geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS), width=.1) +
                    # Axis settings
                    geom_hline(yintercept=1.0) + 
                    coord_cartesian(ylim=c(-1,10)) +
                    scale_x_discrete(breaks=unique(endpt_coxph_hr_res$EXP_AGE),
                                     labels=get_obs_per_strings(endpt_coxph_hr_res, surv_ana)) +
                    # Labels and titles
                    labs(title=endpt,
                         subtitle="1-SD Increment",
                         caption=paste0("Exp: ", surv_ana@study@exp_len, " Wash: ", surv_ana@study@wash_len, " Obs: ",  surv_ana@study@obs_len, " Years", get_surv_descr(surv_ana, surv_type="surv")),
                         x="Observation Age",
                         y="Hazard Ratio (95% CI)") +
                    # Theme
                    IUtils::theme_custom(base_size=16) +
                    theme(text=element_text(size=21),
                          plot.caption=element_text(size=10, hjust=0),
                          panel.grid.major.x =element_blank()) 

        file_path <- check_and_get_file_path(surv_ana, res_type="HR SD")
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

#' Plots the HR from the risk stratified Cox-PH model of a forward study
#'   
#' Creates separate plots for each endpoint for the different age studies.
#' Plots the top 1% compared to the 40-60% group.
#' 
#' @inheritParams plot_hrs
#' @param endpt_coxph_hr_res A tibble. The Cox-PH HR results. Needs
#'                           to at least contain the columns `ENDPOINT`,
#'                           `HR`, `CI_NEG`, `CI_POS`, and `GROUP`.
#' @inheritParams get_n_group_cases
#' 
#' @export 
#' 
#' @author Kira E. Detrois
plot_risk_group_hr <- function(surv_ana,
                               endpt_coxph_hr_res,
                               endpt) {

    comp_group <- get_comp_group(surv_ana@score_type)
    endpt_coxph_hr_res <- dplyr::filter(endpt_coxph_hr_res,
                                        GROUP == comp_group)

    endpt_coxph_hr_res <- dplyr::filter(endpt_coxph_hr_res, GROUP != "no group")
    if(nrow(endpt_coxph_hr_res) > 0) {
        plt <- ggplot2::ggplot(endpt_coxph_hr_res, aes(x=as.character(EXP_AGE), y=HR)) +
                    # Points and error bars
                    geom_point() + 
                    geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS), width=.1) +
                    # Axis settings
                    geom_hline(yintercept=1.0) + 
                    coord_cartesian(ylim=c(-1,10)) +
                    scale_x_discrete(breaks=unique(endpt_coxph_hr_res$EXP_AGE),
                                     labels=get_obs_per_strings(endpt_coxph_hr_res, surv_ana)) +
                    # Labels and titles
                    labs(title=endpt,
                         subtitle=paste0(get_comp_descr(surv_ana, surv_type="HR")),
                         caption=paste0("Exp: ", surv_ana@study@exp_len, " Wash: ", surv_ana@study@wash_len, " Obs: ",  surv_ana@study@obs_len, " Years", get_surv_descr(surv_ana, surv_type="HR")),
                         x="Observation Age",
                         y="Hazard Ratio (95% CI)") +
                    # Theme
                    IUtils::theme_custom(base_size=16) +
                    theme(text=element_text(size=21),
                          plot.caption=element_text(size=10, hjust=0),
                          panel.grid.major.x =element_blank()) 

        file_path <- check_and_get_file_path(surv_ana, res_type="HR RG")
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

get_obs_per_strings <- function(endpt_coxph_hr_res,
                                surv_ana) {
    paste0(unique(endpt_coxph_hr_res$EXP_AGE)+surv_ana@study@exp_len+surv_ana@study@wash_len, 
           "-", 
           unique(endpt_coxph_hr_res$EXP_AGE+surv_ana@study@exp_len)+surv_ana@study@wash_len+surv_ana@study@obs_len)
}