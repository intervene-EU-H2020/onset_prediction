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
                            
    coxph_hr_res <- dplyr::filter(coxph_hr_res, 
                                  !is.na(HR) &    
                                    !is.infinite(CI_NEG) &
                                    !is.infinite(CI_POS))
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
        plot_endpt_rg_hr(coxph_hr_res=coxph_hr_res, surv_ana=surv_ana)
        plot_endpt_sd_hr(coxph_hr_res=coxph_hr_res, surv_ana=surv_ana)
    }
}

plot_endpt_rg_hr <- function(coxph_hr_res,
                             surv_ana) {
    top_group <- dplyr::filter(coxph_hr_res, GROUP == get_comp_group(surv_ana@score_type, surv_ana@bin_cut))
    max_x <- min(max(c(2, round(top_group$CI_POS+0.5)), na.rm=TRUE), 10)
    min_x <- min(c(0.5, round(top_group$CI_NEG-0.5)), na.rm=TRUE)

    plt <- ggplot2::ggplot(top_group,
                            # Plot basics
                            aes(y=ENDPOINT, x=HR)) +
                            geom_point(show.legend = FALSE, size=3) +
                            geom_errorbar(aes(xmin=CI_NEG, xmax=CI_POS), size=1, width=.1) +
                            # Axis settings
                            coord_cartesian(xlim=c(min_x, max_x)) +
                            geom_vline(xintercept = 1.0) +
                            # Legends and labels
                            labs(title=paste0(get_comp_descr(surv_ana, surv_type="HR")),
                                 caption=paste0("Obs: ", surv_ana@study@obs_len, " Years until ", surv_ana@study@obs_end_date, " Wash: ", surv_ana@study@wash_len, " Years", get_surv_descr(surv_ana, surv_type="HR")),
                                 x="Hazard Ratio (95%)",
                                 y="") +
                            # Theme
                            IUtils::theme_custom(base_size=21)

    file_path <- check_and_get_file_path(surv_ana=surv_ana, res_type="HR RG")
    if(!is.null(file_path) & !is.null(plt)) {
            ggsave(file_path,
                width=12,
                height=7,
                dpi=600,
                plot=plt, 
                device="png", 
                bg="white")
    }
}

plot_endpt_sd_hr <- function(coxph_hr_res,
                             surv_ana) {
    top_group <- dplyr::filter(coxph_hr_res, GROUP == "no groups")
    max_x <- min(max(c(2, round(top_group$CI_POS+0.5)), na.rm=TRUE), 10)
    min_x <- min(c(0.5, round(top_group$CI_NEG-0.5)), na.rm=TRUE)

    plt <- ggplot2::ggplot(top_group,
                            # Plot basics
                            aes(y=ENDPOINT, x=HR, color=SCORE)) +
                            # Axis settings
                            coord_cartesian(xlim=c(min_x, max_x)) +
                            geom_vline(xintercept = 1.0) +
                            # Legends and labels
                            labs(caption=paste0("Obs: ", surv_ana@study@obs_len, " Years until ", surv_ana@study@obs_end_date, " Wash: ", surv_ana@study@wash_len, " Years", get_surv_descr(surv_ana, surv_type="surv")),
                                 x="Hazard Ratio (95%)",
                                 y="") +
                            scale_color_manual(values=IUtils::custom_colors_brewer(length(surv_ana@score_type)), 
                                       name="Score", 
                                       labels=surv_ana@score_type) +
                            # Theme
                            IUtils::theme_custom(base_size=21) 
        if(length(surv_ana@score_type) > 1) {
            plt <- plt + 
                    geom_point(position = position_dodge(width = .4), size=3) +
                    geom_errorbar(position = position_dodge(width = .4), size=1,
                                  aes(xmin=CI_NEG, xmax=CI_POS), width=.1) +
                    labs(title="1-SD Increment")
        } else {
            # Points and error bars
            plt <- plt + geom_point(show.legend = FALSE, size=3) + 
                    geom_errorbar(aes(xmin=CI_NEG, xmax=CI_POS), size=1, width=.1) +
                    theme(legend.position = "none") +
                    labs(title=paste0(surv_ana@score_type, " 1-SD Increment"))
        }

    file_path <- check_and_get_file_path(surv_ana=surv_ana, res_type="HR SD")
    if(!is.null(file_path) & !is.null(plt)) {
            ggsave(file_path,
                width=12,
                height=7,
                dpi=600,
                plot=plt, 
                device="png", 
                bg="white")
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

    endpts <- unique(coxph_hr_res$ENDPOINT)

    for(endpt in endpts) {
        surv_ana@study@endpt <- endpt # Cheating the system for easier plotting here
        curnt_coxph_hr_res <- dplyr::filter(coxph_hr_res, GROUP != "no groups")
        if(nrow(curnt_coxph_hr_res) > 0) {
            max_y <- min(max(c(2, round(curnt_coxph_hr_res$CI_POS+0.5)), na.rm=TRUE), 10)
            min_y <- min(c(0, round(curnt_coxph_hr_res$CI_NEG-0.5)), na.rm=TRUE)
            plot_age_rg_hr(surv_ana=surv_ana, 
                           coxph_hr_res=curnt_coxph_hr_res, 
                           endpt=endpt, 
                           min_y=min_y, 
                           max_y=max_y)
        } 
        curnt_coxph_hr_res <- dplyr::filter(coxph_hr_res, GROUP == "no groups")
        if(nrow(curnt_coxph_hr_res) > 0) {
            max_y <- min(max(c(2, round(curnt_coxph_hr_res$CI_POS+0.5)), na.rm=TRUE), 10)
            min_y <- min(c(0, round(curnt_coxph_hr_res$CI_NEG-0.5)), na.rm=TRUE)
            plot_age_sd_hr(surv_ana=surv_ana,  
                           coxph_hr_res=curnt_coxph_hr_res, 
                           endpt=endpt, 
                           min_y=min_y, 
                           max_y=max_y)
        }
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
plot_age_sd_hr <- function(surv_ana,
                           coxph_hr_res,
                           endpt,
                           min_y, 
                           max_y) {
    endpt_coxph_hr_res <- dplyr::filter(coxph_hr_res, ENDPOINT == endpt)
    if(nrow(endpt_coxph_hr_res) > 0) {
        plt <- ggplot2::ggplot(endpt_coxph_hr_res, aes(x=as.character(EXP_AGE), y=HR, color=SCORE)) +
                    # Axis settings
                    geom_hline(yintercept=1.0) + 
                    coord_cartesian(ylim=c(min_y,max_y)) +
                    scale_x_discrete(breaks=unique(endpt_coxph_hr_res$EXP_AGE),
                                     labels=get_obs_per_strings(endpt_coxph_hr_res, surv_ana)) +
                    # Labels and titles
                    labs(title=endpt,
                         subtitle="1-SD Increment",
                         caption=paste0("Exp: ", surv_ana@study@exp_len, " Wash: ", surv_ana@study@wash_len, " Obs: ",  surv_ana@study@obs_len, " Years", get_surv_descr(surv_ana, surv_type="surv")),
                         x="Observation Age",
                         y="Hazard Ratio (95% CI)") +
                    scale_color_manual(values=IUtils::custom_colors_brewer(length(surv_ana@score_type)), 
                                       name="Score", 
                                       labels=surv_ana@score_type) +
                    # Theme
                    IUtils::theme_custom(base_size=21) +
                    theme(panel.grid.major.x=element_blank()) 
        if(length(surv_ana@score_type) > 1) {
            plt <- plt + 
                    geom_point(position = position_dodge(width = .4), size=3,
                               aes(color=SCORE)) +
                    geom_errorbar(position = position_dodge(width = .4), size=1, width=0.1, 
                                  aes(ymin=CI_NEG, ymax=CI_POS, color=SCORE)) 
        } else {
            # Points and error bars
            plt <- plt + geom_point(show.legend = FALSE, size=3) + 
                    geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS), size=1, width=.1) +
                    theme(legend.position = "none")
        }
        
        file_path <- check_and_get_file_path(surv_ana=surv_ana, res_type="HR SD")
       if(!is.null(file_path) & !is.null(plt)) {
            ggsave(file_path,
                   width=7,
                   height=7,
                   dpi=600,
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
plot_age_rg_hr <- function(surv_ana,
                           coxph_hr_res,
                           endpt,
                           min_y,
                           max_y) {

    comp_group <- get_comp_group(surv_ana@score_type)
    endpt_coxph_hr_res <- dplyr::filter(coxph_hr_res,
                                        GROUP == comp_group &
                                            ENDPOINT == endpt)

    if(nrow(endpt_coxph_hr_res) > 0) {
        plt <- ggplot2::ggplot(endpt_coxph_hr_res, aes(x=as.character(EXP_AGE), y=HR)) +
                    # Points and error bars
                    geom_point(size=3) + 
                    geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS), size=1, width=.2) +
                    # Axis settings
                    geom_hline(yintercept=1.0) + 
                    coord_cartesian(ylim=c(min_y, max_y)) +
                    scale_x_discrete(breaks=unique(endpt_coxph_hr_res$EXP_AGE),
                                     labels=get_obs_per_strings(endpt_coxph_hr_res, surv_ana)) +
                    # Labels and titles
                    labs(title=endpt,
                         subtitle=get_comp_descr(surv_ana, surv_type="HR"),
                         caption=paste0("Exp: ", surv_ana@study@exp_len, " Wash: ", surv_ana@study@wash_len, " Obs: ",  surv_ana@study@obs_len, " Years", get_surv_descr(surv_ana, surv_type="HR")),
                         x="Observation Age",
                         y="Hazard Ratio (95% CI)") +
                    # Theme
                    IUtils::theme_custom(base_size=21) +
                    theme(panel.grid.major.x=element_blank()) 


    file_path <- check_and_get_file_path(surv_ana=surv_ana, res_type="HR RG")
       if(!is.null(file_path) & !is.null(plt)) {
            ggsave(file_path,
                   width=7,
                   height=7,
                   dpi=600,
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