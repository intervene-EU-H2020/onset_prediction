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
#'  
#' @export 
#' 
#' @author Kira E. Detrois
plot_hrs <- function(coxph_hr_res,
                     surv_ana,
                     from_file=FALSE,
                     file_dir="",
                     plot_preds="") {
                            
    coxph_hr_res <- filter_out_missing(coxph_hr_res)
    if(!from_file) {
        plot_preds <- surv_ana@plot_preds
    } else {
        res_dir <- parse_res_dir(file_path)
    }

    coxph_hr_res <- set_plot_preds_fctr(coxph_hr_res,
                                        plot_preds)

    if(surv_ana@study@study_type == "forward") {
        plt <- plot_age_hrs(coxph_hr_res, surv_ana)
    } else {
        plt <- plot_endpt_hrs(coxph_hr_res, surv_ana)
    }
}

set_plot_preds_fctr <- function(coxph_hr_res,
                                plot_preds) {
    plot_preds <- stringr::str_replace_all(plot_preds, "[*]", ":")
    coxph_hr_res <- dplyr::filter(coxph_hr_res, VAR %in% plot_preds)
    coxph_hr_res$VAR <- factor(coxph_hr_res$VAR, levels=plot_preds)

    return(coxph_hr_res)
}

filter_out_missing <- function(coxph_hr_res) {
    dplyr::filter(coxph_hr_res, 
                    !is.na(HR) &    
                    !is.infinite(CI_NEG) &
                    !is.infinite(CI_POS))
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
        plot_endpt_sd_hr(coxph_hr_res=coxph_hr_res, surv_ana=surv_ana)
    }
}

plot_endpt_sd_hr <- function(coxph_hr_res,
                             surv_ana) {
    n_preds <- length(unique(coxph_hr_res$VAR))
    top_group <- dplyr::filter(coxph_hr_res, GROUP == "no groups")
    max_x <- min(max(c(2, round(top_group$CI_POS)), na.rm=TRUE), 10)
    min_x <- min(c(0.5, round(top_group$CI_NEG)), na.rm=TRUE)
    plt <- ggplot2::ggplot(top_group,
                            # Plot basics
                            aes(y=ENDPOINT, x=HR, color=VAR)) +
                            # Axis settings
                            coord_cartesian(xlim=c(min_x, max_x)) +
                            geom_vline(xintercept = 1.0) +
                            # Legends and labels
                            labs(caption=paste0("Obs: ", surv_ana@study@obs_len, " Years until ", surv_ana@study@obs_end_date, " Wash: ", surv_ana@study@wash_len, " Years", get_surv_descr(surv_ana, surv_type="surv")),
                                 x="Hazard Ratio (95%)",
                                 y="") +
                            scale_color_manual(values=IUtils::custom_colors_brewer(n_preds), 
                                               name="Predictor") +
                            # Theme
                            IUtils::theme_custom(base_size=21) 
        if(n_preds > 1) {
            plt <- plt + 
                    geom_point(position = position_dodge(width = .5), size=3) +
                    geom_errorbar(position = position_dodge(width = .5), size=1,
                                  aes(xmin=CI_NEG, xmax=CI_POS), width=.1) +
                    labs(title="1-SD Increment")
        } else {
            # Points and error bars
            plt <- plt + geom_point(show.legend = FALSE, size=3) + 
                    geom_errorbar(aes(xmin=CI_NEG, xmax=CI_POS), size=1, width=.1) +
                    theme(legend.position = "none") +
                    labs(title=paste0(surv_ana@plot_preds, " 1-SD Increment"))
        }

    file_path <- check_and_get_file_path(surv_ana=surv_ana, res_type="HR")
    fig_height <- dplyr::case_when(
        n_preds == 2 ~ 7,
        n_preds == 3 ~ 9,
        n_preds == 4 ~ 11,
        n_preds == 5 ~ 14,
    )
    if(!is.null(file_path) & !is.null(plt)) {
            ggsave(file_path,
                width=12,
                height=fig_height,
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
#' @import ggplot2
#' 
#' @author Kira E. Detrois
plot_age_sd_hr <- function(surv_ana,
                           coxph_hr_res,
                           endpt,
                           min_y, 
                           max_y) {
    n_preds <- length(unique(coxph_hr_res$VAR))

    endpt_coxph_hr_res <- dplyr::filter(coxph_hr_res, ENDPOINT == endpt)
    if(nrow(endpt_coxph_hr_res) > 0) {
        plt <- ggplot2::ggplot(endpt_coxph_hr_res, ggplot2::aes(x=as.character(EXP_AGE), y=HR, color=VAR)) +
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
                    scale_color_manual(values=IUtils::custom_colors_brewer(n_preds), 
                                       name="Predictors") +
                    # Theme
                    IUtils::theme_custom(base_size=21) +
                    theme(panel.grid.major.x=element_blank()) 
        if(length(unique(coxph_hr_res$VAR)) > 1) {
            plt <- plt + 
                    geom_point(position = position_dodge(width = .5), size=3,
                               aes(color=VAR)) +
                    geom_errorbar(position = position_dodge(width = .5), size=1, width=0.1, 
                                  aes(ymin=CI_NEG, ymax=CI_POS, color=VAR)) 
        } else {
            # Points and error bars
            plt <- plt + geom_point(show.legend = FALSE, size=3) + 
                    geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS), size=1, width=.1) +
                    theme(legend.position = "none")
        }
        
        file_path <- check_and_get_file_path(surv_ana=surv_ana, res_type="HR")
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