#' Plot the HR from the Cox-PH model
#' 
#' For forward study creates separate plots for each
#' endpoint for the different age studies. For 
#' the backward study creates a single plot with the
#' different endpoints.
#' 
#' @param coxph_hrs A tibble. The Cox-PH HR results. Needs
#'                      to at least contain the columns `ENDPOINT`,
#'                      `HR`, `CI_NEG`, `CI_POS`, and `GROUP`.
#'  
#' @export 
#' @import ggplot2
#' 
#' @author Kira E. Detrois
plot_hrs <- function(coxph_hrs=NULL,
                     surv_ana=NULL,
                     from_file=FALSE,
                     ana_details,
                     fig_height=NULL,
                     fig_width=NULL) {
    if(!from_file) {
        ana_details <- get_ana_details_from_surv_ana(surv_ana)
    } 
    coxph_hrs <- filter_out_missing_hrs(coxph_hrs)
    coxph_hrs <- filter_plot_preds_fctr(coxph_hrs,
                                        ana_details$plot_preds)
    curnt_coxph_hrs <- dplyr::filter(coxph_hrs, GROUP == "no groups")
    if(nrow(curnt_coxph_hrs) > 0) {
        if(ana_details$study_type == "forward") {
            plt <- plot_age_sd_hrs(curnt_coxph_hrs, 
                                   ana_details)
        } else {
            plt <- plot_endpt_sd_hr(coxph_hrs=curnt_coxph_hrs, 
                                    ana_details=ana_details,
                                    fig_height=fig_height,
                                    fig_width=fig_width)
            return(plt)

        }
    }
}

filter_plot_preds_fctr <- function(coxph_hrs,
                                plot_preds) {
    plot_preds <- stringr::str_replace_all(plot_preds, "[*]", ":")
    coxph_hrs <- dplyr::filter(coxph_hrs, VAR %in% plot_preds)
    coxph_hrs$VAR <- factor(coxph_hrs$VAR, levels=plot_preds)

    return(coxph_hrs)
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
plot_endpt_sd_hr <- function(coxph_hrs,
                             ana_details,
                             fig_height,
                             fig_width) {
    max_x <- min(max(c(2, round(coxph_hrs$CI_POS)), na.rm=TRUE), 10)
    min_x <- min(c(0.5, round(coxph_hrs$CI_NEG)), na.rm=TRUE)
    plt <- get_endpt_sd_hr_ggplot(coxph_hrs=coxph_hrs,
                                  ana_details=ana_details,
                                  min_x=min_x,
                                  max_x=max_x)
    file_path <- check_and_get_file_path(ana_details, res_type="HR")
    if(ana_details$write_res) {
        save_plt(file_path=file_path,
                 plt=plt,
                 width=ifelse(!is.null(fig_width), fig_width, 14),
                 height=ifelse(!is.null(fig_height), fig_height, get_endpt_fig_height(coxph_hrs$VAR)))
    }
    return(plt)
    
}

get_endpt_fig_height <- function(coxph_vars) {
    n_preds <- length(unique(coxph_vars))
    dplyr::case_when(
        n_preds == 1 ~ 10,
        n_preds == 2 ~ 10,
        n_preds == 3 ~ 10,
        n_preds == 4 ~ 11,
        n_preds == 5 ~ 14,
    )
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
plot_age_sd_hrs <- function(coxph_hrs,
                            ana_details) {

    max_y <- min(max(c(2, round(coxph_hrs$CI_POS+0.5)), na.rm=TRUE), 10)
    min_y <- min(c(0, round(coxph_hrs$CI_NEG-0.5)), na.rm=TRUE)
    endpts <- unique(coxph_hrs$ENDPOINT)

    for(endpt in endpts) {
        endpt_coxph_hrs <- dplyr::filter(coxph_hrs, 
                                         ENDPOINT == endpt)
        ana_details$endpt <- endpt
        if(nrow(endpt_coxph_hrs) > 0) {
            plt <- get_age_sd_hr_ggplot(coxph_hrs=endpt_coxph_hrs,
                                        ana_details=ana_details,
                                        min_y=min_y,
                                        max_y=max_y)
            file_path <- check_and_get_file_path(
                                ana_details,
                                res_type="HR")
            save_plt(file_path=file_path,
                     plt=plt,
                     width=7,
                     height=7)
        }
    }
    return(NULL)
}


save_plt <- function(file_path, 
                     plt,
                     width,
                     height) {
    if(!is.null(file_path) & !is.null(plt)) {
                    ggsave(file_path,
                        width=width,
                        height=height,
                        dpi=600,
                        plot=plt, 
                        device="png", 
                        bg="white")
    }
}

#' @import ggplot2
get_age_sd_hr_ggplot <- function(coxph_hrs,
                                 ana_details,
                                 min_y,
                                 max_y) {
    plt <- ggplot2::ggplot(coxph_hrs, 
                  # Plot basics
                  aes(x=as.character(EXP_AGE), y=HR, color=VAR)) +
                  # Axis settings
                  geom_hline(yintercept=1.0) + 
                  coord_cartesian(ylim=c(min_y,max_y)) +
                  scale_x_discrete(breaks=unique(coxph_hrs$EXP_AGE),
                                   labels=get_obs_per_strings(coxph_hrs, ana_details)) +
                  # Labels and titles
                  labs(title=ana_details$endpt,
                       subtitle=get_sd_title(coxph_hrs$VAR),
                       caption=get_caption(ana_details),
                       x="Observation Age",
                       y="Hazard Ratio (95% CI)") +
                  # Grid setting
                  theme(panel.grid.major.x=element_blank())
        
    plt <- ggplot_set_general_settings(plt, coxph_hrs$VAR)
    plt <- ggplot_points_and_errors(plt, 
                                    coxph_vars=coxph_hrs$VAR, 
                                    study_type=ana_details$study_type)
    
}

#' @import ggplot2
get_endpt_sd_hr_ggplot <- function(coxph_hrs,
                                   ana_details,
                                   min_x,
                                   max_x) {
        plt <- ggplot2::ggplot(coxph_hrs,
                      # Plot basics
                      aes(y=ENDPOINT, x=HR, color=VAR)) +
                      # Axis settings
                      coord_cartesian(xlim=c(min_x, max_x)) +
                      geom_vline(xintercept=1.0) +
                      # Legends and labels
                      labs(title=get_sd_title(coxph_hrs$VAR),
                           caption=get_caption(ana_details),
                           x="Hazard Ratio (95%)",
                           y="")
        plt <- ggplot_set_general_settings(plt, coxph_hrs$VAR)
        plt <- ggplot_points_and_errors(plt, 
                                        coxph_vars=coxph_hrs$VAR, 
                                        study_type=ana_details$study_type)

        return(plt)
}

#' @import ggplot2
ggplot_set_general_settings <- function(plt,
                                        coxph_vars) {
    n_preds <- length(unique(coxph_vars))
    plt <- plt + scale_color_manual(
                        values=IUtils::custom_colors_brewer(n_preds), 
                        name="Predictor",
                        labels=reformat_preds_pretty(levels(coxph_vars))) +
                 # Theme
                 IUtils::theme_custom(base_size=21) 
    return(plt)
}

#' @import ggplot2
ggplot_points_and_errors <- function(plt,
                                     coxph_vars,
                                     study_type) {
    pos_dodge=0.5
    size=1
    width=0.1

    if(length(unique(coxph_vars)) > 1) {
        # Points and bars with coloring
        plt <- plt + 
               geom_point(position=position_dodge(width=.5), 
                          size=3)
        if(study_type == "forward") {
            plt <- plt + geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS, color=VAR),
                                       position=position_dodge(width=pos_dodge),
                                       size=size, 
                                       width=width)
        } else {
            plt <- plt + geom_errorbar(aes(xmin=CI_NEG, xmax=CI_POS, color=VAR),
                                       position=position_dodge(width=pos_dodge),
                                       size=size, 
                                       width=width)
        }
    } else {
        # No color and no legend
        plt <- plt + 
               geom_point(show.legend = FALSE, size=3) + 
               theme(legend.position = "none")
        if(study_type == "forward") {
            plt <- plt + geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS),
                                       size=size, 
                                       width=width)
        } else {
            plt <- plt + geom_errorbar(aes(xmin=CI_NEG, xmax=CI_POS),
                                       size=size, 
                                       width=width)
        }
    }
    return(plt)
}