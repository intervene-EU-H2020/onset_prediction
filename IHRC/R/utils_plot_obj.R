
#' Creats ggplot for forwar study setup
#' 
#' Creates the ggplot for the HRs vs. observation age for an endpoint 
#' in a forward study.
#'
#' @param coxph_hrs A dataframe. The results of the Cox-PH model for 
#'                  the current endpoint.
#' @param ana_details A list. Details of the current analysis such as 
#'                      study type, predictors, etc.
#' @param min_y A numeric. Minimum value for the y-axis.
#' @param max_y A numeric. Maximum value for the y-axis.
#' 
#' @return A ggplot object.
#' 
#' @export
#' 
#' @author Kira E. Detrois
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
                                   labels=get_obs_age_period_str(coxph_hrs, 
                                                                 ana_details)) +
                  # Labels and titles
                  labs(title=ana_details$endpt,
                       subtitle=get_sd_title(coxph_hrs$VAR),
                       caption=get_caption(ana_details),
                       x="Observation Age",
                       y="Hazard Ratio (95% CI)") +
                  # Grid setting
                  theme(panel.grid.major.x=element_blank())
        
    plt <- ggplot_add_color_and_theme(plt, "Predictor", coxph_hrs$VAR)
    plt <- ggplot_add_points_and_errors(plt, 
                                    coxph_vars=coxph_hrs$VAR, 
                                    study_type=ana_details$study_type)
    
}

#' Creates a ggplot object for endpoint specific HR plots
#'
#' Creates a ggplot object that shows the HRs of the endpoints. 
#' The x-axis shows the HRs, the y-axis shows the endpoints.
#' 
#' @param coxph_hrs A tibble. The Cox-PH HR results. 
#'                      Needs to at least contain the columns `ENDPOINT`, `HR`, 
#'                      `CI_NEG`, `CI_POS`, `VAR`
#' @param ana_details A list. A list of parameters for the analysis. 
#'                       It should contain the following fields: `study_type`, 
#'                      `preds`, `exp_len`, `wash_len`, `obs_len`, `obs_end_date`.
#' @param sort_hrs A logical.  If `True`, the endpoints are sorted by HR.
#' 
#' @return A ggplot object
#' 
#' @export
#' 
#' @import ggplot2
#' 
#' @author Kira E. Detrois
get_endpt_sd_hr_ggplot <- function(coxph_hrs,
                                   ana_details,
                                   sort_hrs) {
    max_x <- min(max(c(2, round(coxph_hrs$CI_POS)), na.rm=TRUE), 10)
    min_x <- min(c(0.5, round(coxph_hrs$CI_NEG)), na.rm=TRUE)
    if(!sort_hrs) {
        plt <- ggplot2::ggplot(coxph_hrs,
                               # Plot basics
                               aes(y=ENDPOINT, x=HR, color=VAR))
    } else {
        plt <- ggplot2::ggplot(coxph_hrs,
                               # Plot basics
                               aes(y=reorder(ENDPOINT, HR), x=HR, color=VAR))
    }
    plt <-  plt + 
            # Axis settings
            coord_cartesian(xlim=c(min_x, max_x)) +
            geom_vline(xintercept=1.0) +
            # Legends and labels
            labs(title=get_sd_title(coxph_hrs$VAR),
                 caption=get_caption(ana_details),
                 x="Hazard Ratio (95%)",
                 y="")
    plt <- ggplot_add_color_and_theme(plt, "Predictor", levels(coxph_hrs$VAR))
    plt <- ggplot_add_points_and_errors(plt, 
                                    coxph_vars=coxph_hrs$VAR, 
                                    study_type=ana_details$study_type)

        return(plt)
}
