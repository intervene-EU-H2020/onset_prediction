
#' Add color and theme settings to the ggplot
#'
#' Adds custom color settings and theme settings to a ggplot object
#' 
#' @param plt A ggplot object. 
#' @param legend_name A character. The legend name. Default: "Predictor".
#' @param color_labels A character (vector). The labels for the legend.
#' @param theme_base_size A numeric. The base font size for theme settings
#' 
#' @return A ggplot object with added color and theme settings
#' 
#' @export
#' 
#' @example 
#' # Create a ggplot object
#' plt <- ggplot(data = my_data, aes(x = x_var, y = y_var)) + 
#'         geom_point()
#' # Add color scale and theme to the plot
#' plt <- ggplot_add_color_and_theme(plt, color_labels = my_data$color_var)
#'
#' @author Kira E. Detrois
ggplot_add_color_and_theme <- function(plt, 
                                       legend_name="Predictor",
                                       color_labels,
                                       theme_base_size=21) {
    n_labels <- length(color_labels)
    plt <- plt + scale_color_manual(
                        values=IUtils::custom_colors_brewer(n_labels), 
                        name=legend_name,
                        labels=reformat_preds_pretty(color_labels)) +
                 IUtils::theme_custom(base_size=theme_base_size) 
    return(plt)
}


#' Add points and error bars to a ggplot
#'
#' @param plt The ggplot object to add points and error bars to.
#' @inheritParams get_sd_title
#' @inheritParams run_surv_studies
#' 
#' @return The modified ggplot object with points and error bars added.
#' 
#' @import ggplot2
#' 
#' @export 
ggplot_add_points_and_errors <- function(plt,
                                     coxph_vars,
                                     study_type) {
    pos_dodge=0.5
    size=1
    width=0.1

    if(length(unique(coxph_vars)) > 1) {
        # Points and bars with coloring
        plt <- plt + geom_point(position=position_dodge(width=.5), 
                                size=3)
        if(study_type == "forward") {
            plt <- plt + geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS, color=VAR),
                                       position=position_dodge(width=pos_dodge),
                                       linewidth=size, 
                                       width=width)
        } else {
            plt <- plt + geom_errorbar(aes(xmin=CI_NEG, xmax=CI_POS, color=VAR),
                                       position=position_dodge(width=pos_dodge),
                                       linewidth=size, 
                                       width=width)
        }
    } else {
        # No color and no legend
        plt <- plt + 
               geom_point(show.legend = FALSE, size=3) + 
               theme(legend.position = "none")
        if(study_type == "forward") {
            plt <- plt + geom_errorbar(aes(ymin=CI_NEG, ymax=CI_POS),
                                       linewidth=size, 
                                       width=width)
        } else {
            plt <- plt + geom_errorbar(aes(xmin=CI_NEG, xmax=CI_POS),
                                       linewidth=size, 
                                       width=width)
        }
    }
    return(plt)
}