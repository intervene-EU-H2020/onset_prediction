#' Creates a plot of the score distribution
#' 
#' @inheritParams calc_endpt_hrs
#' @param save_plot A boolean. Whether to save the plot to a file.
#' @param plot_dir A character. The directory where the plot should
#'                  be saved to.
#' @param plot_descr A character. Extra description to add to the plot
#'                   file name.
#' 
#' @return The plot object.
#' 
#' @import ggplot2
#' @export 
#' 
#' @author Kira E. Detrois
plot_score_distr <- function(score_data,
                             score_col_name="SCORE",
                             score_type,
                             save_plot=FALSE,
                             plot_dir="",
                             plot_descr="") {
    score_data <- dplyr::filter(score_data, 
                                !is.na(get(score_col_name)))

    plt <- ggplot(score_data, aes(x=get(score_col_name))) + 
                geom_histogram(alpha=.8, fill="#214a2a", binwidth = 1.0)  +
                labs(title=paste0(score_type, " Score Histogram"),
                     subtitle=paste0("All ", nrow(score_data), " Individuals"),
                     x=score_type,
                     y="Count") +
                theme_minimal() + 
                theme(text=element_text(size=21))

    if(save_plot)
        ggsave(paste0(plot_dir, score_type, "_score_distr_", plot_descr, ".png"),
               plot=plt, device="png", bg="white")

    return(plt)
}

plot_endpt_score_distr <- function(pheno_data,
                                   score_col_name="SCORE",
                                   endpt,
                                   score_type,
                                   save_plot=FALSE,
                                   plot_dir="",
                                   plot_descr="") {

}