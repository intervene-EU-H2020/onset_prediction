#' Creates a plot of the score distribution
#' 
#' @inheritParams calc_endpt_hrs
#' @param save_plot A boolean. Whether to save the plot to a file.
#' @param plot_dir A character. The directory where the plot should
#'                  be saved to.
#' @param plot_descr A character. Extra description to add to the plot
#'                   file name.
#' 
#' @return The ggplot object.
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

    if(save_plot) {
        ggsave(paste0(plot_dir, score_type, "_score_distr_", plot_descr, ".png"),
               plot=plt, device="png", bg="white")

    }

    return(plt)
}

#' Plots a jittered scatter
#' 
#' Plots a jittered scatter plot of the cases and controls scores
#' eligible under the current study setup.
#' 
#' @inheritParams calc_endpt_hrs
#' @inheritParams run_coxph_ana
#' @param save_plot A boolean. Whether not to save the plots.
#' @param plot_dir A character. Directory to save the plots to.
#' 
#' 
#' @return A ggplot object
#' 
#' @import ggplot2
#' @export 
#' 
#' @author Kira E. Detrois
plot_endpt_score_distr <- function(pheno_data,
                                   score_col_name="SCORE",
                                   endpt,
                                   score_type,
                                   exp_age, 
                                   exp_len,
                                   wash_len,
                                   obs_len,
                                   save_plot=FALSE,
                                   plot_dir="") {
    pheno_data <- dplyr::mutate_at(pheno_data, endpt, as.factor)

    p <- ggplot(pheno_data, 
                aes(x=get(endpt), y=get(score_col_name),
                    colour=get(endpt))) + 
                geom_jitter(width=0.3, show.legend = FALSE) +
                scale_x_discrete(labels=c("Controls", "Cases")) +
                labs(title=paste0(score_type, " Scores for ", endpt),
                     subtitle=paste0("Age: ", exp_age, " Exp: ", exp_len, " Wash: ", wash_len, " Obs: ", obs_len, " Years"),
                     x="",
                     y=paste0(score_type, " Score")) +
                theme_minimal() +
                theme(text=element_text(size=21))

    file_path <- paste0(plot_dir,
            Istudysetup::get_study_file_name(as.list(environment())), 
            "_", 
            score_type, 
            "_score.png")

    ggsave(file_path, p, device="png", bg="white")

    return(p)
}