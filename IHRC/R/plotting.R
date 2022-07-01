#' Creates a plot of the score distribution
#' 
#' @inheritParams add_risk_group_col
#' @inheritParams calc_studies_hrs
#' 
#' @return The ggplot object.
#' 
#' @import ggplot2
#' @export 
#' 
#' @author Kira E. Detrois
plot_score_distr <- function(score_data,
                             score_type,
                             study,
                             write_res,
                             res_dir) {
    score_data <- get_and_filter_endpt_scores(score_data,
                                              score_type,
                                              study@endpt)

    plot_descr <- paste0(study@exp_age, " to ", study@exp_age+study@exp_len)
    if(score_type == "CCI") {
        plt <- ggplot(score_data, aes(x=get("SCORE"))) + 
                    geom_histogram(alpha=.8, fill="#214a2a", binwidth = 1.0)  +
                    labs(title=paste0(score_type, " Score Histogram"),
                        subtitle=paste0("All ",  nrow(score_data), " Eligible Individuals ", plot_descr),
                        x=score_type,
                        y="Count") +
                    coord_cartesian(xlim=c(0,15)) +
                    theme_minimal() + 
                    theme(text=element_text(size=21))
    } else {
        plt <- ggplot(score_data, aes(x=get("SCORE"))) + 
                    geom_histogram(alpha=.8, fill="#214a2a")  +
                    labs(title=paste0(score_type, " Score Histogram"),
                        subtitle=paste0("All ",  nrow(score_data), " Eligible Individuals ", plot_descr),
                        x=score_type,
                        y="Count") +
                    theme_minimal() + 
                    theme(text=element_text(size=21))
    }



    if(write_res) {
        if(Istudy::check_res_dir(write_res, res_dir)) {
            plot_res_dir <- paste0(res_dir, "plots/")
            if(!dir.exists(plot_res_dir)) {
                dir.create(plot_res_dir)
            }
            plot_descr <- paste0(study@exp_age, "_to_", study@exp_age+study@exp_len)

            ggsave(paste0(plot_res_dir, score_type, "_score_distr_", plot_descr, ".png"),
                plot=plt, device="png", bg="white")
        }
    }
    return(plt)
}

#' Plots a jittered scatter
#' 
#' Plots a jittered scatter plot of the cases and controls scores
#' eligible under the current study setup.
#' 
#' @inheritParams add_risk_group_col
#' @inheritParams calc_studies_hrs
#' 
#' @return A ggplot object
#' 
#' @import ggplot2
#' @export 
#' 
#' @author Kira E. Detrois
plot_endpt_score_distr <- function(score_data,
                                   score_type,
                                   study,
                                   write_res,
                                   res_dir) {
        score_data <- dplyr::mutate_at(score_data, 
                                       study@endpt, 
                                       as.factor)

        if(score_type == "CCI") {
            plt <- ggplot(score_data, 
                      aes(x=get(study@endpt),
                          y=SCORE, 
                          color=get(study@endpt))) + 
                    geom_jitter(show.legend=FALSE, width=0.3) +
                    scale_x_discrete(labels=c("Controls", "Cases")) +
                    labs(title=paste0(score_type, " Scores for ", study@endpt),
                        subtitle=paste0("Age: ", study@exp_age, " Exp: ", study@exp_len, " Wash: ", study@wash_len, " Obs: ", study@obs_len, " Years"),
                        x="",
                        y=paste0(score_type, " Score")) +
                    theme_minimal() +
                    theme(text=element_text(size=21))
        } else {
            plt <- ggplot(score_data, 
                      aes(x=get(study@endpt),
                          y=SCORE, 
                          fill=get(study@endpt))) + 
                    geom_boxplot(show.legend=FALSE) +
                    scale_x_discrete(labels=c("Controls", "Cases")) +
                    labs(title=paste0(score_type, " Scores for ", study@endpt),
                        subtitle=paste0("Age: ", study@exp_age, " Exp: ", study@exp_len, " Wash: ", study@wash_len, " Obs: ", study@obs_len, " Years"),
                        x="",
                        y=paste0(score_type, " Score")) +
                    theme_minimal() +
                    theme(text=element_text(size=21))
        }

    if(write_res) {
        if(Istudy::check_res_dir(write_res, res_dir)) {
            plot_res_dir <- paste0(res_dir, "plots/endpts/")
            if(!dir.exists(plot_res_dir)) {
                dir.create(plot_res_dir)
            }
            file_path <- paste0(plot_res_dir,
                                Istudy::get_study_file_name(study), 
                                "_", 
                                score_type, 
                                "_score.png")

            ggsave(file_path, plt, device="png", bg="white")
        }
    }

    return(plt)
}