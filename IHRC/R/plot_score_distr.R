#' Creates a plot of the score distribution
#' 
#' @inheritParams add_risk_group_col
#' @inheritParams run_surv_studies
#' 
#' @return The ggplot object.
#' 
#' @import ggplot2
#' @export 
#' 
#' @author Kira E. Detrois
plot_score_distr <- function(score_data,
                             surv_ana) {
    if(surv_ana@score_type == "CCI") {
        if(surv_ana@study@study_type == "forward") {
            plot_descr <- paste0(surv_ana@study@exp_age, " to ", surv_ana@study@exp_age+surv_ana@study@exp_len)
        }
        else {
            plot_descr <- paste0("until ", surv_ana@study@obs_end)
        }
        plt <- ggplot(score_data, aes(x=get("SCORE"))) + 
                    geom_histogram(fill="#405e46", binwidth = 1.0)  +
                    labs(title=paste0(surv_ana@score_type, " Score Histogram"),
                         subtitle=paste0(nrow(score_data), " Individuals ", plot_descr),
                         x=surv_ana@score_type,
                         y="Count") +
                    coord_cartesian(xlim=c(0,15)) +
                    theme_minimal() + 
                    theme(text=element_text(size=21))
    } else {
        plt <- suppressMessages(ggplot(score_data, aes(x=get("SCORE"))) + 
                    geom_histogram(fill="#405e46", bins=30)  +
                    labs(title=paste0(surv_ana@score_type, " Score Histogram"),
                        subtitle=paste0(nrow(score_data), " Individuals "),
                        x=surv_ana@score_type,
                        y="Count") +
                    theme_minimal() + 
                    theme(text=element_text(size=21)))
    }

    file_path <- check_and_get_file_path(surv_ana,
                                         res_type="distr")
    if(!is.null(file_path)) {
        ggsave(file_path,
               width=7,
               height=7,
               plot=plt, 
               device="png", 
               bg="white")
    }
    return(plt)
}

#' Plots a jittered scatter
#' 
#' Plots a jittered scatter plot of the cases and controls scores
#' eligible under the current study setup.
#' 
#' @inheritParams add_risk_group_col
#' @inheritParams run_surv_studies
#' 
#' @return A ggplot object
#' 
#' @import ggplot2
#' @export 
#' 
#' @author Kira E. Detrois
plot_endpt_score_distr <- function(score_data,
                                   surv_ana) {
    if(nrow(score_data) == 0) {
        return(NULL)
    } else if(Istudy::get_n_cases(score_data, surv_ana@study@endpt) < surv_ana@min_indvs | 
                Istudy::get_n_cntrls(score_data, surv_ana@study@endpt) < surv_ana@min_indvs) {
        return(NULL)
    }
    score_data <- dplyr::mutate_at(score_data, surv_ana@study@endpt, as.factor)

    plt <- ggplot(score_data, 
                      aes(x=get(surv_ana@study@endpt),
                          y=SCORE, 
                          fill=get(surv_ana@study@endpt))) + 
                    geom_boxplot(show.legend=FALSE) +
                    scale_x_discrete(labels=c("Controls", "Cases")) +
                    labs(title=paste0(surv_ana@score_type, " Scores for ", surv_ana@study@endpt),
                         subtitle=get_study_subtitle(surv_ana@study),
                         x="",
                         y=paste0(surv_ana@score_type, " Score")) +
                    theme_minimal() +
                    theme(text=element_text(size=21))

    file_path <- check_and_get_file_path(surv_ana,
                                         res_type="endpt")
    if(!is.null(file_path)) {
        ggsave(file_path,
               width=7,
               height=7,
               plot=plt, 
               device="png", 
               bg="white")
    }
    return(plt)
}

