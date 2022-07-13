#' Creates a plot of the score distribution
#' 
#' @inheritParams add_risk_group_col
#' @inheritParams calc_endpt_studies_hrs
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

    if(score_type == "CCI") {
        if(length(study@exp_len) == 1) {
            plot_descr <- paste0(study@exp_age, " to ", study@exp_age+study@exp_len)
        }
        else {
            plot_descr <- ""
        }
        plt <- ggplot(score_data, aes(x=get("SCORE"))) + 
                    geom_histogram(fill="#405e46", binwidth = 1.0)  +
                    labs(title=paste0(score_type, " Score Histogram"),
                         subtitle=paste0(nrow(score_data), " Individuals ", plot_descr),
                         x=score_type,
                         y="Count") +
                    coord_cartesian(xlim=c(0,15)) +
                    theme_minimal() + 
                    theme(text=element_text(size=21))
    } else {
        plt <- suppressMessages(ggplot(score_data, aes(x=get("SCORE"))) + 
                    geom_histogram(fill="#405e46", bins=30)  +
                    labs(title=paste0(score_type, " Score Histogram"),
                        subtitle=paste0(nrow(score_data), " Individuals "),
                        x=score_type,
                        y="Count") +
                    theme_minimal() + 
                    theme(text=element_text(size=21)))
    }

    file_path <- check_and_get_file_path(score_type=score_type,
                                         study=study,
                                         write_res=write_res,
                                         res_dir=res_dir,
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
#' @inheritParams calc_endpt_studies_hrs
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
                                   min_indvs=5,
                                   write_res,
                                   res_dir) {
    if(is.null(score_data) | 
        Istudy::get_n_cases(score_data, study@endpt) < min_indvs | 
        Istudy::get_n_cntrls(score_data, study@endpt) < min_indvs) {
        return(NULL)
    }
    score_data <- dplyr::mutate_at(score_data, study@endpt, as.factor)

    if(score_type == "CCI") {
        plt <- ggplot(score_data, 
                      aes(x=get(study@endpt),
                          y=SCORE, 
                          color=get(study@endpt))) + 
                geom_jitter(show.legend=FALSE, width=0.3) +
                scale_x_discrete(labels=c("Controls", "Cases")) +
                labs(title=paste0(score_type, " Scores for ", study@endpt),
                     subtitle=get_study_subtitle(study),
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
                         subtitle=get_study_subtitle(study),
                         x="",
                         y=paste0(score_type, " Score")) +
                    theme_minimal() +
                    theme(text=element_text(size=21))
    }

    file_path <- check_and_get_file_path(score_type=score_type,
                                         study=study,
                                         write_res=write_res,
                                         res_dir=res_dir,
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

