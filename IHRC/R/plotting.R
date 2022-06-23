#' Creates a plot of the score distribution
#' 
#' @param envir A list with at least entries `write_res`, 
#'              `score_data`, `score_type`, `exp_age`, and `exp_len`, 
#'              and `res_dir`.
#'  
#' @return The ggplot object.
#' 
#' @import ggplot2
#' @export 
#' 
#' @author Kira E. Detrois
plot_score_distr <- function(envir) {
    score_data <- dplyr::filter(envir$score_data, 
                                    !is.na("SCORE"))
    plt <- ggplot(score_data, aes(x=get("SCORE"))) + 
                    geom_histogram(alpha=.8, fill="#214a2a", binwidth = 1.0)  +
                    labs(title=paste0(envir$score_type, " Score Histogram"),
                        subtitle=paste0("All ", nrow(score_data), " Individuals"),
                        x=envir$score_type,
                        y="Count") +
                    theme_minimal() + 
                    theme(text=element_text(size=21))

    if(envir$write_res) {
        check_res_dir(envir$write_res, envir$res_dir)
        plot_descr <- paste0(envir$exp_age, "_to_", envir$exp_age+envir$exp_len)
        ggsave(paste0(envir$res_dir, envir$score_type, "_score_distr_", plot_descr, ".png"),
               plot=plt, device="png", bg="white")
    }
    return(plt)
}

#' Plots a jittered scatter
#' 
#' Plots a jittered scatter plot of the cases and controls scores
#' eligible under the current study setup.
#' 
#' @param envir A list with at least entries `write_res`, 
#'              `elig_endpt_indv`, `endpt`, `score_type`, and `exp_age`, 
#'              and `exp_len`, `wash_len`, `obs_len`, and `res_dir`.
#' 
#' @return A ggplot object
#' 
#' @import ggplot2
#' @export 
#' 
#' @author Kira E. Detrois
plot_endpt_score_distr <- function(envir) {
        pheno_score_data <- dplyr::mutate_at(envir$pheno_score_data, 
                                             envir$endpt, 
                                             as.factor)
        print(table(pheno_score_data$SCORE))
        plt <- ggplot(pheno_score_data, 
                      aes(x=get(envir$endpt),
                          y=SCORE, 
                          fill=get(envir$endpt))) + 
                    geom_boxplot(show.legend=FALSE) +
                    scale_x_discrete(labels=c("Controls", "Cases")) +
                    labs(title=paste0(envir$score_type, " Scores for ", envir$endpt),
                        subtitle=paste0("Age: ", envir$exp_age, " Exp: ", envir$exp_len, " Wash: ", envir$wash_len, " Obs: ", envir$obs_len, " Years"),
                        x="",
                        y=paste0(envir$score_type, " Score")) +
                    theme_minimal() +
                    theme(text=element_text(size=21))
    if(envir$write_res) {
        check_res_dir(envir$write_res, envir$res_dir)
        file_path <- paste0(envir$res_dir,
                            Istudysetup::get_study_file_name(envir), 
                            "_", 
                            envir$score_type, 
                            "_score.png")

        ggsave(file_path, plt, device="png", bg="white")
    }

    return(plt)
}