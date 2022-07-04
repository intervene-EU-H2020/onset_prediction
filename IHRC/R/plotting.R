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
                         subtitle=paste0(nrow(score_data), "  Individuals ", plot_descr),
                         x=score_type,
                         y="Count") +
                    coord_cartesian(xlim=c(0,15)) +
                    theme_minimal() + 
                    theme(text=element_text(size=21))
    } else {
        plt <- ggplot(score_data, aes(x=get("SCORE"))) + 
                    geom_histogram(alpha=.8, fill="#214a2a")  +
                    labs(title=paste0(score_type, " Score Histogram"),
                        subtitle=paste0(nrow(score_data), " Individuals ", plot_descr),
                        x=score_type,
                        y="Count") +
                    theme_minimal() + 
                    theme(text=element_text(size=21))
    }



    file_path <- check_and_get_file_path(score_type,
                                         study,
                                         write_res,
                                         res_dir,
                                         res_type="distr")
    if(!is.na(file_path)) {
        ggsave(file_path,
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

    file_path <- check_and_get_file_path(score_type,
                                         study,
                                         write_res,
                                         res_dir,
                                         res_type="endpt")
    if(!is.na(file_path)) {
        ggsave(file_path,
               plot=plt, 
               device="png", 
               bg="white")
    }
    return(plt)
}

get_study_subtitle <- function(study) {
    paste0("Age: ", study@exp_age, " Exp: ", study@exp_len, " Wash: ", study@wash_len, " Obs: ", study@obs_len, " Years")
}

read_and_plot_age_hrs <- function(res_dir,
                                  score_type,
                                  bin_cut,
                                  exp_len,
                                  wash_len,
                                  obs_len,
                                  covs,
                                  write_res) {
    coxph_res <- read_coxph_res_file(res_dir, score_type)
    comp_group <- get_comp_group(score_type)
    endpts <- unique(coxph_res$Endpoint)
    for(endpt in endpts) {
        endpt_coxph_res <- dplyr::filter(coxph_res,
                                         Group == comp_group,
                                         Endpoint == endpt)
        plt <- ggplot2::ggplot(endpt_coxph_res, aes(x=Age, y=HR)) +
                labs(title=paste0(endpt),
                     subtitle=paste0("Exp: ", exp_len, " Wash: ", wash_len, " Obs: ", obs_len, " Years"),
                     caption=get_hr_plot_caption(score_type,
                                                 bin_cut,
                                                 covs),
                     x="Age",
                     y="HR") +
                geom_point() + 
                geom_errorbar(aes(ymin=CI_neg, ymax=CI_pos), width=.1) +
                geom_hline(yintercept=1.0) + 
                coord_cartesian(ylim=c(-1,10)) +
                theme_minimal() +
                theme(text=element_text(size=21),
                      plot.caption=element_text(size=10, hjust=0))
        # Hacking the system a bit by putting random age that won't be 
        # used.
        study <- methods::new("study",
                               endpt=endpt,
                               exp_age=20,
                               exp_len=exp_len,
                               wash_len=wash_len,
                               obs_len=obs_len)
        file_path <- check_and_get_file_path(score_type,
                                            study,
                                            write_res,
                                            res_dir,
                                            res_type="HRs")
        if(!is.na(file_path)) {
            ggsave(file_path,
                plot=plt, 
                device="png", 
                bg="white")
        }
    }
}

get_comp_group <- function(score_type,
                           bin_cut=1) {
    if(score_type == "CCI") {
        comp_group <- paste0(">", bin_cut)
    } else if(score_type == "PRS") {
        comp_group <- "(Group 99% - Group 100%]"
    }
    return(comp_group)
}

get_hr_plot_caption <- function(score_type,
                                bin_cut,
                                covs) {
    if(score_type == "CCI") {
        plot_caption <- paste0("CCI >", bin_cut, " vs. <=", bin_cut)

    } else if(score_type == "PRS") {
        plot_caption <- "Top 1% vs. 40-60%"
    }
    plot_caption <- paste0(plot_caption, "   Surv ~ PRS + ", get_pretty_covs_string(covs))
}

get_pretty_covs_string <- function(covs) {
    covs <- stringr::str_replace_all(covs, "_", " ")
    n_pcs <- sum(stringr::str_count(covs, "PC"))
    covs <- stringr::str_to_title(covs)
    if(n_pcs > 1) {
        covs <- covs[stringr::str_detect(covs, "Pc", negate=TRUE)]
        covs <- c(covs, "PCs")
    } else {
        covs[stringr::str_detect(covs, "Pc")] <- stringr::str_to_upper(covs[stringr::str_detect(covs, "Pc")])
    }
    return(stringr::str_flatten(covs, collapse=" + "))
}

read_coxph_res_file <- function(res_dir,
                                score_type) {
    coxph_res <- create_empty_coxph_res_tib()
    coxph_res_dir <- paste0(res_dir, score_type, "/coxph_res/")
    res_files <- list.files(coxph_res_dir, full.names=TRUE)
    for(res_file in res_files) {
        curnt_age <- sub("(.+/)(study_)([0-9]+)(_.+)+$", "\\3", res_file)
        curnt_coxph_res <- readr::read_delim(res_file, 
                                             delim="\t")
        curnt_coxph_res <- tibble::add_column(curnt_coxph_res, 
                                              Age=curnt_age)
        coxph_res <- dplyr::bind_rows(coxph_res, curnt_coxph_res)
    }
    return(coxph_res)
}