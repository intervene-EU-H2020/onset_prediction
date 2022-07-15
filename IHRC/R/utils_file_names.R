#' Creats the file name for the score distribution plots
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_score_distr_file_name <- function(study,
                                      score_type,
                                      obs_end=NULL) {
    plot_descr <- ""
    if(score_type == "CCI") {
        if(is.null(obs_end)) {
            plot_descr <- paste0("_", study@exp_age, "_to_", study@exp_age+study@exp_len)
        } else {
            plot_descr <- paste0("_til_", obs_end)
        }
    } 
    paste0(score_type, "_score_distr", plot_descr, ".png")
}

#' Creats the file name for the HR plots
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_hr_file_name <- function(study,
                             score_type,
                             bin_cut=1,
                             obs_end=NULL) {
    file_name <- paste0(Istudy::get_study_file_name(study, obs_end), "_", score_type)
    if(score_type == "CCI") {
        file_name <- paste0(file_name, "_cut", bin_cut)
    } 
    paste0(file_name, "_HRs.png")
}

#' Creats the file name for the score cut log file
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_score_cut_file_name <- function(study,
                                    score_type,
                                    obs_end=NULL) {
    paste0(Istudy::get_study_file_name(study, obs_end), "_", score_type, "_cut_log.txt")
}

#' Creats the file name for endpoint specific score distribution plot
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_endpt_score_file_name <- function(study,
                                      score_type,
                                      obs_end=NULL) {
    paste0(Istudy::get_study_file_name(study, obs_end), "_", score_type, "_score.png")
}

#' Creats the file name for endpoint specific score distribution plot
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_surv_file_name <- function(study,
                               score_type,
                               obs_end=NULL) {
    paste0(Istudy::get_study_file_name(study, obs_end), "_", score_type, "_surv.png")
}


#' Creats the file name for Cox-PH model results file
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_coxph_res_file_name <- function(study,
                                    score_type,
                                    bin_cut,
                                    obs_end=NULL) {
    if(is.null(obs_end)) {
        file_name <- paste0("e", study@exp_len, "_w", study@wash_len, "_o", 
        study@obs_len, "_", score_type)
    } else {
        file_name <- paste0(obs_end, "_o", study@obs_len, "_w", study@wash_len)
    }  
    if(score_type == "CCI") {
        file_name <- paste0(file_name, "_cut", bin_cut, "_coxph.tsv")
    } else {
        file_name <- paste0(file_name, "_coxph.tsv")
    }
}
