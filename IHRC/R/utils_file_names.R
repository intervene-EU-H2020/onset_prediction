#' Creates directory name for the downsampling factor
#' 
#' @inheritParams run_surv_studies
#' 
#' @author Kira E. Detrois
get_down_dir <- function(downsample_fctr) {
    ifelse(all(is.na(downsample_fctr)),
           "no_down/",
           paste0("down_", downsample_fctr, "/")) 
}

#' Creats the file name for the score distribution plots
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_score_distr_file_name <- function(surv_ana) {
    plot_descr <- ""
    if(surv_ana@score_type == "CCI") {
        if(surv_ana@study@study_type == "forward") {
            plot_descr <- paste0("_", surv_ana@study@exp_age, "_to_", surv_ana@study@exp_age+surv_ana@study@exp_len)
        } else {
            plot_descr <- paste0("_until_", surv_ana@study@obs_end_date)
        }
    } 
    paste0(surv_ana@score_type, "_score_distr", plot_descr, ".png")
}

#' Creats the file name for the HR plots with risk grouping
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_hr_rg_file_name <- function(surv_ana) {
    if(surv_ana@study@study_type == "forward") {
        file_name <- paste0(surv_ana@study@endpt, "_e", surv_ana@study@exp_len, "_w", surv_ana@study@wash_len, "_o", surv_ana@study@obs_len, "_", surv_ana@score_type)
    } else {
        file_name <- paste0(surv_ana@study@obs_end_date, "_o", surv_ana@study@obs_len, "_w", surv_ana@study@wash_len, "_", surv_ana@score_type)
    }
    if(surv_ana@score_type == "CCI") {
        file_name <- paste0(file_name, "_cut", surv_ana@bin_cut)
    } 
    paste0(file_name, "_HRs.png")
}

#' Creats the file name for the HR plots with continuous scores
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_hr_sd_file_name <- function(surv_ana) {
    if(surv_ana@study@study_type == "forward") {
        file_name <- paste0(surv_ana@study@endpt, "_e", surv_ana@study@exp_len, "_w", surv_ana@study@wash_len, "_o", surv_ana@study@obs_len, "_", surv_ana@score_type)
    } else {
        file_name <- paste0(surv_ana@study@obs_end_date, "_o", surv_ana@study@obs_len, "_w", surv_ana@study@wash_len, "_", surv_ana@score_type)
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
get_score_cut_file_name <- function(surv_ana) {
    file_name <- paste0(Istudy::get_study_file_name(surv_ana@study), "_", surv_ana@score_type, "_cut_log.txt")
    return(file_name)
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
get_endpt_score_file_name <- function(surv_ana) {
    paste0(Istudy::get_study_file_name(surv_ana@study), "_",surv_ana@score_type, "_score.png")
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
get_surv_file_name <- function(surv_ana) {
    paste0(Istudy::get_study_file_name(surv_ana@study), "_", surv_ana@score_type, "_surv.png")
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
get_coxph_res_file_name <- function(surv_ana) {
    if(surv_ana@study@obs_end_date == as.Date("3000/01/01")) {
        file_name <- paste0("e", surv_ana@study@exp_len, "_w", surv_ana@study@wash_len, "_o", 
        surv_ana@study@obs_len, "_", surv_ana@score_type)
    } else {
        file_name <- paste0(surv_ana@study@obs_end_date, "_o", surv_ana@study@obs_len, "_w", surv_ana@study@wash_len)
    }  
    if(surv_ana@score_type == "CCI") {
        file_name <- paste0(file_name, "_cut", surv_ana@bin_cut, "_coxph.tsv")
    } else {
        file_name <- paste0(file_name, "_coxph.tsv")
    }
}