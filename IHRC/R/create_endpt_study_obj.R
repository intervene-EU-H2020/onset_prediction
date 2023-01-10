#' Creates an S4 study object
#' 
#' This function creates a study object for a survival analysis, which includes 
#' the data, endpoints, and other parameters needed to perform the analysis. 
#' The study object can be passed to other functions to perform various 
#' calculations and analyses.
#' 
#' @inheritParams run_surv_studies
#' @inheritParams get_n_group_cases
#' @param exp_age An integer. Age at which exposure period starts
#'                   (in years).
#' 
#' @return An S4 study object. 
#' 
#' @importClassesFrom Istudy study
#' @export 
#' 
#' @author Kira E. Detrois
create_endpt_study_obj <- function(study_data,
                                   study_type="forward",
                                   preds="CCI",
                                   endpt="J10_ASTHMA",
                                   exp_age=NULL,
                                   exp_len=NULL,
                                   wash_len=2,
                                   obs_len=8,
                                   obs_end_date=as.Date("2021/01/01"),
                                   down_fctr=NA_integer_,
                                   ancs=c("EAS"),
                                   obs_age_range=c(0,200),
                                   write_res=FALSE,
                                   res_dir=NA_character_) {

    # Check if the study type is "backward" and set the exp_age and exp_len values accordingly
    if (study_type == "backward") {
        if (!is.null(exp_len)) {
        exp_age <- NA_integer_
        } else {
        exp_len <- NA_integer_
        }
    }
    # Set the results directory, if provided
    if(!is.na(res_dir)) {
        res_dir <- paste0(res_dir, "logs/", get_preds_file_name(preds), "_logs/")
    }

    study <- methods::new("study",
                          study_data=study_data,
                          study_type=study_type,
                          endpt=endpt,
                          exp_age=exp_age,
                          exp_len=exp_len,
                          wash_len=wash_len,
                          obs_len=obs_len,
                          obs_end_date=obs_end_date,
                          down_fctr=down_fctr,
                          ancs=ancs,
                          obs_age_range=obs_age_range,
                          write_res=write_res,
                          res_dir=res_dir)
    return(study)
}