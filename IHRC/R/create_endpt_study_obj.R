#' Creates an S4 study object
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
                                   endpt="J10_ASTHMA",
                                   exp_age=NULL,
                                   exp_len=NULL,
                                   wash_len=2,
                                   obs_len=8,
                                   obs_end_date=as.Date("2021/01/01"),
                                   downsample_fctr=NA_integer_,
                                   ancs=c("EAS")) {

    if(study_type == "forward") {
        study <- methods::new("study",
                              study_data=study_data,
                              study_type=study_type,
                              endpt=endpt,
                              exp_age=exp_age,
                              exp_len=exp_len,
                              wash_len=wash_len,
                              obs_len=obs_len,
                              downsample_fctr=downsample_fctr,
                              ancs=ancs)
    } else if(study_type == "backward") {
        if(!is.null(exp_len)) {
            exp_age <- NA_integer_
        } else {
            exp_len <- NA_integer_
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
                              downsample_fctr=downsample_fctr,
                              ancs=ancs)
    }
    
    return(study)
}