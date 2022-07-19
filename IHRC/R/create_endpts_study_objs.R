#' Creates an S4 study object
#' 
#' @inheritParams run_surv_studies
#' @inheritParams get_n_group_cases
#' @param exp_age An integer. Age at which exposure period starts
#'                   (in years).
#' 
#' @return An S4 study object. 
#' @export 
#' 
#' @author Kira E. Detrois
create_endpt_study_obj <- function(pheno_data,
                                   study_type="forward",
                                   endpt="J10_ASTHMA",
                                   exp_age=NULL,
                                   exp_len=10,
                                   wash_len=2,
                                   obs_len=8,
                                   obs_end=NULL,
                                   downsample_fctr=NULL,
                                   ancs=c("EAS")) {

    if(study_type == "forward") {
        study <- methods::new("study",
                              study_type=study_type,
                              endpt=endpt,
                              exp_age=exp_age,
                              exp_len=exp_len,
                              wash_len=wash_len,
                              obs_len=obs_len,
                              downsample_fctr=ifelse(is.null(downsample_fctr), NA_integer_, downsample_fctr),
                              ancs=ancs)
    } else if(study_type == "backward") {
        study <- Istudy::get_backward_study(pheno_data=pheno_data,
                                            endpt=endpt,
                                            wash_len=wash_len,
                                            obs_len=obs_len,
                                            obs_end=obs_end,
                                            downsample_fctr=downsample_fctr,
                                            ancs=ancs)
    }
    
    return(study)
}