#' Creates a vector of study objects for the different endpoints
#' 
#' @inheritParams calc_endpt_studies_hrs
#' @param exp_age An integer. Age at which exposure period starts 
#'                            (in years).
#' @inheritParams run_age_exp_endpt_studies
#' 
#' @export 
#' 
#' @author Kira E. Detrois
create_endpts_study_objs <- function(endpts,
                                     exp_age=30,
                                     exp_len=10,
                                     wash_len=2,
                                     obs_len=8,
                                     downsample_fctr=NULL,
                                     ancs=c("EUR")) {
    endpt_studies <- list()
    for(endpt in endpts) {
        endpt_studies[[endpt]] <- methods::new("study",
                                         endpt=endpt,
                                         exp_age=exp_age,
                                         exp_len=exp_len,
                                         wash_len=wash_len,
                                         obs_len=obs_len,
                                         downsample_fctr=downsample_fctr,
                                         ancs=ancs)
    }
    return(endpt_studies)
}