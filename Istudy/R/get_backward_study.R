#' Creats and S4 study object from a set time point backwards
#' 
#' 
#' Considers all individuals at a set time point. The observation and 
#' washout period are calcualted backwards from this time point. 
#' While the exposure period will be different for each individual 
#' depending on their birth date. 
#' 
#' If no end date is provided with input variable `obs_len` uses the 
#' most recent date across the different `_DATE` columns. 
#' 
#' @param pheno_data A data.frame with columns i.e. `J10_ASTHMA_DATE`, and
#'                      `I9_VTE_DATE`. The diagnosis dates for different
#'                      endpoints.
#' @inheritParams adj_case_cntrl_status
#' @param wash_len An integer. Length of the washout period (in years).
#' @param obs_len An integer. Legnth of the observation period (in years).
#' @param obs_end A Date. The end of the observation period.
#' @param downsample_fctr A numeric. Defines how many controls there
#'                                   should be for every case.
#'                                   Default is NA, which means no
#'                                   downsampling is performed.
#' @param ancs A character (vector). The ancestries to consider.
#' 
#' @export 
get_backward_study <- function(pheno_data,
                               endpt,
                               wash_len=2,
                               obs_len=8,
                               obs_end=NULL,
                               downsample_fctr=NA_real_,
                               ancs=NA_character_) {

    exp_len <- get_indv_exp_len(pheno_data, wash_len, obs_len, obs_end)
    obs_end <- get_obs_end(pheno_data, obs_end)
    study <- methods::new("study",
                          endpt=endpt,
                          exp_age=0,
                          exp_len=exp_len,
                          exp_ids=pheno_data$ID,
                          wash_len=wash_len,
                          obs_len=obs_len,
                          obs_end=obs_end,
                          downsample_fctr=downsample_fctr,
                          ancs=ancs,
                          study_type="backward")
    return(study)
}
