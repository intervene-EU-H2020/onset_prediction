#' Calcualtes HR from a Cox-PH model for each exposure age and endpoint
#' 
#' For more details see \link{calc_studies_hrs} function documentation.
#' 
#' @inheritParams calc_studies_hrs
#' @param score_ages_data A named vector of tibbles. 
#'                        The score data for each indiviuals for different 
#'                        exposure periods. 
#' @param endpts A string. The column names of the endpoints of 
#'                         interest.
#' @param exp_ages An integer. Age at which exposure period starts 
#'                            (in years).
#' @param exp_len An integer. Length of the exposure period
#'                            (in years).
#' @param wash_len An integer. Length of the washout period
#'                             (in years).
#' @param obs_len An integer. Length of the prediction period
#'                            (in years).
#' @param downsample_fctr A numeric. Defines how many controls there
#'                                   should be for every case.
#'                                   Default is NA, which means no
#'                                   downsampling is performed.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
run_age_exp_studies <- function(pheno_data, 
                                score_ages_data,
                                score_type,
                                bin_cut=1,
                                endpts=c("J10_ASTHMA"),
                                exp_ages=c(20,30,40),
                                exp_len=10,
                                wash_len=2,
                                obs_len=8,
                                downsample_fctr=NA_real_,
                                covs=c("SEX", "YEAR_OF_BIRTH"),
                                write_res=FALSE,
                                res_dir=NA) {
    for(exp_age in exp_ages) {
        studies <- create_endpts_study_objs(endpts, 
                                            exp_age,
                                            exp_len,
                                            wash_len,
                                            obs_len,
                                            downsample_fctr)
        calc_studies_hrs(pheno_data, 
                         score_ages_data[[exp_age]],
                         score_type,
                         bin_cut,
                         studies,
                         covs,
                         write_res,
                         res_dir)
    }
    read_and_plot_age_hrs(res_dir,
                          score_type,
                          bin_cut,
                          exp_len,
                          wash_len,
                          obs_len,
                          covs,
                          write_res) 
}

#' Creates a vector of study objects for the different endpoints
#' 
#' @inheritParams calc_studies_hrs
#' @param exp_age An integer. Age at which exposure period starts 
#'                            (in years).
#' @inheritParams run_age_exp_studies
#' 
#' @export 
#' 
#' @author Kira E. Detrois
create_endpts_study_objs <- function(endpts,
                                     exp_age=30,
                                     exp_len=10,
                                     wash_len=2,
                                     obs_len=8,
                                     downsample_fctr=NA_real_,
                                     ancs=c("EUR")) {
    studies <- list()
    for(endpt in endpts) {
        studies[[endpt]] <- methods::new("study",
                                         endpt=endpt,
                                         exp_age=exp_age,
                                         exp_len=exp_len,
                                         wash_len=wash_len,
                                         obs_len=obs_len,
                                         downsample_fctr=downsample_fctr,
                                         ancs=c("EUR"))
    }
    return(studies)
}