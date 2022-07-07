#' Calcualtes HR from a Cox-PH model for each exposure age and endpoint
#' 
#' For more details see \link{calc_endpt_studies_hrs} function documentation.
#' 
#' @inheritParams calc_endpt_studies_hrs
#' @param score_ages_data A named vector of tibbles. 
#'                        The score data for each indiviuals for different 
#'                        exposure periods. 
#' @param endpts A string. The column names of the endpoints of 
#'                         interest.
#' @param exp_ages An integer (vector). Ages at which exposure period 
#'                  starts (in years).
#' @param exp_len An integer. Length of the exposure period ^
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
run_age_exp_endpt_studies <- function(pheno_data, 
                                score_ages_data,
                                score_type,
                                endpts=c("J10_ASTHMA"),
                                exp_ages=c(20,30,40),
                                exp_len=10,
                                wash_len=2,
                                obs_len=8,
                                downsample_fctr=NA_real_,
                                ancs="EUR",
                                covs=c("SEX", "YEAR_OF_BIRTH"),
                                bin_cut=1,
                                write_res=FALSE,
                                res_dir=NA) {
    all_age_hrs_tib <- tibble::tibble()
    for(exp_age in exp_ages) {
        endpt_studies <- create_endpts_study_objs(
                                endpts=endpts, 
                                exp_age=exp_age,
                                exp_len=exp_len,
                                wash_len=wash_len,
                                obs_len=obs_len,
                                downsample_fctr=downsample_fctr,
                                ancs=ancs)

        if(score_type == "CCI") {
            curnt_score_data <- score_ages_data[[as.character(exp_age)]]
        } else { # The data is the same for all exposure ages
            curnt_score_data <- score_ages_data
        }
        plot_studies_score_distr(pheno_data=pheno_data,
                                 score_data=curnt_score_data,
                                 score_type=score_type,
                                 endpt_studies=endpt_studies,
                                 write_res=write_res,
                                 res_dir=res_dir)
        endpt_hrs_tib <- calc_endpt_studies_hrs(
                                  pheno_data=pheno_data, 
                                  score_data=curnt_score_data,
                                  score_type=score_type,
                                  endpt_studies=endpt_studies,
                                  covs=covs,
                                  bin_cut=bin_cut,
                                  write_res=write_res,
                                  res_dir=res_dir)
        all_age_hrs_tib <- dplyr::bind_rows(all_age_hrs_tib, 
                                            endpt_hrs_tib)
    }
    write_hr_res_file(endpt_hrs_tib=all_age_hrs_tib,
                      score_type=score_type,
                      endpt_studies=endpt_studies,
                      bin_cut=bin_cut,
                      write_res=write_res,
                      res_dir=res_dir)

    plot_age_hrs(coxph_hr_res=all_age_hrs_tib,
                 score_type=score_type,
                 exp_len=exp_len,
                 wash_len=wash_len,
                 obs_len=obs_len,
                 covs=covs,
                 bin_cut=bin_cut,
                 write_res=write_res,
                 res_dir=res_dir) 
}

