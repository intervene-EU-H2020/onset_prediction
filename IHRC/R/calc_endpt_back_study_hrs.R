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
calc_endpt_back_study_hrs <- function(elig_score_data, 
                                      score_type,
                                      back_study,
                                      obs_end,
                                      endpt_studies,
                                      downsample_fctr=NULL,
                                      ancs=NA_character_,
                                      covs=c("SEX", "YEAR_OF_BIRTH"),
                                      bin_cut=1,
                                      min_indvs=5,
                                      write_res=FALSE,
                                      res_dir=NULL) {
    endpt_hrs_tib <- create_empty_endpt_hrs_tib()   
    
    coxph <- get_study_coxph_mdl(pheno_score_data=elig_score_data,
                                 score_type=score_type,
                                 study=back_study,
                                 covs=covs,
                                 pred_score="SCORE_GROUP",
                                 bin_cut=bin_cut,
                                 write_res=write_res,
                                 res_dir=res_dir)
    endpt_hrs_tib <- add_coxph_res_row(
                                endpt_hrs_tib=endpt_hrs_tib,
                                coxph_mdl=coxph$mdl,
                                score_type=score_type,
                                study=back_study,
                                elig_indv=coxph$data,
                                min_indvs=min_indvs)

    return(endpt_hrs_tib)
}