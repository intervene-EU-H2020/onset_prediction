 
 #' Calcualtes HR from a Cox-PH model for each endpoint
#' 
#' Fits a Cox-proportional hazards model for the different endpt_studies. 
#' 
#' Selects individuals from the data that are eligible given each of the 
#' endpt_studies. For more details see package `Istudy` and function
#' \link[Istudy]{get_study_elig_indv}. Runs the analyis based on risk 
#' groups. For `PRS` these are based on the quantiles, where the 40-60% 
#' group is the reference group. For `CCI` compares two groups based on a 
#' cutoff defined in `bin_cut`.
#' 
#' For i.e. endpoint `J10_ASTHMA` and `covs = c("SEX", "YEAR_OF_BIRTH")` 
#' the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE_GROUP + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @param pheno_data A data.frame with at least the columns: 
#'                   `ID`, the columns specified in `covs` and 
#'                   i.e. `J10_ASTHMA`, and `J10_ASTHMA_AGE_DAYS` where 
#'                   the columns are the study endpoint and date, which 
#'                   will differ depending on the chosen `endpts`.
#' @param score_data A data.frame. If `score_type = CCI` needs at least 
#'                   column `SCORE`. If `score_type = PRS` columns of 
#'                   form`J10_ASTHMA_PRS` depending on the chosen `endpts`.
#' @param score_type A character. The name of the score used for the model,
#'                                i.e. CCI, or PheRS.
#' @param endpt_studies A vector of S4 classes representing the study setups.
#' @param covs A vector of characters. The column names of the covariates 
#'              to add to the predictor of the Cox-PH model.
#' @param bin_cut A numeric. The binary cutoff value for classifying high
#'                  and low score individuals. Currently only in use if
#'                  the `score_type == CCI`.
#' @param write_res A boolean. Defines whether to save the results to 
#'                             files.
#' @param res_dir A character. The directory to write the results and
#'                             log to.
#' 
#' @return A tibble with columns `Endpoint`, `Score`, `Group`, 
#'          `N_controls`, `N_cases`, `beta`, `std_errs`, `p_val`, `HR`, 
#'          `CI_pos`, `CI_neg`. The Cox-PH analysis results for all 
#'          selected endpoints. 
#' 
#' @export 
#' 
#' @author Kira E. Detrois
plot_studies_score_distr <- function(pheno_data, 
                                     score_data,
                                     score_type,
                                     endpt_studies,
                                     min_indvs=5,
                                     write_res=FALSE,
                                     res_dir=NULL) {

    for(study in endpt_studies) {
        plot_score_distr(score_data=score_data, 
                         score_type=score_type, 
                         study=study, 
                         write_res=write_res, 
                         res_dir=res_dir)
        pheno_score_data <- get_elig_pheno_score_data(
                                pheno_data=pheno_data,
                                score_data=score_data,
                                score_type=score_type,
                                study=study,
                                write_res=write_res,
                                res_dir=res_dir)
        plot_endpt_score_distr(score_data=pheno_score_data,
                               score_type=score_type,
                               study=study,
                               min_indvs=min_indvs,
                               write_res=write_res,
                               res_dir=res_dir)

    }
}