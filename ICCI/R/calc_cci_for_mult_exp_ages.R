
#' Creates a named list of CCI score results for different exposure ages
#' 
#' @param icd_data A data.frame. For details see \link[ICCI]{calc_cci}
#' @param exp_ages An integer (vector). Ages at which exposure period 
#'                  starts (in years).
#' @param exp_len An integer. Length of the exposure period
#'                            (in years).
#' 
#' @return A list. The CCI score results data.frames for the different 
#'                 exposure windows.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
calc_cci_for_mult_exp_ages <- function(icd_data,
                                       exp_ages=c(20,30,40,50),
                                       exp_len=10) {
   score_age_data <- list()
   for(exp_age in exp_ages) {
      score_age_data[[as.character(exp_age)]] <- ICCI::calc_cci(icd_data, 
                                                   exp_start=exp_age, 
                                                   exp_end=exp_age+exp_len) %>% 
                        dplyr::rename(SCORE=CCI_score)
   }

   return(score_age_data)
}