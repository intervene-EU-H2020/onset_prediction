
#' Creates a list with relevant information of the selected data
#' 
#' Gets only the columns relevant for the current endpoint.
#' Creates a list with the data, as well as the selected
#' study period intervals.
#' 
#' @inheritParams get_study_elig_indv
#' 
#' @return A list(`data`, `exp_age`, `exp_len`, `wash_len`, `obs_len`):
#'         \itemize{
#'          \item `data`: The actual data.frame.
#'          \item `exp_age` An integer. Age at which exposure period  
#'                            starts (in years).
#'          \item `exp_len` An integer. Length of the exposure period
#'                               (in years).
#'          \item `wash_len` An integer. Length of the washout period
#'                                (in years).
#'          \item `obs_len` An integer. Length of the prediction period
#'                               (in years).
#'          }
#' 
#' @author Kira E. Detrois
create_return_dt <- function(pheno_data,
                             endpt,
                             exp_age=30,
                             exp_len=10,
                             wash_len=2,
                             obs_len=8) {
    test_endpt_input_correct(as.list(environment()))
    test_length_vars_are_integers(as.list(environment()))
    
    elig_data <- dplyr::select(pheno_data, 
                               ID, 
                               SEX, 
                               DATE_OF_BIRTH, 
                               ANCESTRY, 
                               # Otherwise dplyr will throw error. 
                               # test_endpt_correct already checks that
                               # this is only a single string and not
                               # a vector
                               dplyr::all_of(endpt), 
                               paste0(endpt, "_AGE_DAYS"))

    elig_data <- list(data=elig_data, 
                      exp_age=exp_age,
                      exp_len=exp_len,
                      wash_len=wash_len,
                      obs_len=obs_len,
                      n_cases=get_n_cases(pheno_data, endpt),
                      n_ctrls=get_n_ctrls(pheno_data, endpt))
}