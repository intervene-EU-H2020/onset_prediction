#' Downsamples the number of controls in the data
#' 
#' Given the same number of cases and the same indices
#' will always sample the same individuals. This allows 
#' reproducibility of the study selection.
#' 
#' @inheritParams get_n_cases
#' @inheritParams get_study_elig_indv
#' 
#' @return The downsampled data.frame.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
downsample_cntrls <- function(pheno_data,
                              study) {

    n_cases <- get_n_cases(pheno_data, study@endpt)
    n_cntrls <- study@downsample_fctr*n_cases 

    cntrl_idxs <- which(pheno_data[[study@endpt]] == 0)
    case_idxs <- which(pheno_data[[study@endpt]] == 1)
    set.seed(1923)
    cntrl_idxs_down <- sample(cntrl_idxs, size=n_cntrls, replace=FALSE)

    select_idxs <- sort(c(cntrl_idxs_down, case_idxs))

    return(pheno_data[select_idxs,])
}