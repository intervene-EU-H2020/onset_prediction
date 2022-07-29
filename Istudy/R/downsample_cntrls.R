#' Downsamples the number of controls in the data
#' 
#' Given the same number of cases and the same indices
#' will always sample the same individuals as controls. 
#' This allows reproducibility of the study selection.
#' 
#' @inheritParams get_n_cases
#' @inheritParams get_study_elig_indv
#' 
#' @return The downsampled data.frame.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
downsample_cntrls <- function(study_data,
                              endpt,
                              downsample_fctr) {
    if(!is.na(downsample_fctr)) {
        n_cases <- get_n_cases(study_data, endpt)
        n_cntrls <- downsample_fctr*n_cases 

        cntrl_idxs <- which(study_data[[endpt]] == 0)
        case_idxs <- which(study_data[[endpt]] == 1)

        set.seed(1923) # Making control selection reproducible
        if(length(cntrl_idxs) > n_cntrls) {
            cntrl_idxs_down <- sample(cntrl_idxs, size=n_cntrls, replace=FALSE)
            select_idxs <- sort(c(cntrl_idxs_down, case_idxs))
            study_data <- study_data[select_idxs,]
        }
    }
    return(study_data)
}   