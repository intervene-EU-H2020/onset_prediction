#' Downsamples the number of controls in the data
#' 
#' Given the same selectionf of cases will always sample 
#' the same individuals as controls, to allow reproducibility 
#' of the study selection.
#' 
#' @inheritParams adj_case_cntrl_status
#' @param down_fctr An integer. Defines how many controls there
#'                                   should be for every case.
#'                                   Default is NA, which means no
#'                                   downsampling is performed.
#' 
#' @return The downsampled data.frame.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
downsample_cntrls <- function(study_data,
                              endpt,
                              down_fctr) {
    if(!is.na(down_fctr)) {
        n_cases <- get_n_cases(study_data, endpt)
        n_cntrls <- down_fctr*n_cases 

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