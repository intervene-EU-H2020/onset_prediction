#' Downsamples the number of controls in the data
#' 
#' @param endpt A string. The column name of the current endpoint of 
#'                        interest.
#' @param pheno_data A data.frame with at least the column defined in 
#'                   the variable endpoint.
#' @param downsample_fctr A numeric. Defines how many controls there
#'                                   should be for every case.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
downsample_cntrls <- function(pheno_data,
                              study) {
    
    study@n_cases <- get_n_cases(pheno_data, study@endpt)
    study@n_cntrls <- study@downsample_fctr*study@n_cases 

    cntrl_idxs <- which(pheno_data[[study@endpt]] == 0)
    case_idxs <- which(pheno_data[[study@endpt]] == 1)
    cntrl_idxs_down <- sample(cntrl_idxs, size=study@n_cntrls, replace=FALSE)

    select_idxs <- sort(c(cntrl_idxs_down, case_idxs))

    return(pheno_data[select_idxs,])
}