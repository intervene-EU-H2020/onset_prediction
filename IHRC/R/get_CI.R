
#' Calculate 95% Confidence Interval
#'
#' This function calculates the 95% confidence interval for a set of 
#' estimates using the standard error of the estimates.
#'
#' @param ML A numeric vector of estimates.
#' @param SE A numeric vector of standard errors.
#' @return A list with two elements, `neg` (negative limits of the confidence interval) and 
#' `pos` (positive limits of the confidence interval).
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_CI <- function(ML, SE) {
    CIneg <- ML-1.96*SE
    CIpos <- ML+1.96*SE
    return(list(neg=CIneg, pos=CIpos))
}

