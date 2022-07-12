#' @export 
#' 
#' @author Kira E. Detrois
filter_too_old_and_young <- function(pheno_data,
                                     max_age=90) {
    check_cols_exist(pheno_data, c("EXP_LEN"), "filter_too_old_and_young")
    pheno_data <- dplyr::filter(pheno_data, EXP_LEN > 0)
    pheno_data <- dplyr::filter(pheno_data, EXP_LEN <= max_age)
    return(pheno_data)
}
