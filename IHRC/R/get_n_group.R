#' Gets the number of cases of a score group for a given endpoint
#' 
#' @param pheno_score_data A data.frame with at least columns
#'                          SCORE_GROUP and the columns defined
#'                          by the variabel `endpt`.
#' @param groups A character. The groups in the Cox-PH model.
#' @param endpt A character. The column name of the current endpoint 
#'                              of interest.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_n_group_cases <- function(pheno_score_data, 
                              group_col_name,
                              groups,
                              endpt) {
    get_n_group_indvs(pheno_score_data=pheno_score_data,
                      group_col_name=group_col_name, 
                      groups=groups, 
                      endpt=endpt, 
                      indv_type="cases")
}

#' Gets the number of controls of a score group for a given 
#' endpoint
#' 
#' @inheritParams get_n_group_cases
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_n_group_cntrls <- function(pheno_score_data, 
                               group_col_name,
                               groups,
                               endpt) {
    get_n_group_indvs(pheno_score_data=pheno_score_data, 
                      group_col_name=group_col_name,
                      groups=groups, 
                      endpt=endpt, 
                      indv_type="cntrls")
}

#' Gets the number of cases or controls of a score group for a given
#' endpoint
#' 
#' @inheritParams get_n_group_cases
#' @param indv_type A character. Defines whether to get the cases
#'                      or controls for each group. Has to be
#'                      either `cntrls`, or `cases`.
#' @author Kira E. Detrois
#' 
#' @export 
get_n_group_indvs <- function(pheno_score_data,
                              group_col_name,
                              groups,
                              endpt,
                              indv_type) {
    if(!(all(groups == "no groups"))) {
        n_indvs <- c()
        for(group in groups) {
            group_data <- dplyr::filter(pheno_score_data, 
                                        get(group_col_name) == group)
            if(indv_type == "cntrls") {
                crnt_n_indv <- Istudy::get_n_cntrls(group_data, endpt)
            } else if(indv_type == "cases") {
                crnt_n_indv <- Istudy::get_n_cases(group_data, endpt)
            }
            n_indvs <- c(n_indvs, crnt_n_indv)
        }
    } else {
        if(indv_type == "cntrls") {
            n_indvs <- Istudy::get_n_cntrls(pheno_score_data, endpt)
        } else if(indv_type == "cases") {
            n_indvs <- Istudy::get_n_cases(pheno_score_data, endpt)
        }
    }
    return(n_indvs)
}