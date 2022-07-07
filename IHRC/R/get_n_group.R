#' Gets the number of cases of a score group for a given endpoint
#' 
#' @inheritParams calc_endpt_studies_hrs
#' @param groups A character. The groups in the Cox-PH model.
#' @inheritParams add_coxph_res_row
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_n_group_cases <- function(pheno_score_data, 
                              groups,
                              endpt) {
    if(!(all(groups == "no groups"))) {
        n_cases <- c()
        for(group in groups) {
            group_data <- dplyr::filter(pheno_score_data, 
                                        SCORE_GROUP == group)
            n_cases <- c(n_cases, 
                         Istudy::get_n_cases(group_data, endpt))
        }
    } else {
        n_cases <- Istudy::get_n_cases(pheno_score_data,
                                       endpt)
    }
    return(n_cases)
}

#' Gets the number of controls of a score group for a given 
#' endpoint
#' 
#' @inheritParams calc_endpt_studies_hrs
#' @param groups A character. The groups in the Cox-PH model.
#' @inheritParams add_coxph_res_row
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_n_group_cntrls <- function(pheno_score_data, 
                               groups,
                               endpt) {
    if(!(all(groups == "no groups"))) {
        n_cntrls <- c()
        for(group in groups) {
            group_data <- dplyr::filter(pheno_score_data, 
                                        SCORE_GROUP == group)
            n_cntrls <- c(n_cntrls, 
                        Istudy::get_n_cntrls(group_data, endpt))
        }
    } else {
        n_cntrls <- Istudy::get_n_cntrls(pheno_score_data,
                                            endpt)
    }
    return(n_cntrls)
}