#' Gets the number of cases of a score group for a given endpoint
#' 
#' @inheritParams calc_studies_hrs
#' @param groups A character. The groups in the Cox-PH model.
#' @inheritParams add_coxph_res_row
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_n_group_cases <- function(elig_indv, 
                              groups,
                              endpt) {
    if(!(all(groups == "no groups"))) {
        n_cases <- c()
        for(group in groups) {
            group_data <- dplyr::filter(elig_indv, SCORE_GROUP == group)
            n_cases <- c(n_cases, 
                         Istudy::get_n_cases(group_data, endpt))
        }
    } else {
        n_cases <- Istudy::get_n_cases(elig_indv,
                                      endpt)
    }
    return(n_cases)
}

#' Gets the number of controls of a score group for a given 
#' endpoint
#' 
#' @inheritParams calc_studies_hrs
#' @param groups A character. The groups in the Cox-PH model.
#' @inheritParams add_coxph_res_row
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_n_group_cntrls <- function(elig_indv, 
                               groups,
                               endpt) {
    if(!(all(groups == "no groups"))) {
        n_ctrls <- c()
        for(group in groups) {
            group_data <- dplyr::filter(elig_indv, SCORE_GROUP == group)
            n_ctrls <- c(n_ctrls, 
                        Istudy::get_n_cntrls(group_data, endpt))
        }
    } else {
        n_ctrls <- Istudy::get_n_cntrls(elig_indv,
                                            endpt)
    }
    return(n_ctrls)
}
