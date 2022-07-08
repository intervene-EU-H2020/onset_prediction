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
    get_n_group_indvs(pheno_score_data=pheno_score_data, 
                      groups=groups, 
                      endpt=endpt, 
                      indv_type="cases")
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
    get_n_group_indvs(pheno_score_data=pheno_score_data, 
                      groups=groups, 
                      endpt=endpt, 
                      indv_type="cntrls")
}

#' Gets the number of cases or controls of a score group for a given
#' endpoint
#' 
#' @inheritParams calc_endpt_studies_hrs
#' @param groups A character. The groups in the Cox-PH model.
#' @inheritParams add_coxph_res_row
#' @param indv_type A character. Can either be `cases` or `cntrls`. 
#' 
#' @author Kira E. Detrois
get_n_group_indvs <- function(pheno_score_data,
                              groups,
                              endpt,
                              indv_type) {
    if(!(all(groups == "no groups"))) {
        n_indvs <- c()
        for(group in groups) {
            group_data <- dplyr::filter(pheno_score_data, 
                                        SCORE_GROUP == group)
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