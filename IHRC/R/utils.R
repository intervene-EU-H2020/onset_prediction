#' Joins the two data.frames 
#' 
#' Joins the two data.frames and removes any individuals without
#' a score available.
#' 
#' @inheritParams calc_studies_hrs
#' 
#' @return The joined data.frame
#' 
#' @author Kira E. Detrois
join_dfs <- function(pheno_data,
                     score_data,
                     score_type="CCI",
                     endpt=NA_character_) {
    score_data <- get_and_filter_endpt_scores(score_data, score_type, endpt)
    pheno_score_data <- dplyr::left_join(pheno_data,
                                         score_data,
                                         by="ID")
    pheno_score_data <- dplyr::filter(pheno_score_data, !is.na(SCORE))
    return(pheno_score_data)
}


#' 95% confidence interval given the ML estimator and SE
#' 
#' @param ML maximum likelihood estimator of the parameter
#' @param SE standard error of the ML estimator
get_CI <- function(ML, SE) {
    CIneg <- exp(ML-1.96*SE)
    CIpos <- exp(ML+1.96*SE)
    return(list(neg=CIneg, pos=CIpos))
}

#' Gets the number of cases
#' 
#' @inheritParams calc_studies_hrs
#' @param groups A character. The groups in the Cox-PH model.
#' @inheritParams add_coxph_row
#' 
#' @author Kira E. Detrois
n_group_cases <- function(elig_indv, 
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

get_group_ctrls <- function(elig_indv, 
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

#' @importFrom dplyr %>% 
get_and_filter_endpt_scores <- function(score_data,
                                        score_type,
                                        endpt) {
    if(score_type == "PRS") {
        prs_col_name <- paste0(endpt, "_PRS")
        score_data <- dplyr::select(score_data, 
                                    ID, 
                                    {{ prs_col_name }}) %>% 
                        dplyr::rename("SCORE" = {{ prs_col_name }})
    }
    score_data <- dplyr::filter(score_data, 
                                !is.na("SCORE"))
}