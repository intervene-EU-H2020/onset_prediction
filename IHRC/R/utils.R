#' Preprocesses the score data for the analysis
#' 
#' Checks that `score_col_name` is contained in the data and renames the column
#' to `SCORE`.
#' 
#' @param score_data A data.frame with the score results for each individuals.
#'                   Should have at least column defined in `score_col_name`.
#' @param score_col_name A character. The name of the column with the scores.
#' 
#' @return The data.frame with the score column renamed to `SCORE`.
#' 
#' @export
#' 
#' @author Kira E. Detrois
preprocess_score_data <- function(score_data, 
                                  score_col_name) {
    assertthat::assert_that(score_col_name %in% colnames(score_data),
                            msg=paste0("The score_col_name ", score_col_name, " you gave is not a known column in the score_data data.frame. Have column names: ", paste0(colnames(score_data), collapse=", ")))
    dplyr::rename(score_data, "SCORE"={{ score_col_name }})
}

#' Joins the two data.frames 
#' 
#' Joins the two data.frames and removes any individuals without
#' a score available.
#' 
#' @inheritParams calc_endpt_hrs
#' 
#' @return The joined data.frame
#' 
#' @author Kira E. Detrois
join_dfs <- function(endpt_data,
                     score_data) {
    pheno_score_data <- dplyr::left_join(endpt_data,
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
n_group_cases <- function(elig_endpt_indv, 
                            groups,
                            endpt) {
    if(!(all(groups == "all"))) {
        n_cases <- c()
        for(group in groups) {
            group_data <- dplyr::filter(elig_endpt_indv, SCORE_GROUP == group)
            n_cases <- c(n_cases, 
                        Istudysetup::get_n_cases(group_data, endpt))
        }
    } else {
        n_cases <- Istudysetup::get_n_cases(elig_endpt_indv,
                                            endpt)
    }
    return(n_cases)
}

get_group_ctrls <- function(elig_endpt_indv, 
                            groups,
                            endpt) {
    if(!(all(groups == "all"))) {
        n_ctrls <- c()
        for(group in groups) {
            group_data <- dplyr::filter(elig_endpt_indv, SCORE_GROUP == group)
            n_ctrls <- c(n_ctrls, 
                        Istudysetup::get_n_ctrls(group_data, endpt))
        }
    } else {
        n_ctrls <- Istudysetup::get_n_ctrls(elig_endpt_indv,
                                            endpt)
    }
    return(n_ctrls)
}