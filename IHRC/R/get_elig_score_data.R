#' Gets the full score and phenotype data on the eligible individuals 
#' 
#' Should be run before any analyis on the survival setup is run.
#' 
#' For `CCI` calculates the CCI score based on the ICD-data.
#' Also, renames the score column to `SCORE` and filters out NAs.
#' For the PRS data the score columsn have names in the
#' form of `J10_ASTHMA_PRS`. The function renames the
#' current PRS column of interest to `SCORE`. Then it
#' filters out all NAs in the column. See function 
#' \code{\link{get_prs_endpt_scores}}.
#' 
#' Joins the resulting score data with the phenotypic data
#' on the eligible individuals.
#' 
#' Checks wether there are enough individuals in the case
#' and control groups. Otherwise returns an empty data.frame. 
#' 
#' @inheritParams add_risk_group_col
#' 
#' @return The data.frame with the score data for all eligible individuals
#'          under the study setup of the current survival analysis setup.
#' 
#' @author Kira E. Detrois
get_elig_score_data  <- function(surv_ana) {
    if(Istudy::get_n_cases(surv_ana@study@study_data, surv_ana@study@endpt) > surv_ana@min_indvs &
        Istudy::get_n_cntrls(surv_ana@study@study_data, surv_ana@study@endpt) > surv_ana@min_indvs) {
        curnt_score_data <- get_curnt_score_data(surv_ana)
        pheno_score_data <- join_dfs(pheno_data=surv_ana@study@study_data, 
                                     score_data=curnt_score_data,
                                     score_type=surv_ana@score_type,
                                     endpt=surv_ana@study@endpt)
        return(pheno_score_data)
    } else {
        return(tibble::tibble())
    }
}
