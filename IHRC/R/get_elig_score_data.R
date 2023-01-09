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
#' @return The data.frame with the score data for all eligible individuals
#'          under the study setup of the current survival analysis setup.
#' @export 
#' 
#' @author Kira E. Detrois
get_elig_score_data  <- function(score_type,
                                 study_data,
                                 icd_data=NULL,
                                 atc_data=NULL,
                                 prs_data=NULL,
                                 phers_data=NULL,
                                 endpt=NULL,
                                 min_indvs=5) {
    n_cases <- Istudy::get_n_cases(study_data, endpt)
    n_cntrls <- Istudy::get_n_cntrls(study_data, endpt)
    if(n_cases > min_indvs & n_cntrls > min_indvs) {
        if(any(stringr::str_detect(score_type, "(CCI)|(PRS)|(EI)|(PheRS)|(MED)"))) {
            score_data <- preprocess_score_data(score_type=score_type, 
                                                study_data=study_data,
                                                icd_data=icd_data, 
                                                atc_data=atc_data,
                                                prs_data=prs_data,
                                                phers_data=phers_data,
                                                endpt=endpt)
            if(is.null(score_data)) {
                return(NULL)
            }
            elig_score_data <- join_dfs(study_data=study_data,
                                        score_data=score_data,
                                        score_type=score_type,
                                        endpt=endpt)
        } else {
            elig_score_data <- study_data
        }
        return(elig_score_data)
    } else {
        return(tibble::tibble())
    }
}
