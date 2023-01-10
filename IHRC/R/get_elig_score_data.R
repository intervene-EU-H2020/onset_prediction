#' Extract Eligibility Score Data 
#' 
#' Extract the score and phenotype data of eligible cases and controls for 
#' the Cox-PH analysis of a given endpoint.
#' 
#' If score type includes `CCI`, `PRS`, `EI`, `PheRS`, or `MED`, it also 
#' calls [IHRC::preprocess_score_data] to generate a data frame with the required 
#' score data, and then joins the resulting score data with the phenotypic data
#' on the eligible individuals, using the function [IHRC::join_dfs].
#'  
#' It filters the study data to include only endpoints with both
#' cases and controls that have at least a minimum number of individuals. 
#' 
#' 
#' @return The data.frame with the score data for all eligible individuals
#'          under the study setup of the current survival analysis setup.
#'          Otherwise returns NULL. Also returns NULL if there is something 
#'          wrong with the `score_data`.
#' 
#' @examples
#' get_elig_score_data("CCI", pheno_data, icd_data, endpt="J10_ASTHMA")
#' 
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

    # Minimum number of indvs for both case and controls available
    if(n_cases > min_indvs & n_cntrls > min_indvs) {
        # Need to preprocess score data for these score types
        if(any(stringr::str_detect(score_type, "(CCI)|(PRS)|(EI)|(PheRS)|(MED)|(EDU)"))) {
            score_data <- preprocess_score_data(score_type=score_type, 
                                                study_data=study_data,
                                                icd_data=icd_data, 
                                                atc_data=atc_data,
                                                prs_data=prs_data,
                                                phers_data=phers_data,
                                                endpt=endpt)
            if(is.null(score_data)) {
                elig_score_data <- NULL
            # Joining score data with phenotypic data
            } else {
                elig_score_data <- join_dfs(study_data=study_data,
                                            score_data=score_data,
                                            score_type=score_type,
                                            endpt=endpt)
            }
        # Otherwise only using the phenotypic data
        } else {
            elig_score_data <- study_data
        }
    } else {
        elig_score_data <- NULL
    }
    
    return(elig_score_data)
}
