#' Adding up scores for different ICD-versions of the same 
#' individual
#' 
#' Adds up scores for different ICD-versions of the same 
#' individual. Individuals are considered identical if they have the 
#' same value in the column `ID_num`.
#' 
#' @param cci_scores A data.frame with two columns: one called `score`
#'                   and one column defined by the variable `group_col`
#' 
#' @return A data.frame with the scores added up for each individual.
#'         This result is a tidy data.frame.
#' 
#' @export 
#' 
#' @examples 
#' cci_scores <- tibble::tibble(score = c(7, 0, 2, 3, 1),
#'                              ID_num = c(1, 1, 2, 3, 2))
#' add_up_cci_scores(cci_scores)
#' 
#' @author Kira E. Detrois
add_up_cci_scores <- function(cci_scores) {
    if(nrow(cci_scores) > 0) {
        cci_scores <- dplyr::group_by(cci_scores, ID_num) %>%
                        dplyr::summarise_all(sum)
    } 
    return(cci_scores)
}
