#' Map CCI scores onto IDs
#' 
#' Maps the scores obtained by \code{\link[comorbidity]{score}} back
#' onto the individuals numeric IDs
#' 
#' CAREFUL!! The function score sorts the individuals by their 
#' (numeric) names. 
#' 
#' @param cci_scores A data.frame with at least column `ID_num`, and 
#'                   column `score`.
#' @param IDs_num A numeric. The numeric IDs of the individuals.
#' 
#' @return A tibble with columns `ID_num`, and `score`.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
map_cci_scores_to_num_ids <- function(cci_scores, 
                                      IDs_num) {
    IDs_sorted <- sort(unique(IDs_num))
    cci_scores_tib <- tibble::tibble(ID_num=IDs_sorted,
                                     score=cci_scores)
    return(cci_scores_tib)
}
