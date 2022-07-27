#' Add column with original IDs
#' 
#' Adding back the original IDs of the individuals mapped on to the
#' numeric IDs.
#' 
#' Since \code{\link[comorbidity]{comorbidity}} needs the IDs for each
#' individual to be numeric, the CCI scores need to be calculated, with
#' the original IDs mapped onto numeric ones. Technically, it could be
#' that the original IDs are already numeric but the mapping is done
#' regardless.
#' 
#' @param cci_scores A data.frame with at least column `ID_num`, 
#'                   and ideally column `score`.
#' @param icd_data A data.frame with at least columns `ID_num` and 
#'                  `ID`.
#' 
#' @return A data.frame with an added column with the original IDs 
#'         of the individuals.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
add_back_orig_ids <- function(cci_scores, 
                              icd_data) {
    IUtils::add_map_col(cci_scores,
                        dplyr::select(icd_data, ID, ID_num),
                        "ID_num")
}