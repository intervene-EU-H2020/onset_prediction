#' Adds a column with numeric IDs
#'
#' Adds a new column to the data.frame with numeric IDs which is 
#' called `ID_num`. 
#' 
#' @param icd_data A data.frame with at least column `ID`.
#'
#' @return A data.frame with at least columns `ID`, and `ID_num`.
#'
#' @importFrom dplyr %>%
#' @export
#' 
#' @examples
#' icd_data <- tibble::tibble(ID = c("KT011", "KT002", "KT011"), 
#'                             something = c("t1", "t2", "t3"))
#' add_num_id_col(icd_data)
#' 
#' @author Kira E. Detrois
add_num_id_col <- function(icd_data) {
    id_num_map <- dplyr::select(icd_data, ID) %>%
                    dplyr::distinct(ID)
    id_num_map <- id_num_map %>%
                    tibble::add_column(ID_num = seq_len(nrow(id_num_map)))
    icd_data <- IUtils::add_map_col(icd_data, id_num_map, "ID")
    return(icd_data)
}
