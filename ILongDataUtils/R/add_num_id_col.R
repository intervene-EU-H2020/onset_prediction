#' Adds a column with numeric IDs
#'
#' Adds a new column to the data.frame with numeric IDs which is 
#' called `ID_num`. 
#' 
#' @param long_data A data.frame with at least column `ID`.
#'
#' @return A data.frame with at least columns `ID`, and `ID_num`.
#'
#' @importFrom dplyr %>%
#' @export
#' 
#' @examples
#' long_data <- tibble::tibble(ID = c("KT011", "KT002", "KT011"), 
#'                             something = c("t1", "t2", "t3"))
#' add_num_id_col(long_data)
#' 
#' @author Kira E. Detrois
add_num_id_col <- function(long_data) {
    id_num_map <- dplyr::select(long_data, ID) %>%
                    dplyr::distinct(ID)
    id_num_map <- id_num_map %>%
                    tibble::add_column(ID_num = seq_len(nrow(id_num_map)))
    long_data <- add_map_col(long_data, id_num_map, "ID")
    return(long_data)
}
