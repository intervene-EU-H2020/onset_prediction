#' Adds new column mapped to another column
#' 
#' Adds a new column to a data.frame from a map with a common column.
#'
#' @param long_data A data.frame with at least the column defined
#'                  in variable `common_col`.
#' @param map_tib A data.frame with exactly two columns, 
#'                the `common_col`, and the new column to add.
#' @param common_col A string. 
#'                   The common column as basis of the mapping.
#'
#' @return The data.frame with the new mapped column added.
#' 
#' @export
#' 
#' @examples 
#' long_data <- tibble::tibble(ID_num = c(3, 1), 
#'                             something = c("test1", "test2"))
#' map_tib <- tibble::tibble(ID = c("KT012", "KT0123", "KT012"), 
#'                           ID_num = c(1, 3, 1))
#' add_map_col(long_data, map_tib, common_col="ID_num")
#' 
#' @author Kira E. Detrois
add_map_col <- function(long_data, 
                        map_tib, 
                        common_col) {
    dplyr::left_join(long_data,
                     # Makes sure not to add multiple rows
                     # just because there's the same
                     # entry in the map multiple times.
                     dplyr::distinct(map_tib, .keep_all = TRUE),
                     by = {{common_col}})
} 