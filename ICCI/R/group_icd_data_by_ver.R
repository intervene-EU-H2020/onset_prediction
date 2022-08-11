#' Groups the data according to the ICD-versions
#' 
#' Groups the data.frame according to the ICD-versions, using
#' function \code{\link[dplyr]{group_by}}.
#'
#' @param long_data A data.frame with at least column `ICD_version`.
#'
#' @return A list(`groups`, `group_keys`, `group_idxs`): 
#'         \itemize{
#'          \item `groups`: The grouped tibble. 
#'          \item `group_keys`: A list. The name of each group, so the 
#'                              ICD-versions present in the data.
#'          \item `group_idxs`: A named list. The indices of the 
#'                              elements of each group in the original 
#'                              `long_data`. The names are the group 
#'                              names.
#'         }
#' @importFrom dplyr %>%
#' @export
group_icd_data_by_ver <- function(long_data) {
    groups <- long_data %>%
                dplyr::group_by(ICD_version, .drop=FALSE)
    group_keys <-  groups %>%
                    dplyr::group_keys() %>%
                    dplyr::pull(ICD_version)
    group_idxs <- groups %>%
                    dplyr::group_rows()
    names(group_idxs) <- group_keys

    return(list(data = groups,
                keys = group_keys,
                idxs = group_idxs))
}
