
#' Saves a ggplot object to a file
#'
#' Save a ggplot object as an png image file
#'
#' @param file_path A string. The file path to save the plot to.
#' @param plt ggplot object. The plot to be saved.
#' @param width numeric. The width of the image file.
#' @param height numeric. The height of the image file.
#' 
#' @export
#' 
#' @author Kira E. Detrois
save_plt <- function(file_path, 
                     plt,
                     width,
                     height) {
    if(!is.null(file_path) & !is.null(plt)) {
                    ggsave(file_path,
                        width=width,
                        height=height,
                        dpi=600,
                        plot=plt, 
                        device="png", 
                        bg="white")
    }
}

#' Get the height of the plot depending on number of predictor variables
#'
#' @inheritParams get_sd_title
#' 
#' @return An integer indicating the height of the plot
#' @export
#' 
#' @author Kira E. Detrois
get_endpt_fig_height <- function(coxph_vars) {
    n_preds <- length(unique(coxph_vars))
    dplyr::case_when(
        n_preds == 1 ~ 10,
        n_preds == 2 ~ 10,
        n_preds == 3 ~ 10,
        n_preds == 4 ~ 11,
        n_preds == 5 ~ 14,
    )
}
