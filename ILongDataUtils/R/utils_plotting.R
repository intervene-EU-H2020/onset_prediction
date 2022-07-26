#' Custom color selection 
#' 
#' Based on https://colorbrewer2.org/#type=diverging&scheme=RdBu&n=10
#' 
#' @param N_colors The number of colors
#' 
#' @author Kira E. Detrois
#' 
#' @export 
custom_colors_brewer <- function(N_colors) {
    cols <- dplyr::case_when(
        N_colors == 2 ~ c("#0571B0","#CA0020"),
        N_colors == 3 ~ c("#0571b0","#CA0020", "#424B54"),
        N_colors == 4 ~ c("#0571B0","#CA0020", "#424B54", "#92C5DE"),
        N_colors == 5 ~ c("#0571B0","#CA0020", "#424B54", "#92C5DE", "#F4A582")
    )
    return(cols)
}

#' Custom theme for plottig
#' 
#' Based on the 
#' @param base_size Text size
#' @param base_line_size Line size
#' @param base_rect_size Rectangle size
#' 
#' @author Kira E. Detrois
#'  
#' @export 
theme_custom <- function(base_size = 18,
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(
      rect = element_blank(),
      text = element_text(
        colour = "black"
      )
    ) %+replace%
    ggplot2::theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0
      ),
      axis.text.y = element_text(
        colour = "black"
      ),
      axis.text.y.right = element_text(
        hjust = 1
      ),
      axis.text.x = element_text(
        colour = "black"
      ),
      panel.border = element_blank(),
      strip.text = element_text(
        face = "bold",
        hjust = 0
      ),
      panel.background = element_rect(
        colour = NA,
        fill = NA
      ),
      panel.grid.major.x = element_line(
        colour = "gray50",
        size = 0.25,
        linetype = 2
      ),
      panel.grid.minor.x = element_blank(),
    )
}
