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
    if(N_colors == 1)
      c("#000000")
    else if(N_colors == 2) {
      c("#D5694F", "#224767")
    } else if(N_colors == 3)  {
      c("#D5694F", "#224767", "#EAB034")
    } else if(N_colors == 4) {
      c("#D5694F", "#224767", "#EAB034", "#748AAA")
    } else if(N_colors == 5) {
      c("#D5694F", "#224767", "#EAB034", "#748AAA", "#9E577C")
    }
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
#' @import ggplot2
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
      plot.caption=ggplot2::element_text(size=base_size/2, hjust=0),
      rect = ggplot2::element_blank(),
      text = ggplot2::element_text(
        colour = "black"
      )
    ) %+replace%
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0
      ),
      axis.text.y = ggplot2::element_text(
        colour = "black"
      ),
      axis.text.y.right = ggplot2::element_text(
        hjust = 1
      ),
      axis.text.x = ggplot2::element_text(
        colour = "black"
      ),
      panel.border = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        hjust = 0
      ),
      panel.background = ggplot2::element_rect(
        colour = NA,
        fill = NA
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = "gray50",
        size = 0.25,
        linetype = 2
      ),
      panel.grid.minor.x = ggplot2::element_blank(),
    )
}
