library(tidyverse)

read_bradley_ests <- function(file_path) {
    bradley_hrs <- readr::read_delim(file_path_bradley, delim=",")
    bradley_hrs <- dplyr::filter(bradley_hrs, Sample == "Full Sample")
    bradley_hrs <- dplyr::filter(bradley_hrs, Phenotype != "T1D")
    bradley_hrs <- dplyr::select(bradley_hrs, Phenotype, HR, Cipos, Cineg, Controls, Cases)
    colnames(bradley_hrs) <- c("ENDPOINT", "HR", "CI_NEG", "CI_POS", "N_CONTROLS", "N_CASES")
    return(bradley_hrs)
}

get_endpt_order <- function(coxph_file_path) {
    coxph_hrs <- readr::read_delim(file_path, delim="\t")
    coxph_hrs <- dplyr::filter(coxph_hrs, VAR == "PRS")
    coxph_hrs <- dplyr::arrange(coxph_hrs, desc(HR)) 
    endpts <- tibble::add_column(coxph_hrs, ENDPT_ORDER=1:nrow(coxph_hrs)) %>% 
                dplyr::select(ENDPOINT, ENDPT_ORDER) 
}

write_endpt_order_file <- function(coxph_file_path,
                                   out_path) {
    endpts <- get_endpt_order(coxph_file_path)
    readr::write_delim(paste0(out_path, "/endpt_order.tsv"), 
                       endpts,
                       delim="\t")
}

tib_by_endpt_order <- function(tib,
                               endpt_order) {   
    tib <- dplyr::left_join(endpt_order, tib, by="ENDPOINT", na_matches="na")
    tib <- dplyr::arrange(tib, ENDPT_ORDER)
    return(tib)
}

save_forestplotter <- function(file_path,
                               plt) {
    p_wh <- get_wh(plot = plt, unit = "in")
    ggsave(file_path,
       width=p_wh[1],
       height=p_wh[2],
       dpi=600,
       plot=plt, 
       device="png", 
       bg="white")
}

forestplotter_white_bg <- function(plt) {
    edit_plot(plt, 
               which = "background", gp = gpar(fill = "white")) 
}

forestplotter_edit_stripes <- function(plt,
                                       rows,
                                       color,
                                       skip_white=FALSE) {
    if(!skip_white)
        plt <- forestplotter_white_bg(plt)
    plt <- edit_plot(plt, 
                     row = rows, 
                     which = "background", 
                     gp = gpar(fill = color))
    return(plt)
}