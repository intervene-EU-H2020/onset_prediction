
#' Reads in the setup file in form of a list
#' 
#' Reads in the setup file, using function [IUtils::read_setup_file] and 
#' then manipulates the list elements to the classes needed for the
#' [IHRC::run_surv_studies] funciton.
#' 
#' @param setup_file_path A string. The path to the setup file.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_setup <- function(setup_file_path) {
    setup <- read_setup_file(setup_file_path)
    setup <- split_vecs(setup)

    obs_end_date <- as.Date(setup$obs_end_date)
    #setup["score_type"] <- ifelse(is.null(setup$score_type), "", setup$score_type)
    for(elem_name in names(setup)) {
        setup[[elem_name]] <- stringr::str_remove_all(setup[[elem_name]], " ")
        if(!all(stringr::str_detect(setup[[elem_name]], "\\D"))) {
            setup[[elem_name]] <- as.numeric(setup[[elem_name]])
        } else if(all(stringr::str_detect(setup[[elem_name]], "^(TRUE|FALSE)$"))) {
            setup[[elem_name]] <- as.logical(setup[[elem_name]])
        }
    }
    if(is.null(setup$endpts)) {
        setup$endpts <- IUtils::get_endpts()
    } 
    if(is.null(setup$create_score_combos)) {
        setup$create_score_combos <- FALSE
    }
    if(is.null(setup$read_pheno_score_files)) {
        setup$read_pheno_score_files <- FALSE
    }
    if(is.null(setup$min_indvs)) {
        setup$min_indvs <- 5
    }
    if(is.null(setup$filter_1998)) {
        setup$filter_1998 <- FALSE
    }
    if(is.null(setup$obs_age_range)) {
        setup$obs_age_range <- c(0,200)
    } 
    if(is.null(setup$down_fctr)) {
        setup$down_fctr <- NA_integer_
    }
    if(is.null(setup$ancs)) {
        setup$ancs <- NA_character_
    }
    if(is.null(setup$score_type)) {
        setup$score_type <- c("")
    }
    if(is.null(setup$prs_file_end)) {
        setup$prs_file_end <- "_PRS_hm3.sscore"
    }
    if(is.null(setup$prs_score_col_name)) {
        setup$prs_score_col_name <- "SCORE1_AVG"
    }
    if(is.null(setup$prs_id_col_name)) {
        setup$prs_id_col_name <- "#IID"
    }
    if(is.null(setup$write_progress)) {
        setup$write_progress <- FALSE
    }
    if(is.null(setup$bunch_phenos)) {
        setup$bunch_phenos <- FALSE
    }
    if(is.null(setup$trained_external)) {
        setup$trained_external <- FALSE
    }
    setup$obs_end_date <- obs_end_date
    return(setup)
}


#' Reads in the setup file in form of a list
#' 
#' @param setup_file_path A string. The path to the setup file.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
read_setup_file <- function(setup_file_path) {
    setup_tib <- readr::read_delim(setup_file_path, 
                                   delim="\t",
                                   col_names=FALSE, 
                                   show_col_types=FALSE)
    # Turning tibble into list
    setup <- as.list(setup_tib$X2)
    names(setup) <- setup_tib$X1

    return(setup)
}

#' Splits all the strings in the setup file into vector of strings 
#' 
#' If the strings in the list are in fact vectors splits them into a vector of strings.
#' 
#' @param setup A list. The setup information from the setup file.
#' 
#' @return The updated setup
#' 
#' @author Kira E. Detrois
#' 
#' @export
split_vecs <- function(setup) {
    for(elem_name in names(setup)) {
        if(stringr::str_detect(setup[[elem_name]], ", ")) {
            setup[elem_name] <- stringr::str_split(setup[[elem_name]], pattern=", ")
        } else if(stringr::str_detect(setup[[elem_name]], ",")) {
            setup[elem_name] <- as.vector(stringr::str_split(setup[[elem_name]], pattern=","))
        } 
    }
    return(setup)
}