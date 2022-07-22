
get_comp_group <- function(score_type,
                           bin_cut=1) {
    if("CCI" %in% score_type) {
        comp_group <- paste0(">", bin_cut)
    } else if("PRS" %in% score_type) {
        comp_group <- "(Group 99% - Group 100%]"
    }
    return(comp_group)
}

get_surv_descr <- function(surv_ana,
                           surv_type) {
    plot_caption <- paste0("   Surv ~ ", surv_ana@score_type)
    if(surv_type == "HR") {
        plot_caption <- paste0(plot_caption, " Risk Group")
    }
    plot_caption <- paste0(plot_caption, " + ", get_pretty_covs_string(surv_ana@covs))
    return(plot_caption)
}

get_comp_descr <- function(surv_ana,
                             surv_type) {
    plot_caption <- ""
    if(surv_ana@score_type == "CCI" & surv_type == "HR") {
        plot_caption <- paste0("CCI >", surv_ana@bin_cut, " vs. <=", surv_ana@bin_cut)

    } else if(surv_ana@score_type == "PRS" & surv_type == "HR") {
        plot_caption <- "PRS Top 1% vs. 40-60%"
    }
    return(plot_caption)
}

#' Turns vector of covariates into a string for plots
#' 
#' @inheritParams run_surv_studies
#' @param file_name A boolean. Whether to create a file_name or
#'                   a plot title.
#'  
#' @export 
get_pretty_covs_string <- function(covs,
                                   file_name=FALSE) {
    if(!file_name) {
        covs <- stringr::str_replace_all(covs, "_", " ")
    }
    n_pcs <- sum(stringr::str_count(covs, "PC"))
    covs <- stringr::str_to_title(covs)
    if(n_pcs > 1) {
        covs <- covs[stringr::str_detect(covs, "Pc", negate=TRUE)]
        covs <- c(covs, "PCs")
    } else {
        covs[stringr::str_detect(covs, "Pc")] <- stringr::str_to_upper(covs[stringr::str_detect(covs, "Pc")])
    }
    if(!file_name) {
        pretty_string <- stringr::str_flatten(covs, collapse=" + ")
    } else {
        covs <- stringr::str_to_lower(covs)
        pretty_string <- stringr::str_flatten(covs, collapse="_")
    }
    return(pretty_string)
}

read_coxph_res_file <- function(res_dir,
                                score_type) {
    coxph_res <- create_empty_endpt_hrs_tib()
    coxph_res_dir <- paste0(res_dir, score_type, "/coxph_res/")
    res_files <- list.files(coxph_res_dir, full.names=TRUE)
    for(res_file in res_files) {
        curnt_age <- sub("(.+/)(study_)([0-9]+)(_.+)+$", "\\3", res_file)
        curnt_coxph_res <- suppressMessages(readr::read_delim(res_file, 
                                             delim="\t"))
        curnt_coxph_res <- tibble::add_column(curnt_coxph_res, 
                                              Age=curnt_age)
        coxph_res <- dplyr::bind_rows(coxph_res, curnt_coxph_res)
    }
    return(coxph_res)
}

#' Creates a subtitle for a plot based on the current study setup
#' 
#' @param study An S4 class representing the study setup.
#'  
#' @return A character. The subtitle describing the study setup.
#' 
#' @importFrom lubridate %m-%
#' @export 
#' 
#' @author Kira E. Detrois
get_study_subtitle <- function(study) {
    if(study@obs_end == as.Date("3000/01/01")) {
        study_sub <- paste0("Age: ", study@exp_age, " Exp: ", study@exp_len, " Wash: ", study@wash_len, " Obs: ", study@obs_len, " Years")
    } else {
        exp_end <- study@obs_end %m-% lubridate::years(study@wash_len + study@obs_len)
        study_sub <- paste0("Exp from Birth until ", exp_end)
    }
    return(study_sub)
}   
