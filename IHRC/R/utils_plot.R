#' Filters out HRs that are NA, or where either bound of the CI is infinite
filter_out_missing_hrs <- function(coxph_hrs) {
    dplyr::filter(coxph_hrs, 
                    !is.na(HR) &    
                    !is.infinite(CI_NEG) &
                    !is.infinite(CI_POS))
}

#' Creates a string describing the survival model
#' 
get_surv_descr <- function(preds) {
    plot_caption <- paste0("   Surv ~ ")
    plot_caption <- paste0(plot_caption, 
                           paste0(reformat_preds_pretty(preds), collapse=" + "))
    return(paste0(" ", plot_caption))
}

#' Reformats predictors for plots
reformat_preds_pretty <- function(preds) {
    preds <- stringr::str_replace(preds, "YEAR_OF_BIRTH", "YoB")
    preds <- stringr::str_replace(preds, "BATCH", "Batch")        
    preds <- stringr::str_replace(preds, "EDU", "Edu")
    preds <- stringr::str_replace(preds, "SEX", "Sex")        
    n_pcs <- sum(stringr::str_count(preds, "PC"))
    if(n_pcs > 1) {
        preds <- preds[stringr::str_detect(preds, "PC", negate=TRUE)]
        preds <- c(preds, "PCs")
    } 

    return(preds)
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
    if(study@obs_end_date == as.Date("3000/01/01")) {
        study_sub <- paste0("Age: ", study@exp_age, " Exp: ", study@exp_len, " Wash: ", study@wash_len, " Obs: ", study@obs_len, " Years")
    } else {
        exp_end <- study@obs_end_date %m-% lubridate::years(study@wash_len + study@obs_len)
        study_sub <- paste0("Exp from Birth until ", exp_end)
    }
    return(study_sub)
} 


get_caption <- function(ana_details) {
    if(ana_details$study_type == "backward") {
        study_caption <- paste0("Obs: ", ana_details$obs_len, 
                                " Years until ", ana_details$obs_end_date, 
                                " Wash: ", ana_details$wash_len)
        if(!all(is.na(ana_details$exp_len))) {
            study_caption <- paste0(study_caption, 
                                    " Exp: ", ana_details$exp_len)
        }
        study_caption <- paste0(study_caption, " Years")
    } else {
        study_caption <- paste0("Exp: ", ana_details$exp_len, 
                                " Wash: ", ana_details$wash_len, 
                                " Obs: ",  ana_details$obs_len, " Years")
    }
    caption <- paste0(study_caption, get_surv_descr(ana_details$preds))
    return(caption)
}


get_obs_per_strings <- function(coxph_hrs,
                                ana_details) {
    paste0(unique(coxph_hrs$EXP_AGE)+ana_details$exp_len+ana_details$wash_len, 
           "-", 
           unique(coxph_hrs$EXP_AGE+ana_details$exp_len)+ana_details$wash_len+ana_details$obs_len)
}

get_sd_title <- function(coxph_vars) {
    if(length(unique(coxph_vars)) > 1) {
        title <- "1-SD Increment"
    } else {
        title <- paste0(unique(coxph_vars), " 1-SD Increment")
    }
    return(title)
}