#' Filters out HRs that are NA, or where either bound of the CI is infinite
#'
#' @param coxph_hrs A tibble. The Cox-PH HR results. Needs to at least contain 
#'                 the columns `HR`, `CI_NEG`, and `CI_POS`.
#' @return A tibble with HRs that are not missing and where neither the upper 
#'         or lower bound of the CI is infinite.
#' @export
#' 
#' @author Kira E. Detrois.
filter_out_missing_hrs <- function(coxph_hrs) {
    dplyr::filter(coxph_hrs, 
                    !is.na(HR) &    
                    !is.infinite(CI_NEG) &
                    !is.infinite(CI_POS))
}

#' Returns the description of the survival analysis for use in plots
#'
#' @param preds A character (vector). The predictor variables 
#'              for the survival analysis
#'
#' @return A string with the description of the survival analysis 
#'           in a format suitable for use in plots.
#'
#' @export
#' 
#' @author Kira E. Detrois
get_surv_descr <- function(preds) {
    plot_caption <- paste0("   Surv ~ ")
    plot_caption <- paste0(plot_caption, 
                           paste0(reformat_preds_pretty(preds), collapse=" + "))
    return(paste0(" ", plot_caption))
}

#' Reformats predictors for plots
#' 
#' Replaces certain string patterns by shorter and more pretty versions, 
#' useful for axis labels, legends and captions.
#' 
#' The function also groups all PCs together if there are more than one.
#'
#' @param preds A character (vector). 
#'              The predictors to be reformatted.
#' @return A character vector with the reformatted predictors.
#' 
#' @export
#' 
#' @author Kira E. Detrois
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

#' Get Plot Preds
#'
#' This function returns the predictors to use when plotting hazard ratios. 
#' If `plot_preds` is not NULL, it is returned. Otherwise, `score_type` is returned.
#' 
#' @inheritParams run_surv_studies
#' 
#' @return A character vector of predictors to use when plotting hazard ratios.
#' 
#' @export
#' 
#' @author Kira E. Detrois
get_plot_preds <- function(plot_preds, 
                           score_type) {
    if(is.null(plot_preds)) {
        plot_preds <- score_type
    }
    return(plot_preds)
}

filter_plot_preds_fctr <- function(coxph_hrs,
                                plot_preds) {
    plot_preds <- stringr::str_replace_all(plot_preds, "[*]", ":")
    coxph_hrs <- dplyr::filter(coxph_hrs, VAR %in% plot_preds)
    coxph_hrs$VAR <- factor(coxph_hrs$VAR, levels=plot_preds)

    return(coxph_hrs)
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

#' Returns the plot caption for a HRs plot
#'
#' The function generates a caption that describes the study setup 
#' and the predictor variables used in the Cox-PH model.
#'
#' @param ana_details A list. Details of the survival analysis including
#'                    `study_type`, `obs_len`, `wash_len`, `exp_len`, `preds`.
#'
#' @return A string. The plot caption.
#'
#' @export 
#' 
#' @author Kira E. Detrois
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

#' Create strings for the age in the observation period for each group
#' 
#' These are used as axis labels in the ggplot for the forward study setup
#'
#' @param coxph_hrs A tibble. The Cox-PH HR results. 
#'                   Needs to at least contain the columns `EXP_AGE`-
#' @param ana_details A list. contains the values of `exp_len`,
#'                      `wash_len`, `obs_len`.
#' @return A character vector with the observation period strings
#' for each age group for the axis labels of the plot.
#'
#' @export
#'
#' @author Kira E. Detrois
get_obs_age_period_str <- function(coxph_hrs,
                                ana_details) {
    paste0(unique(coxph_hrs$EXP_AGE)+ana_details$exp_len+ana_details$wash_len, 
           "-", 
           unique(coxph_hrs$EXP_AGE+ana_details$exp_len)+ana_details$wash_len+ana_details$obs_len)
}

#' Gets the title for a plot of HRs for each 1-SD increment
#'
#' Takes the list of variables used in the Cox-PH model, and 
#' depending on the number of unique variables returns a string 
#' "1-SD Increment" or "`Variable_name` 1-SD Increment".
#' 
#' @param coxph_vars A character(vector). The variables used in the Cox-PH model.
#'  
#' @return The title of the plot.
#' @export
#'
#' @author Kira E. Detrois
get_sd_title <- function(coxph_vars) {
    if(length(unique(coxph_vars)) > 1) {
        title <- "1-SD Increment"
    } else {
        title <- paste0(unique(coxph_vars), " 1-SD Increment")
    }
    return(title)
}

