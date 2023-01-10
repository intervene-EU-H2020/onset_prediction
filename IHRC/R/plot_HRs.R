#' Plots the HR from the Cox-PH model
#' 
#' For forward study creates separate plots for each
#' endpoint for the different age studies. For 
#' the backward study creates a single plot with the
#' different endpoints.
#' 
#' It then filters out any missing HRs and filters the coxph_hrs tibble 
#' to only include the variables that are specified in the 
#' ana_details$plot_preds.
#'  
#' @param coxph_hrs A tibble. The Cox-PH HR results. Needs
#'                      to at least contain the columns `ENDPOINT`,
#'                      `HR`, `CI_NEG`, `CI_POS`, and `GROUP`.
#' @param surv_ana A S4 surv_ana object. The analysis setup.
#' @param from_file A logical, denotes whether the results are from a 
#'                  file or not. Default: FALSE.
#' @param ana_details A list. The analysis details for the study.
#'                   See function [IHRC::get_ana_details_from_surv_ana]. 
#' @param sort_hrs A logical. Should the HRs be sorted for the backward 
#'                            study plot. Default: FALSE.
#' @param fig_height A numeric. The height of the plot.
#' @param fig_width A numeric. The width of the plot.
#'  
#' @return A ggplot object with the HRs.
#' @export 
#' 
#' @import ggplot2
#' 
#' @author Kira E. Detrois
plot_hrs <- function(coxph_hrs=NULL,
                     surv_ana=NULL,
                     from_file=FALSE,
                     ana_details,
                     sort_hrs=FALSE,
                     fig_height=NULL,
                     fig_width=NULL) {
    if(!from_file) {
        ana_details <- get_ana_details_from_surv_ana(surv_ana)
    } 
    coxph_hrs <- filter_out_missing_hrs(coxph_hrs)
    # filter out variables that are not in the plot_preds list
    coxph_hrs <- filter_plot_preds_fctr(coxph_hrs,
                                        ana_details$plot_preds)
    curnt_coxph_hrs <- dplyr::filter(coxph_hrs, GROUP == "no groups")

    if(nrow(curnt_coxph_hrs) > 0) {
        if(ana_details$study_type == "forward") {
            plt <- plot_age_sd_hrs(curnt_coxph_hrs, 
                                   ana_details)
        } else {
            plt <- plot_endpt_sd_hr(coxph_hrs=curnt_coxph_hrs, 
                                    ana_details=ana_details,
                                    sort_hrs=sort_hrs,
                                    fig_height=fig_height,
                                    fig_width=fig_width)
            return(plt)
        }
    }
}

#' Plots the HR from the Cox-PH model of a backwards study
#'   
#' Creates a single ggplot with the different endpoints HRs.
#' 
#' @inheritParams plot_hrs
#' 
#' @export 
#' 
#' @author Kira E. Detrois
plot_endpt_sd_hr <- function(coxph_hrs,
                             ana_details,
                             sort_hrs,
                             fig_height,
                             fig_width) {

    plt <- get_endpt_sd_hr_ggplot(coxph_hrs=coxph_hrs,
                                  ana_details=ana_details,
                                  sort_hrs=sort_hrs)
    # Saving resulting plot to file
    if(ana_details$write_res) {
        file_path <- check_and_get_file_path(ana_details, res_type="HR")

        save_plt(file_path=file_path,
                 plt=plt,
                 width=ifelse(!is.null(fig_width), fig_width, 14),
                 height=ifelse(!is.null(fig_height), fig_height, get_endpt_fig_height(coxph_hrs$VAR)))
    }
    return(plt)
}

#' Plots the HR from the Cox-PH model of a forward study
#'   
#' Creates separate plots for each endpoint for the different age studies.
#' Plots both the risk group stratified and continuous HRs.
#' 
#' @inheritParams plot_hrs
#' 
#' @export 
#' 
#' @author Kira E. Detrois
plot_age_sd_hrs <- function(coxph_hrs,
                            ana_details) {
    # Want all plots created for the same setup to have same axis limits
    max_y <- min(max(c(2, round(coxph_hrs$CI_POS+0.5)), na.rm=TRUE), 10)
    min_y <- min(c(0, round(coxph_hrs$CI_NEG-0.5)), na.rm=TRUE)
    
    endpts <- unique(coxph_hrs$ENDPOINT)

    for(endpt in endpts) {
        # Getting current endpoint HR data
        endpt_coxph_hrs <- dplyr::filter(coxph_hrs, 
                                         ENDPOINT == endpt)
        ana_details$endpt <- endpt
        # Plotting
        if(nrow(endpt_coxph_hrs) > 0) {
            plt <- get_age_sd_hr_ggplot(coxph_hrs=endpt_coxph_hrs,
                                        ana_details=ana_details,
                                        min_y=min_y,
                                        max_y=max_y)
            # Saving resulting plot to file
            if(ana_details$write_res) {
                file_path <- check_and_get_file_path(ana_details, res_type="HR")
                save_plt(file_path=file_path,
                         plt=plt,
                         width=7,
                         height=7)
            }
            return(plt)
        }
    }
    return(NULL)
}
