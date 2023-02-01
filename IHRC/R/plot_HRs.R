#' Plots the HR from the Cox-PH model
#' 
#' For forward study creates separate plots for each
#' endpoint for the different age studies. For 
#' the backward study creates a single plot with the
#' different endpoints.
#' 
#' It then filters out any missing HRs and filters the coxph_hrs tibble 
#' to only include the variables that are specified in the `plot_preds`
#'  
#' @param coxph_hrs A tibble. The Cox-PH HR results. Needs
#'                      to at least contain the columns `ENDPOINT`,
#'                      `HR`, `CI_NEG`, `CI_POS`, and `GROUP`.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#'  
#' @return A ggplot object with the HRs.
#' @export 
#' 
#' @import ggplot2
#' 
#' @author Kira E. Detrois
plot_hrs <- function(coxph_hrs=NULL,
                     study_setup,
                     surv_ana) {
    coxph_hrs <- filter_out_missing_hrs(coxph_hrs)
    # filter out variables that are not in the plot_preds list
    coxph_hrs <- filter_plot_preds_fctr(coxph_hrs,
                                        surv_ana@plot_preds)
    crnt_coxph_hrs <- dplyr::filter(coxph_hrs, GROUP == "no groups")

    if(nrow(crnt_coxph_hrs) > 0) {
        if(study_setup@study_type == "forward") {
            #plt <- plot_age_sd_hrs(coxph_hrs=crnt_coxph_hrs, 
            #                       study=study_setup,
            #                       surv_ana=surv_ana)
            return(NULL)
        } else {
            plt <- plot_endpt_sd_hr(coxph_hrs=crnt_coxph_hrs, 
                                    study_setup=study_setup,
                                    surv_ana=surv_ana)
            return(plt)
        }
    }
}

#' Plots the HR from the Cox-PH model of a backwards study
#'   
#' Creates a single ggplot with the different endpoints HRs.
#' 
#' @param coxph_hrs A tibble. The Cox-PH HR results. Needs
#'                      to at least contain the columns `ENDPOINT`,
#'                      `HR`, `CI_NEG`, `CI_POS`, and `GROUP`.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#'   
#' @export 
#' 
#' @author Kira E. Detrois
plot_endpt_sd_hr <- function(coxph_hrs,
                             study_setup,
                             surv_ana) {

    plt <- get_endpt_sd_hr_ggplot(coxph_hrs=coxph_hrs,
                                  study_setup=study_setup,
                                  preds=surv_ana@preds)
    # Saving resulting plot to file
    if(surv_ana@write_res) {
        file_path <- get_full_file_name_path(res_type="HR",
                                             study_setup=study_setup,
                                             surv_ana=surv_ana)

        save_plt(file_path=file_path,
                 plt=plt,
                 width=14,
                 height=get_endpt_fig_height(coxph_hrs$VAR))
    }
    return(plt)
}

#' Plots the HR from the Cox-PH model of a forward study
#'   
#' Creates separate plots for each endpoint for the different age studies.
#' Plots both the risk group stratified and continuous HRs.
#' 
#' @param coxph_hrs A tibble. The Cox-PH HR results. Needs
#'                      to at least contain the columns `ENDPOINT`,
#'                      `HR`, `CI_NEG`, `CI_POS`, and `GROUP`.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#'   
#' @export 
#' 
#' @author Kira E. Detrois
plot_age_sd_hrs <- function(coxph_hrs,
                            study_setup,
                            surv_ana) {
    # Want all plots created for the same setup to have same axis limits
    max_y <- min(max(c(2, round(coxph_hrs$CI_POS+0.5)), na.rm=TRUE), 10)
    min_y <- min(c(0, round(coxph_hrs$CI_NEG-0.5)), na.rm=TRUE)
    
    endpts <- unique(coxph_hrs$ENDPOINT)

    for(endpt in endpts) {
        # Getting current endpoint HR data
        endpt_coxph_hrs <- dplyr::filter(coxph_hrs, 
                                         ENDPOINT == endpt)
        study@endpt <- endpt
        # Plotting
        if(nrow(endpt_coxph_hrs) > 0) {
            plt <- get_age_sd_hr_ggplot(coxph_hrs=endpt_coxph_hrs,
                                        study_setup=study@study_setup,
                                        preds=surv_ana@preds,
                                        endpt=study@endpt,
                                        min_y=min_y,
                                        max_y=max_y)
            # Saving resulting plot to file
            if(surv_ana@write_res) {
                file_path <- get_full_file_name_path(res_type="HR", 
                                                     study_setup=study_setup,
                                                     endpt=endpt, 
                                                     surv_ana=surv_ana)
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
